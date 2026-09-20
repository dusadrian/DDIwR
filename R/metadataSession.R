# Revision-bound metadata session. Stored metadata is captured immediately;
# observation values and their analysis are captured only for requested columns.
.metadataSessionFields <- c(
    "identity",
    "annotations",
    "classification",
    "weight_signal",
    "summaries",
    "categories",
    "category_metadata",
    "dialogr",
    "xml"
)

.metadataSessionValueFields <- c(
    "classification",
    "weight_signal",
    "summaries",
    "categories",
    "xml"
)

.metadataSessionValidateRevision <- function(revision) {
    if (is.null(revision) || !is.atomic(revision) || length(revision) != 1L ||
        anyNA(revision)) {
        stop("Revision must be one non-missing atomic value.")
    }

    return(invisible(NULL))
}

.metadataSessionValidate <- function(session) {
    if (!inherits(session, "ddiwr_metadata_session") || !is.environment(session)) {
        stop("Invalid metadata session.")
    }

    if (isTRUE(session$closed)) {
        stop("Metadata session is closed.")
    }

    return(invisible(NULL))
}

.metadataSessionAssertRevision <- function(session, revision) {
    .metadataSessionValidate(session)
    .metadataSessionValidateRevision(revision)

    if (isTRUE(session$stale)) {
        stop("Metadata session is stale and must be captured again.")
    }

    if (!identical(revision, session$revision)) {
        session$stale <- TRUE
        session$invalidated_by <- revision

        stop("Metadata session revision does not match the host revision.")
    }

    return(invisible(NULL))
}

.metadataSessionPlan <- function(fields) {
    if (!is.character(fields) || anyNA(fields) || anyDuplicated(fields) ||
        any(!is.element(fields, .metadataSessionFields))) {
        stop("Fields must be distinct supported projection names.")
    }

    value_fields <- fields[is.element(fields, .metadataSessionValueFields)]

    return(list(
        fields = fields,
        metadata_only = length(value_fields) == 0L,
        value_fields = value_fields,
        requires_values = length(value_fields) > 0L
    ))
}

.metadataSessionEmptyAnalysis <- function() {
    return(list(
        var_dcml = numeric(),
        var_width = numeric(),
        range_units = character(),
        val_min = numeric(),
        val_max = numeric(),
        stat_min = numeric(),
        stat_max = numeric(),
        stat_mean = numeric(),
        stat_medn = numeric(),
        stat_stdev = numeric(),
        sum_valid = numeric(),
        sum_invalid = numeric(),
        cat_counts = integer(),
        cat_values = character(),
        cat_labels = character(),
        cat_missing = logical(),
        cat_freq = numeric(),
        variable_type = character(),
        weight_numeric_compatible = logical(),
        weight_has_labels = logical(),
        weight_has_observed = logical(),
        weight_has_negative = logical()
    ))
}

.metadataSessionCombineAnalysis <- function(session, positions) {
    empty <- .metadataSessionEmptyAnalysis()
    category_fields <- c("cat_values", "cat_labels", "cat_missing", "cat_freq")
    result <- lapply(names(empty), function(field) {
        values <- session$analysis_cache[[field]][positions]

        if (is.element(field, category_fields)) {
            values <- unlist(values, use.names = FALSE)
        }

        return(switch(typeof(empty[[field]]),
            double = as.numeric(values),
            integer = as.integer(values),
            logical = as.logical(values),
            character = as.character(values)
        ))
    })
    names(result) <- names(empty)

    return(result)
}

.metadataSessionDates <- function(variables) {
    return(unname(vapply(variables, function(variable) {
        return(
            identical(variable$varFormat, "date") ||
            is.element("Date", getElement(variable, "classes"))
        )
    }, logical(1))))
}

.metadataSessionEnsureAnalysis <- function(session, positions) {
    required <- unique(positions)
    missing <- required[!session$analysis_ready[required]]

    if (length(missing) == 0L) {
        return(invisible(NULL))
    }

    values <- .metadataSessionValues(session, missing)
    variables <- .metadataSessionVariables(session, missing)
    dates <- .metadataSessionDates(variables)
    analysis <- collectDataDscrStatsC(
        values,
        variables,
        dates,
        include_projection = TRUE
    )
    category_fields <- c("cat_values", "cat_labels", "cat_missing", "cat_freq")

    for (field in setdiff(names(analysis), category_fields)) {
        session$analysis_cache[[field]][missing] <- analysis[[field]]
    }

    groups <- rep(seq_along(missing), times = analysis$cat_counts)

    for (field in category_fields) {
        segments <- split(analysis[[field]], factor(groups, levels = seq_along(missing)))
        session$analysis_cache[[field]][missing] <- unname(segments)
    }

    session$analysis_ready[missing] <- TRUE

    session$analysis_scans <- session$analysis_scans + 1L
    session$analysed_columns <- session$analysed_columns + length(missing)

    return(invisible(NULL))
}

.metadataSessionValues <- function(session, positions, attributes = FALSE) {
    required <- unique(positions)
    missing <- required[!session$values_ready[required]]

    if (length(missing) > 0L) {
        values <- .Call("metadata_values_copy", session$source,
            as.integer(missing), PACKAGE = "DDIwR")
        session$value_cache[missing] <- values
        session$values_ready[missing] <- TRUE
    }

    if (all(session$values_ready)) {
        session$source <- NULL
    }

    result <- session$value_cache[positions]
    names(result) <- session$names[positions]

    if (attributes) {
        for (index in seq_along(positions)) {
            attributes(result[[index]]) <- session$attributes[[positions[[index]]]]
        }

        dataset_attributes <- session$dataset_attributes
        dataset_attributes$names <- session$names[positions]
        attributes(result) <- dataset_attributes

        return(result)
    }

    return(structure(result, class = "data.frame", row.names = session$row_names))
}

.metadataSessionVariables <- function(session, positions) {
    required <- unique(positions)
    missing <- required[!session$metadata_ready[required]]

    if (length(missing) > 0L) {
        records <- metadataSnapshotRead(session$metadata, missing)
        shells <- lapply(seq_along(missing), function(index) {
            record <- records[[index]]
            column <- vector(session$storage[[missing[[index]]]], 0L)
            names(record)[names(record) == "classes"] <- "class"
            attributes(column) <- record

            return(column)
        })
        names(shells) <- session$names[missing]
        shells <- structure(shells, class = "data.frame", row.names = integer())
        variables <- collectRMetadata(shells, infer_type = FALSE, include_formats = FALSE)
        session$metadata_cache[missing] <- variables
        session$metadata_ready[missing] <- TRUE
    }

    variables <- session$metadata_cache[positions]
    names(variables) <- session$names[positions]

    for (index in seq_along(positions)) {
        variables[[index]]$ID <- session$ids[[positions[[index]]]]
    }

    return(variables)
}

#' Reusable metadata sweep sessions
#'
#' @description Capture metadata once and request ordered subsets for progressive
#' displays, DialogR variable selectors, or DDI Codebook generation.
#' @param from A data frame. The host must invalidate the session after edits,
#' including edits made by reference.
#' @param revision One non-missing atomic revision token, supplied by the host.
#' Every read must supply the current token; a mismatch makes the session stale.
#' @param session An object returned by `metadataSweepCapture()`.
#' @param columns Integer positions, allowing reordering and repetition, or
#' unambiguous variable names. `NULL` selects all columns; `integer()` selects none.
#' @param fields Requested field groups. `NULL` uses the selected profile.
#' @param profile One of `progressive`, `dialogr`, or `codebook`.
#' @param xml_options Named arguments passed to the variable XML generator.
#' @details
#' Capture copies stored metadata for all columns without cleaning labels or
#' scanning observations. Normalization is cached only for requested columns.
#' The progressive profile returns identity, annotations and category definitions
#' without scanning values. Request classification, weight_signal, summaries or
#' categories to analyse only the selected columns and cache their results.
#' Category definitions (`category_metadata`) and frequencies (`categories`) are
#' separate requests. The latter retain DDI's labelled-category semantics.
#' Category definitions include UI rows plus the original typed `labels`,
#' `levels`, `na_values` and `na_range`, without cleaning or inference.
#'
#' The dialogr profile returns row records in `projection$dialogr`, following
#' DialogR's type, measurement, category, display and selector-flag rules.
#' Its value-dependent interpretation is cached per column, separately from DDI
#' statistical analysis. Bare observation copies are shared by both consumers.
#' The codebook profile returns variable XML and export metadata, reusing cached
#' unweighted analysis. Weighted XML retains the existing weighted export path.
#'
#' Generated IDs remain stable for the life of the session. Numeric positions are
#' required to distinguish duplicate names. Hosts schedule batches and yield to
#' their UI between calls; this API does not start background browser workers.
#' Sessions are process-local and must be recaptured after serialization or edits.
#' Value-dependent requests support logical, integer, double and character
#' columns; unsupported storage (such as list columns) can be read as metadata
#' but raises an error when values are requested.
#' `metadataSweepClose()` releases caches and is idempotent; a finalizer also
#' releases them. Closing or invalidating a session never changes the input data.
#' @return Capture returns a session. Read returns an ABI-versioned columnar
#' envelope with revision, columns, completed_fields, projection and cache counters.
#' Invalidate and close return invisible NULL.
#' @seealso [exportCodebook()], [collectMetadata()]
#' @examples
#' session <- metadataSweepCapture(iris, revision = 1L)
#' metadataSweepRead(session, 1L, columns = 1:2)
#' metadataSweepRead(session, 1L, columns = "Species", profile = "dialogr")
#' metadataSweepRead(session, 1L, profile = "codebook")
#' book <- makeElement("codeBook", children = list(makeElement("stdyDscr", fill = TRUE)))
#' path <- tempfile(fileext = ".xml")
#' exportCodebook(book, path, session = session, revision = 1L)
#' unlink(path)
#' metadataSweepClose(session)
#' @export
metadataSweepCapture <- function(from, revision) {
    if (!is.data.frame(from)) {
        stop("The input must be a data frame.")
    }

    .metadataSessionValidateRevision(revision)

    metadata <- metadataSnapshotRaw(from, fields = c("classes", "label",
        "measurement", "labels", "levels", "na_values", "na_range", "xmlang", "ID"))
    complete <- FALSE

    on.exit({
        if (!complete) {
            metadataSnapshotClose(metadata)
        }
    })

    session <- new.env(parent = emptyenv())

    class(session) <- "ddiwr_metadata_session"
    session$metadata <- metadata
    session$attributes <- lapply(from, attributes)
    session$dataset_attributes <- attributes(from)
    session$ids <- unname(vapply(from, function(column) {
        id <- attr(column, "ID", exact = TRUE)

        if (is.null(id) || length(id) == 0L || is.na(id[[1]]) ||
            !nzchar(as.character(id[[1]]))) {
            return(NA_character_)
        }

        return(as.character(id[[1]]))
    }, character(1)))
    missing_ids <- is.na(session$ids)

    if (any(missing_ids)) {
        session$ids[missing_ids] <- generateID(sum(missing_ids))
    }

    session$source <- from
    session$revision <- revision
    session$invalidated_by <- NULL
    session$stale <- FALSE
    session$closed <- FALSE
    session$columns <- ncol(from)
    session$names <- names(from)
    session$storage <- unname(vapply(from, typeof, character(1)))
    session$lengths <- unname(vapply(from, length, integer(1)))
    session$row_names <- attr(from, "row.names", exact = TRUE)
    session$value_cache <- vector("list", ncol(from))
    session$metadata_cache <- vector("list", ncol(from))
    session$metadata_ready <- rep(FALSE, ncol(from))
    session$values_ready <- rep(FALSE, ncol(from))
    session$dialog_cache <- vector("list", ncol(from))
    session$dialog_ready <- rep(FALSE, ncol(from))
    session$analysis_cache <- lapply(.metadataSessionEmptyAnalysis(), function(field) {
        return(field[rep(NA_integer_, ncol(from))])
    })

    for (field in c("cat_values", "cat_labels", "cat_missing", "cat_freq")) {
        session$analysis_cache[[field]] <- vector("list", ncol(from))
    }
    session$analysis_ready <- rep(FALSE, ncol(from))
    session$analysis_scans <- 0L
    session$analysed_columns <- 0L

    reg.finalizer(session, function(candidate) {
        if (!isTRUE(candidate$closed)) {
            try(metadataSweepClose(candidate), silent = TRUE)
        }
    }, onexit = TRUE)

    complete <- TRUE

    return(session)
}

#' @rdname metadataSweepCapture
#' @export
metadataSweepRead <- function(session, revision, columns = NULL,
    fields = NULL, xml_options = list(), profile = "progressive") {
    .metadataSessionAssertRevision(session, revision)

    positions <- .metadataSessionPositions(session, columns)
    profiles <- list(
        progressive = c("identity", "annotations", "category_metadata"),
        dialogr = "dialogr",
        codebook = c("identity", "xml")
    )

    if (!is.character(profile) || length(profile) != 1L || is.na(profile) ||
        !is.element(profile, names(profiles))) {
        stop("Unknown metadata profile.")
    }

    if (is.null(fields)) {
        fields <- profiles[[profile]]
    }

    if (
        !is.list(xml_options) ||
        is.null(names(xml_options)) && length(xml_options) > 0L
    ) {
        stop("XML options must be a named list.")
    }

    if (any(is.element(names(xml_options),
        c("variables", "data", "session", "revision", "columns", ".analysis")))) {
        stop("XML options cannot replace projection inputs.")
    }

    plan <- .metadataSessionPlan(fields)
    variables <- vector("list", length(positions))
    names(variables) <- session$names[positions]

    if (any(is.element(fields, c("identity", "annotations", .metadataSessionValueFields)))) {
        variables <- .metadataSessionVariables(session, positions)
    }
    analysis <- NULL
    selected <- structure(
        vector("list", length(positions)),
        names = session$names[positions],
        class = "data.frame",
        row.names = session$row_names
    )

    if (plan$requires_values) {
        .metadataSessionEnsureAnalysis(session, positions)

        selected[] <- session$value_cache[positions]
        analysis <- .metadataSessionCombineAnalysis(
            session, positions
        )
    }

    result <- .metadataProjectionResult(
        variables = variables,
        selected = selected,
        positions = positions,
        fields = fields,
        analysis = analysis,
        revision = session$revision,
        xml_options = xml_options,
        identity_storage = session$storage[positions],
        identity_length = session$lengths[positions]
    )
    if (is.element("category_metadata", fields)) {
        result$projection$category_metadata <- lapply(positions, function(position) {
            return(.metadataCategoryDefinitions(session$attributes[[position]]))
        })
    }

    if (is.element("dialogr", fields)) {
        missing <- unique(positions[!session$dialog_ready[positions]])

        if (length(missing) > 0L) {
            values <- .metadataSessionValues(session, missing, attributes = TRUE)
            records <- lapply(seq_along(missing), function(index) {
                return(.metadataDialogVariable(names(values)[[index]], values[[index]]))
            })
            session$dialog_cache[missing] <- records
            session$dialog_ready[missing] <- TRUE
        }

        result$projection$dialogr <- unname(session$dialog_cache[positions])
    }
    result$profile <- profile
    result$session <- list(
        analysed_columns = session$analysed_columns,
        analysis_scans = session$analysis_scans,
        cached_columns = which(session$analysis_ready),
        dialog_columns = which(session$dialog_ready),
        normalized_columns = which(session$metadata_ready)
    )

    return(result)
}

#' @rdname metadataSweepCapture
#' @export
metadataSweepInvalidate <- function(session, revision) {
    .metadataSessionValidate(session)
    .metadataSessionValidateRevision(revision)

    session$stale <- TRUE
    session$invalidated_by <- revision

    return(invisible(NULL))
}

#' @rdname metadataSweepCapture
#' @export
metadataSweepClose <- function(session) {
    if (!inherits(session, "ddiwr_metadata_session") || !is.environment(session)) {
        stop("Invalid metadata session.")
    }

    if (isTRUE(session$closed)) {
        return(invisible(NULL))
    }

    metadataSnapshotClose(session$metadata)
    session$source <- NULL
    session$value_cache <- list()
    session$analysis_cache <- list()
    session$analysis_ready <- logical()
    session$metadata_cache <- list()
    session$metadata_ready <- logical()
    session$attributes <- list()
    session$dataset_attributes <- NULL
    session$dialog_cache <- list()
    session$dialog_ready <- logical()
    session$values_ready <- logical()
    session$closed <- TRUE

    return(invisible(NULL))
}

.metadataSessionPositions <- function(session, columns) {
    if (is.null(columns)) {
        return(seq_len(session$columns))
    }

    if (is.character(columns) && !anyNA(columns)) {
        ambiguous <- session$names[duplicated(session$names)]

        if (any(is.element(columns, ambiguous))) {
            stop("Use integer positions for duplicate variable names.")
        }

        columns <- match(columns, session$names)
    }

    if (!is.numeric(columns) || anyNA(columns) ||
        any(!is.finite(columns) | columns != trunc(columns) |
            columns < 1 | columns > session$columns)) {
        stop("Columns must be valid integer positions or unambiguous names.")
    }

    return(as.integer(columns))
}
