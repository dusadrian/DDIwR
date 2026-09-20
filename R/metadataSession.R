# Internal revision-bound metadata session. Metadata is captured immediately;
# observation values and their analysis are captured only for requested columns.
.metadataSessionFields <- c(
    "identity",
    "annotations",
    "classification",
    "weight_signal",
    "summaries",
    "categories",
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
    if (is.null(revision) || is.list(revision) || length(revision) != 1L ||
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

.metadataSessionSplitAnalysis <- function(analysis) {
    count <- length(analysis$cat_counts)
    scalar_fields <- setdiff(
        names(analysis),
        c("cat_values", "cat_labels", "cat_missing", "cat_freq")
    )
    offset <- 0L
    result <- vector("list", count)

    for (index in seq_len(count)) {
        category_count <- analysis$cat_counts[[index]]
        category_positions <- if (category_count > 0L) {
            seq.int(offset + 1L, offset + category_count)
        }
        else {
            integer()
        }
        record <- lapply(analysis[scalar_fields], function(field) {
            return(field[index])
        })

        record$cat_values <- analysis$cat_values[category_positions]
        record$cat_labels <- analysis$cat_labels[category_positions]
        record$cat_missing <- analysis$cat_missing[category_positions]
        record$cat_freq <- analysis$cat_freq[category_positions]
        result[[index]] <- record
        offset <- offset + category_count
    }

    return(result)
}

.metadataSessionCombineAnalysis <- function(records) {
    if (length(records) == 0L) {
        return(.metadataSessionEmptyAnalysis())
    }

    numeric_fields <- c(
        "var_dcml",
        "var_width",
        "val_min",
        "val_max",
        "stat_min",
        "stat_max",
        "stat_mean",
        "stat_medn",
        "stat_stdev",
        "sum_valid",
        "sum_invalid"
    )
    logical_fields <- c(
        "weight_numeric_compatible",
        "weight_has_labels",
        "weight_has_observed",
        "weight_has_negative"
    )
    result <- list()

    for (field in numeric_fields) {
        result[[field]] <- unname(vapply(records, function(record) {
            return(as.numeric(record[[field]]))
        }, numeric(1)))
    }

    result$range_units <- unname(vapply(records, function(record) {
        return(as.character(record$range_units))
    }, character(1)))
    result$cat_counts <- unname(vapply(records, function(record) {
        return(as.integer(record$cat_counts))
    }, integer(1)))
    result$cat_values <- as.character(unname(unlist(
        lapply(records, getElement, "cat_values"),
        use.names = FALSE
    )))
    result$cat_labels <- as.character(unname(unlist(
        lapply(records, getElement, "cat_labels"),
        use.names = FALSE
    )))
    result$cat_missing <- as.logical(unname(unlist(
        lapply(records, getElement, "cat_missing"),
        use.names = FALSE
    )))
    result$cat_freq <- as.numeric(unname(unlist(
        lapply(records, getElement, "cat_freq"),
        use.names = FALSE
    )))
    result$variable_type <- unname(vapply(records, function(record) {
        return(as.character(record$variable_type))
    }, character(1)))

    for (field in logical_fields) {
        result[[field]] <- unname(vapply(records, function(record) {
            return(as.logical(record[[field]]))
        }, logical(1)))
    }

    return(result[names(.metadataSessionEmptyAnalysis())])
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

    values <- .Call(
        "metadata_values_copy",
        session$source,
        as.integer(missing),
        PACKAGE = "DDIwR"
    )
    variables <- metadataSnapshotRead(session$metadata, missing)
    dates <- .metadataSessionDates(variables)
    analysis <- collectDataDscrStatsC(
        values,
        variables,
        dates,
        include_projection = TRUE
    )
    records <- .metadataSessionSplitAnalysis(analysis)

    for (index in seq_along(missing)) {
        position <- missing[[index]]

        session$value_cache[[position]] <- values[[index]]
        session$analysis_cache[[position]] <- records[[index]]
        session$analysis_ready[[position]] <- TRUE
    }

    session$analysis_scans <- session$analysis_scans + 1L
    session$analysed_columns <- session$analysed_columns + length(missing)

    if (length(session$analysis_ready) == 0L || all(session$analysis_ready)) {
        session$source <- NULL
    }

    return(invisible(NULL))
}

metadataSweepCapture <- function(from, revision) {
    if (!is.data.frame(from)) {
        stop("The input must be a data frame.")
    }

    .metadataSessionValidateRevision(revision)

    metadata <- metadataSnapshot(from)
    complete <- FALSE

    on.exit({
        if (!complete) {
            metadataSnapshotClose(metadata)
        }
    })

    session <- new.env(parent = emptyenv())

    class(session) <- "ddiwr_metadata_session"
    session$metadata <- metadata
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
    session$analysis_cache <- vector("list", ncol(from))
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

metadataSweepRead <- function(session, revision,
    columns = seq_len(session$columns),
    fields = c("identity", "annotations", "classification", "weight_signal",
        "summaries", "categories"), xml_options = list()) {
    .metadataSessionAssertRevision(session, revision)

    if (!is.numeric(columns) || anyNA(columns) ||
        any(!is.finite(columns) | columns != trunc(columns) |
            columns < 1 | columns > session$columns)) {
        stop("Columns must be valid integer positions.")
    }

    if (
        !is.list(xml_options) ||
        is.null(names(xml_options)) && length(xml_options) > 0L
    ) {
        stop("XML options must be a named list.")
    }

    if (any(is.element(names(xml_options), c("variables", "data")))) {
        stop("XML options cannot replace projection inputs.")
    }

    plan <- .metadataSessionPlan(fields)
    positions <- as.integer(columns)
    variables <- metadataSnapshotRead(session$metadata, positions)
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
            session$analysis_cache[positions]
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
    result$session <- list(
        analysed_columns = session$analysed_columns,
        analysis_scans = session$analysis_scans,
        cached_columns = which(session$analysis_ready)
    )

    return(result)
}

metadataSweepInvalidate <- function(session, revision) {
    .metadataSessionValidate(session)
    .metadataSessionValidateRevision(revision)

    session$stale <- TRUE
    session$invalidated_by <- revision

    return(invisible(NULL))
}

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
    session$closed <- TRUE

    return(invisible(NULL))
}
