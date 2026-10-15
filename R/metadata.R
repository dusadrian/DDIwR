# DialogR interpretation is deliberately separate from DDI classification.
.metadataText <- function(value, default = "") {
    if (is.null(value) || length(value) == 0L || is.na(value[[1]])) {
        return(default)
    }

    return(as.character(value[[1]]))
}

.metadataCategoryDefinitions <- function(attributes) {
    labels <- attributes$labels
    values <- character()
    text <- character()

    if (length(labels) > 0L) {
        values <- as.character(labels)
        text <- names(labels)

        if (is.null(text) || length(text) == 0L) {
            text <- values
        }
    }
    else if (is.element("factor", attributes$class)) {
        values <- as.character(attributes$levels)
        text <- values
    }

    summary <- paste(utils::head(text, 6L), collapse = ", ")
    missing_only <- setdiff(as.character(attributes$na_values), values)
    values <- c(values, missing_only)
    text <- c(text, rep("", length(missing_only)))
    missing <- is.element(values, as.character(attributes$na_values))
    range <- attributes$na_range
    missing_range <- NULL

    if (length(range) >= 2L) {
        missing_range <- list(min = as.character(range[[1]]),
            max = as.character(range[[2]]))
        numeric_range <- suppressWarnings(as.numeric(range[1:2]))
        numeric_values <- suppressWarnings(as.numeric(values))

        if (all(is.finite(numeric_range))) {
            missing <- missing | (is.finite(numeric_values) &
                numeric_values >= min(numeric_range) &
                numeric_values <= max(numeric_range))
        }
    }

    categories <- lapply(seq_along(values), function(index) {
        return(list(value = values[[index]], label = text[[index]],
            isMissing = missing[[index]]))
    })

    return(list(categories = categories, missingRange = missing_range,
        summary = summary, labels = attributes$labels, levels = attributes$levels,
        na_values = attributes$na_values, na_range = attributes$na_range))
}

.metadataDialogMeasure <- function(column) {
    explicit <- trimws(tolower(.metadataText(attr(column, "measurement", exact = TRUE))))

    if (nzchar(explicit)) {
        tokens <- trimws(strsplit(explicit, ",", fixed = TRUE)[[1]])
        measure <- tokens[[length(tokens)]]

        if (is.element(measure, c("nominal", "ordinal", "interval", "ratio"))) {
            return(measure)
        }

        return(explicit)
    }

    labels <- attr(column, "labels", exact = TRUE)
    count <- length(labels)

    if (count == 0L && is.factor(column)) {
        count <- nlevels(column)
    }

    likely <- tryCatch(
        trimws(as.character(get("likely_measurement", asNamespace("declared"))(column))),
        error = function(error) ""
    )

    if (identical(likely, "quantitative")) {
        return("interval")
    }

    if (identical(likely, "categorical")) {
        if (is.ordered(column) ||
            ((is.numeric(column) || is.integer(column)) && count > 6L)) {
            return("ordinal")
        }

        return("nominal")
    }

    if (is.ordered(column)) {
        return("ordinal")
    }

    if ((is.numeric(column) || is.integer(column)) &&
        (length(labels) == 0L || count > 6L)) {
        return("interval")
    }

    return("nominal")
}

.metadataDialogCalibration <- function(column) {
    values <- numeric()

    if (is.numeric(column) || is.integer(column) || is.logical(column)) {
        values <- tryCatch(as.numeric(column[is.finite(column) & !is.na(column)]),
            error = function(error) numeric())
    }

    unit_interval <- length(values) > 0L && all(values >= 0 & values <= 1)
    whole <- length(values) > 0L && all(values >= 0) &&
        all(abs(values - round(values)) < .Machine$double.eps^0.5)
    binary <- FALSE
    multi <- FALSE

    if (whole) {
        binary <- all(values == 0 | values == 1)
        maximum <- max(values)

        if (identical(min(values), 0) && maximum >= 2) {
            multi <- length(unique(values)) == maximum + 1
        }
    }

    return(list(calibrated = unit_interval || multi, binary = binary))
}

.metadataDialogVariable <- function(name, column) {
    attributes <- attributes(column)
    definitions <- .metadataCategoryDefinitions(attributes)
    source <- column

    if (inherits(column, "declared")) {
        source <- tryCatch(declared::undeclare(column, drop = TRUE),
            error = function(error) column)
    }

    measure <- .metadataDialogMeasure(column)
    calibration <- .metadataDialogCalibration(source)
    raw_calibration <- calibration

    if (inherits(column, "declared")) {
        raw_calibration <- .metadataDialogCalibration(column)
    }
    classes <- class(column)
    classes <- classes[!is.na(classes) & nzchar(classes) & classes != "declared"]
    type <- typeof(column)

    if (length(classes) > 0L) {
        type <- classes[[1]]
    }

    width <- suppressWarnings(as.integer(.metadataText(attributes$width, NA_character_)))

    if (!is.finite(width) || width <= 0L) {
        sample <- tryCatch(utils::head(as.character(stats::na.omit(source)), 100L),
            error = function(error) character())
        widths <- nchar(sample, type = "chars")
        widths <- widths[is.finite(widths)]
        width <- 1L

        if (length(widths) > 0L) {
            width <- max(widths)
        }
    }

    width <- as.integer(max(1L, min(60L, width)))
    decimals <- suppressWarnings(as.integer(.metadataText(attributes$decimals, NA_character_)))

    if (!is.finite(decimals)) {
        decimals <- 0L

        if (is.numeric(column)) {
            sample <- tryCatch(utils::head(column[is.finite(column)], 100L),
                error = function(error) numeric())

            if (length(sample) > 0L && any(
                abs(sample - round(sample)) > .Machine$double.eps^0.5, na.rm = TRUE
            )) {
                decimals <- 3L
            }
        }

        decimals <- min(decimals, max(0L, width - 2L))
    }

    decimals <- as.integer(max(0L, min(8L, decimals)))
    align <- .metadataText(attributes$align)

    if (!is.element(align, c("left", "center", "right"))) {
        align <- "left"

        if (is.numeric(column) || is.integer(column) || is.logical(column)) {
            align <- "right"
        }
    }

    labels <- attributes$labels
    count <- length(labels)

    if (count == 0L && is.factor(column)) {
        count <- nlevels(column)
    }

    categorical <- is.factor(source) || length(labels) > 0L
    intrinsic <- !inherits(source, "Date") &&
        (is.numeric(source) || is.integer(source) || is.logical(source))
    numeric <- (intrinsic &&
        !(identical(measure, "nominal") && count > 0L) &&
        !(identical(measure, "ordinal") && count > 0L && count < 7L)) ||
        (identical(measure, "ordinal") && count >= 7L) || calibration$calibrated
    summary <- definitions$summary

    if (!is.null(definitions$missingRange)) {
        parts <- c(summary, paste0("range ", definitions$missingRange$min,
            ":", definitions$missingRange$max))
        summary <- paste(parts[nzchar(trimws(parts))], collapse = ", ")
    }

    return(list(
        name = name, type = type, role = "data",
        label = .metadataText(attributes[["label"]]), width = width, decimals = decimals,
        values = summary, categories = definitions$categories,
        missingRange = definitions$missingRange, align = align, measure = measure,
        numeric = isTRUE(numeric), factor = categorical,
        declared = isTRUE(declared::is.declared(column)),
        calibrated = raw_calibration$calibrated,
        binary = calibration$binary, character = is.character(source),
        categorical = categorical, date = inherits(source, "Date")
    ))
}

# Internal structured projection for browser and application consumers.
# XML consumes the same analysed columns through an explicit internal argument.
metadataProjection <- function(from, columns = seq_len(ncol(from)),
    fields = c("identity", "annotations", "classification", "weight_signal",
        "summaries", "categories"), revision = NULL, xml_options = list()) {
    if (!is.data.frame(from)) {
        stop("The input must be a data frame.")
    }

    if (!is.numeric(columns) || anyNA(columns) ||
        any(!is.finite(columns) | columns != trunc(columns) |
            columns < 1 | columns > ncol(from))) {
        stop("Columns must be valid integer positions.")
    }

    available <- c("identity", "annotations", "classification", "weight_signal",
        "summaries", "categories", "xml")

    if (!is.character(fields) || anyNA(fields) || anyDuplicated(fields) ||
        any(!is.element(fields, available))) {
        stop("Fields must be distinct supported projection names.")
    }

    if (!is.list(xml_options) || is.null(names(xml_options)) && length(xml_options) > 0L) {
        stop("XML options must be a named list.")
    }
    if (any(is.element(names(xml_options),
        c("variables", "data", ".analysis", "session", "revision", "columns")))) {
        stop("XML options cannot replace projection inputs.")
    }

    positions <- as.integer(columns)
    selected <- from[positions]
    names(selected) <- names(from)[positions]

    variables <- collectRMetadata(
        selected,
        infer_type = FALSE,
        include_formats = FALSE
    )

    value_fields <- c("classification", "weight_signal", "summaries", "categories", "xml")
    needs_analysis <- any(is.element(fields, value_fields))
    analysis <- NULL

    if (needs_analysis) {
        if (length(selected) == 0L) {
            dates <- logical()
            analysis <- list(
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
            )
        }
        else {
            dates <- unname(vapply(variables, function(variable) {
                return(
                    identical(variable$varFormat, "date") ||
                    is.element("Date", getElement(variable, "classes"))
                )
            }, logical(1)))

            analysis <- collectDataDscrStatsC(
                selected,
                variables,
                dates,
                include_projection = TRUE
            )
        }
    }

    return(.metadataProjectionResult(
        variables = variables,
        selected = selected,
        positions = positions,
        fields = fields,
        analysis = analysis,
        revision = revision,
        xml_options = xml_options
    ))
}

.metadataProjectionResult <- function(variables, selected, positions, fields,
    analysis, revision, xml_options, identity_storage = NULL,
    identity_length = NULL, dates = NULL) {
    projection <- list()

    if (is.null(identity_storage)) {
        identity_storage <- unname(vapply(selected, typeof, character(1)))
    }

    if (is.null(identity_length)) {
        identity_length <- unname(vapply(selected, length, integer(1)))
    }

    if (is.element("xml", fields) && is.null(dates)) {
        dates <- unname(vapply(variables, function(variable) {
            return(
                identical(variable$varFormat, "date") ||
                is.element("Date", getElement(variable, "classes"))
            )
        }, logical(1)))
    }

    if (is.element("identity", fields)) {
        projection$identity <- list(
            position = positions,
            name = names(selected),
            ID = lapply(variables, getElement, name = "ID"),
            storage = identity_storage,
            classes = lapply(variables, getElement, name = "classes"),
            length = identity_length
        )
    }

    if (is.element("annotations", fields)) {
        projection$annotations <- list(
            label = lapply(variables, getElement, name = "label"),
            measurement = lapply(variables, getElement, name = "measurement"),
            xmlang = lapply(variables, getElement, name = "xmlang")
        )
    }

    if (is.element("classification", fields)) {
        projection$classification <- analysis$variable_type
    }

    if (is.element("weight_signal", fields)) {
        projection$weight_signal <- list(
            numeric_compatible = analysis$weight_numeric_compatible,
            has_labels = analysis$weight_has_labels,
            has_observed_value = analysis$weight_has_observed,
            has_negative_value = analysis$weight_has_negative
        )
    }

    if (is.element("summaries", fields)) {
        projection$summaries <- list(
            valid = analysis$sum_valid,
            invalid = analysis$sum_invalid,
            minimum = analysis$stat_min,
            maximum = analysis$stat_max,
            mean = analysis$stat_mean,
            median = analysis$stat_medn,
            standard_deviation = analysis$stat_stdev,
            value_minimum = analysis$val_min,
            value_maximum = analysis$val_max,
            width = analysis$var_width,
            decimals = analysis$var_dcml,
            range_units = analysis$range_units
        )
    }

    if (is.element("categories", fields)) {
        projection$categories <- list(
            counts = analysis$cat_counts,
            values = analysis$cat_values,
            labels = analysis$cat_labels,
            missing = analysis$cat_missing,
            frequencies = analysis$cat_freq
        )
    }

    if (is.element("xml", fields)) {
        if (length(selected) == 0L) {
            projection$xml <- character()
            projection$xml_metadata <- list(
                id = character(),
                display_type = character(),
                measurement = character(),
                width = numeric(),
                decimals = numeric()
            )
        }
        else {
            xml_result <- do.call(
                makeXMLvars,
                c(list(variables = variables, data = selected,
                    .analysis = analysis), xml_options)
            )
            projection$xml <- xml_result$xml

            if (isTRUE(xml_options$return_hashes)) {
                projection$hashes <- xml_result$hashes
            }

            xml_stats <- xml_result$stats
            display_type <- rep("", length(selected))
            if (!is.null(xml_stats) && nrow(xml_stats) == length(selected)) {
                format_type <- tolower(as.character(xml_stats$varformat_type))
                format_value <- tolower(as.character(xml_stats$varformat_value))
                display_type[grepl("char", format_type)] <- "String"
                display_type[grepl("num", format_type)] <- "Numeric"
                display_type[grepl("date|time", format_value)] <- "Date"

                projection$xml_metadata <- list(
                    id = as.character(xml_stats$id),
                    display_type = display_type,
                    measurement = as.character(xml_stats$measurement),
                    width = as.numeric(xml_stats$width),
                    decimals = as.numeric(xml_stats$dcml)
                )

                if (!is.null(projection$identity)) {
                    missing_id <- vapply(projection$identity$ID, function(id) {
                        return(is.null(id) || length(id) == 0L ||
                            !nzchar(as.character(id)[1]))
                    }, logical(1))
                    projection$identity$ID[missing_id] <- as.list(xml_stats$id[missing_id])
                }
            }
        }
    }

    return(list(
        abi = 1L,
        layout = "columnar",
        revision = revision,
        columns = positions,
        completed_fields = fields,
        projection = projection
    ))
}

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

# Reusable metadata sweep sessions
#
# @description Capture metadata once and request ordered subsets for progressive
# displays, DialogR variable selectors, or DDI Codebook generation.
# @details Internal metadata infrastructure for host applications, not a
# user-facing data-management API. Exports are retained for downstream
# application integration.
# @param from A data frame. The host must invalidate the session after edits,
# including edits made by reference.
# @param revision One non-missing atomic revision token, supplied by the host.
# Every read must supply the current token; a mismatch makes the session stale.
# @param session An object returned by `metadataSweepCapture()`.
# @param columns Integer positions, allowing reordering and repetition, or
# unambiguous variable names. `NULL` selects all columns; `integer()` selects none.
# @param fields Requested field groups. `NULL` uses the selected profile.
# @param profile One of `progressive`, `dialogr`, or `codebook`.
# @param xml_options Named arguments passed to the variable XML generator.
# @details
# Capture copies stored metadata for all columns without cleaning labels or
# scanning observations. Normalization is cached only for requested columns.
# The progressive profile returns identity, annotations and category definitions
# without scanning values. Request classification, weight_signal, summaries or
# categories to analyse only the selected columns and cache their results.
# Category definitions (`category_metadata`) and frequencies (`categories`) are
# separate requests. The latter retain DDI's labelled-category semantics.
# Category definitions include UI rows plus the original typed `labels`,
# `levels`, `na_values` and `na_range`, without cleaning or inference.
#
# The dialogr profile returns row records in `projection$dialogr`, following
# DialogR's type, measurement, category, display and selector-flag rules.
# Its value-dependent interpretation is cached per column, separately from DDI
# statistical analysis. Bare observation copies are shared by both consumers.
# The codebook profile returns variable XML and export metadata, reusing cached
# unweighted analysis. Weighted XML retains the existing weighted export path.
#
# Generated IDs remain stable for the life of the session. Numeric positions are
# required to distinguish duplicate names. Hosts schedule batches and yield to
# their UI between calls; this API does not start background browser workers.
# Sessions are process-local and must be recaptured after serialization or edits.
# Value-dependent requests support logical, integer, double and character
# columns; unsupported storage (such as list columns) can be read as metadata
# but raises an error when values are requested.
# `metadataSweepClose()` releases caches and is idempotent; a finalizer also
# releases them. Closing or invalidating a session never changes the input data.
# @return Capture returns a session. Read returns an ABI-versioned columnar
# envelope with revision, columns, completed_fields, projection and cache counters.
# Invalidate and close return invisible NULL.
# @seealso [exportCodebook()], [collectMetadata()]
# @examples
# session <- metadataSweepCapture(iris, revision = 1L)
# metadataSweepRead(session, 1L, columns = 1:2)
# metadataSweepRead(session, 1L, columns = "Species", profile = "dialogr")
# metadataSweepRead(session, 1L, profile = "codebook")
# book <- makeElement("codeBook", children = list(makeElement("stdyDscr", fill = TRUE)))
# path <- tempfile(fileext = ".xml")
# exportCodebook(book, path, session = session, revision = 1L)
# unlink(path)
# metadataSweepClose(session)
#' @export
#' @noRd
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

#' @export
#' @noRd
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

#' @export
#' @noRd
metadataSweepInvalidate <- function(session, revision) {
    .metadataSessionValidate(session)
    .metadataSessionValidateRevision(revision)

    session$stale <- TRUE
    session$invalidated_by <- revision

    return(invisible(NULL))
}

#' @export
#' @noRd
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

# Internal baseline: normalize once, then retain only owned C metadata.
# Value scans and worker execution are deliberately separate implementation steps.
metadataSnapshot <- function(from) {
    if (!is.data.frame(from)) {
        stop("The input must be a data frame.")
    }

    records <- collectRMetadata(from, infer_type = FALSE, include_formats = FALSE)
    pointer <- .Call("metadata_snapshot_create", records, PACKAGE = "DDIwR")

    return(structure(list(pointer = pointer, columns = ncol(from)),
        class = "ddiwr_metadata_snapshot"))
}

metadataSnapshotRead <- function(snapshot, columns = seq_len(snapshot$columns), fields = NULL,
    normalize = FALSE, threads = 1L) {
    if (!inherits(snapshot, "ddiwr_metadata_snapshot")) {
        stop("Invalid metadata snapshot.")
    }

    if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize)) {
        stop("Normalize must be TRUE or FALSE.")
    }

    if (normalize && is.null(snapshot$fields)) {
        stop("Normalization requires a raw metadata snapshot.")
    }

    if (!is.numeric(threads) || length(threads) != 1L || is.na(threads) ||
        !is.element(threads, 1:4)) {
        stop("Threads must be an integer from one to four.")
    }

    if (!is.numeric(columns) || anyNA(columns) ||
        any(!is.finite(columns) | columns != trunc(columns) |
            columns < 1 | columns > snapshot$columns)) {
        stop("Columns must be valid integer positions.")
    }

    available <- c("classes", "label", "measurement", "labels", "na_values",
        "na_range", "xmlang", "ID")

    if (!is.null(snapshot$fields)) {
        available <- snapshot$fields
    }

    if (!is.null(fields) && (!is.character(fields) || anyNA(fields) ||
        any(!is.element(fields, available)) || anyDuplicated(fields))) {
        stop("Fields must be distinct supported metadata names.")
    }

    if (normalize) {
        result <- .Call("metadata_snapshot_normalized", snapshot$pointer,
            as.integer(columns), fields, as.integer(threads), PACKAGE = "DDIwR")
        records <- result[[1]]
        records <- lapply(records, function(record) {
            pending <- names(record)[attr(record, "ddiwr_cleanup_pending")]
            attr(record, "ddiwr_cleanup_pending") <- NULL

            for (field in intersect(c("label", "measurement"), pending)) {
                record[field] <- list(cleanup(record[[field]]))
            }

            if (is.element("labels", pending) && !is.null(record$labels)) {
                labels <- record$labels
                if (is.character(labels)) {
                    labels <- cleanup(labels)
                }
                names(labels) <- cleanup(names(labels))
                record$labels <- labels
            }

            return(record)
        })
    }
    else {
        records <- .Call("metadata_snapshot_read", snapshot$pointer,
            as.integer(columns), fields, PACKAGE = "DDIwR")
    }

    return(records)
}

# ASCII cleanup runs in the R-free kernel; locale-dependent text retains R rules.
metadataNormalizeText <- function(text) {
    if (!is.character(text)) {
        return(cleanup(text))
    }

    result <- .Call("metadata_clean_text", text, PACKAGE = "DDIwR")
    fallback <- result[[2]]
    output <- result[[1]]

    if (any(fallback)) {
        output[fallback] <- cleanup(text[fallback])
    }

    return(output)
}

# Capture stored attributes without cleaning, inference or factor-label synthesis.
# A NULL entry means that attribute was absent; an empty vector stays empty.
metadataSnapshotRaw <- function(from, fields = c("position", "storage", "length",
    "classes", "label", "labels", "levels", "measurement", "nature", "na_values",
    "na_range", "xmlang", "ID", "format.spss", "format.stata", "tzone")) {
    if (!is.data.frame(from)) {
        stop("The input must be a data frame.")
    }

    available <- c("position", "storage", "length", "classes", "label", "labels",
        "levels", "measurement", "nature", "na_values", "na_range", "xmlang",
        "ID", "format.spss", "format.stata", "tzone")

    if (!is.character(fields) || anyNA(fields) || anyDuplicated(fields) ||
        any(!is.element(fields, available))) {
        stop("Fields must be distinct supported raw metadata names.")
    }

    pointer <- .Call("metadata_snapshot_raw", from, fields, PACKAGE = "DDIwR")

    return(structure(list(pointer = pointer, columns = ncol(from), fields = fields),
        class = "ddiwr_metadata_snapshot"))
}

metadataSnapshotClose <- function(snapshot) {
    if (!inherits(snapshot, "ddiwr_metadata_snapshot")) {
        stop("Invalid metadata snapshot.")
    }

    .Call("metadata_snapshot_close", snapshot$pointer, PACKAGE = "DDIwR")

    return(invisible(NULL))
}

# Import data with a prepared metadata sweep session
#
# @description
# Import a dataset and immediately prepare metadata for a declared consumer
# profile. The returned session remains reusable for every profile; `prepare`
# controls only which projection is warm when the import returns.
#
# @details Internal application integration helper, not a user-facing import
# function. Use [convert()] for ordinary dataset imports. The export is retained
# for downstream application integration.
#
# @param from Path to a dataset supported by [convert()].
# @param revision One non-missing atomic revision token for the imported data.
# @param prepare One of `"progressive"`, `"dialogr"`, or `"codebook"`.
# @param columns Integer positions or unambiguous variable names to prepare.
# `NULL` prepares every column. Progressive consumers should provide their
# initial visible range.
# @param fields Optional field groups passed to [metadataSweepRead()].
# @param xml_options Named arguments passed to the variable XML generator.
# @param ... Import arguments passed to [convert()]. The import destination is
# always R; `to` must not be supplied.
#
# @return An object with `data`, `session`, and the prepared `projection`.
# Close `session` with [metadataSweepClose()] when it is no longer needed.
#
# @examples
# \dontrun{
# imported <- metadataSweepImport(
#     "survey.sav",
#     revision = 1L,
#     prepare = "progressive",
#     columns = 1:20
# )
# metadataSweepClose(imported$session)
# }
#
#' @export
#' @noRd
metadataSweepImport <- function(from, revision, prepare = "progressive",
    columns = NULL, fields = NULL, xml_options = list(), ...) {
    .metadataSessionValidateRevision(revision)

    profiles <- c("progressive", "dialogr", "codebook")
    if (!is.character(prepare) || length(prepare) != 1L || is.na(prepare) ||
        !is.element(prepare, profiles)) {
        stop("Prepare must be one supported metadata profile.")
    }

    dots <- list(...)
    if (is.element("to", names(dots))) {
        stop("Metadata sweep imports always import to R; do not supply 'to'.")
    }

    data <- do.call(convert, c(list(from = from, to = NULL), dots))
    if (!is.data.frame(data)) {
        stop("The imported object is not a data frame.")
    }

    session <- metadataSweepCapture(data, revision)
    complete <- FALSE

    on.exit({
        if (!complete) {
            metadataSweepClose(session)
        }
    })

    projection <- metadataSweepRead(
        session = session,
        revision = revision,
        columns = columns,
        fields = fields,
        xml_options = xml_options,
        profile = prepare
    )

    result <- structure(
        list(
            data = data,
            session = session,
            projection = projection
        ),
        class = "ddiwr_metadata_import"
    )
    complete <- TRUE

    return(result)
}
