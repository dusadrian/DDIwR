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
