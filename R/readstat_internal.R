source_file <- function(file) {
    list(normalizePath(file, mustWork = FALSE))
}

restore_variable_metadata <- function(x, template) {
    storage_attributes <- c("format.spss", "format.stata", "format.sas", "width")
    preserved <- c(storage_attributes, "display_width", "spss.attributes",
        "stata.characteristics", "spss_metadata", "sas_metadata", "stata_metadata")
    changed_storage <- is.character(x) != is.character(template)

    # String formats and storage lengths cannot describe numeric category codes.
    if (changed_storage) {
        for (name in storage_attributes) {
            attr(x, name) <- NULL
        }

        preserved <- setdiff(preserved, storage_attributes)
    }

    for (name in preserved) {
        attr(x, name) <- attr(template, name, exact = TRUE)
    }

    if (changed_storage) {
        storage_fields <- list(
            spss_metadata = c("format", "width"),
            sas_metadata = c("format", "informat", "width", "label_tags", "catalog_rules"),
            stata_metadata = c("format", "width", "type")
        )

        for (name in names(storage_fields)) {
            if (!is.null(attr(template, name, exact = TRUE))) {
                metadata <- vendor_metadata(template, name)
                metadata[intersect(names(metadata), storage_fields[[name]])] <- NULL
                attr(x, name) <- metadata
            }
        }
    }

    temporal_classes <- intersect(class(template), c("Date", "POSIXct", "POSIXt",
        "hms", "difftime"))

    if (length(temporal_classes) > 0L && is.element(typeof(x), c("integer", "double"))) {
        class(x) <- c(intersect(class(x), "declared"), temporal_classes)
        attr(x, "tzone") <- attr(template, "tzone", exact = TRUE)
        attr(x, "units") <- attr(template, "units", exact = TRUE)
    }

    return(x)
}

as_declared <- function(data, trusted = FALSE) {
    data[] <- lapply(data, function(x) {
        template <- x
        labels <- attr(x, "labels", exact = TRUE)
        na_values <- attr(x, "na_values", exact = TRUE)
        na_range <- attr(x, "na_range", exact = TRUE)
        label <- attr(x, "label", exact = TRUE)
        na_index <- attr(x, "na_index", exact = TRUE)

        has_metadata <- !is.null(labels) || !is.null(na_values) ||
            !is.null(na_range) || !is.null(label) || !is.null(na_index)

        if (!has_metadata) {
            return(x)
        }

        if (is.null(na_index) && (!is.null(na_values) || !is.null(na_range))) {
            x_plain <- plain_values(x)

            if (inherits(x, "Date") || inherits(x, "POSIXct")) {
                x_plain <- as.numeric(x_plain)
            }

            missing_codes <- na_values

            if (!is.null(na_range) && length(na_range) == 2) {
                in_range <- !is.na(x_plain) & x_plain >= na_range[1] & x_plain <= na_range[2]
                range_codes <- x_plain[in_range]
                if (length(range_codes) > 0) {
                    missing_codes <- c(missing_codes, unique(range_codes))
                }
            }

            if (!is.null(missing_codes) && length(missing_codes) > 0) {
                w <- which(is.element(x_plain, missing_codes))
                if (length(w) > 0) {
                    codes <- x_plain[w]
                    x[w] <- NA
                    na_index <- w
                    names(na_index) <- as.character(codes)
                }
            }
        }

        if (isTRUE(trusted)) {
            attr(x, "na_index") <- na_index
            attr(x, "date") <- inherits(x, "Date")
            class(x) <- unique(c("declared", class(x)))

            return(restore_variable_metadata(x, template))
        }

        out <- declared::direct_declared(
            x = x,
            na_index = na_index,
            na_values = na_values,
            na_range = na_range,
            labels = labels,
            label = label
        )

        return(restore_variable_metadata(out, template))
    })
    class(data) <- "data.frame"
    data
}

as_declared_dataset <- function(data) {
    out <- data

    for (index in seq_along(out)) {
        # Re-declaring an empty character column can change its storage type.
        if (!declared::is.declared(data[[index]])) {
            out[[index]] <- restore_variable_metadata(
                declared::as.declared(data[[index]]), data[[index]]
            )
        }
    }

    class(out) <- "data.frame"

    return(out)
}

make_declared <- function(
    x, labels = NULL, na_values = NULL, na_range = NULL, label = NULL,
    na_index = NULL, template = NULL
) {
    xclass <- class(x)
    xdate <- isTRUE(attr(template, "date", exact = TRUE)) || inherits(x, "Date")
    measurement <- attr(template, "measurement", exact = TRUE)

    attributes(x) <- NULL
    attr(x, "na_index") <- na_index
    attr(x, "na_values") <- na_values
    attr(x, "na_range") <- na_range
    attr(x, "labels") <- labels
    attr(x, "label") <- label
    attr(x, "date") <- xdate
    attr(x, "measurement") <- measurement
    class(x) <- unique(c("declared", xclass))

    return(restore_variable_metadata(x, template))
}

var_metadata <- function(x) {
    labels <- attr(x, "labels", exact = TRUE)

    if (is.null(labels) && is.factor(x)) {
        xlevels <- levels(x)
        labels <- setNames(seq_along(xlevels), xlevels)
    }

    list(
        label = attr(x, "label", exact = TRUE),
        labels = labels,
        na_values = attr(x, "na_values", exact = TRUE),
        na_range = attr(x, "na_range", exact = TRUE),
        na_index = attr(x, "na_index", exact = TRUE)
    )
}

plain_values <- function(x) {
    attrs <- attributes(x)

    if (is.null(attrs)) {
        return(x)
    }

    attrs[c(
        "labels", "na_values", "na_range", "label", "na_index",
        "measurement", "format.spss", "format.stata", "format.sas", "date",
        "width", "display_width", "spss.attributes", "stata.characteristics",
        "spss_metadata", "sas_metadata", "stata_metadata"
    )] <- NULL

    attributes(x) <- attrs

    xclass <- class(x)
    if (!is.null(xclass)) {
        xclass <- setdiff(xclass, "declared")
        if (length(xclass) == 0) {
            attr(x, "class") <- NULL
        }
        else {
            class(x) <- xclass
        }
    }

    x
}

ensure_format <- function(x, type = c("SPSS", "Stata", "SAS")) {
    type <- match.arg(type)
    attribute <- switch(type,
        SPSS = "spss_metadata", Stata = "stata_metadata", SAS = "sas_metadata"
    )
    metadata <- vendor_metadata(x, attribute)

    if (!is.element("format", names(metadata))) {
        format_value <- effective_vendor_metadata(x, attribute, "format",
            switch(type, SPSS = "format.spss", Stata = "format.stata", SAS = "format.sas"))

        if (is.null(format_value)) {
            format_value <- getFormat(x, type = type)
        }

        metadata["format"] <- list(format_value)
        attr(x, attribute) <- metadata
    }

    return(x)
}

recode_vector <- function(x, old, new) {
    if (is.null(x) || length(x) == 0 || length(old) == 0) {
        return(x)
    }

    index <- match(x, old)
    w <- which(!is.na(index))

    if (length(w) == 0) {
        return(x)
    }

    x[w] <- new[index[w]]
    x
}

recode_to_spss_native <- function(x, labels = NULL, na_values = NULL, old, new) {
    .Call(
        "recode_to_spss_",
        x,
        if (is.null(labels)) NULL else labels,
        if (is.null(na_values)) NULL else na_values,
        as.character(old),
        new,
        PACKAGE = "DDIwR"
    )
}

recode_to_spss_full_native <- function(
    x, labels = NULL, na_values = NULL, na_index = NULL, old, new
) {
    .Call(
        "recode_to_spss_full_",
        x,
        if (is.null(labels)) NULL else labels,
        if (is.null(na_values)) NULL else na_values,
        if (is.null(na_index)) NULL else na_index,
        as.character(old),
        new,
        PACKAGE = "DDIwR"
    )
}

#' Build a dictionary of missing-value recodes
#'
#' @description
#' Build a dataset-level dictionary of missing values and their recoded targets
#' for internal SPSS-style normalization or export-oriented Stata/SAS mapping.
#'
#' @param dataset A data frame.
#' @param to Target software for the recoding dictionary.
#' @param start Starting code for generated SPSS-style numeric missings.
#'
#' @return A data frame with columns such as `label`, `old`, `new`, `count`,
#' and `n_variables`.
#'
#' @export
buildDictionary <- function(dataset, to = c("SPSS", "Stata", "SAS"), start = -91) {
    to <- toupper(to[1])
    if (!to %in% c("SPSS", "STATA", "SAS")) {
        stop("Argument 'to' must be one of 'SPSS', 'Stata', or 'SAS'.", call. = FALSE)
    }
    if (to %in% c("STATA", "SAS") && !can_build_dictionary(dataset, to = to)) {
        stop("Too many overall missing values.", call. = FALSE)
    }
    .Call(
        "build_dictionary_",
        dataset,
        to,
        as.integer(start),
        PACKAGE = "DDIwR"
    )
}

build_dictionary <- buildDictionary

can_build_dictionary <- function(dataset, to = c("Stata", "SAS"), limit = 26L) {
    to <- toupper(to[1])
    if (!to %in% c("STATA", "SAS")) {
        return(TRUE)
    }
    isTRUE(.Call(
        "can_build_dictionary_",
        dataset,
        to,
        as.integer(limit),
        PACKAGE = "DDIwR"
    ))
}

prepare_foreign_export_missings <- function(
    data, to = c("Stata", "SAS"), dictionary = NULL
) {
    to <- match.arg(to)

    if (!is.null(dictionary)) {
        return(recodeMissings(dataset = data, to = to, dictionary = dictionary))
    }

    for (name in names(data)) {
        column <- data[[name]]
        attrs <- attributes(column)

        if (is.null(attrs) || !any(is.element(
            c("label", "labels", "na_values", "na_range", "na_index"),
            names(attrs)
        ))) {
            next
        }

        recoded <- recodeMissings(
            dataset = data[name],
            to = to
        )
        data[[name]] <- recoded[[1]]
    }

    data
}

readstat_parallel_supported <- function() {
    isTRUE(.Call(
        "readstat_parallel_supported_",
        PACKAGE = "DDIwR"
    ))
}

read_sav <- function(file, encoding = NULL, user_na = FALSE, n_max = -1L, skip = 0L) {
    if (is.null(encoding)) {
        encoding <- ""
    }

    num_threads <- getOption("DDIwR.readstat_threads", 0L)
    parallel_enabled <- isTRUE(getOption("DDIwR.readstat_parallel_sav", TRUE))
    data <- NULL

    if (parallel_enabled && readstat_parallel_supported() &&
        is.numeric(num_threads) && length(num_threads) == 1L &&
        !is.na(num_threads) && as.integer(num_threads) != 1L &&
        identical(as.integer(n_max), -1L) && identical(as.integer(skip), 0L)) {
        data <- tryCatch(
            .Call(
                "declared_df_parse_sav_file_parallel_prototype",
                source_file(file),
                encoding,
                user_na,
                as.integer(num_threads),
                PACKAGE = "DDIwR"
            ),
            error = function(e) NULL
        )
    }

    if (is.null(data)) {
        data <- .Call(
            "declared_df_parse_sav_file",
            source_file(file),
            encoding,
            user_na,
            integer(),
            as.integer(n_max),
            as.integer(skip),
            PACKAGE = "DDIwR"
        )
    }

    as_declared(data, trusted = TRUE)
}

sav_parallel_prototype <- function(file, num_threads = getOption("DDIwR.readstat_threads", 0L)) {
    .Call(
        "declared_sav_parallel_prototype",
        source_file(file),
        as.integer(num_threads),
        PACKAGE = "DDIwR"
    )
}

read_sav_parallel_prototype <- function(file, encoding = NULL, user_na = FALSE, num_threads = getOption("DDIwR.readstat_threads", 0L)) {
    if (is.null(encoding)) {
        encoding <- ""
    }
    .Call(
        "declared_df_parse_sav_file_parallel_prototype",
        source_file(file),
        encoding,
        user_na,
        as.integer(num_threads),
        PACKAGE = "DDIwR"
    ) |>
        as_declared(trusted = TRUE)
}

read_por <- function(file, user_na = FALSE, n_max = -1L, skip = 0L) {
    data <- .Call(
        "declared_df_parse_por_file",
        source_file(file),
        "",
        user_na,
        integer(),
        as.integer(n_max),
        as.integer(skip),
        PACKAGE = "DDIwR"
    )

    as_declared(data, trusted = TRUE)
}

read_dta <- function(file, encoding = NULL, n_max = -1L, skip = 0L, num_threads = getOption("DDIwR.readstat_threads", NULL)) {
    if (is.null(encoding)) {
        encoding <- ""
    }
    if (is.null(num_threads)) {
        num_threads <- 0L
    }

    use_parallel <- readstat_parallel_supported() &&
        is.numeric(num_threads) && length(num_threads) == 1L &&
        !is.na(num_threads) && as.integer(num_threads) != 1L

    if (use_parallel) {
        data <- .Call(
            "declared_df_parse_dta_file_parallel",
            source_file(file),
            encoding,
            integer(),
            as.integer(n_max),
            as.integer(skip),
            as.integer(num_threads),
            PACKAGE = "DDIwR"
        )
    }
    else {
        data <- .Call(
            "declared_df_parse_dta_file",
            source_file(file),
            encoding,
            integer(),
            as.integer(n_max),
            as.integer(skip),
            PACKAGE = "DDIwR"
        )
    }

    as_declared(data, trusted = TRUE)
}

read_sas <- function(data_file, catalog_file = NULL, encoding = NULL, catalog_encoding = encoding, n_max = -1L, skip = 0L) {
    if (is.null(encoding)) {
        encoding <- ""
    }
    if (is.null(catalog_encoding)) {
        catalog_encoding <- ""
    }

    spec_cat <- if (is.null(catalog_file)) list() else source_file(catalog_file)

    data <- .Call(
        "declared_df_parse_sas_file",
        source_file(data_file),
        spec_cat,
        encoding,
        catalog_encoding,
        integer(),
        as.integer(n_max),
        as.integer(skip),
        PACKAGE = "DDIwR"
    )

    as_declared(data, trusted = TRUE)
}

read_xpt <- function(file, n_max = -1L, skip = 0L) {
    data <- .Call(
        "declared_df_parse_xpt_file",
        source_file(file),
        integer(),
        as.integer(n_max),
        as.integer(skip),
        PACKAGE = "DDIwR"
    )

    as_declared(data, trusted = TRUE)
}

adjust_tz <- function(df) {
    datetime <- vapply(df, inherits, "POSIXt", FUN.VALUE = logical(1))
    df[datetime] <- lapply(df[datetime], function(x) {
        if (identical(attr(x, "tzone"), "UTC")) {
            return(x)
        }

        x_attr <- attributes(x)
        x <- format(x, usetz = FALSE, format = "%Y-%m-%d %H:%M:%S")
        x <- as.POSIXct(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
        attr_miss <- setdiff(names(x_attr), c(names(attributes(x)), "names"))
        attributes(x)[attr_miss] <- x_attr[attr_miss]
        x
    })
    df
}

days_offset <- function(vendor) {
    switch(
        vendor,
        SPSS = 141428,
        STATA = 3653,
        SAS = 3653,
        0
    )
}

adjust_datetime_from_r <- function(x, vendor) {
    if (inherits(x, "POSIXct")) {
        value <- unclass(x)
        value[!is.na(value)] <- value[!is.na(value)] + days_offset(vendor) * 86400
        if (identical(vendor, "STATA")) {
            value[!is.na(value)] <- value[!is.na(value)] * 1000
        }
        return(value)
    }

    if (inherits(x, "Date")) {
        value <- unclass(x)
        value[!is.na(value)] <- value[!is.na(value)] + days_offset(vendor)
        if (identical(vendor, "SPSS")) {
            value[!is.na(value)] <- value[!is.na(value)] * 86400
        }
        return(value)
    }

    x
}

stata_file_format <- function(version) {
    version <- as.integer(version)

    if (version == 15L) {
        119L
    } else if (version == 14L) {
        118L
    } else if (version == 13L) {
        117L
    } else if (version == 12L) {
        115L
    } else if (version %in% c(10L, 11L)) {
        114L
    } else if (version %in% c(8L, 9L)) {
        113L
    } else {
        admisc::stopError(sprintf("Stata version '%s' is not currently supported.", version))
    }
}

vendor_metadata <- function(object, attribute, scope = "Variable") {
    metadata <- attr(object, attribute, exact = TRUE)

    if (is.null(metadata)) {
        return(list())
    }

    if (!is.list(metadata) || is.object(metadata)) {
        stop(scope, ": '", attribute, "' must be a named list.", call. = FALSE)
    }

    field_names <- names(metadata)

    if (length(metadata) > 0L && (
        is.null(field_names) || anyNA(field_names) ||
        any(!nzchar(field_names)) || anyDuplicated(field_names) > 0L
    )) {
        stop(scope, ": '", attribute,
            "' fields must have nonempty, unique names.", call. = FALSE)
    }

    return(metadata)
}

effective_vendor_metadata <- function(object, attribute, field, legacy = NULL) {
    metadata <- vendor_metadata(object, attribute)

    if (is.element(field, names(metadata))) {
        return(metadata[[field, exact = TRUE]])
    }

    if (!is.null(legacy)) {
        return(attr(object, legacy, exact = TRUE))
    }

    return(NULL)
}

prepare_vendor_metadata <- function(data, target, overrides = character()) {
    attribute <- switch(target,
        sav = "spss_metadata", dta = "stata_metadata",
        sas7bdat = "sas_metadata", xpt = "sas_metadata"
    )
    column_fields <- switch(target,
        sav = c(format = "format.spss", width = "width", display_width = "display_width"),
        dta = c(format = "format.stata", width = "width"),
        sas7bdat = c(format = "format.sas", width = "width"),
        xpt = c(format = "format.sas", width = "width")
    )
    dataset_fields <- switch(target,
        sav = c("notes", "mr_sets"), dta = c("notes", "version"),
        sas7bdat = "table_name", xpt = c("table_name", "version")
    )
    dataset_fields <- c(dataset_fields, "created", "modified")
    supported_columns <- c(names(column_fields), switch(target,
        sav = c("measure", "alignment"), dta = "type",
        sas7bdat = c("informat", "label_tags", "catalog_rules"), xpt = c("informat", "label_tags")
    ))
    unsupported <- character()

    validate <- function(metadata, supported, scope) {
        for (field in names(metadata)) {
            value <- metadata[[field, exact = TRUE]]

            if (is.null(value)) {
                next
            }

            if (!is.element(field, supported)) {
                unsupported <<- c(unsupported, paste0(scope, ": ", attribute, "$", field))
                next
            }

            if (identical(field, "modified") && !is.element(target, c("sas7bdat", "xpt"))) {
                next
            }

            valid <- switch(field,
                format =, informat =, table_name = {
                    is.character(value) && length(value) == 1L &&
                        !is.na(value) && nzchar(value)
                },
                width =, display_width = {
                    is.element(typeof(value), c("integer", "double")) &&
                        !is.object(value) && length(value) == 1L &&
                        is.finite(value) && value >= 0 &&
                        value <= .Machine$integer.max && value == trunc(value)
                },
                notes = {
                    is.character(value) && !anyNA(value)
                },
                label_tags = {
                    is.character(value) && !is.object(value) && !anyNA(value) &&
                        all(is.element(value, c("", ".", "_", LETTERS)))
                },
                catalog_rules = {
                    is.data.frame(value)
                },
                mr_sets = {
                    is.list(value) && !is.object(value)
                },
                created =, modified = {
                    inherits(value, "POSIXct") && length(value) == 1L &&
                        is.finite(as.numeric(value)) &&
                        as.numeric(value) >= -62135596800 &&
                        as.numeric(value) <= 253402300799 &&
                        as.numeric(value) == trunc(as.numeric(value))
                },
                measure =, alignment =, type = {
                    choices <- switch(field,
                        measure = c("nominal", "ordinal", "scale"),
                        alignment = c("left", "right", "center"),
                        type = c("byte", "int", "long", "float", "double", "strL")
                    )

                    is.character(value) && length(value) == 1L &&
                        !is.na(value) && is.element(value, choices)
                },
                version = {
                    versions <- 8:15

                    if (target == "xpt") {
                        versions <- c(5, 8)
                    }

                    is.element(typeof(value), c("integer", "double")) &&
                        !is.object(value) && length(value) == 1L &&
                        !is.na(value) && is.element(value, versions)
                }
            )

            if (!isTRUE(valid)) {
                stop(scope, ": invalid '", attribute, "$", field, "'.", call. = FALSE)
            }

            if (is.element(field, c("created", "modified")) &&
                is.element(target, c("sav", "xpt"))) {
                year <- as.integer(format(value, "%Y", tz = ""))
                first_year <- switch(target, sav = 1970L, xpt = 1960L)

                if (is.na(year) || year < first_year || year >= first_year + 100L) {
                    stop(scope, ": '", attribute, "$", field, "' must use a year from ",
                        first_year, " through ", first_year + 99L, ".", call. = FALSE)
                }
            }
        }

        return(invisible(NULL))
    }

    options <- vendor_metadata(data, attribute, "Dataset")
    options[intersect(names(options), overrides)] <- NULL
    validate(options, dataset_fields, "Dataset")

    if (is.element(target, c("sav", "dta")) && is.element("notes", names(options))) {
        attr(data, "notes") <- options[["notes", exact = TRUE]]
    }

    if (target == "dta" && is.element("notes", names(options))) {
        characteristics <- attr(data, "stata.characteristics", exact = TRUE)
        note_names <- grepl("^note[1-9][0-9]*$", names(characteristics))
        note_indices <- suppressWarnings(as.numeric(sub("^note", "", names(characteristics))))
        note_names <- note_names & !is.na(note_indices) & note_indices <= .Machine$integer.max
        raw_notes <- sub(" +$", "", unname(characteristics[note_names]))
        raw_notes <- raw_notes[nzchar(raw_notes)]

        # Preserve original numbering when the imported notes are unchanged.
        if (!identical(options[["notes", exact = TRUE]], raw_notes)) {
            characteristics <- characteristics[!grepl("^note[0-9]+$", names(characteristics))]
            attr(data, "stata.characteristics") <- characteristics
        }
    }

    for (index in seq_along(data)) {
        scope <- paste0("Variable '", names(data)[index], "'")
        metadata <- vendor_metadata(data[[index]], attribute, scope)
        validate(metadata, supported_columns, scope)

        type <- metadata[["type", exact = TRUE]]

        if (target == "dta" && !is.null(type) &&
            (identical(type, "strL") != is.character(data[[index]]))) {
            stop(scope, ": 'stata_metadata$type' does not match the column's storage type.",
                call. = FALSE)
        }

        for (field in intersect(names(metadata), names(column_fields))) {
            attr(data[[index]], column_fields[[field]]) <- metadata[[field, exact = TRUE]]
        }

        width <- metadata[["width", exact = TRUE]]

        if (target == "sas7bdat" && !is.character(data[[index]])) {
            width <- attr(data[[index]], "width", exact = TRUE)

            if (!is.null(width) &&
                !(is.element(typeof(width), c("integer", "double")) &&
                    !is.object(width) && length(width) == 1L &&
                    !is.na(width) && is.element(width, c(0, 3:8)))) {
                stop(scope, ": numeric SAS7BDAT 'sas_metadata$width' or legacy 'width' ",
                    "must be 0 or 3 through 8.", call. = FALSE)
            }
        }

        if (!is.null(width) && !is.character(data[[index]]) && target == "xpt" &&
            !is.element(width, c(0, 3:8))) {
            stop(scope, ": 'sas_metadata$width' for numeric XPT data must be 0 or 3 through 8.",
                call. = FALSE)
        }

        if (!is.null(width) && !is.character(data[[index]]) &&
            target == "sav" && !is.element(width, c(0, 8))) {
            unsupported <- c(unsupported, paste0(scope, ": ", attribute, "$width (numeric storage)"))
        }
    }

    if (length(unsupported) > 0L) {
        warning("Vendor metadata not yet supported by the ", target, " writer: ",
            paste(unsupported, collapse = "; "), ".", call. = FALSE)
    }

    return(list(data = data, options = options))
}

validate_spss_attributes <- function(data) {
    validate <- function(object, scope) {
        attributes <- attr(object, "spss.attributes", exact = TRUE)

        if (is.null(attributes)) {
            return(invisible(NULL))
        }

        if (!is.list(attributes) || is.object(attributes)) {
            stop(scope, ": 'spss.attributes' must be a named list of character vectors.", call. = FALSE)
        }

        if (length(attributes) == 0L) {
            return(invisible(NULL))
        }

        attribute_names <- names(attributes)

        if (is.null(attribute_names) || anyNA(attribute_names) ||
            any(!nzchar(attribute_names)) || anyDuplicated(toupper(attribute_names)) > 0L) {
            stop(scope, ": SPSS custom attribute names must be nonempty and unique ignoring case.", call. = FALSE)
        }

        if (any(nchar(enc2utf8(attribute_names), type = "bytes") > 64L) ||
            any(!grepl("^[$]?[[:alpha:]@][[:alnum:]@#$_.]*$", attribute_names))) {
            stop(scope, ": invalid SPSS custom attribute name (maximum 64 UTF-8 bytes).", call. = FALSE)
        }

        for (name in attribute_names) {
            values <- attributes[[name]]

            if (!is.character(values) || length(values) == 0L || anyNA(values)) {
                stop(scope, ": custom attribute '", name,
                    "' must contain one or more non-missing strings.", call. = FALSE)
            }

            if (any(grepl("\n", values, fixed = TRUE))) {
                stop(scope, ": custom attribute '", name,
                    "' cannot contain line feeds in SPSS files.", call. = FALSE)
            }
        }

        return(invisible(NULL))
    }

    validate(data, "Dataset")

    for (name in names(data)) {
        validate(data[[name]], paste0("Variable '", name, "'"))
    }

    return(invisible(NULL))
}

prepare_spss_mr_sets <- function(data) {
    metadata <- attr(data, "spss_metadata", exact = TRUE)
    sets <- metadata[["mr_sets", exact = TRUE]]

    if (is.null(sets) || length(sets) == 0L) {
        return(data)
    }

    scalar_string <- function(value) {
        return(is.character(value) && length(value) == 1L && !is.na(value) &&
            !grepl("[\r\n]", value))
    }

    set_names <- names(sets)

    if (is.null(set_names) || anyNA(set_names) || any(!nzchar(set_names)) ||
        anyDuplicated(toupper(set_names)) > 0L) {
        stop("'spss_metadata$mr_sets' must have nonempty, case-insensitively unique set names.",
            call. = FALSE)
    }

    fields <- c("type", "label", "is_dichotomy", "counted_value", "counted_string",
        "variables", "label_from_var_label")

    for (name in set_names) {
        scope <- paste0("'spss_metadata$mr_sets[[\"", name, "\"]]' ")
        entry <- sets[[name]]
        if (!is.list(entry) || is.object(entry) || is.null(names(entry)) ||
            anyNA(names(entry)) || anyDuplicated(names(entry)) > 0L ||
            any(!is.element(names(entry), fields))) {
            stop(scope, "must be a named list of supported fields.", call. = FALSE)
        }

        type <- entry[["type", exact = TRUE]]
        variables <- entry[["variables", exact = TRUE]]

        if (!scalar_string(type) || !is.element(type, c("C", "D", "E"))) {
            stop(scope, "requires type 'C', 'D' or 'E'.", call. = FALSE)
        }

        if (!is.character(variables) || length(variables) < 2L || anyNA(variables) ||
            anyDuplicated(variables) > 0L || any(!is.element(variables, names(data)))) {
            stop(scope, "requires at least two distinct, existing variable names.", call. = FALSE)
        }

        string_variables <- unname(vapply(data[variables], is.character, logical(1L)))

        if (any(string_variables) && !all(string_variables)) {
            stop(scope, "cannot mix string and numeric variables.", call. = FALSE)
        }

        label <- entry[["label", exact = TRUE]]
        label_source <- entry[["label_from_var_label", exact = TRUE]]
        dichotomy <- entry[["is_dichotomy", exact = TRUE]]

        if (is.null(label)) {
            label <- ""
        }

        if (is.null(label_source)) {
            label_source <- FALSE
        }

        if (!scalar_string(label) || nchar(enc2utf8(label), type = "bytes") > 256L ||
            !is.logical(label_source) || length(label_source) != 1L || is.na(label_source) ||
            (label_source && (type != "E" || nzchar(label)))) {
            stop(scope, "has an invalid label or label_from_var_label flag.", call. = FALSE)
        }

        if (!is.null(dichotomy) && !identical(dichotomy, type != "C")) {
            stop(scope, "has is_dichotomy inconsistent with type.", call. = FALSE)
        }

        counted <- entry[["counted_string", exact = TRUE]]
        counted_value <- entry[["counted_value", exact = TRUE]]

        if (!is.null(counted_value) && (!is.numeric(counted_value) ||
            length(counted_value) != 1L || !is.finite(counted_value) ||
            counted_value != trunc(counted_value))) {
            stop(scope, "requires a finite whole-number counted_value.", call. = FALSE)
        }

        if (type == "C") {
            if (!is.null(counted) || !is.null(counted_value)) {
                stop(scope, "cannot specify a counted value for a category set.", call. = FALSE)
            }
        }
        else {
            if (is.null(counted) && !any(string_variables) && !is.null(counted_value)) {
                counted <- sprintf("%.0f", counted_value)
            }

            if (!scalar_string(counted)) {
                stop(scope, "requires counted_string (or counted_value for numeric variables).",
                    call. = FALSE)
            }

            counted <- sub(" +$", "", counted)

            if (!any(string_variables) && (!grepl("^[+-]?[0-9]+$", counted) ||
                !is.finite(suppressWarnings(as.numeric(counted))))) {
                stop(scope, "requires an integer counted value for numeric variables.", call. = FALSE)
            }
        }

        sets[[name]] <- list(type = type, label = label, is_dichotomy = type != "C",
            counted_value = counted_value, counted_string = counted, variables = variables,
            label_from_var_label = label_source)
    }

    metadata$mr_sets <- sets
    attr(data, "spss_metadata") <- metadata

    return(data)
}

validate_dataset_label <- function(label, vendor, max_bytes) {
    valid <- is.null(label) || (is.character(label) && length(label) == 1L &&
        !is.na(label) && nchar(enc2utf8(label), type = "bytes") <= max_bytes)

    if (!isTRUE(valid)) {
        stop("Dataset: ", vendor, " label must be NULL or one non-missing character string ",
            "of at most ", max_bytes, " UTF-8 bytes.", call. = FALSE)
    }

    return(invisible(NULL))
}

write_sav <- function(data, path, compress = "byte", adjust_tz = TRUE,
    label = attr(data, "label", exact = TRUE)) {
    validate_dataset_label(label, "SPSS", 64L)
    data_out <- prepare_vendor_metadata(data, "sav")$data
    attr(data_out, "label") <- label
    validate_spss_attributes(data_out)
    notes <- attr(data_out, "notes", exact = TRUE)

    if (!is.null(notes) && (!is.character(notes) || anyNA(notes) ||
        any(nchar(enc2utf8(notes), type = "bytes") > 80L))) {
        stop("Dataset: SPSS notes must be non-missing strings of at most 80 UTF-8 bytes each.",
            call. = FALSE)
    }

    if (isTRUE(compress)) {
        compress <- "zsav"
    } else if (isFALSE(compress)) {
        compress <- "none"
    }

    if (isTRUE(adjust_tz)) {
        data_out <- adjust_tz(data_out)
    }

    data_out <- as_declared(data_out)
    data_out <- prepare_spss_mr_sets(data_out)
    invisible(.Call(
        "declared_write_sav_",
        data_out,
        normalizePath(path, mustWork = FALSE),
        compress,
        PACKAGE = "DDIwR"
    ))
    invisible(data)
}

validate_stata_characteristics <- function(data, file_format) {
    unicode <- file_format >= 118L

    validate_values <- function(values, scope) {
        encoded <- enc2utf8(values)

        if (!unicode) {
            encoded <- iconv(encoded, from = "UTF-8", to = "windows-1252", sub = NA_character_)

            if (anyNA(encoded)) {
                stop(scope, ": Stata versions before 14 require characteristics representable in Windows-1252.", call. = FALSE)
            }
        }

        if (any(nchar(encoded, type = "bytes") > 67783L)) {
            stop(scope, ": Stata characteristic values cannot exceed 67783 bytes (67784 including the terminator).", call. = FALSE)
        }

        return(invisible(NULL))
    }

    validate <- function(object, scope) {
        characteristics <- attr(object, "stata.characteristics", exact = TRUE)

        if (is.null(characteristics)) {
            return(invisible(NULL))
        }

        if (!is.list(characteristics) || is.object(characteristics)) {
            stop(scope, ": 'stata.characteristics' must be a named list of single strings.", call. = FALSE)
        }

        if (length(characteristics) == 0L) {
            return(invisible(NULL))
        }

        characteristic_names <- names(characteristics)

        if (is.null(characteristic_names) || anyNA(characteristic_names) ||
            any(!nzchar(characteristic_names)) || anyDuplicated(characteristic_names) > 0L) {
            stop(scope, ": Stata characteristic names must be nonempty and unique (case sensitive).", call. = FALSE)
        }

        if (unicode) {
            name_pattern <- "^[[:alpha:]_][[:alnum:]_]*$"
            max_name_bytes <- 128L
        }
        else {
            name_pattern <- "^[A-Za-z_][A-Za-z0-9_]*$"
            max_name_bytes <- 32L
        }

        if (any(!grepl(name_pattern, characteristic_names)) ||
            any(nchar(characteristic_names, type = "chars") > 32L) ||
            any(nchar(enc2utf8(characteristic_names), type = "bytes") > max_name_bytes)) {
            stop(scope, ": invalid Stata characteristic name (maximum 32 characters).", call. = FALSE)
        }

        for (name in characteristic_names) {
            value <- characteristics[[name]]

            if (!is.character(value) || length(value) != 1L || is.na(value)) {
                stop(scope, ": characteristic '", name, "' must be a single non-missing string.", call. = FALSE)
            }

            validate_values(value, scope)
        }

        return(invisible(NULL))
    }

    validate(data, "Dataset")

    for (name in names(data)) {
        validate(data[[name]], paste0("Variable '", name, "'"))
    }

    characteristics <- attr(data, "stata.characteristics", exact = TRUE)
    has_note_characteristics <- any(grepl("^note[0-9]+$", names(characteristics)))
    notes <- attr(data, "notes", exact = TRUE)

    if (!has_note_characteristics && !is.null(notes)) {
        if (!is.character(notes) || anyNA(notes)) {
            stop("Dataset: Stata notes must be non-missing strings.", call. = FALSE)
        }

        validate_values(notes, "Dataset notes")
    }

    return(invisible(NULL))
}

write_dta <- function(
    data, path, version = 14, label = attr(data, "label"), strl_threshold = 2045,
    adjust_tz = TRUE, dictionary = NULL
) {
    overrides <- character()

    if (!missing(version)) {
        overrides <- "version"
    }

    prepared <- prepare_vendor_metadata(data, "dta", overrides)

    if (missing(version) && !is.null(prepared$options[["version", exact = TRUE]])) {
        version <- prepared$options[["version", exact = TRUE]]
    }

    file_format <- stata_file_format(version)
    data_out <- prepared$data
    validate_stata_characteristics(data_out, file_format)

    if (file_format < 117L) {
        for (name in names(data_out)) {
            metadata <- vendor_metadata(data_out[[name]], "stata_metadata")

            if (identical(metadata[["type", exact = TRUE]], "strL")) {
                stop("Variable '", name, "': 'stata_metadata$type = strL' requires Stata 13 or later.",
                    call. = FALSE)
            }
        }
    }

    if (isTRUE(adjust_tz)) {
        data_out <- adjust_tz(data_out)
    }

    data_out <- as_declared(data_out)
    invisible(.Call(
        "declared_write_dta_",
        data_out,
        normalizePath(path, mustWork = FALSE),
        file_format,
        label,
        as.integer(strl_threshold),
        dictionary,
        PACKAGE = "DDIwR"
    ))
    invisible(data)
}

write_sas <- function(data, path, dictionary = NULL, catalog_file = NULL, auto_catalog = FALSE) {
    validate_dataset_label(attr(data, "label", exact = TRUE), "SAS7BDAT", 256L)
    data_out <- prepare_vendor_metadata(data, "sas7bdat")$data
    data_out <- as_declared(data_out)

    automatic_catalog <- auto_catalog && is.null(catalog_file) && any(vapply(data_out,
        function(column) {
            labels <- attr(column, "labels", exact = TRUE)
            rules <- vendor_metadata(column, "sas_metadata")[["catalog_rules", exact = TRUE]]

            return(length(labels) > 0L || !is.null(rules) ||
                (is.null(labels) && is.factor(column) && nlevels(column) > 0L))
        }, logical(1)
    ))

    if (automatic_catalog) {
        catalog_file <- paste0(tools::file_path_sans_ext(path), ".sas7bcat")
    }

    if (!is.null(catalog_file)) {
        destinations <- c(path, catalog_file)

        if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
            !is.character(catalog_file) || length(catalog_file) != 1L ||
            is.na(catalog_file) || !nzchar(catalog_file)) {
            stop("path and catalog_file must be single non-missing file paths.", call. = FALSE)
        }

        destinations <- normalizePath(path.expand(destinations), mustWork = FALSE)
        destinations <- file.path(normalizePath(dirname(destinations), mustWork = FALSE),
            basename(destinations))

        if (anyDuplicated(destinations) > 0L || any(dir.exists(destinations)) ||
            any(!dir.exists(dirname(destinations)))) {
            stop("SAS data and catalog destinations must be distinct files in existing directories.",
                call. = FALSE)
        }

        prepared <- prepare_sas_catalog(data_out)
        data_out <- prepared$data
        staged <- vapply(dirname(destinations), function(directory) {
            return(tempfile(".ddiwr-sas-", tmpdir = directory))
        }, character(1))
        on.exit(unlink(staged), add = TRUE)

        .Call("declared_write_sas_catalog_", data_out, staged[2], prepared$formats,
            dictionary, PACKAGE = "DDIwR")
        .Call("declared_write_sas_", data_out, staged[1], dictionary, PACKAGE = "DDIwR")
        publish_sas_pair(staged, destinations)

        return(invisible(data_out))
    }

    invisible(.Call(
        "declared_write_sas_",
        data_out,
        normalizePath(path, mustWork = FALSE),
        dictionary,
        PACKAGE = "DDIwR"
    ))
    invisible(data_out)
}

write_xpt <- function(
    data, path, version = 8, name = NULL, label = attr(data, "label"),
    adjust_tz = TRUE, dictionary = NULL
) {
    overrides <- character()

    if (!missing(version)) {
        overrides <- c(overrides, "version")
    }

    if (!missing(name)) {
        overrides <- c(overrides, "table_name")
    }

    prepared <- prepare_vendor_metadata(data, "xpt", overrides)

    if (missing(version) && !is.null(prepared$options[["version", exact = TRUE]])) {
        version <- prepared$options[["version", exact = TRUE]]
    }

    if (missing(name)) {
        name <- prepared$options[["table_name", exact = TRUE]]
    }

    if (is.null(name)) {
        name <- tools::file_path_sans_ext(basename(path))
    }

    data_out <- prepared$data

    if (isTRUE(adjust_tz)) {
        data_out <- adjust_tz(data_out)
    }

    data_out <- as_declared(data_out)
    invisible(.Call(
        "declared_write_xpt_",
        data_out,
        normalizePath(path, mustWork = FALSE),
        as.integer(version),
        name,
        label,
        dictionary,
        PACKAGE = "DDIwR"
    ))
    invisible(data)
}
