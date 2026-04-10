source_file <- function(file) {
    list(normalizePath(file, mustWork = FALSE))
}

as_declared <- function(data) {
    data[] <- lapply(data, function(x) {
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

        out <- declared::direct_declared(
            x = x,
            na_index = na_index,
            na_values = na_values,
            na_range = na_range,
            labels = labels,
            label = label
        )
        attr(out, "format.spss") <- attr(x, "format.spss", exact = TRUE)
        attr(out, "format.stata") <- attr(x, "format.stata", exact = TRUE)
        attr(out, "format.sas") <- attr(x, "format.sas", exact = TRUE)
        out
    })
    class(data) <- "data.frame"
    data
}

is_tag_code <- function(x) {
    is.character(x) && length(x) > 0 && all(grepl("^[a-z]$", tolower(x)))
}

is_stata_declared <- function(x) {
    if (!inherits(x, "declared")) {
        return(FALSE)
    }

    na_values <- attr(x, "na_values", exact = TRUE)
    na_index <- attr(x, "na_index", exact = TRUE)

    is_tag_code(na_values) ||
        is_tag_code(names(na_index))
}

needs_spss_recode <- function(data) {
    any(vapply(data, is_stata_declared, logical(1)))
}

make_declared <- function(
    x, labels = NULL, na_values = NULL, na_range = NULL, label = NULL,
    na_index = NULL, template = NULL
) {
    xclass <- class(x)
    xdate <- isTRUE(attr(template, "date", exact = TRUE)) || inherits(x, "Date")
    measurement <- attr(template, "measurement", exact = TRUE)
    format_spss <- attr(template, "format.spss", exact = TRUE)
    format_stata <- attr(template, "format.stata", exact = TRUE)
    format_sas <- attr(template, "format.sas", exact = TRUE)

    attributes(x) <- NULL
    attr(x, "na_index") <- na_index
    attr(x, "na_values") <- na_values
    attr(x, "na_range") <- na_range
    attr(x, "labels") <- labels
    attr(x, "label") <- label
    attr(x, "date") <- xdate
    attr(x, "measurement") <- measurement
    attr(x, "format.spss") <- format_spss
    attr(x, "format.stata") <- format_stata
    attr(x, "format.sas") <- format_sas
    class(x) <- unique(c("declared", xclass))
    x
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
        "measurement", "format.spss", "format.stata", "format.sas", "date"
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
        "ddiwr_recode_to_spss_",
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
        "ddiwr_recode_to_spss_full_",
        x,
        if (is.null(labels)) NULL else labels,
        if (is.null(na_values)) NULL else na_values,
        if (is.null(na_index)) NULL else na_index,
        as.character(old),
        new,
        PACKAGE = "DDIwR"
    )
}

read_sav <- function(file, encoding = NULL, user_na = FALSE) {
    if (is.null(encoding)) {
        encoding <- ""
    }

    num_threads <- getOption("DDIwR.readstat_threads", 0L)
    parallel_enabled <- isTRUE(getOption("DDIwR.readstat_parallel_sav", TRUE))
    data <- NULL

    if (parallel_enabled && is.numeric(num_threads) && length(num_threads) == 1L &&
        !is.na(num_threads) && as.integer(num_threads) != 1L) {
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
            -1L,
            0L,
            PACKAGE = "DDIwR"
        )
    }

    as_declared(data)
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
        as_declared()
}

read_por <- function(file, user_na = FALSE) {
    data <- .Call(
        "declared_df_parse_por_file",
        source_file(file),
        "",
        user_na,
        integer(),
        -1L,
        0L,
        PACKAGE = "DDIwR"
    )

    as_declared(data)
}

read_dta <- function(file, encoding = NULL, num_threads = getOption("DDIwR.readstat_threads", NULL)) {
    if (is.null(encoding)) {
        encoding <- ""
    }
    if (is.null(num_threads)) {
        num_threads <- 0L
    }

    data <- .Call(
        "declared_df_parse_dta_file_parallel",
        source_file(file),
        encoding,
        integer(),
        -1L,
        0L,
        as.integer(num_threads),
        PACKAGE = "DDIwR"
    )

    as_declared(data)
}

read_sas <- function(data_file, catalog_file = NULL, encoding = NULL, catalog_encoding = encoding) {
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
        -1L,
        0L,
        PACKAGE = "DDIwR"
    )

    as_declared(data)
}

read_xpt <- function(file) {
    data <- .Call(
        "declared_df_parse_xpt_file",
        source_file(file),
        integer(),
        -1L,
        0L,
        PACKAGE = "DDIwR"
    )

    as_declared(data)
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

materialize_na_index <- function(x, vendor) {
    if (inherits(x, "Date") || inherits(x, "POSIXct")) {
        out <- x
        attr(out, "labels") <- NULL
        attr(out, "na_values") <- NULL
        attr(out, "na_range") <- NULL
        attr(out, "na_index") <- NULL
        xclass <- class(out)
        if (!is.null(xclass)) {
            class(out) <- setdiff(xclass, "declared")
        }
        return(out)
    }

    na_index <- attr(x, "na_index", exact = TRUE)

    if (is.null(na_index) || length(na_index) == 0) {
        return(x)
    }

    positions <- unname(na_index)
    codes <- names(na_index)

    if (identical(vendor, "STATA") && any(is_tag_code(codes))) {
        return(x)
    }

    out <- x
    xclass <- class(out)
    if (!is.null(xclass)) {
        xclass <- setdiff(xclass, "declared")
        if (length(xclass) == 0) {
            attr(out, "class") <- NULL
        } else {
            class(out) <- xclass
        }
    }

    out <- if (inherits(x, "Date") || inherits(x, "POSIXct")) {
        adjust_datetime_from_r(out, vendor)
    } else {
        out
    }

    if (is.character(out)) {
        out[positions] <- codes
    } else {
        out[positions] <- type.convert(codes, as.is = TRUE)
    }

    attr(out, "na_index") <- NULL

    if (inherits(x, "Date")) {
        class(out) <- setdiff(class(x), "Date")
    } else if (inherits(x, "POSIXct")) {
        class(out) <- setdiff(class(x), c("POSIXct", "POSIXt"))
        attr(out, "tzone") <- NULL
    }

    out
}

prepare_export <- function(data, vendor) {
    data[] <- lapply(data, materialize_na_index, vendor = vendor)
    class(data) <- "data.frame"
    data
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

write_sav <- function(data, path, compress = "byte", adjust_tz = TRUE) {
    if (isTRUE(compress)) {
        compress <- "zsav"
    } else if (isFALSE(compress)) {
        compress <- "none"
    }

    data_out <- if (isTRUE(adjust_tz)) adjust_tz(data) else data
    data_out <- prepare_export(data_out, vendor = "SPSS")
    invisible(.Call(
        "declared_write_sav_",
        data_out,
        normalizePath(path, mustWork = FALSE),
        compress,
        PACKAGE = "DDIwR"
    ))
    invisible(data)
}

write_dta <- function(
    data, path, version = 14, label = attr(data, "label"), strl_threshold = 2045,
    adjust_tz = TRUE
) {
    data_out <- if (isTRUE(adjust_tz)) adjust_tz(data) else data
    data_out <- prepare_export(data_out, vendor = "STATA")
    invisible(.Call(
        "declared_write_dta_",
        data_out,
        normalizePath(path, mustWork = FALSE),
        stata_file_format(version),
        label,
        as.integer(strl_threshold),
        PACKAGE = "DDIwR"
    ))
    invisible(data)
}

write_sas <- function(data, path) {
    data <- prepare_export(data, vendor = "SAS")
    invisible(.Call(
        "declared_write_sas_",
        data,
        normalizePath(path, mustWork = FALSE),
        PACKAGE = "DDIwR"
    ))
    invisible(data)
}

write_xpt <- function(
    data, path, version = 8, name = NULL, label = attr(data, "label"),
    adjust_tz = TRUE
) {
    if (is.null(name)) {
        name <- tools::file_path_sans_ext(basename(path))
    }

    data_out <- if (isTRUE(adjust_tz)) adjust_tz(data) else data
    data_out <- prepare_export(data_out, vendor = "SAS")
    invisible(.Call(
        "declared_write_xpt_",
        data_out,
        normalizePath(path, mustWork = FALSE),
        as.integer(version),
        name,
        label,
        PACKAGE = "DDIwR"
    ))
    invisible(data)
}
