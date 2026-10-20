prepare_sas_catalog <- function(data) {
    formats <- character(length(data))
    references <- toupper(vapply(data, function(column) {
        format <- attr(column, "format.sas", exact = TRUE)

        if (is.null(format)) {
            return("")
        }

        return(sub("[0-9.]+$", "", format))
    }, character(1)))
    builtin <- c("F", "BEST", "BESTD", "E", "COMMA", "COMMAX", "DOLLAR",
        "DOLLARX", "PERCENT", "PERCENTN", "Z", "HEX", "BINARY", "IB", "PIB", "RB")

    for (index in seq_along(data)) {
        column <- data[[index]]
        labels <- attr(column, "labels", exact = TRUE)
        rules <- vendor_metadata(column, "sas_metadata")[["catalog_rules", exact = TRUE]]

        if (!is.null(rules)) {
            rules <- validate_sas_catalog_rules(rules, is.character(column))
            metadata <- vendor_metadata(column, "sas_metadata")
            metadata$catalog_rules <- rules
            attr(data[[index]], "sas_metadata") <- metadata
            labels <- setNames(rep(if (is.character(column)) { "" } else { 0 }, nrow(rules)),
                rules$label)
        }

        if (is.factor(column) && is.null(labels)) {
            labels <- setNames(seq_along(levels(column)), levels(column))
            attr(data[[index]], "labels") <- labels
        }

        if (is.null(labels) || length(labels) == 0L) {
            next
        }

        scope <- paste0("Variable '", names(data)[index], "'")

        character_keys <- is.character(column) && is.character(labels)
        numeric_keys <- is.element(typeof(column), c("integer", "double", "logical")) &&
            is.element(typeof(labels), c("integer", "double"))

        if (!character_keys && !numeric_keys) {
            stop(scope, ": SAS catalog label keys must match the column's character or numeric type.",
                call. = FALSE)
        }

        if (character_keys && is.null(rules)) {
            if (anyNA(labels) || any(nchar(enc2utf8(labels), type = "bytes") > 32767L)) {
                stop(scope, ": character SAS catalog keys must be non-missing and at most ",
                    "32767 UTF-8 bytes.", call. = FALSE)
            }

            # SAS compares fixed-width character values after space padding.
            labels <- sub(" +$", "", labels)

            if (anyDuplicated(unname(enc2utf8(labels))) > 0L) {
                stop(scope, ": duplicate character SAS catalog keys after removing trailing spaces.",
                    call. = FALSE)
            }

            tags <- vendor_metadata(column, "sas_metadata")[["label_tags", exact = TRUE]]

            if (!is.null(tags)) {
                stop(scope, ": sas_metadata$label_tags applies only to numeric SAS catalogs.",
                    call. = FALSE)
            }

            attr(data[[index]], "labels") <- labels
        }

        temporal <- NULL

        if (inherits(column, "Date")) {
            temporal <- "DATE9"
        }
        else if (inherits(column, "POSIXct")) {
            temporal <- "DATETIME25.6"
        }
        else if (inherits(column, "difftime")) {
            temporal <- "TIME20.6"
            units <- attr(column, "units", exact = TRUE)
            multiplier <- c(secs = 1, mins = 60, hours = 3600, days = 86400, weeks = 604800)

            if (length(units) != 1L || !is.element(units, names(multiplier))) {
                stop(scope, ": temporal SAS catalog columns require explicit difftime units.",
                    call. = FALSE)
            }

            # Missing codes identify categories, not durations in the column's units.
            tags <- vendor_metadata(column, "sas_metadata")[["label_tags", exact = TRUE]]
            ordinary <- !is.na(labels)

            if (is.null(rules) && !is.null(tags)) {
                if (length(tags) != length(labels)) {
                    stop(scope, ": 'sas_metadata$label_tags' must match the value-label vector.",
                        call. = FALSE)
                }

                ordinary <- ordinary & tags == ""
            }
            else if (is.null(rules)) {
                missing_codes <- c(attr(column, "na_values", exact = TRUE),
                    suppressWarnings(as.numeric(names(attr(column, "na_index", exact = TRUE)))))
                ordinary <- ordinary & !is.element(labels, missing_codes)
            }

            values <- unclass(plain_values(column))
            finite <- is.finite(values)
            values[finite] <- values[finite] * unname(multiplier[units])

            if (any(!is.finite(values[finite]))) {
                stop(scope, ": duration values overflow SAS time seconds.", call. = FALSE)
            }

            attributes(values) <- attributes(column)
            class(values) <- c(intersect(class(column), "declared"), "hms", "difftime")
            attr(values, "units") <- "secs"
            labels[ordinary] <- labels[ordinary] * unname(multiplier[units])
            if (is.null(rules)) {
                attr(values, "labels") <- labels
            }
            data[[index]] <- values
        }

        if (!is.null(temporal)) {
            metadata <- vendor_metadata(data[[index]], "sas_metadata")

            if (is.element("informat", names(metadata)) && is.null(metadata[["informat", exact = TRUE]])) {
                stop(scope, ": a labelled temporal SAS column cannot clear its informat; ",
                    "it preserves date/time meaning alongside the custom format.", call. = FALSE)
            }

            if (!is.element("informat", names(metadata))) {
                metadata$informat <- temporal
            }

            attr(data[[index]], "sas_metadata") <- metadata
        }

        texts <- names(labels)

        if (is.null(texts) || anyNA(texts) || any(nchar(enc2utf8(texts), type = "bytes") > 32767L)) {
            stop(scope, ": value labels need non-missing names of at most 32767 UTF-8 bytes.",
                call. = FALSE)
        }

        format <- attr(column, "format.sas", exact = TRUE)
        prefix <- ""

        if (character_keys) {
            prefix <- "$"
        }

        if (is.null(format)) {
            number <- index
            name <- paste0(prefix, "DDI", number, "F")

            while (is.element(name, references)) {
                number <- number + length(data)
                name <- paste0(prefix, "DDI", number, "F")
            }

            format <- name
        }
        else {
            parts <- regmatches(toupper(format), regexec(
                "^(\\$?[A-Z_](?:[A-Z0-9_]*[A-Z_])?)([0-9]+)?(?:\\.([0-9]*))?$",
                toupper(format), perl = TRUE
            ))[[1]]

            if (length(parts) == 0L || nchar(parts[2], type = "bytes") > 32L ||
                (startsWith(parts[2], "$") != character_keys) ||
                (!character_keys && is.element(parts[2], builtin)) ||
                (character_keys && is.element(parts[2], c("$CHAR", "$CHARZ", "$HEX",
                    "$QUOTE", "$UPCASE", "$VARYING", "$ASCII", "$EBCDIC")))) {
                stop(scope, ": catalog labels require a user-defined SAS format name; ",
                    "use clearFormats() to generate one.", call. = FALSE)
            }

            name <- parts[2]
            width <- suppressWarnings(as.numeric(parts[3]))
            digits <- suppressWarnings(as.numeric(parts[4]))

            if ((nzchar(parts[3]) && (!is.finite(width) || width > 65535)) ||
                (nzchar(parts[4]) && (!is.finite(digits) || digits > 65535))) {
                stop(scope, ": SAS format width and decimals must not exceed 65535.", call. = FALSE)
            }

            if (character_keys && is.finite(digits) && digits != 0) {
                stop(scope, ": character SAS formats cannot specify decimal places.", call. = FALSE)
            }

            format <- name

            if (is.finite(width) && width > 0) {
                format <- paste0(format, width)
            }

            if (is.finite(digits) && digits > 0) {
                format <- paste0(format, ".", digits)
            }
        }

        references[index] <- name
        formats[index] <- name
        attr(data[[index]], "format.sas") <- format
    }

    return(list(data = data, formats = unname(formats)))
}

publish_sas_pair <- function(staged, destinations) {
    backups <- rep(NA_character_, length(destinations))
    installed <- rep(FALSE, length(destinations))
    published <- FALSE

    on.exit({
        if (!published) {
            for (index in rev(seq_along(destinations))) {
                if (installed[index]) {
                    unlink(destinations[index])
                }

                if (!is.na(backups[index]) && !file.rename(backups[index], destinations[index])) {
                    warning("Could not restore SAS export destination; original retained at ",
                        backups[index], call. = FALSE)
                }
            }
        }
        else {
            unlink(backups[!is.na(backups)])
        }
    }, add = TRUE)

    for (index in seq_along(destinations)) {
        if (file.exists(destinations[index])) {
            backup <- tempfile(".ddiwr-sas-backup-", tmpdir = dirname(destinations[index]))

            if (!file.rename(destinations[index], backup)) {
                stop("Could not preserve SAS export destination: ", destinations[index], call. = FALSE)
            }

            backups[index] <- backup
        }
    }

    for (index in seq_along(destinations)) {
        if (!file.rename(staged[index], destinations[index])) {
            stop("Could not publish SAS export destination: ", destinations[index], call. = FALSE)
        }

        installed[index] <- TRUE
    }

    published <- TRUE

    return(invisible(NULL))
}

validate_sas_catalog_rules <- function(rules, character_keys) {
    if (!is.data.frame(rules) || nrow(rules) == 0L ||
        anyDuplicated(names(rules)) > 0L ||
        !all(is.element(c("start", "label"), names(rules)))) {
        stop("sas_metadata$catalog_rules must be a nonempty data frame with start and label.",
            call. = FALSE)
    }

    fields <- c("start", "end", "label", "hlo", "sexcl", "eexcl", "default", "fuzz")

    if (any(!is.element(names(rules), fields))) {
        stop("Unknown SAS catalog rule fields.", call. = FALSE)
    }

    defaults <- list(end = rules$start, hlo = "", sexcl = "N", eexcl = "N",
        default = max(1L, nchar(rules$label, type = "bytes")), fuzz = 0)

    for (field in setdiff(names(defaults), names(rules))) {
        rules[[field]] <- defaults[[field]]
    }

    rules <- rules[fields]

    for (field in fields[1:6]) {
        if (!is.character(rules[[field]]) || anyNA(rules[[field]])) {
            stop("SAS catalog rule ", field, " must contain non-missing strings.", call. = FALSE)
        }
    }

    if (any(nchar(enc2utf8(rules$label), type = "bytes") > 32767L) ||
        any(!is.element(rules$hlo, c("", "L", "H", "LH", "O"))) ||
        any(!is.element(rules$sexcl, c("N", "Y"))) ||
        any(!is.element(rules$eexcl, c("N", "Y")))) {
        stop("Invalid SAS catalog rule label, HLO or endpoint exclusion.", call. = FALSE)
    }

    width <- rules$default

    if (!is.numeric(width) || anyNA(width) || any(!is.finite(width)) ||
        any(width < 1 | width > 32767 | width != trunc(width)) ||
        length(unique(width)) != 1L) {
        stop("SAS catalog rule default width must be one shared integer from 1 through 32767.",
            call. = FALSE)
    }

    fuzz <- rules$fuzz

    if (!is.numeric(fuzz) || anyNA(fuzz) || any(!is.finite(fuzz)) ||
        any(fuzz < 0) || length(unique(fuzz)) != 1L ||
        (character_keys && any(fuzz != 0))) {
        stop("SAS catalog fuzz must be one shared finite nonnegative number; character rules use zero.",
            call. = FALSE)
    }

    rules$fuzz <- as.double(fuzz)
    rules$default <- as.integer(width)
    other <- rules$hlo == "O"
    low <- grepl("L", rules$hlo, fixed = TRUE)
    high <- grepl("H", rules$hlo, fixed = TRUE)

    if (sum(other) > 1L || any(other & (rules$sexcl != "N" | rules$eexcl != "N"))) {
        stop("SAS catalog rules allow one OTHER rule without excluded endpoints.", call. = FALSE)
    }

    rules$start[other] <- ""
    rules$end[other] <- ""
    rules$start[low] <- "LOW"
    rules$end[high] <- "HIGH"
    endpoints <- c(rules$start, rules$end)
    special <- c(other | low, other | high)

    if (character_keys) {
        if (any(nchar(enc2utf8(endpoints[!special]), type = "bytes") > 32767L)) {
            stop("Character SAS range endpoints must fit in 32767 UTF-8 bytes.",
                call. = FALSE)
        }

        endpoints <- sub(" +$", "", endpoints)
        rules$start <- endpoints[seq_len(nrow(rules))]
        rules$end <- endpoints[nrow(rules) + seq_len(nrow(rules))]
        keys <- unique(enc2utf8(endpoints[!special]))
        key_widths <- nchar(keys, type = "bytes")
        padded <- paste0(keys, strrep(" ", max(c(0L, key_widths)) - key_widths))
        keys <- keys[order(padded, method = "radix")]
        rank <- match(enc2utf8(endpoints), keys) + 1L
        rank[c(low, rep(FALSE, nrow(rules)))] <- 1L
        rank[c(rep(FALSE, nrow(rules)), high)] <- length(keys) + 2L
    }
    else {
        missing_keys <- c("._", ".", paste0(".", LETTERS))
        numeric <- suppressWarnings(as.numeric(endpoints))
        missing <- match(endpoints, missing_keys)

        if (any(!special & is.na(missing) & !is.finite(numeric))) {
            stop("Numeric SAS rule endpoints must be finite numbers or SAS missing tokens.",
                call. = FALSE)
        }

        keys <- sort(unique(numeric[!special & is.na(missing)]))
        rank <- match(numeric, keys) + 29L
        rank[!is.na(missing)] <- missing[!is.na(missing)]
        rank[c(low, rep(FALSE, nrow(rules)))] <- 29L
        rank[c(rep(FALSE, nrow(rules)), high)] <- length(keys) + 30L
    }

    start <- rank[seq_len(nrow(rules))]
    end <- rank[nrow(rules) + seq_len(nrow(rules))]
    selected <- which(!other)

    if (any(start[selected] > end[selected]) ||
        any(start[selected] == end[selected] &
            (rules$sexcl[selected] == "Y" | rules$eexcl[selected] == "Y"))) {
        stop("SAS catalog rules contain a reversed or empty interval.", call. = FALSE)
    }

    selected <- selected[order(start[selected], end[selected])]

    if (length(selected) > 1L) {
        previous <- head(selected, -1L)
        following <- tail(selected, -1L)
        overlaps <- end[previous] > start[following] |
            (end[previous] == start[following] &
                rules$eexcl[previous] == "N" & rules$sexcl[following] == "N")

        if (any(overlaps)) {
            stop("Overlapping SAS catalog rules are not supported.", call. = FALSE)
        }
    }

    rownames(rules) <- NULL

    return(rules)
}
