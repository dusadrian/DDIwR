#' @name recodeMissings
#'
#' @title Consistent recoding of (extended) missing values
#'
#' @description
#' A function to recode all missing values to either SPSS or Stata types,
#' uniformly (re)using the same codes across all variables.
#'
#' @details
#' When a dictionary is not provided, it is automatically constructed from the
#' available data and metadata, using negative numbers starting from -91 and up
#' to 27 letters starting with "a".
#'
#' If the dataset contains mixed variables with SPSS and Stata style missing
#' values, unless otherwise specified in a dictionary it uses other codes than
#' the existing ones.
#'
#' For the SPSS type of missing values, the resulting variables are coerced to a
#' declared labelled format.
#'
#' Unlike SPSS, Stata does not allow labels for character values. Both cannot be
#' transported from SPSS to Stata, it is either one or another. If labels are
#' more important to preserve than original values (especially the information
#' about the missing values), the argument `chartonum` replaces all character
#' values with suitable, non-overlapping numbers and adjusts the labels
#' accordingly.
#'
#' If no labels are found in the metadata, the original values are preserved.
#'
#' @examples
#' # x <- data.frame(
#' #     A = declared(
#' #         c(1:5, -92),
#' #         labels = c(Good = 1, Bad = 5, NR = -92),
#' #         na_values = -92
#' #     ),
#' #     B = labelled(
#' #         c(1:5, tagged_na('a')),
#' #         labels = c(DK = tagged_na('a'))
#' #     ),
#' #     C = declared(
#' #         c(1, -91, 3:5, -92),
#' #         labels = c(DK = -91, NR = -92),
#' #         na_values = c(-91, -92)
#' #     )
#' # )
#'
#' # xrec <- recodeMissings(x, to = "Stata")
#'
#' # attr(xrec, "dictionary")
#'
#' # dictionary <- data.frame(
#' #     old = c(-91, -92, "a"),
#' #     new = c("c", "d", "c")
#' # )
#' # recodeMissings(x, to = "Stata", dictionary = dictionary)
#'
#' # recodeMissings(x, to = "SPSS")
#'
#' # dictionary$new <- c(-97, -98, -97)
#'
#' # recodeMissings(x, to = "SPSS", dictionary = dictionary)
#'
#' # recodeMissings(x, to = "SPSS", start = 991)
#'
#' # recodeMissings(x, to = "SPSS", start = -8)
#'
#' @return A data frame with all missing values recoded consistently.
#'
#' @author Adrian Dusa
#'
#' @param dataset A data frame
#' @param to Software to recode missing values for
#' @param dictionary
#' A named vector, with corresponding Stata missing codes to SPSS missing values
#' @param start
#' A named vector, with corresponding Stata missing codes to SPSS missing values
#' @param ... Other internal arguments
#'
#' @export


`recodeMissings` <- function(
    dataset, to = c("SPSS", "Stata", "SAS"), dictionary = NULL, start = -91, ...
) {

    dots <- list(...)
    to <- toupper(match.arg(to))
    tospss <- to == "SPSS"

    to_declared <- !isFALSE(dots$to_declared)

    error <- TRUE
    if (is.data.frame(dataset)) {
        i <- 1
        while (i <= ncol(dataset) & error) {
            attrx <- attributes(dataset[[i]])
            if (any(is.element(
                c("label", "labels", "na_value", "na_range"),
                names(attrx)
            ))) {
                error <- FALSE
            }
            i <- i + 1
        }
    }

    if (error) {
        admisc::stopError(
            paste(
                "The input does not seem to contain any",
                "metadata about values and labels."
            )
        )
    }

    charvar <- unname(sapply(dataset, is.character))

    spss <- unname(sapply(dataset, function(x) {
        inherits(x, "declared") && !.ddiwr_is_stata_declared(x) &&
        (
            !is.null(attr(x, "labels", exact = TRUE)) ||
            !is.null(attr(x, "na_values", exact = TRUE)) ||
            !is.null(attr(x, "na_range", exact = TRUE))
        )
    }))

    stata <- unname(sapply(dataset, function(x) {
        inherits(x, "declared") && .ddiwr_is_stata_declared(x)
    }))

    allMissing <- list()

    for (variable in names(dataset[, spss | stata, drop = FALSE])) {
        template <- getElement(dataset, variable)
        x <- .ddiwr_plain_values(template)
        metadata <- .ddiwr_var_metadata(template)
        labels <- metadata$labels
        missing <- metadata$na_values
        na_range <- metadata$na_range
        na_index <- metadata$na_index

        if (!is.null(na_index) && length(na_index) > 0) {
            idx_values <- names(na_index)
            if (is.numeric(idx_values) || .Call("ddiwr_all_numeric_chars_", idx_values, PACKAGE = "DDIwR")) {
                idx_values <- admisc::asNumeric(idx_values)
            }
            missing <- c(missing, idx_values)
        }

        if (!is.null(na_range)) {
            misvals <- x[x >= na_range[1] & x <= na_range[2]]
            missing <- c(missing, misvals[!is.na(misvals)])

            if (!is.null(labels)) {
                if (is.numeric(labels) || .Call("ddiwr_all_numeric_chars_", labels, PACKAGE = "DDIwR")) {
                    lbls <- admisc::asNumeric(labels)
                    missing <- c(
                        missing,
                        lbls[lbls >= na_range[1] & lbls <= na_range[2]]
                    )
                }
            }
        }

        missing <- sort(unique(missing))

        if (length(missing) == 0) {
            missing <- NULL
        }

        if (!is.null(missing)) {
            names(missing) <- ""
        }

        if (
            is.element("labels", names(metadata)) &&
            any(is.element(missing, labels))
        ) {
            wel <- which(is.element(missing, labels))
            names(missing)[wel] <- names(labels)[
                match(missing[wel], labels)
            ]

        }

        allMissing[[variable]] <- missing
    }

    umispss <- unlist(unname(allMissing[spss[spss | stata]]))
    umistata <- unlist(unname(allMissing[stata[spss | stata]]))

    if (!is.null(umispss)) {
        umispss[order(names(umispss), decreasing = TRUE)]
        umispss <- umispss[!duplicated(umispss)]
    }

    if (!is.null(umistata)) {
        umistata[order(names(umistata), decreasing = TRUE)]
        umistata <- umistata[!duplicated(umistata)]
    }

    torecode <- data.frame(
        spss = c(rep(TRUE, length(umispss)), rep(FALSE, length(umistata))),
        label = c(names(umispss), names(umistata)),
        old = c(unname(umispss), unname(umistata))
    )

    if (nrow(torecode) == 0) {
        # There is no information about missing values
        return(dataset)
    }

    torecode <- torecode[order(torecode$label, decreasing = TRUE), ]
    torecode <- torecode[order(torecode$spss, decreasing = tospss), ]
    torecode$new <- torecode$old
    wi <- which(torecode$spss != tospss)

    if (length(wi) > 0) {
        for (i in wi) {
            if (nzchar(torecode$label[i])) {
                wl <- which(torecode$label == torecode$label[i])
                if (length(wl) > 1) {
                    torecode$new[i] <- torecode$old[wl[1]]
                }
            }
        }
    }

    torecode <- torecode[order(torecode$old), ]
    torecode <- torecode[order(torecode$spss, decreasing = tospss), ]


    nchars <- nchar(torecode$new)
    torecode$new <- sapply(
        strsplit(as.character(torecode$new), split = ""),
        function(x) {
            if (x[1] != "-") {
                x <- unique(x)
            }
            paste(x, collapse = "")
        }
    )

    torecode <- torecode[order(torecode$new, nchars), ]
    torecode <- torecode[order(torecode$spss, decreasing = tospss), ]


    torecode$new <- match(torecode$new, unique(torecode$new))
    if (tospss) {
        mcodes <- seq(max(5000, nrow(torecode) + 1)) + abs(start) - 1
        if (start < 0) {
            mcodes <- -1 * mcodes
        }

        torecode$new <- mcodes[torecode$new]
    }
    else {
        toomany <- max(torecode$new) > length(letters)
        if (toomany) {
            # TODO: recode variable by variable...?
            admisc::stopError("Too many overall missing values.")
        }
        torecode$new <- letters[torecode$new]
    }

    torecode$label[is.na(torecode$label)] <- ""

    if (is.null(dictionary)) {
        if (isTRUE(dots$return_dictionary)) {
            return(torecode[, -1])
        }
        dictionary <- torecode
    }
    else {
        if (length(setdiff(torecode$old, dictionary$old)) > 0) {
            admisc::stopError(
                "Missing values in the data not present in the dictionary."
            )
        }
    }


    # now recode the respective variables according to the dictionary
    old <- dictionary$old
    new <- dictionary$new
    pnold <- if (is.numeric(old)) rep(TRUE, length(old)) else admisc::possibleNumeric(old, each = TRUE)
    old_keys <- .ddiwr_match_keys(old)

    old <- tolower(old)
    if (is.character(new)) {
        new <- tolower(new)
    }

    for (variable in names(dataset)[spss | stata]) {
        template <- dataset[[variable]]
        x <- .ddiwr_plain_values(template)
        metadata <- .ddiwr_var_metadata(template)
        labels_x <- metadata$labels
        na_values_x <- metadata$na_values
        na_range_x <- metadata$na_range
        na_index_x <- metadata$na_index

        if (!is.null(na_values_x) | !is.null(na_range_x)) {
            if (tospss) {
                recoded <- .ddiwr_recode_to_spss_full_native(
                    x = x,
                    labels = labels_x,
                    na_values = na_values_x,
                    na_index = na_index_x,
                    old = old,
                    new = new
                )
                x <- recoded$x
                labels_x <- recoded$labels
                na_values_x <- recoded$na_values
                na_index_x <- recoded$na_index

                if (is.numeric(na_values_x) || .Call("ddiwr_all_numeric_chars_", na_values_x, PACKAGE = "DDIwR")) {
                    na_values_x <- unique(as.numeric(na_values_x))
                    na_values_x <- sort(na_values_x, decreasing = all(na_values_x < 0))
                }
                labels_numeric <- !is.null(labels_x) &&
                    (is.numeric(labels_x) || .Call("ddiwr_all_numeric_chars_", labels_x, PACKAGE = "DDIwR"))
                if (labels_numeric) {
                    label_names <- names(labels_x)
                    labels_x <- as.numeric(labels_x)
                    names(labels_x) <- label_names
                }
                if (!is.null(labels_x) && !labels_numeric) {
                    x <- as.character(x)
                    if (length(na_values_x) > 0) {
                        na_values_x <- as.character(na_values_x)
                    }
                }

                if (length(na_range_x) > 0) {

                    # update na_range, this is from SPSS to an SPSS type variable
                    updated <- logical(2)
                    copy_range <- na_range_x

                    if (na_range_x[1] == -Inf) {
                        updated[1] <- TRUE
                        if (start > 0) {
                            na_range_x[1] <- Inf
                            copy_range <- rev(copy_range)
                            updated <- rev(updated)
                        }
                    }

                    if (na_range_x[2] == Inf) {
                        updated[2] <- TRUE
                        if (start < 0) {
                            na_range_x[2] <- -Inf
                            copy_range <- rev(copy_range)
                            updated <- rev(updated)
                        }
                    }

                    na_range_x <- sort(na_range_x)

                    for (d in which(pnold)) {
                        if (identical(na_range_x[1], as.numeric(old[d]))) {
                            na_range_x[1] <- new[d]
                            updated[1] <- TRUE
                        }

                        if (identical(na_range_x[2], as.numeric(old[d]))) {
                            na_range_x[2] <- new[d]
                            updated[2] <- TRUE
                        }
                    }

                    if (!all(updated)) {
                        difference <- diff(copy_range)

                        if (updated[1]) {
                            na_range_x[2] <- na_range_x[1] + difference
                        }
                        else if (updated[2]) {
                            na_range_x[1] <- na_range_x[2] - difference
                        }
                        else {
                            na_range_x <- range(new)
                        }
                    }
                }

                dataset[[variable]] <- .ddiwr_make_declared(
                    x = x,
                    labels = labels_x,
                    na_values = if (length(na_values_x) > 0) na_values_x else NULL,
                    na_range = if (length(na_range_x) > 0) na_range_x else NULL,
                    label = metadata$label,
                    na_index = if (length(na_index_x) > 0) na_index_x else NULL,
                    template = template
                )
            }
            else if (is.numeric(x)) {
                # it makes sense to check for character variables, since neither
                # Stata nor SAS do not accept missing values for chars technically,
                # a char var with missing value would be "valid" in SPSS but it doesn't
                # matter if recoding to Stata or SAS, it's like it would not exist

                attributes(x) <- NULL

                selection <- logical(length(old))

                if (!is.null(na_values_x)) {
                    selection <- !is.na(match(old_keys, .ddiwr_match_keys(na_values_x)))
                }
                else if (!is.null(na_range_x)) {
                    if (any(pnold)) {
                        selection[pnold] <-
                            as.numeric(old[pnold]) >= min(na_range_x) &
                            as.numeric(old[pnold]) <= max(na_range_x)
                    }
                }

                if (any(selection)) {
                    old_x <- old[selection]
                    new_x <- new[selection]
                    na_index_x <- integer(0)
                    template_na_index <- attr(template, "na_index", exact = TRUE)

                    index <- match(.ddiwr_match_keys(x), .ddiwr_match_keys(old_x))
                    wh <- which(!is.na(index))

                    if (length(wh) > 0) {
                        x[wh] <- NA
                        na_index_x <- wh
                        names(na_index_x) <- new_x[index[wh]]
                    }

                    if (!is.null(template_na_index) && length(template_na_index) > 0) {
                        idx_codes <- .ddiwr_match_keys(names(template_na_index))
                        map_codes <- .ddiwr_match_keys(old_x)
                        idx_match <- match(idx_codes, map_codes)
                        use_idx <- which(!is.na(idx_match))

                        if (length(use_idx) > 0) {
                            extra_pos <- unname(template_na_index[use_idx])
                            extra_tag <- new_x[idx_match[use_idx]]
                            keep <- !(extra_pos %in% na_index_x)

                            if (any(keep)) {
                                extra_pos <- extra_pos[keep]
                                names(extra_pos) <- extra_tag[keep]
                                na_index_x <- c(na_index_x, extra_pos)
                            }
                        }
                    }

                    labels_x <- .ddiwr_recode_vector(labels_x, old_x, new_x)

                    dataset[[variable]] <- .ddiwr_make_declared(
                        x,
                        labels = labels_x,
                        na_values = unique(new_x),
                        label = metadata$label,
                        na_index = na_index_x,
                        template = template
                    )
                }
            }
        }
    }

    attr(dataset, "dictionary") <- dictionary[, -1]

    return(dataset)
}
