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
#' x <- data.frame(
#'     A = declared(
#'         c(1:5, -92),
#'         labels = c(Good = 1, Bad = 5, NR = -92),
#'         na_values = -92
#'     ),
#'     B = labelled(
#'         c(1:5, haven::tagged_na('a')),
#'         labels = c(DK = haven::tagged_na('a'))
#'     ),
#'     C = declared(
#'         c(1, -91, 3:5, -92),
#'         labels = c(DK = -91, NR = -92),
#'         na_values = c(-91, -92)
#'     )
#' )
#'
#' xrec <- recodeMissings(x, to = "Stata")
#'
#' attr(xrec, "dictionary")
#'
#' dictionary <- data.frame(
#'     spss = c(TRUE, TRUE, FALSE),
#'     label = c("DK", "NR", "DK"),
#'     old = c(-91, -92, "a"),
#'     new = c("c", "d", "c")
#' )
#' recodeMissings(x, to = "Stata", dictionary = dictionary)
#'
#' recodeMissings(x, to = "SPSS")
#'
#' dictionary$new <- c(-97, -98, -97)
#'
#' recodeMissings(x, to = "SPSS", dictionary = dictionary)
#'
#' recodeMissings(x, to = "SPSS", start = 991)
#'
#' recodeMissings(x, to = "SPSS", start = -8)
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

    error_null <- ifelse(isFALSE(dots$error_null), FALSE, TRUE)
    to_declared <- ifelse(isFALSE(dots$to_declared), FALSE, TRUE)


    if (is.data.frame(dataset)) {
        error <- TRUE
        i <- 1
        while (i <= ncol(dataset) & error) {
            attrx <- attributes(dataset[[i]])
            if (
                any(
                    is.element(
                        c("labels", "na_value", "na_range"),
                        names(attrx)
                    )
                )
            ) {
                error <- FALSE
            }
            i <- i + 1
        }

        if (error && error_null) {
            admisc::stopError(
                paste(
                    "The input does not seem to contain any",
                    "metadata about values and labels."
                )
            )
        }
    }
    else {
        admisc::stopError(
            "The input should be a data frame containing labelled variables."
        )
    }

    dataDscr <- collectMetadata(dataset, error_null = error_null)
    charvar <- unname(sapply(dataset, is.character))
    
    spss <- unname(sapply(dataset, function(x) {
        !is.null(attr(x, "labels", exact = TRUE)) &&
        (
            inherits(x, "haven_labelled_spss") || inherits(x, "declared")
        )
    }))

    stata <- unname(sapply(dataset, function(x) {
        is.double(x) &&
        !is.null(attr(x, "labels", exact = TRUE)) &&
        (
            inherits(x, "haven_labelled") & !inherits(x, "haven_labelled_spss")
        )
    }))

    allMissing <- list()

    for (variable in names(dataset[, spss | stata, drop = FALSE])) {
        x <- declared::undeclare(dataset[[variable]], drop = TRUE)
        attributes(x) <- NULL
        metadata <- dataDscr[[variable]]
        labels <- metadata[["labels"]]
        na_range <- metadata[["na_range"]]
        missing <- metadata[["na_values"]]

        if (!is.null(na_range)) {
            misvals <- x[x >= na_range[1] & x <= na_range[2]]
            missing <- c(missing, misvals[!is.na(misvals)])

            if (!is.null(labels)) {
                if (admisc::possibleNumeric(labels)) {
                    lbls <- admisc::asNumeric(labels)
                    missing <- c(
                        missing,
                        lbls[lbls >= na_range[1] & lbls <= na_range[2]]
                    )
                }
            }
        }

        missing <- sort(unique(missing))

        tagged <- haven::is_tagged_na(labels)
        if (any(tagged)) {
            labels[tagged] <- haven::na_tag(labels[tagged])
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

            # temp <- list()
            # for (variable in names(dataset[, spss, drop = FALSE])) {
            #     x <- declared::undeclare(dataset[[variable]], drop = TRUE)
            #     attributes(x) <- NULL
            #     metadata <- dataDscr[[variable]]
            #     labels <- metadata[["labels"]]
            #     na_values <- metadata[["na_values"]]
            #     na_range <- range(as.numeric(metadata[["na_range"]]))

            #     if (!is.null(na_range)) {
            #         temp[[variable]] <- x[x >= na_range[1] & x <= na_range[2]]
            #     }
            #     else {
            #         temp[[variable]] <- x[x < 0 & !is.element(x, na_values)]
            #     }
                
            # }

            ###-----------------------------------------------------------------
            # if (length(umispss) > 0) {
            #     temp <- lapply(dataset[spss], function(x) {
            #         na_values <- attr(x, "na_values")
            #         na_range <- attr(x, "na_range")

            #         if (is.character(x) | is.null(na_values)) {
            #             return(NA)
            #         }
                    
            #         x <- declared::undeclare(x, drop = TRUE)

            #         if (is.null(na_values)) {
            #             return(x[x >= min(na_range) & x <= max(na_range)])
            #         }
            #         else {
            #             return(x[x < 0 & !is.element(x, na_values)])
            #         }
            #     })

            #     attributes(temp) <- NULL
            #     # make sure the attributed missing values do not overlap existing negative numbers
            #     mcodes <- setdiff(mcodes, unique(temp))
            # }
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
            return(torecode)
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
    nold <- admisc::possibleNumeric(old, each = TRUE)

    for (variable in names(dataset[, spss | stata, drop = FALSE])) {
        x <- declared::undeclare(dataset[[variable]], drop = TRUE)
        attributes(x) <- NULL # for haven_labelled with tagged NAs

        metadata <- dataDscr[[variable]]
        labels_x <- metadata[["labels"]]
        na_values_x <- metadata[["na_values"]]
        na_range_x <- metadata[["na_range"]]

        if (!is.null(na_values_x) | !is.null(na_range_x)) {
            if (tospss) {
                selection <- logical(length(old))

                if (!is.null(na_values_x)) {
                    selection <- is.element(old, na_values_x)
                }
                else if (!is.null(na_range_x)) {
                    na_range_x <- range(as.numeric(na_range_x))
                    
                    if (any(nold)) {
                        selection[nold] <-
                            as.numeric(old[nold]) >= min(na_range_x) &
                            as.numeric(old[nold]) <= max(na_range_x)
                    }
                }

                if (any(selection)) {
                    old_x <- old[selection]
                    new_x <- new[selection]
                    spss_x <- dictionary$spss[selection]

                    for (d in seq(length(old_x))) {
                        
                        if (!is.null(na_values_x)) {
                            na_values_x[na_values_x == old_x[d]] <- new_x[d]
                        }

                        if (spss_x[d]) {
                            x[x == old_x[d]] <- new_x[d]

                            labels_x[labels_x == old_x[d]] <- new_x[d]
                        }
                        else {
                            x[
                                haven::is_tagged_na(x, old_x[d])
                            ] <- new_x[d]

                            labels_x[
                                haven::is_tagged_na(labels_x, old_x[d])
                            ] <- new_x[d]
                        }
                    }
                }

                if (admisc::possibleNumeric(na_values_x)) {
                    na_values_x <- as.numeric(na_values_x)
                }
                if (!is.null(labels_x) && !admisc::possibleNumeric(labels_x)) {
                    x <- as.character(x)
                    if (length(na_values_x) > 0) {
                        na_values_x <- as.character(na_values_x)
                    }
                }

                callist <- list(
                    x = x,
                    labels = labels_x,
                    label = metadata[["label"]]
                )

                if (length(na_values_x) > 0) {
                    callist$na_values <- na_values_x
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

                    for (d in which(nold)) {
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
                        # two scenarios, something like:
                        # 1. range c(-99, -95) and only one of them is in the dictionary

                        if (updated[1]) {
                            na_range_x[2] <- na_range_x[1] + difference
                        }
                        else if (updated[2]) {
                            na_range_x[1] <- na_range_x[2] - difference
                        }
                        else {
                            # 2. range is c(-99, -95), with say a value of -97 missing
                            # and none of them are in the dictionary
                            na_range_x <- range(new)
                        }
                    }

                    callist$na_range <- na_range_x
                }

                if (to_declared) {
                    dataset[[variable]] <- do.call(declared::declared, callist)
                }
                else {
                    dataset[[variable]] <- do.call(haven::labelled_spss, callist)
                }

            }
            else if (is.numeric(x)) {
                # it makes sense to check for character variables, since neither
                # Stata nor SAS do not accept missing values for chars technically,
                # a char var with missing value would be "valid" in SPSS but it doesn't
                # matter if recoding to Stata or SAS, it's like it would not exist
                
                attributes(x) <- NULL

                selection <- logical(length(old))

                if (!is.null(na_values_x)) {
                    selection <- is.element(old, na_values_x)
                }
                else if (!is.null(na_range_x)) {
                    nold <- admisc::possibleNumeric(old, each = TRUE)
                    
                    if (any(nold)) {
                        selection[nold] <-
                            as.numeric(old[nold]) >= min(na_range_x) &
                            as.numeric(old[nold]) <= max(na_range_x)
                    }
                }

                if (any(selection)) {
                    old_x <- old[selection]
                    new_x <- new[selection]

                    if (admisc::possibleNumeric(old_x)) {
                        old_x <- admisc::asNumeric(old_x)
                    }

                    for (d in seq(length(old_x))) {
                        x[is.element(x, old_x[d])] <- haven::tagged_na(new_x[d])
                        labels_x[
                            is.element(labels_x, old_x[d])
                        ] <- haven::tagged_na(new_x[d])
                    }

                    dataset[[variable]] <- haven::labelled(
                        x,
                        labels = labels_x,
                        label = metadata[["label"]]
                    )
                }
            }
        }
    }

    attr(dataset, "dictionary") <- dictionary

    return(dataset)
}
