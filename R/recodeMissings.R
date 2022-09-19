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
#' available data and metadata, using negative numbers starting from -91 and up to
#' 27 letters starting with "a".
#'
#' If the dataset contains mixed variables with SPSS and Stata style missing
#' values, unless otherwise specified in a dictionary it uses other codes than the
#' existing ones.
#'
#' For the SPSS type of missing values, the resulting variables are coerced to a
#' declared labelled format.
#'
#' Unlike SPSS, Stata does not allow labels for character values. Both cannot be
#' transported from SPSS to Stata, it is either one or another. If labels are
#' more important to preserve than original values (especially the information
#' about the missing values), the argument \code{chartonum} replaces all character
#' values with suitable, non-overlapping numbers and adjusts the labels accordingly.
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
#' recodeMissings(x, to = "Stata", dictionary = c("a" = -91, "b" = -92))
#'
#' recodeMissings(x, to = "SPSS")
#'
#' recodeMissings(x, to = "SPSS", dictionary = c("a" = -91))
#'
#' @return A data frame with all missing values recoded consistently.
#'
#' @author Adrian Dusa
#'
#' @param dataset A data frame
#' @param to Software to recode missing values for
#' @param dictionary
#' A named vector, with corresponding Stata missing codes to SPSS missing values
#' @param ... Other internal arguments
#'
#' @export


`recodeMissings` <- function(
    dataset, to = c("SPSS", "Stata"), dictionary = NULL, ...
) {

    to <- toupper(match.arg(to))
    too_many <- FALSE

    dots <- list(...)

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

    spss <- unlist(lapply(dataset, function(x) {
        # it makes sense to check for character variables, since
        # neither Stata nor SAS do not accept missing values for chars
        # technically, a char var with missing value would be "valid" in SPSS
        # but it doesn't matter if recoding to Stata, it's like it would not exist
        !is.character(x) &&
        !is.null(attr(x, "labels", exact = TRUE)) &&
        (
            inherits(x, "haven_labelled_spss") || inherits(x, "declared")
        )
    }))

    if ((sum(spss) == 0 & to == "STATA") | (sum(spss) == ncol(dataset) & to == "SPSS")) {
        if (isTRUE(dots$return_dictionary)) {
            return(NULL)
        }
        return(dataset)
    }

    allMissing <- list()

    for (variable in names(dataset)) {
        x <- dataset[[variable]]
        attrx <- attributes(x)
        attributes(x) <- NULL

        metadata <- dataDscr[[variable]]
        labels <- metadata[["labels"]]
        missing <- NULL

        if (is.element("na_values", names(metadata))) {
            missing <- metadata[["na_values"]]
        }

        if (is.element("na_range", names(metadata))) {
            na_range <- metadata$na_range
            misvals <- x[x >= na_range[1] & x <= na_range[2]]
            missing <- c(missing, misvals[!is.na(misvals)], na_range)

            if (!is.null(labels)) {
                if (admisc::possibleNumeric(labels)) {
                    labels <- admisc::asNumeric(labels)
                    missing <- c(
                        missing,
                        labels[labels >= na_range[1] & labels <= na_range[2]]
                    )
                }
            }

            missing <- sort(unique(missing))
        }

        if (
            is.element("labels", names(metadata)) &&
            any(is.element(missing, metadata[["labels"]]))
        ) {
            wel <- which(is.element(missing, metadata[["labels"]]))
            names(missing)[wel] <- names(metadata[["labels"]])[
                match(missing[wel], metadata[["labels"]])
            ]
            missing <- missing[wel]
        }

        allMissing[[variable]] <- missing

    }


    missingSPSS <- missingStata <- NULL

    # just to initiate
    umis <- data.frame(labels = c(), codes = c(), rec = c())

    if (sum(spss) > 0) {
        umis <- unlist(unname(allMissing[names(spss)[spss]]))
        umis <- unique(data.frame(labels = names(umis), codes = umis))

        if (nrow(umis) == 0) {
            # There is no information about missing values
            return(dataset)
        }

        umis <- umis[order(umis$labels, umis$codes), ]
        umis$rec <- umis$codes
        ulabels <- unique(umis[, 1])

        for (i in seq(length(ulabels))) {
            if (ulabels[i] != "") {
                uel <- is.element(umis$labels, ulabels[i])
                umis$rec[uel] <- umis$codes[uel][length(umis$codes[uel])]
            }
        }

        umis <- umis[order(umis$rec), ]
        neg <- umis$rec < 0
        wneg <- c()
        if (any(neg)) {
            wneg <- which(neg)[order(umis$rec[neg], decreasing = TRUE)]
        }
        umis <- umis[c(wneg, which(!neg)), ]
        # print(umis)
        missingSPSS <- setNames(umis$codes, umis$rec)
        # names(missingSPSS) <- umis$rec

        # missingSPSS <- sort(unique(unname(unlist(allMissing[spss]))))
    }

    if (sum(!spss) > 0) {
        missingStata <- sort(unique(unlist(allMissing[!spss])))
    }

    if (to == "SPSS") {
        torecode <- missingStata
        # return(dataset)
        temp <- unlist(lapply(undeclare(dataset), function(x) {
            attributes(x) <- NULL
            if (is.character(x)) return(NA)
            return(x[x < 0])
        }))
        # temp <- unlist(lapply(dataset, function(x) {
        #     attributes(x) <- NULL
        #     if (is.character(x)) return(NA)
        #     return(x[x < 0])
        # }))
        attributes(temp) <- NULL
        all_neg <- unique(temp)

        if (!identical(all_neg, NA) && length(all_neg) > 0) {
            all_neg <- all_neg[!is.na(all_neg)]
        }

        if (!is.null(torecode)) {
            toreplace <- 90 + seq(length(missingStata) + length(all_neg) + length(missingSPSS))
            toreplace <- -1 * setdiff(toreplace, missingSPSS)
            toreplace <- setdiff(toreplace, all_neg)
            torecode <- setNames(toreplace[seq(length(missingStata))], missingStata)
            # names(torecode) <- missingStata
        }
    }
    else if (to == "STATA") {

        torecode <- missingSPSS

        if (!is.null(torecode)) {

            if (sum(spss) == ncol(dataset)) {

                if (length(unique(names(torecode))) > length(letters)) {
                    too_many <- TRUE
                }
                else {
                    unms <- unique(names(torecode))
                    names(torecode) <- letters[match(names(torecode), unms)]
                }
            }
            else { # mix of SPSS and non SPSS (perhaps even Stata) variables
                available <- setdiff(letters, missingStata)
                if (length(unique(names(torecode))) > length(available)) {
                    too_many <- TRUE
                }
                else {
                    # names(torecode) <- available[seq(length(torecode))]
                    unms <- unique(names(torecode))
                    names(torecode) <- available[match(names(torecode), unms)]
                }
            }
        }
    }

    if (is.null(dictionary)) {
        dictionary <- torecode
        nms <- names(dictionary)
        if (any(duplicated(dictionary))) {
            for (i in which(duplicated(dictionary))) {
                first <- which(dictionary == dictionary[i])[1]
                nms[nms == nms[i]] <- nms[first]
            }
            names(dictionary) <- nms
            dfd <- data.frame(nms = nms, vals = dictionary)
            dfd <- unique(dfd)
            dfd <- dfd[order(dfd$vals, dfd$nms), ]
            dictionary <- setNames(dfd$vals, dfd$nms)
            # names(dictionary) <- dfd$nms
        }

        unms <- unique(nms)
        if (all(is.element(unms, letters))) {
            for (i in seq(length(unms))) {
                nms[nms == unms[i]] <- letters[i]
            }
        }
        else {
            for (i in seq(length(unms))) {
                nms[nms == unms[i]] <- -90 - i
            }
        }

        if (isTRUE(dots$return_dictionary)) {
            return(dictionary)
        }
    }
    else if (!is.null(torecode)) {
        if (to == "SPSS") {
            diffs <- setdiff(names(torecode), names(dictionary))
        }
        else if (to == "STATA") {
            diffs <- setdiff(unname(torecode), unname(dictionary))
        }

        if (length(diffs) > 0) {
            admisc::stopError(
                "Missing values in the data not present in the dictionary."
            )
        }
    }

    na_values <- NULL
    # now recode the respective variables according to the dictionary
    if (!is.null(dictionary)) {
        nms <- names(dictionary)
        na_values <- unname(dictionary)
    }

    for (i in seq(ncol(dataset))) {
        # print(i)
        x <- unclass(declared::undeclare(dataset[[i]]))
        metadata <- dataDscr[[i]]
        labels <- metadata[["labels"]]
        na_values_i <- metadata[["na_values"]]
        na_range_i <- metadata[["na_range"]]

        if (!is.null(na_values_i) | !is.null(na_range_i)) {
            if (to == "SPSS") {
                if (!spss[i] && !is.null(dictionary)) {

                    na_values_i <- na_values[is.element(nms, metadata[["na_values"]])]
                    nms_i <- nms[is.element(nms, metadata[["na_values"]])]

                    # if (i == 10) print(na_values_i)
                    if (length(na_values_i) > 0) {
                        for (d in seq(length(na_values_i))) {
                            if (nchar(nms[d]) == 1) {
                                x[haven::is_tagged_na(x, nms_i[d])] <- na_values_i[d]
                                labels[haven::is_tagged_na(labels, nms_i[d])] <- na_values_i[d]
                            }
                        }
                    }
                }

                if (!is.null(labels) && !admisc::possibleNumeric(labels)) {
                    x <- as.character(x)
                    if (length(na_values_i) > 0) {
                        na_values_i <- as.character(na_values_i)
                    }
                }

                # cat(
                #     paste(
                #         i,
                #         "--",
                #         paste(
                #             na_values,
                #             collapse = ","
                #         ),
                #         "--",
                #         paste(
                #             na_values_i,
                #             collapse = ","
                #         ),
                #         "--",
                #         paste(
                #             metadata[["na_values"]],
                #             collapse = ","
                #         ),
                #         "\n"
                #     )
                # )

                callist <- list(
                    x = x,
                    labels = labels,
                    label = metadata[["label"]]
                )

                if (length(na_values_i) > 0) {
                    if (is.character(na_values_i) && length(na_values_i) > 3) {
                        na_values_i <- na_values_i[1:3]
                    }

                    if (length(na_values_i) > 3) {
                        callist$na_range <- sort(range(na_values_i))
                    }
                    else {
                        callist$na_values <- na_values_i
                    }
                }

                if (to_declared) {
                    dataset[[i]] <- do.call(declared::declared, callist)
                }
                else {
                    dataset[[i]] <- do.call(haven::labelled_spss, callist)
                }

            }
            else if (to == "STATA") {

                if (spss[i] & !is.null(dictionary)) {
                    attributes(x) <- NULL

                    selection <- rep(TRUE, length(dictionary))

                    if (!is.null(na_values_i)) {
                        selection <- is.element(dictionary, na_values_i)
                    }
                    else if (!is.null(na_range_i)) {
                        selection <- as.numeric(dictionary) >= na_range_i[1] & as.numeric(dictionary) <= na_range_i[2]
                    }

                    dic_i <- dictionary[selection]


                    if (any(duplicated(dic_i))) {
                        # multiple missing labels with the same code
                        selabels <- umis$labels[selection]

                        if (!is.null(na_values[i])) {
                            na_labels <- names(labels)[
                                is.element(
                                    labels,
                                    if (is.numeric(labels)) as.numeric(na_values_i) else na_values_i
                                )
                            ]
                        }
                        else if (!is.null(na_range_i)) {
                            num_labels <- as.numeric(labels)
                            na_labels <- names(labels)[
                                num_labels >= na_range_i[1] & num_labels <= na_range_i[2]
                            ]
                        }

                        dic_i <- dic_i[is.element(selabels, na_labels)]
                    }

                    nms_i <- names(dic_i)
                    if (admisc::possibleNumeric(dic_i)) {
                        dic_i <- admisc::asNumeric(dic_i)
                    }

                    x <- admisc::asNumeric(x)

                    for (d in seq(length(dic_i))) {
                        x[is.element(x, dic_i[d])] <- haven::tagged_na(nms_i[d])
                        labels[
                            is.element(labels, dic_i[d])
                        ] <- haven::tagged_na(nms_i[d])
                    }

                    dataset[, i] <- haven::labelled(
                        x,
                        labels = labels,
                        label = metadata[["label"]]
                    )
                }
                else {
                    if (inherits(dataset[[i]], "declared") & !to_declared) {
                        dataset[[i]] <- x
                    }
                    # otherwise leave it intact
                }
            }
        }
    }

    attr(dataset, "dictionary") <- dictionary

    return(dataset)
}
