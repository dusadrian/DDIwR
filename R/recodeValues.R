`recodeValues` <- function(
    dataset, to = c("SPSS", "Stata"), dictionary = NULL, chartonum = TRUE, ...
) {

    to <- toupper(match.arg(to))
    too_many <- FALSE
    to_declared <- TRUE

    dots <- list(...)

    error_null <- TRUE
    if (is.element("error_null", names(dots))) {
        error_null <- dots$error_null
    }

    if (is.element("to_declared", names(dots))) {
        to_declared <- dots$to_declared
    }

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
        !is.null(attr(x, "labels")) &&
        (
            inherits(x, "haven_labelled_spss") || inherits(x, "declared")
        )
    }))

    if ((sum(spss) == 0 & to == "STATA") | (sum(spss) == ncol(dataset) & to == "SPSS")) {
        return(dataset)
    }

    # build a dictionary based on existing metadata
    if (to == "STATA") {
        dataset <- declared::as.haven(dataset)
    }

    allMissing <- list()
    
    for (variable in names(dataset)) {
        x <- dataset[[variable]]
        attrx <- attributes(x)
        attributes(x) <- NULL

        metadata <- dataDscr[[variable]]
        missing <- NULL

        if (is.element("na_values", names(metadata))) {
            missing <- metadata[["na_values"]]
        }

        if (is.element("na_range", names(metadata))) {
            na_range <- metadata$na_range
            misvals <- x[x >= na_range[1] & x <= na_range[2]]
            missing <- sort(unique(c(missing, misvals[!is.na(misvals)])))
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
            admisc::stopError("There is no information about categories, labels and missing values")
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
        missingSPSS <- umis$codes
        names(missingSPSS) <- umis$rec
        
        # missingSPSS <- sort(unique(unname(unlist(allMissing[spss]))))
    }

    if (sum(!spss) > 0) {
        missingStata <- sort(unique(unlist(allMissing[!spss])))
    }

    if (to == "SPSS") {
        torecode <- missingStata
        temp <- undeclare(dataset)
        all_neg <- unique(temp[temp < 0])
        attributes(all_neg) <- NULL
        
        if (length(all_neg) > 0) {
            all_neg <- all_neg[!is.na(all_neg)]
        }
        if (!is.null(torecode)) {
            toreplace <- 90 + seq(length(missingStata) + length(all_neg) + length(missingSPSS))
            toreplace <- -1 * setdiff(toreplace, missingSPSS)
            toreplace <- setdiff(toreplace, all_neg)
            torecode <- toreplace[seq(length(missingStata))]
            names(torecode) <- missingStata
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
        x <- unclass(dataset[[i]])
        metadata <- dataDscr[[i]]
        labels <- metadata[["labels"]]
        na_values_i <- metadata[["na_values"]]
        na_range_i <- metadata[["na_range"]]

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

            # print(callist)
            # cat("====================\n")
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

                pN <- TRUE
                nullabels <- is.null(labels)
                ux <- unique(c(x[!is.na(x)], unname(labels)))
                pnux <- admisc::possibleNumeric(ux, each = TRUE)

                pN <- ifelse(all(is.na(ux)), TRUE, all(pnux))

                nums <- c()
                if (any(pnux)) {
                    nums <- as.numeric(ux[pnux])
                }


                # transform character values and labels to numeric
                # Stata does not allow character values
                if (any(!pnux) & chartonum & !nullabels) {
                    nms_l <- names(labels)

                    cux <- ux[!pnux]
                    lux <- length(cux)
                    n <- l <- 1
                    
                    while (l <= lux) {
                        if (is.element(n, nums)) {
                            n <- n + 1
                        }
                        else {
                            x[x == cux[l]] <- n
                            labels[labels == cux[l]] <- n
                            n <- n + 1
                            l <- l + 1
                        }
                    }

                    labels <- as.numeric(labels)
                    names(labels) <- nms_l
                }
                
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

                if (i == 100) {
                    return(list(x, labels, metadata[["label"]]))
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

    attr(dataset, "dictionary") <- dictionary

    return(dataset)
}
