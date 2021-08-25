`recodeMissing` <- 
function(
    dataset,
    to = c("SPSS", "Stata"),
    dictionary = NULL,
    ...) {

    to <- toupper(match.arg(to))

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

    # build a dictionary based on existing metadata

    allMissing <- vector(mode = "list", length = length(dataset))
    names(allMissing) <- names(dataset)

    for (i in seq(length(dataset))) {

        x <- dataset[[i]]
        attrx <- attributes(x)
        attributes(x) <- NULL

        if (is.element("declared", attrx$class) & to == "STATA") {
            na_index <- attrx$na_index
            
            if (!is.null(na_index)) {
                nms <- names(na_index)
                if (admisc::possibleNumeric(nms) || all(is.na(nms))) {
                    nms <- admisc::asNumeric(nms)
                    if (admisc::wholeNumeric(nms)) {
                        nms <- as.integer(nms)
                    }
                }
                
                x[na_index] <- nms
            }
            
            attrx$class <- c("haven_labelled_spss", "haven_labelled", "vctrs_vctr", class(x))

            dataset[[i]] <- x
            attrx$na_index <- NULL
            attributes(dataset[[i]]) <- attrx
        }

        attributes(x) <- NULL
        metadata <- dataDscr[[i]]
        missing <- c()

        if (is.element("na_values", names(metadata))) {
            missing <- metadata$na_values
        }

        if (is.element("na_range", names(metadata))) {
            na_range <- metadata$na_range
            misvals <- x[x >= na_range[1] & x <= na_range[2]]
            missing <- sort(unique(c(missing, misvals[!is.na(misvals)])))
        }

        allMissing[[i]] <- missing
    }


    missingSPSS <- missingStata <- NULL
    if (sum(spss) > 0) {
        missingSPSS <- sort(unique(unname(unlist(allMissing[spss]))))
    }

    if (sum(!spss)> 0) {
        missingStata <- sort(unique(unname(unlist(allMissing[!spss]))))
    }

    if (to == "SPSS") {
        if (is.null(missingStata)) {
            torecode <- NULL
        }
        else {
            if (sum(!spss) == 0) {
                if (error_null) {
                    message("Variables are already defined as SPSS")
                }
                return(dataset)
            }

            if (sum(spss) == 0) { # all variables are defined in Stata style
                torecode <- -1 * seq(length(missingStata))
                names(torecode) <- missingStata
            }
            else { # mix of Stata and SPSS variables
                torecode <- setdiff(
                    seq(-1, -100),
                    missingSPSS
                )[
                    seq(length(missingStata))
                ]
                names(torecode) <- missingStata
            }
        }
    }
    else if (to == "STATA") {

        if (is.null(missingSPSS)) {
            torecode <- NULL
        }
        else {
            if (sum(spss) == 0) { # No SPSS variables
                if (error_null) {
                    message("Variables are already defined as Stata")
                }
                return(dataset)
            }

            torecode <- missingSPSS
            torecode[torecode < 0] <- rev(torecode[torecode < 0])

            if (sum(!spss) == 0) { # all variables are defined in SPSS style
                if (length(torecode) > length(letters)) {
                    admisc::stopError(
                        paste(
                            "Too many unique missing values (%s),",
                            "Stata allows only 26."
                        )
                    )
                }
                names(torecode) <- letters[seq(length(missingSPSS))]
            }
            else { # mix of Stata and SPSS variables
                available <- setdiff(letters, missingStata)
                if (length(torecode) > length(available)) {
                    error <- paste(
                        "Too many unique missing values (%s),",
                        "with only %s Stata slots available."
                    )

                    admisc::stopError(
                        sprintf(
                            error,
                            length(torecode),
                            length(available)
                        )
                    )
                }
                names(torecode) <- setdiff(letters, missingStata)[
                    seq(length(torecode))
                ]
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
                paste(
                    "Found missing values in the data",
                    "that are not present in the dictionary."
                )
            )
        }
    }

    values <- NULL
    # now recode the respective variables according to the dictionary
    if (!is.null(dictionary)) {
        nms <- names(dictionary)
        values <- unname(dictionary)
    }

    for (i in seq(ncol(dataset))) {
        x <- unclass(dataset[[i]])
        metadata <- dataDscr[[i]]
        values_i <- metadata[["na_values"]]
        labels <- metadata[["labels"]]

        if (to == "SPSS") {
            if (!spss[i] & !is.null(dictionary)) {
                values_i <- values[is.element(nms, metadata[["na_values"]])]
                if (length(values_i) > 0) {
                    for (d in seq(length(values_i))) {
                        condition <-
                            length(metadata[["na_values"]][d]) > 0 &&
                            nchar(metadata[["na_values"]][d]) == 1

                        if (condition) {
                            x[
                                haven::is_tagged_na(
                                    x,
                                    metadata[["na_values"]][d]
                                )
                            ] <- values_i[d]

                            labels[
                                haven::is_tagged_na(
                                    labels,
                                    metadata[["na_values"]][d]
                                )
                            ] <- values_i[d]
                        }
                    }
                }
            }

            if (!admisc::possibleNumeric(labels)) {
                x <- as.character(x)
                if (length(values_i) > 0) {
                    values_i <- as.character(values_i)
                }
            }
            # cat(
            #     paste(
            #         i,
            #         "--",
            #         paste(
            #             values,
            #             collapse = ","
            #         ),
            #         "--",
            #         paste(
            #             values_i,
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

            if (length(values_i) > 0) {
                if (is.character(values_i) && length(values_i) > 3) {
                    values_i <- values_i[1:3]
                }

                if (length(values_i) > 3) {
                    callist$na_range <- sort(range(values_i))
                }
                else {
                    callist$na_values <- values_i
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
                for (d in seq(length(dictionary))) {
                    x[is.element(x, values[d])] <- haven::tagged_na(nms[d])
                    labels[
                        is.element(labels, values[d])
                    ] <- haven::tagged_na(nms[d])
                }

                dataset[, i] <- haven::labelled(
                    x,
                    labels = labels,
                    label = metadata[["label"]]
                )
            }
            else {
                dataset[, i] <- unclass(x)
            }
        }
    }

    attr(dataset, "dictionary") <- dictionary

    return(dataset)
}
