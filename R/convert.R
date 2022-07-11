`convert` <- function(
    from, to = NULL, declared = TRUE, chartonum = FALSE, recode = TRUE,
    encoding = "UTF-8", csv = NULL, ...
) {
    if (missing(from)) {
        admisc::stopError("Argument 'from' is missing.")
    }

    funargs <- unlist(lapply(match.call(), deparse)[-1])
    
    # if (missing(to)) {
    #     admisc::stopError("sprintf("Argument %s is missing.", dQuote("to").")
    # }

    dots <- list(...)


    user_na <- ifelse(isFALSE(dots$user_na), FALSE, TRUE)

    # embed the data in the XML file
    embed <- ifelse(isFALSE(dots$embed), FALSE, TRUE)

    dictionary <- NULL
    if (is.element("dictionary" , names(dots))) {
        dictionary <- dots$dictionary
    }

    Robject <- FALSE
    if (is.character(from)) {
        tp_from <- treatPath(from, type = "*", single = TRUE)
    }
    else if (is.element("data.frame", class(from))) {
        Robject <- TRUE
        # filename <- as.character(substitute(from))
        # filename <- admisc::getName(funargs["from"], object = TRUE)
        filename <- all.vars(substitute(from))[1]

        # it can have a length > 1, when something like this might be used:
        # foo(subset(dd[, c("A", "B")], subset = A == 1)) # "dd" "A"
        # or
        # foo(dd[, i]) # "dd" "i"
        # in any case, the first is (or should be) always the object's name

        tp_from <- list(
            completePath = normalizePath("~"),
            filenames = filename,
            fileext = "RDS"
        )
    }
    else {
        admisc::stopError("Unsuitable input.")
    }

    tp_to <- to

    if (!is.null(to)) {
        
        if (identical(toupper(to), "EXCEL")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "xlsx", sep = ".")
            )
        }
        else if (identical(toupper(to), "DDI") | identical(toupper(to), "XML")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "xml", sep = ".")
            )
        }
        else if (identical(toupper(to), "SPSS")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "sav", sep = ".")
            )
        }
        else if (identical(toupper(to), "STATA")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "dta", sep = ".")
            )
        }
        else if (identical(toupper(to), "R")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "rds", sep = ".")
            )
        }
        else if (identical(toupper(to), "SAS")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "sas7bdat", sep = ".")
            )
        }
        else if (identical(toupper(to), "XPT")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "xpt", sep = ".")
            )
        }
        else if (!grepl("[.]", to)) {
            admisc::stopError(
                "Unknown destination software."
            )
        }

        tp_to <- treatPath(to, single = TRUE, check = FALSE)

        if (tp_to$fileext == "XLS") {
            tp_to$fileext <- "XLSX"
        }

        known_extensions <- c("RDS", "SAV", "DTA", "XML", "SAS7BDAT", "XPT", "XLSX") # 

        if (is.na(tp_to$fileext)) {
            admisc::stopError(
                "Cannot determine the destination software without a file extension."
            )
        }
        else if (!is.element(tp_to$fileext, known_extensions)) {
            admisc::stopError("Unknown destination software.")
        }
    }

    if (tp_from$fileext == "XML") {
        codeBook <- getMetadata(
            from,
            declared = declared,
            encoding = encoding,
            toext = tp_to$fileext
        )

        if (is.element("datafile", names(codeBook[["fileDscr"]]))) {
            data <- codeBook[["fileDscr"]][["datafile"]]
        }
        else {
            if (is.null(csv)) {
                files <- getFiles(tp_from$completePath, "*")

                csvexists <- FALSE
                csvfiles <- files$fileext == "CSV"
                if (any(csvfiles)) {
                    csvexists <- is.element(
                        toupper(tp_from$filenames),
                        toupper(files$filenames[csvfiles])
                    )
                    csvfile <- files$files[csvfiles][
                        match(
                            toupper(tp_from$filenames),
                            toupper(files$filenames[csvfiles])
                        )
                    ]
                }

                if (!csvexists) {
                    admisc::stopError("Datafile not found.")
                }

                csv <- file.path(tp_from$completePath, csvfile)
            }
            else {
                # test if the csv is a path
                treatPath(csv)
            }

            callist <- list(file = csv)
            for (f in names(formals(utils::read.csv))) {
                if (is.element(f, names(dots))) {
                    callist[[f]] <- dots[[f]]
                }
            }

            header <- ifelse(isFALSE(callist$header), FALSE, TRUE)

            data <- do.call("read.csv", callist)
            
            if (ncol(data) == length(codeBook$dataDscr)) {
                if (header) {
                    if (!identical(names(data), names(codeBook$dataDscr))) {
                        admisc::stopError("The .csv file does not match the DDI Codebook")
                    }
                }
                else {
                    names(data) <- names(codeBook$dataDscr)
                }
            }
            if (ncol(data) == length(codeBook$dataDscr) + 1) {
                if (header) {
                    if (!identical(names(data)[-1], names(codeBook$dataDscr))) {
                        admisc::stopError("The .csv file does not match the DDI Codebook")
                    }
                }
                else {
                    names(data) <- c("row_names_csv_file", names(codeBook$dataDscr))
                }

                rownames(data) <- data[, 1]
                data <- subset(
                    data,
                    select = seq(2, ncol(data))
                )
                # data <- data[, -1, drop = FALSE]
            }
            

            # return(list(data = data, codeBook = codeBook))
            data <- make_labelled(data, codeBook$dataDscr)
        }

        attr(data, "stdyDscr") <- codeBook[["stdyDscr"]]

        return(data)
    }
    else {
        if (tp_from$fileext == "XLS" || tp_from$fileext == "XLSX") {
            if (requireNamespace("readxl", quietly = TRUE)) {
                callist <- list(path = from)
                for (f in names(formals(readxl::read_excel))) {
                    if (is.element(f, names(dots))) {
                        callist[[f]] <- dots[[f]]
                    }
                }

                data <- do.call("read_excel", callist)
                variables <- NULL
                callist$sheet <- "variables"
                admisc::tryCatchWEM(variables <- do.call("read_excel", callist))
                
                values <- NULL
                callist$sheet <- "values"
                admisc::tryCatchWEM(values <- do.call("read_excel", callist))
                if (is.null(values)) {
                    callist$sheet <- "codes"
                    admisc::tryCatchWEM(values <- do.call("read_excel", callist))
                }

                if (!is.null(variables) & !is.null(values)) {
                    for (v in colnames(data)) {
                        callist <- list(x = data[[v]])
                        label <- NULL
                        admisc::tryCatchWEM(label <- variables$label[variables$name == v])
                        if (length(label) == 1) {
                            if (!identical(label, "") & !is.na(label)) {
                                callist$label <- label
                            }
                        }

                        labels <- NULL
                        admisc::tryCatchWEM(labels <- values$value[values$variable == v])
                        if (is.null(labels)) {
                            admisc::tryCatchWEM(labels <- values$code[values$variable == v])
                        }
                        
                        nms <- NULL
                        admisc::tryCatchWEM(nms <- values$label[values$variable == v])

                        vmissing <- NULL
                        admisc::tryCatchWEM(vmissing <- values$missing[values$variable == v])

                        if (length(labels) > 0 & length(nms) > 0 & length(vmissing) > 0) {
                            if (admisc::possibleNumeric(labels)) {
                                labels <- admisc::asNumeric(labels)
                            }
                            
                            if (!all(is.na(vmissing)) && any(vmissing == "y")) {
                                callist$na_values <- labels[which(vmissing == "y")]
                            }

                            names(labels) <- nms
                            callist$labels <- labels
                        }

                        data[[v]] <- do.call("declared", callist)

                    }
                }
            }
        }
        else if (tp_from$fileext == "SAV") {
            fargs <- names(formals(read_sav))
            arglist <- dots[is.element(names(dots), fargs)]
            arglist$file <- from
            arglist$user_na <- user_na
            arglist$encoding <- encoding
            data <- do.call(haven::read_sav, arglist) # haven_labelled variables
        }
        else if (tp_from$fileext == "POR") {
            fargs <- names(formals(read_por))
            arglist <- dots[is.element(names(dots), fargs)]
            arglist$file <- from
            arglist$user_na <- user_na
            data <- do.call(haven::read_por, arglist)
        }
        else if (tp_from$fileext == "DTA") {
            fargs <- names(formals(read_dta))
            arglist <- dots[is.element(names(dots), fargs)]
            arglist$file <- from
            arglist$encoding <- encoding
            data <- do.call(haven::read_dta, arglist)

            # return(list(data = data, to = to, dictionary = dictionary, chartonum = chartonum, to_declared = FALSE, error_null = FALSE))
            if (recode) {
                data <- recodeValues(
                    dataset = data,
                    to = "SPSS",
                    dictionary = dictionary,
                    chartonum = chartonum,
                    to_declared = FALSE,
                    error_null = FALSE
                )
            }
            else {
                if (admisc::anyTagged(data)) {
                    declared <- FALSE
                }
            }
        }
        else if (tp_from$fileext == "SAS7BDAT") {
            fargs <- names(formals(read_sas))
            arglist <- dots[is.element(names(dots), fargs)]
            arglist$data_file <- from
            arglist$encoding <- encoding
            data <- do.call(haven::read_sas, arglist)

            if (recode) {
                data <- recodeValues(
                    dataset = data,
                    to = "SPSS",
                    dictionary = dictionary,
                    chartonum = chartonum,
                    to_declared = FALSE,
                    error_null = FALSE
                )
            }

        }
        else if (tp_from$fileext == "XPT") {
            fargs <- names(formals(haven::read_xpt))
            arglist <- dots[is.element(names(dots), fargs)]
            arglist$file <- from
            # arglist$encoding <- encoding
            data <- do.call(haven::read_xpt, arglist)

            if (recode) {
                data <- recodeValues(
                    dataset = data,
                    to = "SPSS",
                    dictionary = dictionary,
                    chartonum = chartonum,
                    to_declared = FALSE,
                    error_null = FALSE
                )
            }

        }
        else if (tp_from$fileext == "RDS" & !Robject) {
            data <- readRDS(from)
        }
        else {
            data <- from
        }

        codeBook <- getMetadata(data, error_null = FALSE)
        
        codeBook$fileDscr$fileName <- tp_from$files

        filetypes <- c("SPSS", "SPSS", "Stata", "SAS", "XPT", "R", "DDI", "Excel", "Excel")
        fileexts <- c("SAV", "POR", "DTA", "SAS7BDAT", "XPT", "RDS", "XML", "XLS", "XLSX")

        codeBook$fileDscr$fileType <- filetypes[which(fileexts == tp_from$fileext)]
    }

    subset <- dots$subset
    if (!is.null(subset)) {
        data <- eval(parse(text = paste("data <- subset(data, ", subset, ")")))
    }
    
    if (is.null(to)) {
        if (declared) {
            data <- declared::as.declared(data)
            class(data) <- "data.frame"
            return(invisible(data))
        }

        return(invisible(data))
    }
    else {
        if (tp_to$fileext == "XML") {

            if (is.null(codeBook)) {
                admisc::stopError(
                    "The input does not seem to contain any metadata."
                )
            }

            if (admisc::anyTagged(data)) {
                admisc::stopError("DDI does not support extended missing codes")
            }

            data[] <- lapply(data, function(x) {
                if (is.factor(x)) {
                    x <- as.numeric(x)
                }
                return(x)
            })

            codeBook$fileDscr$datafile <- data

            if (!embed) {
                write.csv(
                    x = data,
                    file = file.path(
                        tp_to$completePath,
                        paste(tp_to$filenames[1], "csv", sep = ".")
                    ),
                    row.names = FALSE,
                    na = ""
                )
            }
            
            # return(list(codeBook = codeBook, file = to))
            
            attrdata <- attributes(data)
            if (is.element("stdyDscr", names(attrdata))) {
                codeBook$stdyDscr <- attrdata$stdyDscr
            }

            exportDDI(codeBook, to, ... = ...)
        }
        else if (identical(tp_to$fileext, "SAV")) {
            data[] <- lapply(data, function(x) {
                if (!is.element("format.spss", names(attributes(x)))) {
                    attr(x, "format.spss") <- getFormat(x, type = "SPSS")
                }
                return(x)
            })

            if (admisc::anyTagged(data)) {
                admisc::stopError("SPSS does not support extended missing codes")
            }

            # return(data)
            haven::write_sav(declared::as.haven(data), to)
        }
        else if (identical(tp_to$fileext, "DTA")) {
            data <- declared::as.haven(data)

            colnms <- colnames(data)
            arglist <- list(data = data)

            rechars <- c()
            
            for (i in seq(ncol(data))) {
                x <- data[[colnms[i]]]
                metadata <- codeBook$dataDscr[[colnms[i]]]
                labels <- metadata$labels
                if (is.null(labels)) {
                    labels <- attr(x, "labels", exact = TRUE)
                }

                if (!is.null(labels)) {
                    if (admisc::possibleNumeric(x)) {
                        if (!admisc::wholeNumeric(admisc::asNumeric(x))) {
                            admisc::stopError(
                                sprintf(
                                    "Stata does not allow labels for non-integer variables (e.g. \"%s\").",
                                    colnms[i]
                                )
                            )
                        }
                    }

                    if (is.character(x)) {
                        rechars <- c(rechars, i)
                        # Stata does not allow labels for character variables
                        if (chartonum && !is.null(labels)) {
                            x <- recodeCharcat(x, metadata = metadata)
                        }
                        else {
                            label <- attr(x, "label", exact = TRUE)
                            x <- as.character(x)
                            attr(x, "label") <- label
                        }

                        data[[colnms[i]]] <- x
                    }
                }
            }


            if (recode) {
                data <- recodeValues(
                    data,
                    to = "Stata",
                    dictionary = dictionary,
                    to_declared = FALSE,
                    error_null = FALSE
                )
            }
            
            # return(data)
            data[] <- lapply(data, function(x) {
                attr(x, "format.spss") <- NULL
                if (is.null(attr(x, "format.stata"))) {
                    attr(x, "format.stata") <- getFormat(x, type = "Stata")
                }
                return(x)
            })

            # for (i in seq(length(rechars))) {
            #     cat("--------", colnms[rechars[i]], "--------\n")
            #     print(attributes(data[[rechars[i]]]))
            # }

            arglist <- list(data = data)

            if (is.element("version", names(dots))) {
                arglist$version <- dots$version
            }


            # if (requireNamespace("readstata13", quietly = TRUE)) {

            #     attr(callist$data, "var.labels") <- lapply(data, function(x) {
            #         result <- attr(x, "label", exact = TRUE)
            #         result <- lapply(result, function(x){
            #             if (is.null(x)) "" else x
            #         })
            #         base::unlist(result, use.names = TRUE)
            #     })
                
            #     callist$file <- to
            #     callist$compress <- TRUE
            #     do.call(readstata13::save.dta13, callist)
            # }
            # else {
                arglist$path <- to
                
                do.call(haven::write_dta, arglist)
                # return(invisible(arglist$data))
            # }
        }
        else if (identical(tp_to$fileext, "RDS")) {
            if (declared) {
                data <- declared::as.declared(data)
                class(data) <- "data.frame"
                saveRDS(data, to)
            }
            else {
                saveRDS(data, to)
            }
        }
        else if (identical(tp_to$fileext, "XLSX")) {
            labels <- sapply(data, function(x) {
                lbl <- attr(x, "label", exact = TRUE)
                if (is.null(lbl)) {
                    lbl <- ""
                }
                return(lbl)
            })

            varFormat <- lapply(codeBook$dataDscr, function(x) {
                as.numeric(
                    unlist(
                        strsplit(
                            substring(x$varFormat[1], 2),
                            split = "\\."
                        )
                    )
                )
            })

            x <- list(
                data = data,
                variables = data.frame(
                    name = names(labels),
                    label = labels,
                    type = sapply(data, mode)
                ),
                values = data.frame(
                    variable = character(0),
                    value = character(0),
                    label = character(0),
                    missing = character(0)
                )
            )
            
            x$variables$width <- sapply(varFormat, "[[", 1)
            x$variables$decimals <- sapply(varFormat, function(x) {
                ifelse(length(x) > 1, x[2], NA)
            })

            for (v in names(codeBook$dataDscr)) {
                labels <- codeBook$dataDscr[[v]][["labels"]]
                if (!is.null(labels)) {
                    temp <- data.frame(
                        variable = v,
                        value = labels,
                        label = names(labels),
                        missing = NA
                    )

                    na_values <- codeBook$dataDscr[[v]][["na_values"]]
                    
                    if (!is.null(na_values)) {
                        temp$missing[is.element(labels, na_values)] <- "y"
                    }

                    x$values <- rbind(x$values, temp)
                }
            }

            writexl::write_xlsx(x, path = to)
        }
        else {
            if (identical(tp_to$fileext, "SAS7BDAT")) {
                fargs <- names(formals(haven::write_sas))
                arglist <- dots[is.element(names(dots), fargs)]
                arglist$data <- declared::as.haven(data)
                arglist$path <- to
                do.call(haven::write_sas, arglist)
            }
            else if (identical(tp_to$fileext, "XPT")) {
                fargs <- names(formals(haven::write_xpt))
                arglist <- dots[is.element(names(dots), fargs)]
                arglist$data <- declared::as.haven(data)
                arglist$path <- to
                do.call(haven::write_xpt, arglist)
            }
            
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "sas", sep = ".")
            )

            ### TODO: recodeValues() for the dictionary, only if recode = TRUE?

            setupfile(
                codeBook = getMetadata(arglist$data),
                file = to,
                type = "SAS",
                recode = recode,
                catalog = TRUE,
                dictionary = recodeValues(
                    arglist$data, to = "Stata", return_dictionary = TRUE
                )
            )
        }
    }
}
