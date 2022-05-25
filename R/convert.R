`convert` <- function(
    from, to = NULL, declared = TRUE, recode = TRUE, encoding = "UTF-8", ...
) {
    if (missing(from)) {
        admisc::stopError("Argument 'from' is missing.")
    }
    
    # if (missing(to)) {
    #     admisc::stopError("sprintf("Argument %s is missing.", dQuote("to").")
    # }

    dots <- list(...)


    user_na <- TRUE # force reading the value labels
    if (
        is.element("user_na", names(dots)) && is.atomic(dots$user_na) && 
        length(dots$user_na) == 1 && is.logical(dots$user_na)
    ) {
        user_na <- dots$user_na
    }


    embed <- TRUE # embed the data in the XML file
    if (
        is.element("embed", names(dots)) && is.atomic(dots$embed) && 
        length(dots$embed) == 1 && is.logical(dots$embed)
    ) {
        embed <- dots$embed
    }

    chartonum <- TRUE
    if (
        is.element("chartonum", names(dots)) && is.atomic(dots$chartonum) && 
        length(dots$chartonum) == 1 && is.logical(dots$chartonum)
    ) {
        chartonum <- dots$chartonum
    }

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
        filename <- as.character(substitute(from))
        if (length(filename) > 1) {
            # something like ess[, c("idno", "ctzshipd")] was used
            filename <- "dataset"
        }
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
            spss = identical(tp_to$fileext, "SAV")
        )

        if (is.element(
            "datafile",
            names(codeBook[["fileDscr"]])
        )) {
            data <- codeBook[["fileDscr"]][["datafile"]]
        }
        else {
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

            data <- read.csv(
                file.path(tp_from$completePath, csvfile),
                as.is = TRUE
            )

            if (ncol(data) == length(codeBook$dataDscr) + 1) {
                rownames(data) <- data[, 1]
                data <- subset(
                    data,
                    select = seq(2, ncol(data))
                )
                # data <- data[, -1, drop = FALSE]
            }
            # return(list(data = data, codeBook = codeBook))
            data <- make_labelled(
                data,
                codeBook$dataDscr,
                spss = identical(tp_to$fileext, "SAV")
            )
        }
    }
    else {
        if (tp_from$fileext == "XLS" || tp_from$fileext == "XLSX") {
            # if (require(readxl, quietly = TRUE)) {
                callist <- list(path = from)
                for (f in names(formals(read_excel))) {
                    if (is.element(f, names(dots))) {
                        callist[[f]] <- dots[[f]]
                    }
                }

                data <- do.call(
                    paste(
                        "read",
                        tolower(tp_from$fileext),
                        sep = "_"
                    ),
                callist)
            # }
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
                declared <- FALSE
            }
        }
        else if (tp_from$fileext == "SAS7BDAT") {
            fargs <- names(formals(read_sas))
            arglist <- dots[is.element(names(dots), fargs)]
            arglist$file <- from
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
            exportDDI(codeBook, to, ... = ...)
        }
        else if (identical(tp_to$fileext, "SAV")) {
            data[] <- lapply(data, function(x) {
                if (!is.element("format.spss", names(attributes(x)))) {
                    attr(x, "format.spss") <- getFormat(x)
                }
                return(x)
            })
            # return(data)
            haven::write_sav(declared::as.haven(data), to)
        }
        else if (identical(tp_to$fileext, "DTA")) {
            data <- declared::as.haven(data)
            colnms <- colnames(data)
            arglist <- list(data = data)
            
            if (!is.null(codeBook)) {
                for (i in seq(ncol(data))) {
                    values <- codeBook$dataDscr[[colnms[i]]]$values
                    if (!is.null(values)) {
                        if (admisc::possibleNumeric(values)) {
                            if (!admisc::wholeNumeric(admisc::asNumeric(values))) {
                                admisc::stopError(
                                    sprintf(
                                        "Stata does not allow labels for non-integer variables (e.g. \"%s\").",
                                        colnms[i]
                                    )
                                )
                            }
                        }
                    }
                }

                # return(list(
                #     data = data,
                #     dictionary = dictionary,
                #     chartonum = chartonum,
                #     to_declared = FALSE,
                #     error_null = FALSE
                # ))
                
                if (recode) {
                    arglist <- list(
                        data = recodeValues(
                            data,
                            to = "Stata",
                            dictionary = dictionary,
                            chartonum = chartonum,
                            to_declared = FALSE,
                            error_null = FALSE
                        )
                    )
                }
            }
            
            # return(data)

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
            writexl::write_xlsx(data, to)
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

            setupfile(
                getMetadata(arglist$data),
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
