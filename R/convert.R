`convert` <- function(
    from, to = NULL, declared = TRUE, recode = TRUE, ...
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

    encoding <- "UTF-8"
    
    if (
        is.element("encoding", names(dots)) && is.atomic(dots$encoding)
    ) {
        if (is.null(dots$encoding)) {
            encoding <- NULL
        }
        else if (length(dots$encoding) == 1 && is.character(dots$encoding)) {
            encoding <- dots$encoding
        }
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
        tp_from <- list(
            completePath = normalizePath("~"),
            filenames = as.character(substitute(from)),
            fileext = "RDS"
        )
    }
    else {
        admisc::stopError("Unsuitable input.")
    }

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
        else if (!grepl("[.]", to)) {
            admisc::stopError(
                "Unknown destination software."
            )
        }

        tp_to <- treatPath(to, single = TRUE, check = FALSE)

        if (tp_to$fileext == "XLS") {
            tp_to$fileext <- "XLSX"
        }

        known_extensions <- c("RDS", "SAV", "DTA", "XML", "SAS7BDAT", "XLSX") # 

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
            spss = ifelse(
                is.null(to),
                FALSE,
                identical(tp_to$fileext, "SAV")
            )
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

            # data <- suppressMessages(readr::read_csv(file.path(tp_from$completePath, csvfile)))
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
            data <- do.call(haven::read_sav, arglist)
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
        else if (tp_from$fileext == "RDS" & !Robject) {
            data <- readr::read_rds(from)
            # data <- readRDS(from)
        }
        else {
            data <- from
        }

        codeBook <- getMetadata(data, error_null = FALSE)
        
        
        codeBook$fileDscr$fileName <- tp_from$files

        filetypes <- c("SPSS", "SPSS", "Stata", "SAS", "R", "DDI", "Excel", "Excel")
        fileexts <- c("SAV", "POR", "DTA", "SAS7BDAT", "RDS", "XML", "XLS", "XLSX")

        codeBook$fileDscr$fileType <- filetypes[which(fileexts == tp_from$fileext)]
    }
    
    if (!is.null(to)) {
        if (tp_to$fileext == "XML") {

            if (is.null(codeBook)) {
                admisc::stopError(
                    "The input does not seem to contain any metadata."
                )
            }

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
                
                # readr::write_csv(
                #     data,
                #     file.path(
                #         tp_to$completePath,
                #         paste(tp_to$filenames[1], "csv", sep = ".")
                #     )
                # )
            }
            # return(list(codeBook = codeBook, file = to))
            exportDDI(codeBook, to, ... = ...)
        }
        else if (identical(tp_to$fileext, "SAV")) {
            data[] <- lapply(data, function(x) {
                if (any(names(attributes(x)) == "format.spss")) return(x)
                attr(x, "format.spss") <- getFormat(x)
                return(x)
            })
            
            haven::write_sav(declared::as.haven(data), to)
        }
        else if (identical(tp_to$fileext, "DTA")) {
            colnms <- colnames(data)
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
            else {
                arglist <- list(data = data)
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
                readr::write_rds(data, to)
                # saveRDS(data, to)
            }
            else {
                readr::write_rds(data, to)
                # saveRDS(data, to)
            }
        }
        else if (identical(tp_to$fileext, "SAS7BDAT")) {

            haven::write_sas(declared::as.haven(data), to)

            #     # perhaps ask https://github.com/rogerjdeangelis
        }
        else if (identical(tp_to$fileext, "XLSX")) {
            writexl::write_xlsx(data, to)
        }
    }
    else {

        if (declared) {
            data <- declared::as.declared(data)
            class(data) <- "data.frame"
            return(invisible(data))
        }

        return(invisible(data))
    }
}
