`convert` <- function(from, to = NULL, embed = FALSE, ...) {
    if (missing(from)) {
        admisc::stopError("Argument 'from' is missing.")
    }
    
    # if (missing(to)) {
    #     admisc::stopError("sprintf("Argument %s is missing.", dQuote("to").")
    # }

    dots <- list(...)

    targetOS <- "" # for the XML file output of function exportDDI()
    if (is.element("OS", names(dots))) {
        targetOS <- dots$OS
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
            completePath = "~",
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
            data <- haven::read_spss(from, user_na = TRUE)
        }
        else if (tp_from$fileext == "POR") {
            data <- haven::read_por(from, user_na = TRUE)
        }
        else if (tp_from$fileext == "DTA") {
            data <- haven::read_dta(from)
            data <- recodeMissing(
                data,
                to = "SPSS",
                dictionary = dictionary,
                to_declared = FALSE,
                error_null = FALSE
            )
        }
        else if (tp_from$fileext == "SAS7BDAT") {
            data <- haven::read_sas(from)
        }
        else if (tp_from$fileext == "RDS" & !Robject) {
            data <- readr::read_rds(from)
        }
        else {
            data <- from
        }

        codeBook <- getMetadata(data, error_null = FALSE)
    }

    # The current OS might not always be the same with the target OS aboe
    currentOS <- Sys.info()[["sysname"]]
    
    if (!is.null(to)) {
        if (Robject) {
            data <- declared::undeclare(data)
        }

        if (tp_to$fileext == "XML") {

            if (is.null(codeBook)) {
                admisc::stopError(
                    "The input does not seem to contain any metadata."
                )
            }

            codeBook$fileDscr$datafile <- data

            if (!embed) {
                write.csv(
                    file = file.path(
                        tp_to$completePath,
                        paste(tp_to$filenames[1], "csv", sep = ".")
                    ),
                    x = data,
                    row.names = FALSE
                )
                
                # readr::write_csv(
                #     data,
                #     file.path(
                #         tp_to$completePath,
                #         paste(tp_to$filenames[1], "csv", sep = ".")
                #     )
                # )
            }
            # return(list(codeBook = codeBook, file = to, embed = embed, OS = targetOS))
            
            exportDDI(codeBook, to, embed = embed, OS = targetOS)
        }
        else if (identical(tp_to$fileext, "SAV")) {
            data[] <- lapply(data, function(x) {
                if (any(names(attributes(x)) == "format.spss")) return(x)
                attr(x, "format.spss") <- getFormat(x)
                return(x)
            })
            
            haven::write_sav(declared::as_haven(data), to)
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

                callist <- list(
                    data = recodeMissing(
                        data,
                        to = "Stata",
                        dictionary = dictionary,
                        error_null = FALSE
                    )
                )
            }
            else {
                callist <- list(data = data)
            }


            if (is.element("version", names(dots))) {
                callist$version <- dots$version
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
                callist$path <- to
                do.call(haven::write_dta, callist)
            # }
        }
        else if (identical(tp_to$fileext, "RDS")) {
            readr::write_rds(declared::as_declared(data), to)
        }
        else if (identical(tp_to$fileext, "SAS7BDAT")) {

            haven::write_sas(data, to)

            #     # perhaps ask https://github.com/rogerjdeangelis
        }
        else if (identical(tp_to$fileext, "XLSX")) {
            writexl::write_xlsx(data, to)
        }
    }
    else {
        
        return(
            invisible(
                as.data.frame(declared::as_declared(data))
            )
        )
        
    }
}
