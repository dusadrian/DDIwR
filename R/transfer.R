`transfer` <- function(from, to, embed = FALSE, executable = "", ...) {
    if (missing(from)) {
        cat("\n")
        stop("Argument 'from' is missing.\n\n", call. = FALSE)
    }
    
    if (missing(to)) {
        cat("\n")
        stop(sprintf("Argument %s is missing.\n\n", dQuote("to")), call. = FALSE)
    }

    `tryCatchWEM` <- function(expr) {
            # modified version of
        # http://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
        toreturn <- list()
        output <- withVisible(withCallingHandlers(
            tryCatch(expr, error = function(e) {
                toreturn$error <<- e$message
                NULL
            }), warning = function(w) {
                toreturn$warning <<- c(toreturn$warning, w$message)
                invokeRestart("muffleWarning")
            }, message = function(m) {
                toreturn$message <<- paste(toreturn$message, m$message, sep = "")
                invokeRestart("muffleMessage")
            }
        ))
        
        if (output$visible) {
            if (!is.null(output$value)) {
                toreturn$output <- capture.output(output$value)
            }
        }
        
        if (length(toreturn) > 0) {
            return(toreturn)
        }
    }

    `wholeNumber` <- function(x) {
        all(floor(x) == x, na.rm = TRUE)
    }

    other.args <- list(...)

    targetOS <- "" # for the XML file output of function exportDDI()
    if (is.element("OS", names(other.args))) {
        targetOS <- other.args$OS
    }

    version <- 12
    if (is.element("version", names(other.args))) {
        version <- other.args$version
    }
    
    tp_from <- treatPath(from, single = TRUE)

    if (identical(toupper(to), "DDI") | identical(toupper(to), "XML")) {
        to <- file.path(tp_from$completePath, paste(tp_from$filenames, "xml", sep = "."))
    }
    else if (identical(toupper(to), "SPSS")) {
        to <- file.path(tp_from$completePath, paste(tp_from$filenames, "sav", sep = "."))
    }
    else if (identical(toupper(to), "STATA")) {
        to <- file.path(tp_from$completePath, paste(tp_from$filenames, "dta", sep = "."))
    }
    else if (identical(toupper(to), "R")) {
        to <- file.path(tp_from$completePath, paste(tp_from$filenames, "rds", sep = "."))
    }
    # else if (identical(toupper(to), "SAS")) {
    #     to <- file.path(tp_from$completePath, paste(tp_from$filenames, "sas7bdat", sep = "."))
    # }

    tp_to <- treatPath(to, single = TRUE, check = FALSE)
    known_extensions <- c("RDS", "SAV", "DTA", "XML") # , "SAS7BDAT"

    if (is.na(tp_to$fileext)) {
        cat("\n")
        stop(sprintf("Cannot determine the destination software without an extension.\n\n", dQuote("to")), call. = FALSE)
    }
    else if (!is.element(tp_to$fileext, known_extensions)) {
        cat("\n")
        stop(sprintf("Unknown destination software.\n\n", dQuote("to")), call. = FALSE)
    }
    
    if (tp_from$fileext == "XML") {
        codeBook <- getMetadata(from)
        
        if (is.element("datafile", names(codeBook[["fileDscr"]]))) {
            data <- codeBook[["fileDscr"]][["datafile"]]
        }
        else {
            files <- getFiles(tp_from$completePath, "*")
            
            csvexists <- FALSE
            csvfiles <- toupper(files$fileext) == "CSV"
            if (any(csvfiles)) {
                csvexists <- is.element(toupper(tp_from$filenames), toupper(files$filenames[csvfiles]))
                csvfile <- files$files[csvfiles][match(toupper(tp_from$filenames), toupper(files$filenames[csvfiles]))]
            }
            
            if (!csvexists) {
                cat("\n")
                stop("Datafile not found.\n\n", call. = FALSE)
            }

            # data <- suppressMessages(readr::read_csv(file.path(tp_from$completePath, csvfile)))
            data <- read.csv(file.path(tp_from$completePath, csvfile), as.is = TRUE)
            if (ncol(data) == length(codeBook$dataDscr) + 1) {
                rownames(data) <- data[, 1]
                data <- data[, -1, drop = FALSE]
            }
            # return(list(data = data, codeBook = codeBook))
            data <- convertibble(tibble::as_tibble(data), codeBook$dataDscr)
        }
    }
    else {
        if (tp_from$fileext == "SAV") {
            data <- haven::read_spss(from, user_na = TRUE)
        }
        else if (tp_from$fileext == "POR") {
            data <- haven::read_por(from, user_na = TRUE)
        }
        else if (tp_from$fileext == "DTA") {
            data <- haven::read_dta(from)
        }
        else if (tp_from$fileext == "RDS") {
            data <- readr::read_rds(from)
        }
        # else if (tp_from$fileext == "SAS7BDAT") {
        #     data <- haven::read_sas(from)
        # }

        codeBook <- getMetadata(data)
    }

    
    if (!identical(executable, "")) {
        tmpfile <- tempfile()
        tp_temp <- treatPath(tmpfile, check = FALSE)
    }

    # The current OS might not always be the same with the target OS aboe
    currentOS <- Sys.info()[["sysname"]]

    tc <- NULL

    if (identical(tp_to$fileext, "SAV")) {
        haven::write_sav(data, to)

        if (!identical(executable, "")) {
            tp_SPSS <- treatPath(executable) # just to make sure the executable exists
            tempsps <- file.path(tp_temp$completePath, paste(tp_temp$files, "sps", sep = "."))
            tempspj <- file.path(tp_temp$completePath, paste(tp_temp$files, "spj", sep = "."))

            enter <- getEnter(OS = currentOS)

            # temp <- "1/test.sps"
            
            setupfile(codeBook, file = tempsps, type = "SPSS", script = TRUE, to = to, OS = currentOS)

            sink(tempspj)
            cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", enter,
                "<job xmlns=\"http://xml.spss.com/spss/production\"", enter,
                "print=\"false\" syntaxErrorHandling=\"continue\"", enter,
                "syntaxFormat=\"interactive\"", enter,
                "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"", enter,
                "xsi:schemaLocation=\"http://xml.spss.com/spss/production http://xml.spss.com/spss/production/production-1.0.xsd\">", enter,
                "<output outputFormat=\"viewer\" outputPath=\"\"/>", enter,
                "<syntax syntaxPath=\"", tempsps, "\"/>", enter,
                "</job>")

            # TO DO: check how a .spj file looks like in recent versions of SPSS

            if (currentOS == "Darwin") {
                # Applications/IBM/SPSS/Statistics/23/SPSS/Statistics.app/Contents/MacOS/stats test.spj -production silent
            }
            else if (currentOS == "Windows") {
                tc <- tryCatchWEM(system(paste(executable, tempspj, "-production silent"), intern = TRUE, ignore.stderr = TRUE))
                # C:/Program Files/IBM/SPSS/Statistics/23/stats.exe  C:/test.spj -production silent
            }
            else if (currentOS == "Linux") {

            }

            suppressMessages(unlink(tempsps))
            suppressMessages(unlink(tempspj))
        }
    }
    else if (tp_to$fileext == "XML") {
        codeBook$fileDscr$datafile <- data
        if (!embed) {
            write.csv(as.data.frame(data), file.path(tp_from$completePath, paste(tp_to$filenames[1], "csv", sep = ".")), row.names = FALSE)
            # readr::write_csv(data, file.path(tp_from$completePath, paste(tp_to$filenames[1], "csv", sep = ".")))
        }
        
        # return(list(codeBook = codeBook, file = to, embed = embed, OS = targetOS))
        exportDDI(codeBook, to, embed = embed, OS = targetOS)
        
    }
    else if (identical(tp_to$fileext, "DTA")) {
        
        colnms <- colnames(data)
        for (i in seq(ncol(data))) {
            if (!is.null(codeBook$dataDscr[[colnms[i]]]$values)) {
                if (is.numeric(codeBook$dataDscr[[colnms[i]]]$values)) {
                    if (!wholeNumber(data[[colnms[i]]])) {
                        cat("\n")
                        stop("Stata does not support categorical variables with non-integer values.\n\n", call. = FALSE)
                    }
                }
            }
        }
        
        haven::write_dta(data, to, version = version)

        if (!identical(executable, "")) {
            codeBook$fileDscr$datafile <- data

            tp_Stata <- treatPath(executable) # just to make sure the executable exists
            temp <- file.path(tp_temp$completePath, paste(tp_temp$files, "do", sep = "."))
            # temp <- "/Users/dusadrian/1/tmp.do"

            setupfile(codeBook, file = temp, type = "Stata", script = TRUE, to = to, OS = currentOS)

            if (currentOS == "Darwin") {
                tc <- tryCatchWEM(system(paste(executable, " -e do ", temp, sep = ""), intern = TRUE, ignore.stderr = TRUE))
                # system("/Applications/Stata/Stata.app/Contents/MacOS/Stata -e do tempscript.do")
            }
            else if (currentOS == "Windows") {
                tc <- tryCatchWEM(system(paste("\"", executable, "\"", " /e do ", temp, sep = ""), intern = TRUE, ignore.stderr = TRUE))
                # system("\"C:/Program Files/Stata12/Stata.exe\" /e do C:/tempscript.do")
            }
            else if (currentOS == "Linux") {
                tc <- tryCatchWEM(system(paste("\"", executable, "\"", " -b do ", temp, " &", sep = ""), intern = TRUE, ignore.stderr = TRUE))
            }

            suppressMessages(unlink(temp))
            suppressMessages(unlink(file.path(tp_Stata$completePath, "temp.log")))
        }
    }
    else if (identical(tp_to$fileext, "RDS")) {
        readr::write_rds(data, to)
    }
    # else if (identical(tp_to$fileext, "SAS7BDAT")) {

    #     # perhaps ask https://github.com/rogerjdeangelis
    
    #     if (!identical(executable, "")) {
    #         tp_SAS <- treatPath(executable) # just to make sure the executable exists
    #         temp <- file.path(tp_temp$completePath, paste(tp_temp$files, "sas", sep = "."))

    #         setupfile(codeBook, file = temp, type = "Stata", script = TRUE, to = to, OS = currentOS)

    #         # There is no SAS for MacOS
    #         if (currentOS == "Windows") {
    #             tc <- tryCatchWEM(system(paste(executable, "-sysin", temp), intern = TRUE, ignore.stderr = TRUE))
    #             # system("C:/path-to\sas.exe -sysin C:/tempscript.sas")
    #         }
    #         else if (currentOS == "Linux") {
    #             tc <- tryCatchWEM(system(paste(executable, temp))) # ?
    #             # /opt/sas/sasb /pathto/tempscript.sas –log /pathtologfile/prog.log –print /pathtooutput/prog.lst
    #         }

    #         suppressMessages(unlink(temp))
    #     }
    # }

    if (is.element("error", names(tc))) {
        cat("\n")
        stop("The file was produced, but there was an error with the executable.\n\n", call. = FALSE)
    }

    return(invisible(NULL))
}
