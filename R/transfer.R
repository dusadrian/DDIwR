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

    other.args <- list(...)

    OS <- ""
    if (is.element("OS", names(other.args))) {
        OS <- other.args$OS
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
    else if (identical(toupper(to), "SAS")) {
        to <- file.path(tp_from$completePath, paste(tp_from$filenames, "sas7bdat", sep = "."))
    }
    else if (identical(toupper(to), "R")) {
        to <- file.path(tp_from$completePath, paste(tp_from$filenames, "Rdata", sep = "."))
    }

    tp_to <- treatPath(to, single = TRUE, check = FALSE)
    knownext <- c("R", "SAS7BDAT", "SAV", "DTA", "XML")

    if (is.na(tp_to$fileext)) {
        cat("\n")
        stop(sprintf("Cannot determine the destination software without an extension.\n\n", dQuote("to")), call. = FALSE)
    }
    else if (!is.element(tp_to$fileext, knownext)) {
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
            csvfiles <- is.element("CSV", toupper(files$fileext))
            if (any(csvfiles)) {
                csvexists <- is.element(toupper(tp_from$filenames), toupper(files$filenames[csvfiles]))
                csvfile <- files$files[csvfiles][match(toupper(tp_from$filenames), toupper(files$filenames[csvfiles]))]
            }

            if (!csvexists) {
                cat("\n")
                stop("Data not found.\n\n", call. = FALSE)
            }

            data <- suppressMessages(readr::read_csv(file.path(tp_from$completePath, csvfile)))
            data <- convertibble(data, codeBook$dataDscr)
        }
    }
    else if (tp_from$fileext == "SAV") {
        data <- haven::read_spss(from, user_na = TRUE)
        codeBook <- getMetadata(from)
    }
    else if (tp_from$fileext == "POR") {
        data <- haven::read_por(from, user_na = TRUE)
        codeBook <- getMetadata(from)
    }


    if (!identical(executable, "")) {
        tmpfile <- tempfile()
        tp_temp <- treatPath(tmpfile)
    }


    if (identical(tp_to$fileext, "SAV")) {
        haven::write_sav(data, to)
    }
    else if (tp_to$fileext == "XML") {
        codeBook$fileDscr$datafile <- data
        if (!embed) {
            readr::write_csv(data, file.path(tp_from$completePath, paste(tp_to$filenames[1], "csv", sep = ".")))
        }
        
        # return(list(codeBook = codeBook, file = to, embed = embed, OS = OS))
        exportDDI(codeBook, to, embed = embed, OS = OS)
        
    }
    else if (identical(tp_to$fileext, "DTA")) {
        codeBook$fileDscr$datafile <- data
        haven::write_dta(data, to, version = version)

        if (!identical(executable, "")) {
            tp_Stata <- treatPath(executable) # just to make sure the executable exists
            temp <- file.path(tp_temp$completePath, paste(tp_temp$files, "do", sep = ".")
            # Here, OS is not always the same with the above OS (that is a target, this is current)
            OS <- Sys.info()[["sysname"]] == "Darwin"

            setupfile(codeBook, file = temp, script = TRUE, to = to)

            if (OS == "Darwin") { # | OS == "Linux"
                tc <- tryCatchWEM(system(paste(executable, " -e do ", temp, sep = ""), intern = TRUE, ignore.stderr = TRUE))
                # system("/Applications/Stata/Stata.app/Contents/MacOS/Stata -e do tempscript.do")
            }
            else if (OS == "Windows") {
                tc <- tryCatchWEM(system(paste("\"", executable, "\"", " /e do ", temp, sep = ""), intern = TRUE, ignore.stderr = TRUE))
                # system("\"C:/Program Files/Stata12/Stata.exe\" /e do C:/tempscript.do")
            }

            unlink(temp)
            unlink(file.path(tp_Stata$completePath, "temp.log"))

            if (is.element("error", names(tc))) {
                cat("\n")
                stop("The file was produced, but there was an error with the executable.\n\n", call. = FALSE)
            }
        }
    }
    else if (identical(tp_to$fileext, "SAS7BDAT")) {

        if (!identical(executable, "")) {
            tp_SAS <- treatPath(executable) # just to make sure the executable exists
            temp <- file.path(tp_temp$completePath, paste(tp_temp$files, "sas", sep = ".")
            # /path-to/sas tempscript.sas
            # c:\path-to\sas.exe -sysin tempscript.sas
            # Linux:
            # /opt/sas/sasb /pathto/tempscript.sas –log /pathtologfile/prog.log –print /pathtooutput/prog.lst

        }
    }

    if (!identical(executable, "")) {
        unlink(tmpfile)
    }
}
