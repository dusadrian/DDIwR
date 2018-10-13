`transfer` <- function(from, to, embed = FALSE, execute = "", ...) {
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
    tp_to <- treatPath(to, single = TRUE, check = FALSE)

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



    if (tp_to$fileext == "SAV") {
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
    else if (tp_to$fileext == "DTA") {
        codeBook$fileDscr$datafile <- data
        haven::write_dta(data, to, version = version)

        setupfile(codeBook, file = "temp.do", script = TRUE, to = to)
        
        # sink("temp.do")

        # cat(paste(c("use \"/Users/dusadrian/1/test.dta\"",
        # "replace AGR = .a if AGR == -7",
        # "replace AGR = .b if AGR == -1",
        # "replace AGE = .a if AGE == -7",
        # "replace AGE = .b if AGE == -1",
        # "save \"/Users/dusadrian/1/test.dta\", replace",
        # ""
        # ), collapse = "\n"))

        # sink()

        return()

        tc <- tryCatchWEM(system(paste(execute, "-e do temp.do"), intern = TRUE, ignore.stderr = TRUE))

        # system("/Applications/Stata/Stata.app/Contents/MacOS/Stata -e do temp.do")
        unlink("temp.do")

        if (is.element("error", names(tc))) {
            cat("\n")
            stop("The file was produced, but there was an error executing the recoding script.\n\n", call. = FALSE)
        }
        ## eventual # unlink("temp.log")

    }

}

