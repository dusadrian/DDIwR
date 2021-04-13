`convert` <- function(from, to, embed = FALSE, ...) {
    if (missing(from)) {
        cat("\n")
        stop("Argument 'from' is missing.\n\n", call. = FALSE)
    }
    
    if (missing(to)) {
        cat("\n")
        stop(sprintf("Argument %s is missing.\n\n", dQuote("to")), call. = FALSE)
    }

    other.args <- list(...)

    targetOS <- "" # for the XML file output of function exportDDI()
    if (is.element("OS", names(other.args))) {
        targetOS <- other.args$OS
    }

    Robject <- FALSE
    if (is.character(from)) {
        tp_from <- treatPath(from, type = "*", single = TRUE)
    }
    else if (all(is.element(c("tbl_df", "data.frame"), class(from)))) {
        Robject <- TRUE
        tp_from <- list(completePath = ".", filenames = as.character(substitute(from)), fileext = "RDS")
    }
    else {
        cat("\n")
        stop("Unsuitable input.\n\n", call. = FALSE)
    }

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
    else if (identical(toupper(to), "SAS")) {
        to <- file.path(tp_from$completePath, paste(tp_from$filenames, "sas7bdat", sep = "."))
    }
    else if (!grepl("[.]", to)) {
        cat("\n")
        stop(sprintf("Unknown destination software.\n\n", dQuote("to")), call. = FALSE)
    }

    tp_to <- treatPath(to, single = TRUE, check = FALSE)
    known_extensions <- c("RDS", "SAV", "DTA", "XML", "SAS7BDAT") # 

    if (is.na(tp_to$fileext)) {
        cat("\n")
        stop(sprintf("Cannot determine the destination software without an extension.\n\n", dQuote("to")), call. = FALSE)
    }
    else if (!is.element(tp_to$fileext, known_extensions)) {
        cat("\n")
        stop(sprintf("Unknown destination software.\n\n", dQuote("to")), call. = FALSE)
    }
    
    if (tp_from$fileext == "XML") {
        codeBook <- getMetadata(from, spss = identical(tp_to$fileext, "SAV"))
        
        if (is.element("datafile", names(codeBook[["fileDscr"]]))) {
            data <- codeBook[["fileDscr"]][["datafile"]]
        }
        else {
            files <- getFiles(tp_from$completePath, "*")
            
            csvexists <- FALSE
            csvfiles <- files$fileext == "CSV"
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
            data <- convertibble(tibble::as_tibble(data), codeBook$dataDscr, spss = identical(tp_to$fileext, "SAV"))
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
        else if (tp_from$fileext == "SAS7BDAT") {
            data <- haven::read_sas(from)
        }
        else if (tp_from$fileext == "RDS" & !Robject) {
            data <- readr::read_rds(from)
        }
        else {
            data <- from
        }

        codeBook <- getMetadata(data)
    }

    
    na_vars <- unlist(lapply(data, function(x) {
        
        if (inherits(x, "haven_labelled")) {
            if (is.double(x)) {
                return(any(is_tagged_na(x)))
            }

            return(!is.null(attr(x, "na_values")))
        }

        return(FALSE)
    }))

    if (any(na_vars)) {
        
    }

    # The current OS might not always be the same with the target OS aboe
    currentOS <- Sys.info()[["sysname"]]

    tc <- NULL
    dictionary <- NULL
    if (is.element("dictionary" , names(other.args))) {
        dictionary <- other.args[["dictionary"]]
    }

    if (tp_to$fileext == "XML") {
        codeBook$fileDscr$datafile <- data
        if (!embed) {
            write.csv(as.data.frame(data), file.path(tp_from$completePath, paste(tp_to$filenames[1], "csv", sep = ".")), row.names = FALSE)
            # readr::write_csv(data, file.path(tp_from$completePath, paste(tp_to$filenames[1], "csv", sep = ".")))
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
        
        haven::write_sav(recodeMissing(data, to = "SPSS", dictionary = dictionary), to)
    }
    else if (identical(tp_to$fileext, "DTA")) {
        
        colnms <- colnames(data)
        for (i in seq(ncol(data))) {
            if (!is.null(values <- codeBook$dataDscr[[colnms[i]]]$values)) {
                if (admisc::possibleNumeric(values)) {
                    if (!admisc::wholeNumeric(admisc::asNumeric(values))) {
                        cat("\n")
                        stop(sprintf("Stata does not allow labels for non-integer variables (e.g. \"%s\").\n\n", colnms[i]), call. = FALSE)
                    }
                }
            }
        }
        
        callist <- list(data = recodeMissing(data, to = "Stata", dictionary = dictionary))
        
        if (is.element("version", names(other.args))) {
            callist$version <- other.args$version
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
        readr::write_rds(data, to)
    }
    else if (identical(tp_to$fileext, "SAS7BDAT")) {

        haven::write_sas(data, to)

        #     # perhaps ask https://github.com/rogerjdeangelis
    }

    if (is.element("error", names(tc))) {
        cat("\n")
        stop("The file was produced, but there was an error with the binary executable.\n\n", call. = FALSE)
    }

    return(invisible(NULL))
}
