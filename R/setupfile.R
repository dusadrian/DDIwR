setupfile <- function(codeBook, file = "", type = "all", csv = "", OS = "", ...) {
    
    # TO DO: when codeBook is a path to a file or directory, it should be (only) XML and not R anymore
    on.exit(suppressWarnings(sink()))
    
    other.args <- list(...)
    
    outdir <- identical(file, "")
    if (is.element("outdir", names(other.args))) {
        outdir <- other.args$outdir
    }
    
    saveFile <- FALSE
    if (is.element("saveFile", names(other.args))) {
        saveFile <- other.args$saveFile
    }
    
    indent <- 4
    if (is.element("indent", names(other.args))) {
        indent <- other.args$indent
    }
    
    script <- FALSE
    if (is.element("script", names(other.args))) {
        script <- other.args$script
    }

    forcenum  <- ""
    if (is.element("forcenum", names(other.args))) {
        forcenum <- other.args$forcenum
    }

    delim <- ","
    if (is.element("delim", names(other.args))) {
        delim <- other.args$delim
    }
    else if (is.element("sep", names(other.args))) {
        delim <- other.args$sep
    }
    
    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }
    
    `variablesMissing` <- function(dataDscr) {
        lapply(dataDscr, function(x) {
            if (!is.element("labels", names(x))) return(FALSE)

            labels <- x[["labels"]]
            ismiss <- is.element(labels, x[["na_values"]])
            
            if (is.element("na_range", names(x))) {
                na_range <- x[["na_range"]]
                ismiss <- ismiss | (labels >= na_range[1] & labels <= na_range[2])
            }

            return(ismiss)
        })
    }

    `valuesMissing` <- function(dataDscr, range = FALSE) {
        lapply(dataDscr, function(x) {
            na_values <- NULL
            if (is.element("na_values", names(x))) {
                na_values <- sort(x[["na_values"]])
            }
            labels <- x[["labels"]]
            na_range <- x[["na_range"]]
            if (length(na_range) > 0) {
                if (range) {
                    if (na_range[1] == -Inf) na_range[1] <- "LO"
                    if (na_range[2] == Inf) na_range[1] <- "HI"
                    na_values <- c(na_values, paste(na_range, collapse = " THRU "))
                }
                else {
                    na_values <- c(na_values, labels[labels >= na_range[1] & labels <= na_range[2]])
                }
            }
            return(na_values)
        })
    }
    
    
    if (missing(codeBook)) {
        cat("\n")
        stop("The argument <codeBook> is missing, with no default.\n\n", call. = FALSE)
    }
    else if (all(is.character(codeBook))) { # all() just in case someone provides a vector by mistake
        
        if (length(codeBook) > 1) {
            cat("\n")
            stop("The \"codeBook\" argument should contain a single path to the list object.\n\n", call. = FALSE)
        }
        
        xmlfiles <- FALSE
        
        labelist <- treatPath(codeBook, type = "R")
        
        if (length(labelist) == 1) {
            labelist <- treatPath(codeBook, type = "XML")
            if (length(labelist) == 1) {
                cat("\n")
                stop(gsub("XML", "R or .XML", labelist), call. = FALSE)
            }
            else {
                xmlfiles <- TRUE
            }
        }
        else if (length(labelist$fileext) == 1) {
            if (labelist$fileext == "XML") {
                xmlfiles <- TRUE
            }
        }
        
        outdir <- outdir | length(labelist$fileext) > 1
        
        csvdatadir <- FALSE # by default
        
        
        # now trying to assess what the csv argument is
        # it can be an object containing csv data, or
        # it can be a string containing a path to the data
        
        
        
        if (all(is.character(csv))) {
            if (csv != "") {
                if (length(csv) > 1) {
                    cat("\n")
                    stop("The \"csv\" argument should contain a single path to the .csv file.\n\n", call. = FALSE)
                }
                
                csvlist <- treatPath(csv, type = "csv")
                if (length(csvlist) > 1) {
                    datadir <- csvlist$completePath
                    csvdatadir <- TRUE
                }
                else {
                    cat("\nNOTE:", csvlist)
                          # since "csvlist" is now an error message from treatPath()
                }
            }
            else {
                # it's important to differentiate between "data" and "Data", for OSs that are case sensitive
                csvdatadir <- file.exists(datadir <- file.path(labelist$completePath, "data"))
                datathere <- csvdatadir
                csvdatadir <- file.exists(datadir <- file.path(labelist$completePath, "Data"))
                
                if (csvdatadir) {
                    csvlist <- treatPath(datadir, type = "csv")
                    if (length(csvlist) == 1) {
                        csvdatadir <- FALSE
                        cat(paste("\nNOTE: There is a ", ifelse(datathere, "data", "Data"), " directory within ",
                            labelist$completePath, ". "), csvlist)
                    }
                }
            }
        }
        
        
        if (csvdatadir) {
            csvfiles <- csvlist$files
            csvnames <- csvlist$filenames
            csvext <- csvlist$fileext
            
            cat ("Processing (including data directory):\n")
        }
        else {
            
            if (is.data.frame(csv)) {
                if (length(labelist$files) > 1) {
                    cat("\n")
                    stop("There are multiple files containing labels and only one csv file provided.\n\n", call. = FALSE)
                }
            }
            
            cat("Processing (no data directory):\n")
        }
        
        for (i in seq(length(labelist$files))) {
            
            if (xmlfiles) {
                obj <- DDIwR::getMetadata(file.path(labelist$completePath, labelist$files[i]), fromsetupfile = TRUE, saveFile = saveFile)
                dataDscrObject <- obj[["dataDscr"]]
                csv <- obj[["fileDscr"]][["datafile"]]
            }
            else {
                aa <- ls()
                
                tryCatch(eval(parse(file.path(labelist$completePath, labelist$files[i]))), error = function(x) {
                    stop(paste("\nThere is an error associated with the file \"", labelist$files[i], "\", see below:\n       ", gsub("Error in ", "", as.character(x)), sep = ""), call. = FALSE)
                })
                
                bb <- ls()
                bb <- setdiff(bb, aa) # bb[-which(bb == "aa")]
            }
            
            currentdir <- getwd()
            
            if (csvdatadir) {
                
                if (is.element(labelist$filenames[i], csvnames)) {
                    cat(labelist$filenames[i], "\n")
                    position <- match(labelist$filenames[i], csvnames)
                    
                    
                    for (j in seq(length(position))) {
                        
                        if (is.element(csvext[position[j]], c("CSV", "CSV.GZ"))) {
                                                                                                   # delim is already set from the function's formal argument
                            csvreadfile <- read.csv(file.path(datadir, csvfiles[position[j]]), sep = delim, header = TRUE, as.is = TRUE)
                            
                            if (ncol(csvreadfile) == 1) {
                                delim <- getDelimiter(file.path(datadir, csvfiles[position[j]]))
                                
                                if (delim == "unknown") {
                                    stop(paste("Unknown column separator for the file", csvfiles[position[j]],
                                               "\nShould be either \",\" or \";\" or tab separated.\n\n"), call. = FALSE)
                                }
                                
                                csvreadfile <- read.csv(file.path(datadir, csvfiles[position[j]]), sep = delim, header = TRUE, as.is = TRUE)
                            }
                            
                            
                            if (!xmlfiles) {
                                dataDscrObject <- get(setdiff(bb, aa))
                            }
                            
                            tryCatch(Recall(dataDscrObject, type = type, csv = csvreadfile, delim = delim, OS = OS,
                                            file = labelist$filenames[i], outdir = outdir, ... = ...),
                                error = function(x) {
                                    # if no sink() is needed, an invisible warning message will be returned
                                    tryCatch(sink(), warning = function(y) return(invisible(y)))
                                    setwd(currentdir)
                                    cat(paste("     There is an error associated with the file \"", labelist$filenames[i], "\", see below:\n     ", sep = ""))
                                    cat(as.character(x))
                                })
                        }
                    }
                }
                else {
                    cat(labelist$filenames[i], "(no .csv file)", "\n")
                    if (!xmlfiles) {
                        dataDscrObject <- get(setdiff(bb, aa))
                    }
                    
                    tryCatch(Recall(dataDscrObject, type = type, delim = delim, OS = OS,
                                    file = labelist$filenames[i], outdir = outdir, ... = ...),
                        error = function(x) {
                            tryCatch(sink(), warning=function(y) return(invisible(y)))
                            setwd(currentdir)
                            cat(paste("     There is an error associated with the file \"", labelist$filenames[i], "\", see below:\n     ", sep = ""))
                            cat(as.character(x))
                        })
                }
            }
            else {
                cat(labelist$filenames[i], "\n")
                
                if (is.data.frame(csv)) {
                    if (length(labelist$filenames) == 1) {
                        if (!xmlfiles) {
                            obj <- get(setdiff(bb, aa))
                            dataDscrObject <- obj$dataDscr
                            data <- obj$fileDscr$datafile
                        }

                        tryCatch(Recall(dataDscrObject, type = type, csv = csv, delim = delim, OS = OS,
                                        file = labelist$filenames[i], outdir = outdir, ... = ...),
                        error = function(x) {
                            tryCatch(sink(), warning=function(y) return(invisible(y)))
                            setwd(currentdir)
                            cat(paste("     There is an error associated with the file \"", labelist$filenames[i], "\", see below:\n     ", sep = ""))
                            cat(as.character(x))
                        })
                    }
                }
                else {
                    # there is really no csv data
                        if (!xmlfiles) {
                            dataDscrObject <- get(setdiff(bb, aa))
                        }
                        tryCatch(Recall(dataDscrObject, type = type, delim = delim, OS = OS,
                                        file = labelist$filenames[i], outdir = outdir, ... = ...),
                        error = function(x) {
                            tryCatch(sink(), warning=function(y) return(invisible(y)))
                            setwd(currentdir)
                            cat(paste("     There is an error associated with the file \"", labelist$filenames[i], "\", see below:\n     ", sep = ""))
                            cat(as.character(x))
                        })
                }
            }
            
            if (!xmlfiles) {
                rm(list = c(eval(setdiff(bb, aa)), "bb", "aa"))
            }
        }
        
        cat("\nSetup files created in:\n", file.path(currentdir, "Setup files"), "\n\n", sep = "")
        
        return(invisible())
    }
    else if (is.list(codeBook)) {
        if (!is.null(codeBook[["fileDscr"]][["datafile"]])) {
            csv <- codeBook[["fileDscr"]][["datafile"]]
        }
        dataDscr <- codeBook[["dataDscr"]]
        dataDscr_objname <- deparse(substitute(codeBook))
    }
    else {
        cat("\n")
        stop("Unknown input for the argument <codeBook>.\n\n", call. = FALSE)
    }
    
    anymissing <- any(unlist(lapply(dataDscr, function(x) {
        if (!is.element("labels", names(x))) return(FALSE)
        return(is.element("na_values", names(x)))
    })))
    
    
    csvlist <- NULL # initialization
    if (all(is.character(csv))) {
        if (all(csv != "")) {
            if (length(csv) > 1) {
                cat("\n")
                stop("The \"csv\" argument should contain a single path to the .csv file.\n\n", call. = FALSE)
            }
            
            csvlist <- treatPath(csv, type = "CSV")
            if (length(csvlist) > 1) {
                # no error
                if (length(csvlist$files) > 1) {
                    cat("\n")
                    stop("There is only one object containing metadata and multiple csv files.\n\n", call. = FALSE)
                }
            }
            else {
                # There is a single string returned by treatPath(), with an error message
                cat("\nNOTE:", csvlist)
                csv <- "" # back to the default value
            }
        }
    }
    
    checkvarlab <- function(x) {
        all(unlist(lapply(x, function(x) {
            if (is.list(x)) {
                return(is.element("label", names(x)))
            }
            return(FALSE)
        })))
    }
    
    
    if (is.null(names(dataDscr)) | !checkvarlab(dataDscr)) {
        cat("\n")
        stop("The argument <codeBook> does not contain variables and / or labels.\n\n", call. = FALSE)
    }
    
    if (!is.element(toupper(type), c("SPSS", "STATA", "SAS", "R", "ALL"))) {
        cat("\n")
        stop("Unknown destination type.\n\n", call. = FALSE)
    }
    
    enter <- getEnter(OS = OS)
    
    names(dataDscr) <- toupper(names(dataDscr))
    varnames <- names(dataDscr)
    maxchars <- max(nchar(varnames))
    varcheck <- rep(0, length(varnames))
    formats <- FALSE
    
    csv_is_df <- is.data.frame(csv)
    
    csv_is_path <- FALSE
    if (length(csv) == 1) { # csv is a character vector of length 1, i.e. a path 
        if (is.character(csv)) {
            if (csv != "") {
                csv_is_path <- TRUE
            }
        }
    }
    
    if (csv_is_df | csv_is_path) {
        
        if (!is.null(csvlist)) {
                                                                                           # delim is already set from the function's formal argument
            csvreadfile <- read.csv(file.path(csvlist$completePath, csvlist$files[1]), sep = delim, header = TRUE, as.is=TRUE)
            
            if (ncol(csvreadfile) == 1) {
            
                delim <- getDelimiter(file.path(csvlist$completePath, csvlist$files[1]))
                
                if (delim == "unknown") {
                    stop(paste("Unknown column separator for the file", csvlist$files[1],
                               "\nShould be either \",\" or \";\" or tab separated.\n\n"), call. = FALSE)
                }
                
                csvreadfile <- read.csv(file.path(csvlist$completePath, csvlist$files[1]), sep = delim, header = TRUE, as.is=TRUE)
            }
            
            # cat("\n")
            # cat("Found \"", csvlist$files[1], "\" in the directory \"", csvlist$completePath, "\". Using that as the .csv file.\n\n", sep = "")
            
            csv <- csvreadfile
        }
        
        colnames(csv) <- toupper(colnames(csv))
        csvnames <- colnames(csv)
        spssformats <- sasformats <- rep("", length(csvnames))
        if (!is.data.frame(csv)) {
            cat("\n")
            stop("The csv file should be a data frame.\n\n", call. = FALSE)
        }
        
        gofurther <- TRUE
        
        plusnames <- setdiff(csvnames, varnames)
        # print(plusnames)
        
        if (length(plusnames) > 0) {
            if (length(plusnames) == length(csvnames)) {
                cat("    None of the variables in the .csv file have metadata information.\n",
                    "    (perhaps the .csv file doesn't have the variable names in the first row?)\n", sep = "")
                gofurther <- FALSE
            }
            else {
                cat("    There is no metadata information for the following variables in the .csv file:\n")
                plusnames <- strwrap(paste(plusnames, collapse=", "), 75)
                for (pnms in plusnames) {
                    cat("       ", pnms, "\n")
                }
                cat("\n")
            }
        }
        
        
        plusnames <- setdiff(varnames, csvnames)
        if (length(plusnames) > 0) {
            cat("    There is metadata information for the following variables, but *not* in the .csv file:\n")
            plusnames <- strwrap(paste(plusnames, collapse=", "), 75)
            for (pnms in plusnames) {
                cat("       ", pnms, "\n")
            }
            
            if (gofurther) {
                cat("       ", ifelse(length(plusnames) == 1, "This variable", "These variables"), "will be omitted.\n")
            }
            else {
                cat("\n")
            }
        }
        
        nrowscsv <- nrow(csv)

        
        if (gofurther) {
            
            # filter the variable labels
            dataDscr <- dataDscr[is.element(varnames, csvnames)]
            
            varnames <- names(dataDscr)
            maxchars <- max(nchar(varnames))
            varcheck <- rep(0, length(varnames))
            vartypes <- emptyvars <- vector(mode = "list", length = ncol(csv))
            names(vartypes) <- names(emptyvars) <- csvnames
            
            printNOTE <- FALSE
            
            for (i in seq(ncol(csv))) {
                
                vartypes[i] <- "numeric"
                decimals <- FALSE
                
                tempvar <- csv[[varnames[i]]]
                emptyvars[[i]] <- all(is.na(tempvar))
                
                if (is.character(tempvar)) {
                    vartypes[i] <- "string"
                    
                    # %in% performs better than == when NAs are present
                    # any(tempvar == ".") generates an error, while
                    # any(tempvar %in% ".") doesn't generate any error
                    
                    if (any(tempvar %in% ".")) { # Stata type empty cells
                        if (sum(tempvar %in% "") == 0) {
                            # test if there are pure blank cells
                            # if none than a "." must be representing a Stata type missing
                            printNOTE <- TRUE
                            tempvar[tempvar %in% "."] <- NA
                        }
                    }
                }
                
                
                ####
                # the following code is needed just in case there are multibyte characters somewhere
                # that prevents nchar() from running properly (if so, it generates an error)
                nofchars <- tryCatch(nchar(as.character(tempvar)), error = function(x) return(x))
                
                if (is.list(nofchars)) {
                    # if an error is generated, nchar()'s output is a list
                    # a multibyte error should be the only one that nchar() throws
                    # don't know what other error would be possible with nchar()
                    
                    tempvar2 <- as.character(tempvar)
                    error <- unlist(strsplit(nofchars[[1]], split = " "))
                    tempvar2 <- tempvar2[-as.numeric(error[length(error)])]
                    
                    cat("    There are multibyte characters in this data (ex. ", csvnames[i], ", position ", error[length(error)], ")\n", sep = "")
                    
                    while (TRUE) {
                        
                        nofchars <- tryCatch(nchar(tempvar2), error = function(x) return(x))
                        
                        if (!is.list(nofchars)) break

                        error <- unlist(strsplit(nofchars[[1]], split = " "))
                        tempvar2 <- tempvar2[-as.numeric(error[length(error)])]
                    }
                    
                    if (length(nofchars) == 0) {
                        nofchars <- 1
                    }
                    else {
                        nofchars[is.na(tempvar2)] <- 0
                    }
                }
                else {
                    nofchars[is.na(tempvar)] <- 0
                }
                ####
                
                maxvarchar <- max(nofchars)
                
                haslabels <- is.element("labels", names(dataDscr[[csvnames[i]]]))
                
                if (haslabels) {
                    if (is.character(dataDscr[[csvnames[i]]][["labels"]])) {
                        vartypes[i] <- "string"
                        # gofurther <- FALSE
                        maxvarchar <- max(maxvarchar, nchar(dataDscr[[csvnames[i]]][["labels"]]))
                    }
                }
                
                if (vartypes[i] == "numeric") {
                    
                    tempvar2 <- tempvar[!is.na(tempvar)]
                    
                    if (any(tempvar2 - floor(tempvar2) > 0)) { # has decimals
                        decimals <- TRUE
                    }
                    else {
                        if (haslabels) {
                            if (is.numeric(dataDscr[[csvnames[i]]][["labels"]])) {
                                maxvarchar <- max(maxvarchar, nchar(dataDscr[[csvnames[i]]][["labels"]]))
                            }
                        }
                    }
                    
                    spssformats[i] <- sprintf("F%d.%d", maxvarchar, ifelse(decimals, 2, 0))

                }
                else if (vartypes[i] == "string") {
                    sasformats[i] <- "$"
                    spssformats[i] <- paste("A", maxvarchar, sep = "")
                }
                else { # all the values are missing, and no value labels exist
                    spssformats[i] <- paste("F1.0", sep = "")
                }
                
                varcheck[i] <- 1
            }
            
            if (printNOTE) {
                cat("    NOTE: some variable(s) in this file have a \".\" sign to represent a missing.\n")
                cat("    The .csv file might not import properly in some software.\n\n")
            }
            
            formats <- all(spssformats != "")
        }
        
        ## TO check if any of the existing metadata variables is not found in the CSV data file
    }
    
    
    
    stringvars <- lapply(dataDscr, function(x) {
        charvar <- FALSE
        if (is.element("labels", names(x))) {
            charvar <- is.character(x[["labels"]])
        }
        return(charvar)
    })

    
    if (outdir) {
        file <- dataDscr_objname
    }
    else {
        if (identical(file, "")) {
            if (grepl("\"", dataDscr_objname)) {
                file <- readline("Name for the setup file:\n")
            }
            else {
                file <- dataDscr_objname
            }
        }
        else {
            tp <- treatPath(file, check = FALSE)
            if (is.na(tp$fileext)) {
                if (identical(type, "all")) {
                    cat("\n")
                    stop("Could not determine the file type.\n\n", call. = FALSE)
                }
                else {
                    if (type == "SPSS") file <- file.path(tp$completePath, paste(tp$files, "sps", sep = "."))
                    if (type == "Stata") file <- file.path(tp$completePath, paste(tp$files, "do", sep = "."))
                    if (type == "SAS") file <- file.path(tp$completePath, paste(tp$files, "sas", sep = "."))
                    if (type == "R") file <- file.path(tp$completePath, paste(tp$files, "R", sep = "."))
                }
            }
            else {
                if (!identical(type, "all")) {
                    if ((tp$fileext == "SPS" & type != "SPSS") |
                        (tp$fileext == "DO" & type != "Stata") |
                        (tp$fileext == "SAS" & type != "SAS") |
                        (tp$fileext == "R" & type != "R")) {
                        cat("\n")
                        stop("Incompatible type and file extension.\n\n", call. = FALSE)
                    }
                }
                else {
                    if (tp$fileext == "SPS") type <- "SPSS"
                    if (tp$fileext == "DO") type <- "Stata"
                    if (tp$fileext == "SAS") type <- "SAS"
                    if (tp$fileext == "R") type <- "R"
                }
            }
        }
    }
    
    
    haslabels <- unlist(lapply(dataDscr, function(x) is.element("labels", names(x))))
    uniquevals <- unique(lapply(dataDscr[haslabels], function(x) return(x[["labels"]])))
    
    
    uniqueList <- lapply(uniquevals, function(uniques) {
        vars <- sapply(dataDscr[haslabels],
                    function(x) {
                        identical(x[["labels"]], uniques)
                    })
        return(names(vars[vars]))
    })
    
    
    # initiate an empty list
    numerical_with_strings <- list()
    
    if (!identical(forcenum, c(""))) {
        
        if (!all(is.element(forcenum, csvnames))) {
            cat("The following <numerical> variable names were not found in the data:\n",
                "\"", paste(setdiff(forcenum, csvnames), collapse = "\", \""), "\"\n\n", sep = "")
        }
        
        which_numerical <- intersect(csvnames, forcenum)
        
        if (length(which_numerical) > 0) {
            
            # test which values are not numbers in the respective variables
            
            # has_strings  <- lapply(csv[, which_numerical, drop = FALSE], function(x) {
            #    grep("^-?[0-9]+([.]?[0-9]+)?$", x, perl=TRUE, invert=TRUE)
            # })
            
            has_strings  <- lapply(csv[, which_numerical, drop = FALSE], function(x) {
                # as.character() is useful if the variable is an R factor
                which(is.na(suppressWarnings(as.numeric(as.character(x)))))
            })
            
            numerical_with_strings  <- has_strings[unlist(lapply(has_strings, length)) > 0]
        }
    }
    
    

    if (type == "SPSS" | type == "all") {
        dataDscr2 <- dataDscr
        printMISSING <- FALSE
        currentdir <- getwd()
        
        if (outdir) {
            if (!file.exists("Setup files")) {
                dir.create("Setup files")
            }
            
            if (!file.exists(file.path("Setup files", "SPSS"))) {
                dir.create(file.path("Setup files", "SPSS"))
            }

            setwd(file.path("Setup files", "SPSS"))
        }
        
        
        sink(ifelse(grepl("\\.sps", file), file, paste(file, ".sps", sep = "")))

        
        if (script) {
            to <- treatPath(other.args$to, check = FALSE)
            cat("GET FILE = \"",  file.path(to$completePath, to$files[1]), "\" .", enter, enter, sep = "")
        }
        else {
            cat("* ------------------------------------------------------------------------------", enter, enter,
                "* --- CONFIGURATION SECTION - START ---", enter, enter, enter, sep = "")

            if (formats) {
                cat("* The following command should contain the complete path and", enter,
                    "* name of the .csv file to be read (e.g. \"C:/file.csv\")", enter,
                    "* Change CSV_DATA_PATH to your filename, below:", enter, enter,
                    "FILE HANDLE csvpath /NAME=\"CSV_DATA_PATH\" .", enter, enter, enter, sep = "")
            }
            
            cat("* The following command should contain the complete path and", enter,
                "* name of the final .sav file (e.g. \"C:/file.sav\")", enter,
                "* Change SAV_DATA_PATH to your filename, below:", enter, enter,
                "FILE HANDLE savfile /NAME=\"SAV_DATA_PATH\" .", enter, enter, enter,
                "* --- CONFIGURATION SECTION -  END  ---", enter, enter,
                "* ------------------------------------------------------------------------------", enter, enter, enter, enter,
                "* There should be nothing to change below this line", enter,                                                            
                "* ------------------------------------------------------------------------------", enter, enter, enter, enter, sep = "")
       

            if (formats) {
                cat("* -------------- Start Definition Macro --------------", enter, enter,
                    "SET LOCALE = \"English\" .", enter,
                    "SHOW LOCALE .", enter, enter, # SET DECIMAL = COMMA . * (might be another idea)
                    "* --------------     Read Raw Data      --------------", enter, enter,
                    "GET DATA", enter,
                    " /TYPE=TXT", enter,
                    " /FILE=csvpath", enter,
                    " /DELCASE=LINE", enter,
                    " /ARRANGEMENT=DELIMITED", enter,
                    " /DELIMITERS='", ifelse(delim == "\t", "\\t", delim), "'", enter,
                    " /QUALIFIER='\"'", enter,
                    " /FIRSTCASE=2", enter,
                    " /IMPORTCASE=ALL", enter,
                    " /VARIABLES=", enter, sep = "")
                
                maxcharcsv <- max(nchar(csvnames))
                for (i in seq(length(csvnames))) {
                    cat(csvnames[i], paste(rep(" ", maxcharcsv - nchar(csvnames[i]) + 1), collapse=""), spssformats[i], sep = "")
                    if (i == length(csvnames)) {
                        cat(" .")
                    }
                    cat(enter)
                }
                cat("CACHE .", enter, "EXECUTE .", enter, enter,
                    "* ------------------------------------------------------------------------------", enter, enter, enter, sep = "")
            }
            else {
                cat("GET FILE = savfile .", enter, enter, enter, sep = "")
            }
            
            
            if (any(unlist(stringvars))) {
                cat("* --- Recode string variables which have labels, to numeric variables ---", enter, enter, enter)
                stringvars <- stringvars[unlist(stringvars)]
                for (i in names(stringvars)) {
                    
                    oldvalues <- dataDscr2[[i]][["labels"]]
                    newvalues <- seq(length(oldvalues))
                    nummiss <- logical(length(newvalues))
                    
                    if (is.element("na_values", names(dataDscr2[[i]]))) {
                        for (j in dataDscr2[[i]][["na_values"]]) {
                            missval <- admisc::asNumeric(j)
                            if (!is.na(missval)) {
                                if (!is.element(missval, newvalues)) {
                                    newvalues[j] <- missval
                                    nummiss[j] <- TRUE
                                }
                            }
                        }
                    }
                    
                    
                    if (csv_is_df | csv_is_path) {
                        if (emptyvars[[i]]) {
                            cat("* Variable ", toupper(i), " is completely empty, recoding command skipped.", enter, enter,
                                "ALTER TYPE ", toupper(i), "(F", max(nchar(newvalues)),".0) .", enter, enter, sep = "")
                        }
                    }
                    else {
                        precommand <- paste("RECODE ", toupper(i), " ", sep = "")
                        postcommand <- "(\""
                        command <- paste(precommand, postcommand, sep = "")
                        
                        for (j in newvalues[!nummiss]) {
                            
                            postcommand <- paste(postcommand, dataDscr2[[i]][["labels"]][j], "\"", " = ", j, sep = "")
                            command <- paste(command, dataDscr2[[i]][["labels"]][j], "\"", " = ", j, sep = "")
                            if (j == length(dataDscr2[[i]][["labels"]][!nummiss])) {
                                command <- paste(command, ") (else = copy) INTO TEMPVRBL .", enter, "EXECUTE .", enter, enter, sep = "")
                            }
                            else {
                                if (nchar(postcommand) > 70) {
                                    postcommand <- paste(paste(rep(" ", nchar(precommand)), collapse=""), "(\"", sep = "")
                                    command <- paste(command, ")", enter, paste(rep(" ", nchar(precommand)), collapse=""), "(\"", sep = "")
                                }
                                else {
                                    command <- paste(command, ") (\"", sep = "")
                                }
                            }
                        }
                        
                        cat(command)
                        
                        cat("ALTER TYPE ", toupper(i), "(F", max(nchar(newvalues)),".0) .", enter, enter,
                            "COMPUTE ", toupper(i), " = TEMPVRBL .", enter, "EXECUTE .", enter, enter, sep = "")
                        cat("DELETE VARIABLES TEMPVRBL .", enter, "EXECUTE .", enter, enter, sep = "")
                    }
                    
                    names(newvalues) <- names(oldvalues)
                    dataDscr2[[i]][["labels"]] <- newvalues

                    if (is.element("na_values", names(dataDscr2[[i]]))) {
                        dataDscr2[[i]][["na_values"]] <- newvalues[match(dataDscr2[[i]][["na_values"]], oldvalues)]
                    }
                }
                cat(enter, enter, sep = "")
            }
            
            
            
            if (length(numerical_with_strings) > 0) {
                cat("* --- Force variables as numeric --- ", enter, enter, sep = "")
                
                for (i in names(numerical_with_strings)) {
                    
                    tempvar <- csv[, i]
                    
                    ##### code to recode and transform into numeric
                    cat("RECODE ", toupper(i), " (\"", paste(sort(unique(tempvar[numerical_with_strings[[i]]])), collapse = "\" = \"\") (\""), "\" = \"\") .", enter,
                        "EXECUTE .", enter, enter, sep = "")
                    
                    tempvar <- tempvar[-numerical_with_strings[[i]]]
                    tempvar <- as.numeric(tempvar)
                    
                    cat ("ALTER TYPE ", toupper(i), " (F", max(nchar(tempvar)), ".", ifelse(any(tempvar - floor(tempvar) > 0), 2, 0), ") .", enter,
                        "EXECUTE .", enter, enter, sep = "")
                    
                }
                cat(enter)
            }
            
            
            
            cat("* --- Add variable labels --- ", enter, enter,
                "VARIABLE LABELS", enter, sep = "")
            
            for (i in seq(length(varnames))) {
                cat(varnames[i], paste(rep(" ", maxchars - nchar(varnames[i])), collapse=""), " \"", dataDscr2[[i]][["label"]][1], "\"", sep = "")
                if (i == length(varnames)) {
                    cat(" .", enter, "EXECUTE .", sep = "")
                }
                cat(enter)
            }
            cat(enter, enter, sep = "")
            
            cat("* --- Add value labels --- ", enter, enter,
                "VALUE LABELS", enter, sep = "")
            
            
            for (i in seq(length(uniqueList))) {
                n <- uniqueList[[i]][1]

                cat(splitrows(uniqueList[[i]], enter, 80), enter, sep = "")
                
                #if (all(is.character(dataDscr2[[n]][["labels"]]))) {
                #    cat(paste(paste("\"", dataDscr2[[n]][["labels"]], "\" \"", names(dataDscr2[[n]][["labels"]]), "\"", sep = ""), collapse="\n"))
                #}
                #else {
                    cat(paste(paste(dataDscr2[[n]][["labels"]], " \"", names(dataDscr2[[n]][["labels"]]), "\"", sep = ""), collapse=enter))
                #}
                if (i == length(uniqueList)) {
                    cat(" .", enter, "EXECUTE .", enter, sep = "")
                }
                else {
                    cat(enter, "/", enter, sep = "")
                }
            }
            cat(enter, enter, sep = "")
                
       
       
        }

        
        if (anymissing) {

            missvaRs <- variablesMissing(dataDscr2)
            withmiss <- unlist(lapply(missvaRs, any))

            missvaLs <- valuesMissing(dataDscr2[withmiss], range = TRUE)
            nms <- names(missvaLs)
            uniqueMissList <- lapply(unique(missvaLs), function(x) {
                nms[unlist(lapply(missvaLs, function(y) {
                    identical(x, y)
                }))]
            })

            missvaLs <- unique(missvaLs)

            # sink()
            # setwd(currentdir)
            # return(list(dataDscr2=dataDscr2, haslabels=haslabels, missvaRs=missvaRs, withmiss=withmiss, missing = missing))
            # return(list(missvaLs=missvaLs, uniqueMissList=uniqueMissList))
            
            cat("* --- Add missing values --- ", enter, enter,
                "MISSING VALUES", enter, sep = "")
                
            for (i in seq(length(uniqueMissList))) {
                if (any(grepl("THRU", missvaLs[[i]]))) {
                    cat(splitrows(toupper(uniqueMissList[[i]]), enter, 80))
                    if (length(missvaLs[[i]]) <= 2) { # at most one discrete missing value and a missing range
                        cat(" (", paste(missvaLs[[i]], collapse = ", ") , ")", sep = "")
                    }
                    else {
                        cat(" (", paste(missvaLs[[i]][c(1, which(grepl("THRU", missvaLs[[i]])))], collapse = ", ") , ")", sep = "")
                        cat("  * more than one distinct missing value found, next to a missing range")
                    }
                }
                else {

                    if (length(missvaLs[[i]]) < 4) {
                        cat(splitrows(toupper(uniqueMissList[[i]]), enter, 80))
                        cat(" (", paste(missvaLs[[i]], collapse=", ") , ")", sep = "")
                    }
                    else {
                        absrange <- abs(range(missvaLs[[i]]))
                        
                        if (all(missvaLs[[i]] < 0)) {
                            cat(splitrows(toupper(uniqueMissList[[i]]), enter, 80))
                            cat(" (LOWEST THRU ", max(missvaLs[[i]]) , ")", sep = "")
                        }
                        else {
                            # check if the missing values range doesn't contain any other (non-missing) values
                            checklist <- list()
                            for (mv in uniqueMissList[[i]]) {
                                allvalues <- dataDscr2[[mv]][["labels"]]
                                nonmiss <- setdiff(allvalues, missvaLs[[i]])
                                checklist[[mv]] <- any(is.element(nonmiss, seq(min(missvaLs[[i]]), max(missvaLs[[i]]))))
                            }
                            
                            checklist <- unlist(checklist)
                            
                            # print(checklist)
                            
                            if (any(checklist)) {
                                # at least one variable has a non-missing value within the range of the missing values
                                printMISSING <- TRUE
                                
                                # now trying to see if at least some of the variables can be "rescued"
                                if (any(!checklist)) {
                                    ###
                                    # is this working...? TO TEST
                                    cat(splitrows(names(checklist)[!checklist], enter, 80))
                                    # cat(paste(names(checklist)[!checklist], collapse=", "))
                                    ###
                                    cat(paste(" (", min(missvaLs[[i]]), " TO ", max(missvaLs[[i]]) , ")", enter, sep = ""))
                                    checklist <- checklist[checklist]
                                }
                                
                                ###
                                # is this working...? TO TEST
                                cat(splitrows(names(checklist), enter, 80))
                                # cat(paste(names(checklist), collapse=", "))
                                ###
                                cat(paste(" (", paste(missvaLs[[i]][1:3], collapse=", ") , ")", sep = ""))
                                cat(ifelse(i == length(uniqueMissList), " .", ""))
                                cat("  * more than three distinct missing values found")
                            }
                            else {
                                cat(splitrows(toupper(uniqueMissList[[i]]), enter, 80))
                                cat(" (", min(missvaLs[[i]]), " TO ", max(missvaLs[[i]]) , ")", sep = "") 
                            }
                        }
                    }
                }
                
                cat(ifelse(i == length(uniqueMissList), " .", ""))
                cat(enter)
            }
            cat(enter, enter, sep = "")
            
            
        }

        outfile <- ifelse(script, paste("\"", file.path(to$completePath, to$files[1]), "\"", sep = ""), "savfile")
        
        cat("* --- Save the .sav file --- ", enter, enter,
            "SAVE OUTFILE=", outfile, enter, "/KEEP", enter, sep = "")
            
        
        if (formats) {
            for (n in csvnames) {
                cat(toupper(n), enter, sep = "")
            }
        }
        else {
            for (n in names(dataDscr2)) {
                cat(toupper(n), enter, sep = "")
            }
        }
        
        cat("  /COMPRESSED .", enter, "EXECUTE .", enter, sep = "")
        
        # finish writing and close the .sps file
        sink()
        
        if (printMISSING) {
            # this would be printed on the screen
            cat("    For some variables, more than 3 distinct missing values were found.\n")
            cat("    Only the first three were used.\n\n")
        }
        
        setwd(currentdir)
        
    }
    
    
    if (type == "Stata" | type == "all") {
        
        dataDscr2 <- dataDscr
        currentdir <- getwd()

        if (outdir) {
            if (!file.exists("Setup files")) {
                dir.create("Setup files")
            }
            
            if (!file.exists(file.path("Setup files", "Stata"))) {
                dir.create(file.path("Setup files", "Stata"))
            }

            setwd(file.path("Setup files", "Stata"))
        }
        
        sink(ifelse(grepl("\\.do", file), file, paste(file, ".do", sep = "")))
        
        if (script) {
            to <- treatPath(other.args$to, check = FALSE)
            cat("use \"",  file.path(to$completePath, to$files[1]), "\"", enter, enter, enter, sep = "")
        }
        else {
            cat("/* Initialization commands */", enter,
                "clear", enter,
                "capture log close", enter,
                "set more off", enter,
                "version 12.0", enter,
                "set linesize 250", enter,
                "set varabbrev off", enter,
                "set memory 1G // not necessary in Stata 12",
                enter, enter, enter,
                "* ----------------------------------------------------------------------------",
                enter, enter,
                "* --- CONFIGURATION SECTION - START ---",
                enter, enter,
                "* The following command should contain the complete path and",
                enter,
                "* name of the Stata log file.",
                enter,
                "* Change LOG_FILENAME to your filename, below:",
                enter,
                "local log_file \"LOG_FILENAME\"",
                enter, enter, enter,
                "* The following command should contain the complete path and",
                enter,
                "* name of the CSV file, usual file extension \".csv\"",
                enter,
                "* Change CSV_DATA_PATH to your filename, below:",
                enter,
                "local csvpath \"CSV_DATA_PATH\"",
                enter, enter, enter,
                "* The following command should contain the complete path and",
                enter,
                "* name of the final STATA file, usual file extension \".dta\"",
                enter,
                "* Change STATA_DATA_PATH to your filename, below:",
                enter,
                "local filepath \"STATA_DATA_PATH\"",
                enter, enter, enter,
                "* --- CONFIGURATION SECTION - END ---",
                enter, enter,
                "* ----------------------------------------------------------------------------",
                enter, enter, enter, enter,
                "* There should be nothing to change below this line",
                enter,
                "* ----------------------------------------------------------------------------",
                enter, enter, enter, enter,
                "log using \"`log_file'\", replace text", enter, enter, sep = "")

            if (formats) {
                cat("insheet using \"`csvpath'\", comma names case",
                    enter, enter,
                    "* Note that some variables in the csv raw data file might be in lowercase",
                    enter,
                    "* To ensure that the dataset contains only variable names in uppercase",
                    enter, enter,
                    "foreach var of varlist _all {",
                    enter,
                    "    local newname = upper(\"`var'\")",
                    enter,
                    "    cap rename \`var\' \`newname\'",
                    enter,
                    "}",
                    enter, enter, enter, sep = "")
            }
            else {
                cat("use \"`filepath'\"", enter, enter, enter, sep = "")
            }
        }
        
        
        
        if (any(unlist(stringvars))) {
            cat("* Recode string variables which have labels, to numeric variables", enter, enter, sep = "")
            stringvars <- stringvars[unlist(stringvars)]
            
            for (i in names(stringvars)) {

                oldvalues <- dataDscr2[[i]][["labels"]]

                # recode every letter to a number, but keep the potentially numbers
                # something like "A", "B" and "-9" will be recoded to 1, 2 and -9
                # and someting like "A", "B" and "2" will be recoded to 1, 3 and 2
                newvalues <- suppressWarnings(as.numeric(as.character(oldvalues)))
                newvalues[is.na(newvalues)] <- setdiff(seq(1000), newvalues)[seq(sum(is.na(newvalues)))]
                names(newvalues) <- names(oldvalues)


                # the recode command cannot be used because it only allows numeric variables
                cat("generate TEMPVAR = .", enter, sep = "")
                for (j in seq(length(newvalues))) {
                    cat("replace TEMPVAR = ", newvalues[j], " if ", toupper(i), " == \"", oldvalues[j], "\"", enter, sep = "")
                }
                cat("drop ", toupper(i), enter, "rename TEMPVAR ", toupper(i), enter, enter, sep = "")
                
                dataDscr2[[i]][["labels"]] <- newvalues

                # just in case the old missing values were not numbers
                if (is.element("na_values", names(dataDscr2[[i]]))) {
                    dataDscr2[[i]][["na_values"]] <- newvalues[match(dataDscr2[[i]][["na_values"]], oldvalues)]
                }
            }

            if (!script) {
                cat(enter, enter, sep = "")
            }
        }
        
        if (length(numerical_with_strings) > 0) {
            # This has to do with the (now deprecated) argument "forcenum"

            cat("* Force variables as numeric", enter, enter, sep = "")
            
            for (i in names(numerical_with_strings)) {
                
                tempvar <- csv[, i]
                
                for (j in sort(unique(tempvar[numerical_with_strings[[i]]]))) {
                    cat("replace ", toupper(i), " = \"\" if ", toupper(i), " == \"", j, enter, sep = "")
                }
                
                cat ("destring ", toupper(i), ", replace", enter, enter, sep = "")
                
            }
            cat(enter)
        }

        
        if (anymissing) {

            missvaRs <- variablesMissing(dataDscr2)
            withmiss <- unlist(lapply(missvaRs, any))
            
            uniquemiss <- sort(unique(unlist(mapply(function(x, y) {
                x[["labels"]][y]
            }, dataDscr2[withmiss], missvaRs[withmiss], SIMPLIFY = FALSE))))

            newmiss <- paste(".", letters[seq(length(uniquemiss))], sep = "")
            
            # sink()
            # return(list(dataDscr2=dataDscr2, withmiss=withmiss, missvaRs=missvaRs))

            # we need a third dataDscr because dataDscr2 might have been altered when recoding the strings to numerical
            dataDscr3 <- dataDscr2

            dataDscr3[withmiss] <- mapply(function(x, y) {
                x[["labels"]][y] <- newmiss[match(x[["labels"]][y], uniquemiss)]
                x[["na_values"]]   <- newmiss[match(x[["na_values"]],   uniquemiss)]
                return(x)
            }, dataDscr2[withmiss], missvaRs[withmiss], SIMPLIFY = FALSE)

            cat(enter, "* Recode missing values", enter, enter, sep = "")
            
            for (i in names(dataDscr2[withmiss])) {
                for (j in which(missvaRs[[i]])) {
                    cat("replace ", i, " = ", dataDscr3[[i]][["labels"]][j], " if ", i, " == ", dataDscr2[[i]][["labels"]][j], enter, sep = "")
                }
            }

            dataDscr2 <- dataDscr3

            cat(enter, enter)

        }
        
        
        
        maxchars <- max(nchar(names(dataDscr2)))
        
        cat("* Definition of variable labels", enter, enter, sep = "")
        
        # cat(maxchars, enter)
        
        for (n in names(dataDscr)) {
            cat(paste("label variable ", toupper(n), paste(rep(" ", maxchars - nchar(n)), collapse=""), " \"", dataDscr2[[n]][["label"]][1], "\"", enter, sep = ""))
        }
        cat(enter, enter, sep = "")
        
        cat("* Definition of category labels", enter, enter, sep = "")
        
        for (i in seq(length(uniqueList))) {
            n <- uniqueList[[i]][1]
            
            headerlabel <- paste("label define LABELS_GROUP_", i, sep = "")
            
            cat(paste("label define LABELS_GROUP_", i, " ", sep = ""),
                paste(paste(paste(dataDscr2[[n]][["labels"]], " \"", names(dataDscr2[[n]][["labels"]]), "\"", sep = ""),
                            collapse=" "),
                        enter, sep = ""),
                sep = "")
        }
        
        cat(enter, enter, sep = "")
        cat("* Attachment of category labels to variables", enter, enter, sep = "")
        
        for (i in seq(length(uniqueList))) {
            n <- uniqueList[[i]][1]
            for (j in uniqueList[[i]]) {
                cat(paste("label values ", toupper(j), paste(rep(" ", maxchars - nchar(j)), collapse=""), " LABELS_GROUP_", i, enter, sep = ""))
            }
        }
        
        if (script) {
            cat(enter, "save \"",  file.path(to$completePath, to$files[1]), "\", replace", enter, sep = "")
            # cat("use \"",  file.path(to$completePath, to$files[1]), "\"", enter, sep = "")
        }
        else  {
            cat(enter, "compress", enter,
            "save \"`filepath'\", replace", enter, enter,
            "log close", enter,
            "set more on", enter, sep = "")
        }
        
        sink()
        setwd(currentdir)
    }
    
    
    if (type == "SAS" | type == "all") {
        dataDscr2 <- dataDscr
        currentdir <- getwd()

        if (outdir) {
            if (!file.exists("Setup files")) {
                dir.create("Setup files")
            }
            
            if (!file.exists(file.path("Setup files", "SAS"))) {
                dir.create(file.path("Setup files", "SAS"))
            }

            setwd(file.path("Setup files", "SAS"))
        }
        
        sink(ifelse(grepl("\\.sas", file), file, paste(file, ".sas", sep = "")))

        if (script) {
            to <- treatPath(other.args$to, check = FALSE)
            cat("LIBNAME datadir \"", to$completePath, "\";", enter, enter, sep = "")
            sasimport <- paste("datadir", to$filenames[1], sep = ".")
        }
        else {
            sasimport <- "datadir.&sasfile"
            cat("* ------------------------------------------------------------------------------ ;", enter, enter,
                "* --- CONFIGURATION SECTION - START ---                                          ;", enter, enter, enter, sep = "")                                            

            if (formats) {
                cat("* The following command should contain the complete path and                     ;", enter,
                    "* name of the .csv file to be read (e.g. \"C:/file.csv\")                          ;", enter,
                    "* Change CSV_DATA_PATH to your filename, below                                   ;", enter, enter,
                    "FILENAME csvpath \"CSV_DATA_PATH\";", enter, enter, enter, sep = "")
                        
            # cat("* It is assumed the data file was created under Windows (end of line is CRLF);", enter,
            #     "* If the csv file was created under Unix,  change eol=LF;", enter,
            #     "* If the csv file was created under MacOS, change eol=CR below;", enter, enter,
            #     "%LET eol=CRLF;", enter, enter, enter, sep = "")
            }
            
            cat("* The following command should contain the complete path of the                  ;", enter,
                "* directory where the final file will be saved (e.g. \"C:/Data\")                  ;", enter,
                "* Change SAS_DATA_FOLDER to your directory name, below                           ;", enter, enter,
                "LIBNAME datadir \"SAS_DATA_FOLDER\";", enter, enter, enter, sep = "")
                    
            cat("* The following command should contain the name of the output SAS file only      ;", enter,
                "* (without quotes, and without the .sas7bdat extension)                          ;", enter,
                "* Change SAS_FILE_NAME to your output file name, below                           ;", enter, enter,
                "%LET sasfile = SAS_FILE_NAME;", enter, enter, enter,
                "* --- CONFIGURATION SECTION -  END ---                                           ;", enter, enter,
                "* ------------------------------------------------------------------------------ ;", enter, enter, enter, enter,
                "* There should be nothing to change below this line;", enter,
                "* ------------------------------------------------------------------------------ ;", enter, enter, enter, enter, sep = "")
            
            if (formats) {
                cat("* --- Read the raw data file --- ;", enter, enter,
                    "DATA ", sasimport, ";", enter, enter,
                    "INFILE csvpath", enter,
                    "       DLM=", ifelse(delim == "\t", "'09'X", paste("\"", delim, "\"", sep = "")), enter,
                    "       FIRSTOBS=2", enter,
                    #### "       TERMSTR=&eol", enter, #### line commented out
                    "       DSD", enter,
                    "       TRUNCOVER", enter,
                    "       LRECL=512", enter,
                    "       ;", enter, enter,
                    
                    "INPUT  ", toupper(csvnames[1]), ifelse(sasformats[1] == "$", " $", ""), enter, sep = "")
                
                for (i in seq(2, length(csvnames))) {
                    cat("       ", csvnames[i], ifelse(sasformats[i] == "$", " $", ""), enter, sep = "")
                }
                cat("       ;", enter, sep = "")
                cat("RUN;", enter, enter, sep = "")
                    # "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")
            }
        }

        # cat("DATA ", sasimport, ";", enter, enter, enter, sep = "")
        
        if (any(unlist(stringvars))) {
            cat("* --- Recode string variables which have labels, to numeric variables --- ;", enter, enter, sep = "")
                
            for (i in names(stringvars)) {
                if (stringvars[[i]]) {
                    
                    oldvalues <- dataDscr2[[i]][["labels"]]
                    
                    # recode every letter to a number, but keep the potentially numbers
                    # something like "A", "B" and "-9" will be recoded to 1, 2 and -9
                    # and someting like "A", "B" and "2" will be recoded to 1, 3 and 2
                    newvalues <- suppressWarnings(as.numeric(as.character(oldvalues)))
                    newvalues[is.na(newvalues)] <- setdiff(seq(1000), newvalues)[seq(sum(is.na(newvalues)))]
                    names(newvalues) <- names(oldvalues)
                    
                    
                    cat("DATA ", sasimport, ";", enter, enter,
                        "    SET ", sasimport, ";", enter, enter,
                        "    TEMPVAR = INPUT(", i, ", ?? best.);", enter, enter, sep = "")
                    
                    for (j in seq(length(newvalues))) {
                        cat("IF (", i, " = '", oldvalues[j], "') THEN TEMPVAR = ", newvalues[j], ";", enter, sep = "")
                    }
                    
                    # cat(enter, "RUN;", enter, enter, "DATA ", sasimport, ";", enter, enter,
                    #     "    SET ", sasimport, ";", enter, enter,
                    cat(enter, "DROP ", i, ";", enter,
                        "RENAME TEMPVAR = ", i, ";", enter, enter,
                        "RUN;", enter, enter, sep = "")
                    
                    # just in case the old missing values were not numbers
                    dataDscr2[[i]][["labels"]] <- newvalues
                    if (is.element("na_values", names(dataDscr2[[i]]))) {
                        dataDscr2[[i]][["na_values"]] <- newvalues[match(dataDscr2[[i]][["na_values"]], oldvalues)]
                    }
                }
            }
        }
        
        
        if (length(numerical_with_strings) > 0) {
            
            cat("* --- Force variables as numeric --- ;", enter, enter, sep = "")
            
            cat("DATA ", sasimport, ";", enter, enter,
                "    SET ", sasimport, ";", enter, enter,
                "    TEMPVAR = INPUT(", i, ", ?? best.);", enter, enter,
                "RUN;", enter, enter, 
                "DATA ", sasimport, ";", enter, enter,
                "    SET ", sasimport, ";", enter, enter,
                "    DROP ", i, ";", enter,
                "    RENAME TEMPVAR = ", i, ";", enter, enter,
                "RUN;", enter, enter, sep = "")
            # for (i in toupper(names(numerical_with_strings))) {
            #     cat("TEMPVAR = INPUT(", i, ", ?? best.);", enter, enter,
            #         "DROP ", i, ";", enter,
            #         "RENAME TEMPVAR = ", i, ";", enter, enter, sep = "")
            # }
            
            # cat("* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")
        }
        
        
        if (any(unlist(stringvars)) | length(numerical_with_strings) > 0) {
            cat("* --- Reorder the variables in their original positions --- ;", enter, enter,
                "DATA ", sasimport, ";", enter, enter,
                "    RETAIN ", gsub(",", "", splitrows(toupper(names(dataDscr2)), enter, 70, "           ")), ";", enter, enter,
                "    SET ", sasimport, ";", enter, enter,
                "RUN;", enter, enter, sep = "")

            #     "RETAIN ", gsub(",", "", splitrows(toupper(names(dataDscr2)), enter, 70, "           ")), ";", enter, enter, sep = "")
                # "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")
        }


        if (anymissing) {
            missvaRs <- variablesMissing(dataDscr2)
            withmiss <- unlist(lapply(missvaRs, any))
            
            uniquemiss <- sort(unique(unlist(mapply(function(x, y) {
                x[["labels"]][y]
            }, dataDscr2[withmiss], missvaRs[withmiss], SIMPLIFY = FALSE))))

            newmiss <- paste(".", letters[seq(length(uniquemiss))], sep = "")

            dataDscr3 <- dataDscr2

            dataDscr3[withmiss] <- mapply(function(x, y) {
                x[["labels"]][y] <- newmiss[match(x[["labels"]][y], uniquemiss)]
                x[["na_values"]]   <- newmiss[match(x[["na_values"]],   uniquemiss)]
                return(x)
            }, dataDscr2[withmiss], missvaRs[withmiss], SIMPLIFY = FALSE)

            # cat("* --- Recode missing values --- ;", enter, enter, sep = "")
            # cat(c(paste("ARRAY miss(", sum(withmiss), ")", sep = ""), names(dataDscr2)[withmiss], ";", enter), fill = 70)
            # cat("DO i = 1 to ", sum(withmiss), ";", enter, sep = "")
            cat("* --- Recode missing values ---                                                  ;", enter, enter,
                "DATA ", sasimport, ";", enter, enter,
                "    SET ", sasimport, ";", enter, enter, sep = "")
            cat("ARRAY miss(", sum(withmiss), ") ", paste(names(dataDscr2)[withmiss], collapse = " "), ";", enter,
                "    DO i = 1 to ", sum(withmiss), ";", enter, sep = "")
            
            for (i in seq(length(uniquemiss))) {
                cat("    IF miss(i) = ", uniquemiss[i], " THEN miss(i) = ", newmiss[i], ";", enter, sep = "")
            }
            
            cat("END;", enter, "DROP i;", enter, enter, "RUN;", enter, enter, sep = "")
                # "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")
                
            dataDscr2 <- dataDscr3
        }
        
        
        if (!script) {
            cat("* --- Add variable labels --- ;", enter, enter,
                "DATA ", sasimport, ";", enter, enter,
                "    SET ", sasimport, ";", enter, enter, sep = "")
            
            for (i in seq(length(varnames))) {
                cat("    LABEL ", varnames[i], paste(rep(" ", maxchars - nchar(varnames[i]) + 1), collapse=""), "=", " \"", dataDscr2[[i]][["label"]][1], "\";", enter, sep = "")
            }
            
            cat(enter, "RUN;", enter, enter, sep = "")
            #     "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")
        }
        
        cat("* --- Create value labels groups --- ;", enter, enter,
            "PROC FORMAT;", enter, enter, sep = "")
        
        for (i in seq(length(uniqueList))) {
            n <- uniqueList[[i]][1]
            cat(paste("VALUE LABELS_", i, "_GROUP", enter, sep = ""),
                paste(paste(paste("      ", dataDscr2[[n]]$values, " = \"", names(dataDscr2[[n]]$values), "\"", sep = ""),
                            collapse=enter), #paste("\n", paste(rep(" ", nchar(headerlabel)), collapse=""), sep = "")),
                      ";", enter, enter, sep = ""),
                sep = "")
        }
		  
        cat("RUN;", enter, enter, sep = "")
            # "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")
        
        cat("* --- Format variables with value labels --- ;", enter, enter,
            "DATA ", sasimport, ";", enter, enter,
            "    SET ", sasimport, ";", enter, enter, "    FORMAT", enter, sep = "")
        
        for (i in seq(length(uniqueList))) {
            n <- uniqueList[[i]][1]
            for (j in uniqueList[[i]]) {
                cat("    ", toupper(j), paste(rep(" ", maxchars - nchar(j)), collapse=""), " LABELS_", i, "_GROUP", ".", enter, sep = "")
            }
        }
          
        cat("    ;", enter, enter, "RUN;", enter, enter,sep = "")
            # "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")
        
        if (!script) {
            cat("* --- Save data to a sas type file --- ;", enter, enter,
                "DATA datadir.&sasfile;", enter, enter,
                "    SET ", sasimport, ";", enter, enter,
                "RUN;", enter, sep = "")
        }

        
        sink()
        setwd(currentdir)
    }

    
    if (type == "R" | type == "all") {
        printMISSING <- FALSE
        currentdir <- getwd()
        
        if (outdir) {
            if (!file.exists("Setup files")) {
                dir.create("Setup files")
            }
            
            if (!file.exists(file.path("Setup files", "R"))) {
                dir.create(file.path("Setup files", "R"))
            }

            setwd(file.path("Setup files", "R"))
        }
        
        
        sink(ifelse(grepl("\\.R", file), file, paste(file, ".R", sep = "")))
        
        cat("# ------------------------------------------------------------------------------", enter, enter,
            "# --- CONFIGURATION SECTION - START ---", enter, enter, enter, sep = "")

        # if (formats) {
            cat("# The following command should contain the complete path and", enter,
                "# name of the .csv file to be read (e.g. \"C:/CIS 2008/Data/ALL.csv\")", enter,
                "# Change CSV_DATA_PATH to your filename, below:", enter, enter,
                "csvpath <- \"CSV_DATA_PATH\"", enter, enter, enter, sep = "")
        # }
        
        cat("# The following command should contain the complete path and", enter,
            "# name of the .Rdata file to be saved (e.g. \"C:/CIS 2008/Data/ALL.Rdata\")", enter,
            "# Change RDATA_PATH to your filename, below:", enter, enter,
            "rdatapath <- \"RDATA_PATH\"", enter, enter, enter, sep = "")
        
                  
        # if (formats) {
            cat("# --- Read the raw data ---", enter, enter,
                # "rdatafile <- readr::read_delim(csvpath, delim = \"", ifelse(delim == "\t", "\\t", delim), "\")",
                "rdatafile <- read.csv(csvpath)", 
                enter, enter, "names(rdatafile) <- toupper(names(rdatafile))    # all variable names to upper case",
                enter, enter, enter, sep = "")
        # }
        # else {
        #     cat("# \"rdatafile\" should be an R data.frame (usually read from a .csv file)\n\n")
        # }
        
        cat("# --- CONFIGURATION SECTION -  END  ---", enter, enter,
            "# There should be nothing to change below this line", enter,                                                            
            "# ------------------------------------------------------------------------------", enter, enter, enter, enter, sep = "")
        
        
        if (length(numerical_with_strings) > 0) {
            
            cat("# --- Force variables as numeric ---", enter, enter, sep = "")
            
            maxnchars <- max(nchar(names(numerical_with_strings)))
            
            for (i in toupper(names(numerical_with_strings))) {
                cat("rdatafile[ , \"", i, "\"]", paste(rep(" ", maxnchars - nchar(i)), collapse=""),
                    " <- suppressWarnings(as.numeric(rdatafile[ , \"", i, "\"]", "))", enter, sep = "")
            }
        }
        
        cat("# --- Set the variable metadata attributes --- ", enter,
            "# packages haven and mixed should be installed", enter, enter, sep = "")
        
        writeMetadata(dataDscr, OS = OS, indent = indent)
        
        cat(enter, enter, sep = "")
        
        cat("# --- Save the R data file --- ", enter, enter,
            "rfilename <- unlist(strsplit(basename(rdatapath), split=\"\\\\.\"))[1]", enter,
            "rdatapath <- file.path(dirname(rdatapath), paste(rfilename, \".Rdata\", sep=\"\"))", enter,
            "assign(rfilename, rdatafile)", enter,
            "eval(parse(text = paste(\"save(\", rfilename, \", file=rdatapath)\", sep=\"\")))",
            enter, enter, enter, enter,
            "# ------------------------------------------------------------------------------",
            enter, enter,
            "# --- Clean up the working space --- ", enter, enter,
            "rm(rfilename, rdatafile, csvpath, rdatapath", sep = "")
        
        # if (any(unlist(stringvars))) {
        #     cat(", tempvar")
        # }
        
        cat(")", enter, sep = "")
        
        
        
        # finish writing and close the .R file
        sink()
        
        setwd(currentdir)
        
    }
}
