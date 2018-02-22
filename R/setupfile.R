setupfile <- function(obj = "", type = "all", csv = "", miss = "", uniqueid = "",
                      SD = "", delimiter = ",", OS = "windows", outfile = "",
                      forcenum = c(""), ...) {
    
    
    # change the internalbls argument into a very unique name, just in case the
    # list object in the external R file(s) are named "internalbls" as well
    internalbls <- obj
    internalbls_objname <- deparse(substitute(obj))
    rm(obj)
    
    other.args <- list(...)
    
    if ("pathIsFolder" %in% names(other.args)) {
        pathIsFolder <- other.args$pathIsFolder
    }
    
    saveFile <- FALSE
    if ("saveFile" %in% names(other.args)) {
        saveFile <- other.args$saveFile
    }
    
    indent <- 4
    if ("indent" %in% names(other.args)) {
        indent <- other.args$indent
    }
    
    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }
    
    getNofDecimals <- function(number) {
        decs <- nchar(format(abs(number),scientific=FALSE))-(trunc(log10(max(1,trunc(abs(number)))))+1)-1
        return(ifelse(decs < 0, 0, decs))
    }
    
    
    
    if (all(is.character(internalbls))) { # all() just in case someone provides a vector by mistake
        
        if (length(internalbls) > 1) {
            cat("\n")
            stop("The lbls argument should contain a single path to the list object.\n\n", call. = FALSE)
        }
        
        xmlfiles <- FALSE
        
        labelist <- treatPath(internalbls, type = "R")
        
        if (length(labelist) == 1) {
            labelist <- treatPath(internalbls, type = "XML")
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
        
        pathIsFolder <- length(labelist$fileext) > 1
        
        if (!file.exists("Setup files")) {
            dir.create("Setup files")
        }
        
        csvdatadir <- FALSE # by default
        
        
        # now trying to assess what the csv argument is
        # it can be an object containing csv data, or
        # it can be a string containing a path to the data
        
        
        
        if (all(is.character(csv))) {
            if (csv != "") {
                if (length(csv) > 1) {
                    cat("\n")
                    stop("The csv argument should contain a single path to the list object.\n\n", call. = FALSE)
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
        
        
        # when pathIsFolder, it is unlikely that uniqueid is the same for all data files
        # so set uniqueid to "" to ignore it.
        
        # if (pathIsFolder) {
        #     uniqueid <- ""
        # }
        
        for (i in seq(length(labelist$files))) {
            
            if (xmlfiles) {
                internalblsObject <- getMetadata(file.path(labelist$completePath, labelist$files[i]), fromsetupfile = TRUE, saveFile = saveFile)
            }
            else {
                aa <- ls()
                
                tryCatch(eval(parse(file.path(labelist$completePath, labelist$files[i]))), error = function(x) {
                    stop(paste("\nThere is an error associated with the file \"", labelist$files[i], "\", see below:\n       ", gsub("Error in ", "", as.character(x)), sep=""), call. = FALSE)
                })
                
                bb <- ls()
                bb <- bb[-which(bb == "aa")]
            }
            
            currentdir <- getwd()
            
            if (csvdatadir) {
                
                if (labelist$filenames[i] %in% csvnames) {
                    cat(labelist$filenames[i], "\n")
                    position <- match(labelist$filenames[i], csvnames)
                    
                    
                    for (j in seq(length(position))) {
                        
                        if (csvext[position[j]] %in% c("CSV", "CSV.GZ")) {
                                                                                                   # delimiter is already set from the function's formal argument
                            csvreadfile <- read.csv(file.path(datadir, csvfiles[position[j]]), sep = delimiter, header = TRUE, as.is = TRUE)
                            
                            if (ncol(csvreadfile) == 1) {
                                delimiter <- getDelimiter(file.path(datadir, csvfiles[position[j]]))
                                
                                if (delimiter == "unknown") {
                                    stop(paste("Unknown column separator for the file", csvfiles[position[j]],
                                               "\nShould be either \",\" or \";\" or tab separated.\n\n"), call. = FALSE)
                                }
                                
                                csvreadfile <- read.csv(file.path(datadir, csvfiles[position[j]]), sep = delimiter, header = TRUE, as.is = TRUE)
                            }
                            
                            
                            if (!xmlfiles) {
                                internalblsObject <- get(setdiff(bb, aa))
                            }
                            
                            tryCatch(Recall(internalblsObject, type = type, miss = miss, csv = csvreadfile, uniqueid = uniqueid, SD = SD,
                                            delimiter = delimiter, OS = OS, outfile = labelist$filenames[i], pathIsFolder = pathIsFolder, ... = ...),
                                error = function(x) {
                                    # if no sink() is needed, an invisible warning message will be returned
                                    tryCatch(sink(), warning=function(y) return(invisible(y)))
                                    setwd(currentdir)
                                    cat(paste("     There is an error associated with the file \"", labelist$filenames[i], "\", see below:\n     ", sep=""))
                                    cat(as.character(x))
                                })
                        }
                    }
                }
                else {
                    cat(labelist$filenames[i], "(no .csv file)", "\n")
                    if (!xmlfiles) {
                        internalblsObject <- get(setdiff(bb, aa))
                    }
                    
                    tryCatch(Recall(internalblsObject, type = type, miss = miss, uniqueid = uniqueid, SD = SD, 
                                    delimiter = delimiter, OS = OS, outfile = labelist$filenames[i], pathIsFolder = pathIsFolder, ... = ...),
                        error = function(x) {
                            tryCatch(sink(), warning=function(y) return(invisible(y)))
                            setwd(currentdir)
                            cat(paste("     There is an error associated with the file \"", labelist$filenames[i], "\", see below:\n     ", sep=""))
                            cat(as.character(x))
                        })
                }
            }
            else {
                cat(labelist$filenames[i], "\n")
                
                if (is.data.frame(csv)) {
                    if (length(labelist$filenames) == 1) {
                        if (!xmlfiles) {
                            internalblsObject <- get(setdiff(bb, aa))
                        }
                        tryCatch(Recall(internalblsObject, type = type, miss = miss, csv = csv, uniqueid = uniqueid, SD = SD,
                                        delimiter = delimiter, OS = OS, outfile = labelist$filenames[i], pathIsFolder = pathIsFolder, ... = ...),
                        error = function(x) {
                            tryCatch(sink(), warning=function(y) return(invisible(y)))
                            setwd(currentdir)
                            cat(paste("     There is an error associated with the file \"", labelist$filenames[i], "\", see below:\n     ", sep=""))
                            cat(as.character(x))
                        })
                    }
                }
                else {
                    # there is really no csv data
                        if (!xmlfiles) {
                            internalblsObject <- get(setdiff(bb, aa))
                        }
                        tryCatch(Recall(internalblsObject, type = type, miss = miss, uniqueid = uniqueid, SD = SD, 
                                        delimiter = delimiter, OS = OS, outfile = labelist$filenames[i], pathIsFolder = pathIsFolder, ... = ...),
                        error = function(x) {
                            tryCatch(sink(), warning=function(y) return(invisible(y)))
                            setwd(currentdir)
                            cat(paste("     There is an error associated with the file \"", labelist$filenames[i], "\", see below:\n     ", sep=""))
                            cat(as.character(x))
                        })
                }
            }
            
            if (!xmlfiles) {
                rm(list = c(eval(setdiff(bb, aa)), "bb", "aa"))
            }
        }
        
        # if (pathIsFolder & (type == "all" | type == "R")) {
        #     cat("\nMissing values not treated for R when using an entire directory.\n")
        #     cat("Run individual commands and specify the uniqueid for each dataset.\n")
        # }
        
        cat("\nSetup files created in:\n", file.path(currentdir, "Setup files"), "\n\n", sep="")
        
        return(invisible())
    }
    
    # if (identical(miss, "")) {
        missngs <- unique(unlist(lapply(internalbls, function(x) {
            if (is.element("missing", names(x))) {
                return(names(x$values)[is.element(x$values, x$missing)])
            }
        })))
        
        if (!is.null(missngs)) {
            miss <- missngs
        }
    # }
    
    
    csvlist <- NULL # initialization
    if (all(is.character(csv))) {
        if (all(csv != "")) {
            if (length(csv) > 1) {
                cat("\n")
                stop("The csv argument should contain a single path to the list object.\n\n", call. = FALSE)
            }
            
            csvlist <- treatPath(csv, type = "CSV")
            if (length(csvlist) > 1) {
                # no error
                if (length(csvlist$files) > 1) {
                    cat("\n")
                    stop("There is only one object containing labels and multiple csv files.\n\n", call. = FALSE)
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
            else {
                return(FALSE)
            }
        })))
    }
    
    
    if (is.null(names(internalbls)) | !checkvarlab(internalbls)) {
        cat("\n")
        stop("The object does not contain variables and / or labels.\n\n", call. = FALSE)
    }
    
    if (!(type %in% c("SPSS", "Stata", "SAS", "R", "all"))) {
        cat("\n")
        stop("The argument <type> can only be: \"SPSS\", \"Stata\", \"SAS\", \"R\", or \"all\".\n\n", call. = FALSE)
    }
    
    enter <- getEnter(OS=OS)
    
    names(internalbls) <- toupper(names(internalbls))
    varnames <- names(internalbls)
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
                                                                                           # delimiter is already set from the function's formal argument
            csvreadfile <- read.csv(file.path(csvlist$completePath, csvlist$files[1]), sep = delimiter, header = TRUE, as.is=TRUE)
            
            if (ncol(csvreadfile) == 1) {
            
                delimiter <- getDelimiter(file.path(csvlist$completePath, csvlist$files[1]))
                
                if (delimiter == "unknown") {
                    stop(paste("Unknown column separator for the file", csvlist$files[1],
                               "\nShould be either \",\" or \";\" or tab separated.\n\n"), call. = FALSE)
                }
                
                csvreadfile <- read.csv(file.path(csvlist$completePath, csvlist$files[1]), sep = delimiter, header = TRUE, as.is=TRUE)
            }
            
            # cat("\n")
            # cat("Found \"", csvlist$files[1], "\" in the directory \"", csvlist$completePath, "\". Using that as the .csv file.\n\n", sep="")
            
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
        
        
        if (length(plusnames) > 0) {
            if (length(plusnames) == length(csvnames)) {
                cat("    None of the variables in the .csv file have metadata information.\n",
                    "    (perhaps the .csv file doesn't have the variable names in the first row?)\n", sep="")
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
            internalbls <- internalbls[is.element(varnames, csvnames)]
            
            varnames <- names(internalbls)
            maxchars <- max(nchar(varnames))
            varcheck <- rep(0, length(varnames))
            vartypes <- emptyvars <- vector(mode="list", length=ncol(csv))
            names(vartypes) <- names(emptyvars) <- csvnames
            
            printNOTE <- FALSE
            
            for (i in seq(ncol(csv))) {
                vartypes[i] <- "numeric"
                decimals <- FALSE
                
                tempvar <- csv[, i]
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
                    
                    tempvar2 <- tempvar
                    error <- unlist(strsplit(nofchars[[1]], split=" "))
                    tempvar2 <- tempvar2[-as.numeric(error[length(error)])]
                    
                    cat("    There are multibyte characters in this data (ex. ", csvnames[i], ", position ", error[length(error)], ")\n", sep="")
                    
                    multibyte <- TRUE
                    
                    while(multibyte) {
                        
                        nofchars <- tryCatch(nchar(as.character(tempvar2)), error = function(x) return(x))
                        if (is.list(nofchars)) {
                            error <- unlist(strsplit(nofchars[[1]], split=" "))
                            tempvar2 <- tempvar2[-as.numeric(error[length(error)])]
                        }
                        else {
                            multibyte <- FALSE
                        }
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
                
                hasvalues <- is.element("values", names(internalbls[[csvnames[i]]]))
                
                if (hasvalues) {
                    if (is.character(internalbls[[csvnames[i]]]$values)) {
                        vartypes[i] <- "string"
                        # gofurther <- FALSE
                        maxvarchar <- max(maxvarchar, nchar(internalbls[[csvnames[i]]]$values))
                    }
                }
                
                if (vartypes[i] == "numeric") {
                    
                    tempvar2 <- tempvar[!is.na(tempvar)]
                    
                    if (any(tempvar2 - floor(tempvar2) > 0)) { # has decimals
                        decimals <- TRUE
                    }
                    else {
                        if (hasvalues) {
                            if (is.numeric(internalbls[[csvnames[i]]]$values)) {
                                maxvarchar <- max(maxvarchar, nchar(internalbls[[csvnames[i]]]$values))
                            }
                        }
                    }
                    
                    if (decimals) {
                        spssformats[i] <- paste("F", maxvarchar, ".2", sep="")
                    }
                    else {
                        spssformats[i] <- paste("F", maxvarchar, ".0", sep="")
                    }
                }
                else if (vartypes[i] == "string") {
                    sasformats[i] <- "$"
                    spssformats[i] <- paste("A", maxvarchar, sep="")
                }
                else { # all the values are missing, and no value labels exist
                    spssformats[i] <- paste("F1.0", sep="")
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
    
    
    
    stringvars <- lapply(internalbls, function(x) {
        charvar <- FALSE
        if (is.element("values", names(x))) {
            charvar <- is.character(x$values)
        }
        return(charvar)
    })
    
    
    if (identical(outfile, "")) {
        if (grepl("\"", internalbls_objname)) {
            outfile <- readline("Name for the setup file:\n")
        }
        else {
            outfile <- internalbls_objname
        }
    }
    
    hasvalues <- unlist(lapply(internalbls, function(x) is.element("values", names(x))))
    uniquevals <- unique(lapply(internalbls[hasvalues], function(x) return(x$values)))
    
    uniqueList <- lapply(uniquevals, function(uniques) {
        vars <- sapply(names(internalbls)[hasvalues],
                     function(x) {
                         ifelse(length(internalbls[[x]]$values) == length(uniques),
                                all(names(internalbls[[x]]$values) == names(uniques)), FALSE)
                     })
        return(names(vars[vars]))
    })
    
    
    # initiate an empty list
    numerical_with_strings <- list()
    
    if (!identical(forcenum, c(""))) {
        
        if (!all(forcenum %in% csvnames)) {
            cat("The following <numerical> variable names were not found in the data:\n",
                "\"", paste(forcenum[!forcenum %in% csvnames], collapse="\", \""), "\"\n\n", sep="")
        }
        
        which_numerical <- csvnames[csvnames %in% forcenum]
        
        if (length(which_numerical) > 0) {
            
            # test which values are not numbers in the respective variables
            
            # has_strings  <- lapply(csv[, which_numerical, drop = FALSE], function(x) {
            #    grep("^-?[0-9]+([.]?[0-9]+)?$", x, perl=TRUE, invert=TRUE)
            # })
            
            has_strings  <- lapply(csv[, which_numerical, drop = FALSE], function(x) {
                which(is.na(suppressWarnings(as.numeric(x))))
            })
            
            numerical_with_strings  <- has_strings[unlist(lapply(has_strings, length)) > 0]
        }
    }
    
    
    if (type == "SPSS" | type == "all") {
        internalbls2 <- internalbls
        printMISSING <- FALSE
        
        if (!file.exists("Setup files")) {
            dir.create("Setup files")
        }
        
        if (!file.exists(file.path("Setup files", "SPSS"))) {
            dir.create(file.path("Setup files", "SPSS"))
        }
        
        
        currentdir <- getwd()
        setwd(file.path("Setup files", "SPSS"))
        sink(ifelse(length(grep("\\.sps", outfile)) > 0, outfile, paste(outfile, ".sps", sep="")))
        
        cat("* ------------------------------------------------------------------------------", enter, enter,
            "* --- CONFIGURATION SECTION - START ---", enter, enter, enter, sep="")

        if (formats) {
            cat("* The following command should contain the complete path and", enter,
                "* name of the .csv file to be read (e.g. \"C:/CIS 2008/Data/ALL.csv\")", enter,
                "* Change CSV_DATA_PATH to your filename, below:", enter, enter,
                "FILE HANDLE csvpath /NAME=\"CSV_DATA_PATH\" .", enter, enter, enter, sep="")
        }
        
        cat("* The following command should contain the complete path and", enter,
            "* name of the .sav file to be saved (e.g. \"C:/CIS 2008/Data/ALL.sav\")", enter,
            "* Change SAV_DATA_PATH to your filename, below:", enter, enter,
            "FILE HANDLE savfile /NAME=\"SAV_DATA_PATH\" .", enter, enter, enter,
            "* --- CONFIGURATION SECTION -  END  ---", enter, enter,
            "* ------------------------------------------------------------------------------", enter, enter, enter, enter,
            "* There should be nothing to change below this line", enter,                                                            
            "* ------------------------------------------------------------------------------", enter, enter, enter, enter, sep="")
                  
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
                " /DELIMITERS='", ifelse(delimiter == "\t", "\\t", delimiter), "'", enter,
                " /QUALIFIER='\"'", enter,
                " /FIRSTCASE=2", enter,
                " /IMPORTCASE=ALL", enter,
                " /VARIABLES=", enter, sep="")
            
            maxcharcsv <- max(nchar(csvnames))
            for (i in seq(length(csvnames))) {
                cat(csvnames[i], paste(rep(" ", maxcharcsv - nchar(csvnames[i]) + 1), collapse=""), spssformats[i], sep="")
                if (i == length(csvnames)) {
                    cat(" .")
                }
                cat(enter)
            }
            cat("CACHE .", enter, "EXECUTE .", enter, enter,
                "* ------------------------------------------------------------------------------", enter, enter, enter, sep="")
        }
        
        
        if (any(unlist(stringvars))) {
            cat("* --- Recode string variables which have labels, to numeric variables ---", enter, enter, enter)
            stringvars <- stringvars[unlist(stringvars)]
            for (i in names(stringvars)) {
                
                vals <- seq(length(internalbls2[[i]]$values))
                
                if (is.numeric(miss)) {
                    missvals <- !is.na(match(miss, internalbls2[[i]]$values))
                }
                else {
                    missvals <- !is.na(match(names(internalbls2[[i]]$values), miss))
                }
                
                if (any(missvals)) {
                    nummiss <- logical(length(missvals))
                    for (j in which(missvals)) {
                        missval <- suppressWarnings(as.numeric(internalbls2[[i]]$values[j]))
                        if (!is.na(missval)) {
                            if (!is.element(missval, vals)) {
                                vals[j] <- missval
                                nummiss[j] <- TRUE
                            }
                        }
                    }
                }
                
                
                if (csv_is_df | csv_is_path) {
                    if (emptyvars[[i]]) {
                        cat("* Variable ", toupper(i), " is completely empty, recoding command skipped.", enter, enter,
                            "ALTER TYPE ", toupper(i), "(F", max(nchar(vals)),".0) .", enter, enter, sep="")
                    }
                }
                else {
                    precommand <- paste("RECODE ", toupper(i), " ", sep="")
                    postcommand <- "(\""
                    command <- paste(precommand, postcommand, sep = "")
                    
                    for (j in vals[!nummiss]) {
                        
                        postcommand <- paste(postcommand, internalbls2[[i]]$values[j], "\"", " = ", j, sep = "")
                        command <- paste(command, internalbls2[[i]]$values[j], "\"", " = ", j, sep = "")
                        if (j == length(internalbls2[[i]]$values[!nummiss])) {
                            command <- paste(command, ") (else = copy) INTO TEMPVRBL .", enter, "EXECUTE .", enter, enter, sep="")
                        }
                        else {
                            if (nchar(postcommand) > 70) {
                                postcommand <- paste(paste(rep(" ", nchar(precommand)), collapse=""), "(\"", sep="")
                                command <- paste(command, ")", enter, paste(rep(" ", nchar(precommand)), collapse=""), "(\"", sep="")
                            }
                            else {
                                command <- paste(command, ") (\"", sep="")
                            }
                        }
                    }
                    
                    cat(command)
                    
                    cat("ALTER TYPE ", toupper(i), "(F", max(nchar(vals)),".0) .", enter, enter,
                        "COMPUTE ", toupper(i), " = TEMPVRBL .", enter, "EXECUTE .", enter, enter, sep="")
                    cat("DELETE VARIABLES TEMPVRBL .", enter, "EXECUTE .", enter, enter, sep="")
                }
                
                names(vals) <- names(internalbls2[[i]]$values)
                internalbls2[[i]]$values <- vals
            }
            cat(enter, enter, sep="")
        }
        
        
        
        if (length(numerical_with_strings) > 0) {
            cat("* --- Force variables as numeric --- ", enter, enter, sep="")
            
            for (i in names(numerical_with_strings)) {
                
                tempvar <- csv[, i]
                
                ##### code to recode and transform into numeric
                cat("RECODE ", toupper(i), " (\"", paste(sort(unique(tempvar[numerical_with_strings[[i]]])), collapse = "\" = \"\") (\""), "\" = \"\") .", enter,
                    "EXECUTE .", enter, enter, sep="")
                
                tempvar <- tempvar[-numerical_with_strings[[i]]]
                tempvar <- as.numeric(tempvar)
                
                cat ("ALTER TYPE ", toupper(i), " (F", max(nchar(tempvar)), ".", ifelse(any(tempvar - floor(tempvar) > 0), 2, 0), ") .", enter,
                     "EXECUTE .", enter, enter, sep="")
                
            }
            cat(enter)
        }
        
        
        
        cat("* --- Add variable labels --- ", enter, enter,
            "VARIABLE LABELS", enter, sep="")
        
        for (i in seq(length(varnames))) {
            cat(varnames[i], paste(rep(" ", maxchars - nchar(varnames[i])), collapse=""), " \"", internalbls2[[i]]$label[1], "\"", sep="")
            if (i == length(varnames)) {
                cat(" .", enter, "EXECUTE .", sep="")
            }
            cat(enter)
        }
        cat(enter, enter, sep="")
        
        cat("* --- Add value labels --- ", enter, enter,
            "VALUE LABELS", enter, sep="")
        
        
        for (i in seq(length(uniqueList))) {
            n <- uniqueList[[i]][1]
            
            cat(splitrows(uniqueList[[i]], enter, 80), enter, sep="")
            
            #if (all(is.character(internalbls2[[n]]$values))) {
            #    cat(paste(paste("\"", internalbls2[[n]]$values, "\" \"", names(internalbls2[[n]]$values), "\"", sep=""), collapse="\n"))
            #}
            #else {
                cat(paste(paste(internalbls2[[n]]$values, " \"", names(internalbls2[[n]]$values), "\"", sep=""), collapse=enter))
            #}
            if (i == length(uniqueList)) {
                cat(" .", enter, "EXECUTE .", enter, sep="")
            }
            else {
                cat(enter, "/", enter, sep="")
            }
        }
        cat(enter, enter, sep="")
        
        
        if (!identical(miss, "")) {
        
            if (is.numeric(miss)) {
                missvars <- lapply(internalbls2[hasvalues], function(x) !is.na(match(miss, x$values)))
                withmiss <- as.vector(unlist(lapply(missvars, any)))
                missvals <- lapply(missvars[withmiss], function(x) sort(as.vector(miss[x])))
            }
            else {
                missvars <- lapply(internalbls2[hasvalues], function(x) !is.na(match(names(x$values), miss)))
                withmiss <- as.vector(unlist(lapply(missvars, any)))
                missvals <- lapply(which(withmiss), function(x) sort(as.vector(internalbls2[hasvalues][[x]]$values[missvars[[x]]])))
            }
            
            
            missvals <- unique(lapply(missvals, function(x) {
                if (!any(is.na(suppressWarnings(as.numeric(x))))) {
                    x <- as.numeric(x)
                }
                return(x)
            }))
            
            msngs <- sort(unique(unlist(missvals)))
            
            withmiss2 <- which(withmiss)
            
            if (length(missvals) > 0) {
                uniqueMissList <- list()
                for (i in seq(length(missvals))) {
                    vars <- NULL
                    for (j in withmiss2) {
                        y <- unname(internalbls2[[which(hasvalues)[j]]]$values[missvars[[j]]])
                        if (all(is.element(missvals[[i]], y)) & length(missvals[[i]]) == length(y)) {
                            vars <- c(vars, names(internalbls2)[which(hasvalues)[j]])
                        }
                    }
                    uniqueMissList[[i]] <- vars
                }
                
                if (length(uniqueMissList) > 0) {
                
                    cat("* --- Add missing values --- ", enter, enter,
                        "MISSING VALUES", enter, sep="")
                        
                    for (i in seq(length(uniqueMissList))) {
                        if (length(missvals[[i]]) < 4) {
                            cat(splitrows(toupper(uniqueMissList[[i]]), enter, 80))
                            cat(" (", paste(missvals[[i]], collapse=", ") , ")", sep="")
                        }
                        else {
                            absrange <- abs(range(missvals[[i]]))
                            
                            if (all(missvals[[i]] < 0)) {
                                cat(splitrows(toupper(uniqueMissList[[i]]), enter, 80))
                                cat(" (LOWEST THRU ", max(missvals[[i]]) , ")", sep="")
                            }
                            else {
                                # check if the missing values range doesn't contain any other (non-missing) values
                                checklist <- list()
                                for (mv in uniqueMissList[[i]]) {
                                    allvalues <- internalbls2[[mv]]$values
                                    nonmiss <- allvalues[!allvalues %in% missvals[[i]]]
                                    checklist[[mv]] <- any(nonmiss %in% seq(min(missvals[[i]]), max(missvals[[i]])))
                                }
                                
                                checklist <- unlist(checklist)
                                
                                # print(checklist)
                                
                                if (any(checklist)) {
                                    # at least one variable has a non-missing value within the range of the missing values
                                    printMISSING <- TRUE
                                    
                                    # now trying to see if at least some of the variables can be "rescued"
                                    if (any(!checklist)) {
                                        ###
                                        # poate merge...? DE TESTAT
                                        cat(splitrows(names(checklist)[!checklist], enter, 80))
                                        # cat(paste(names(checklist)[!checklist], collapse=", "))
                                        ###
                                        cat(paste(" (", min(missvals[[i]]), " TO ", max(missvals[[i]]) , ")", enter, sep=""))
                                        checklist <- checklist[checklist]
                                    }
                                    
                                    ###
                                    # poate merge...? DE TESTAT
                                    cat(splitrows(names(checklist), enter, 80))
                                    # cat(paste(names(checklist), collapse=", "))
                                    ###
                                    cat(paste(" (", paste(missvals[[i]][1:3], collapse=", ") , ")", sep=""))
                                    cat(ifelse(i == length(uniqueMissList), " .", ""))
                                    cat("  * more than three distinct missing values found")
                                }
                                else {
                                    cat(splitrows(toupper(uniqueMissList[[i]]), enter, 80))
                                    cat(" (", min(missvals[[i]]), " TO ", max(missvals[[i]]) , ")", sep="") 
                                }
                            }
                        }
                        cat(ifelse(i == length(uniqueMissList), " .", ""))
                        cat(enter)
                    }
                    cat(enter, enter, sep="")
                }
            }
        }
        
        cat(paste("* --- Save the .sav file --- ", enter, enter,
                  "SAVE OUTFILE=savfile", enter, "/KEEP", enter, sep=""))
        
        if (formats) {
            for (n in csvnames) {
                cat(toupper(n), enter, sep="")
            }
        }
        else {
            for (n in names(internalbls2)) {
                cat(toupper(n), enter, sep="")
            }
        }
        
        cat("  /COMPRESSED .", enter, "EXECUTE .", enter, sep="")
        
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
        internalbls2 <- internalbls
        
        if (!file.exists("Setup files")) {
            dir.create("Setup files")
        }
        
        if (!file.exists(file.path("Setup files", "Stata"))) {
            dir.create(file.path("Setup files", "Stata"))
        }
        
        currentdir <- getwd()
        setwd(file.path("Setup files", "Stata"))
        sink(ifelse(length(grep("\\.do", outfile)) > 0, outfile, paste(outfile, ".do", sep="")))
        
        cat("/* Initialization commands */", enter,
            "clear", enter,
            "capture log close", enter,
            "set more off", enter,
            "version 12.0", enter,
            "set linesize 250", enter,
            "set varabbrev off", enter,
            "set memory 1G // not necessary in Stata 12",
            ifelse(SD == ";", "\n#delimit ;", ""), enter, enter, enter,
            "* ----------------------------------------------------------------------------",
            ifelse(SD == ";", " ;", ""), enter, enter,
            "* --- CONFIGURATION SECTION - START ---",
            ifelse(SD == ";", "                                        ;", ""), enter, enter,
            "* The following command should contain the complete path and",
            ifelse(SD == ";", "                   ;", ""), enter,
            "* name of the Stata log file.",
            ifelse(SD == ";", "                                                  ;", ""), enter,
            "* Change LOG_FILENAME to your filename, below:",
            ifelse(SD == ";", "                                 ;", ""), enter,
            "local log_file \"LOG_FILENAME\"",
            ifelse(SD == ";", " ;", ""), enter, enter, enter,
            "* The following command should contain the complete path and",
            ifelse(SD == ";", "                   ;", ""), enter,
            "* name of the CSV file, usual file extension \".csv\"",
            ifelse(SD == ";", "                            ;", ""), enter,
            "* Change CSV_DATA_PATH to your filename, below:",
            ifelse(SD == ";", "                                ;", ""), enter,
            "local csvpath \"CSV_DATA_PATH\"",
            ifelse(SD == ";", " ;", ""), enter, enter, enter,
            "* The following command should contain the complete path and",
            ifelse(SD == ";", "                   ;", ""), enter,
            "* name of the STATA file, usual file extension \".dta\"",
            ifelse(SD == ";", "                          ;", ""), enter,
            "* Change STATA_DATA_PATH to your filename, below:",
            ifelse(SD == ";", "                              ;", ""), enter,
            "local statapath \"STATA_DATA_PATH\"",
            ifelse(SD == ";", " ;", ""), enter, enter, enter,
            "* --- CONFIGURATION SECTION - END ---",
            ifelse(SD == ";", "                                          ;", ""), enter, enter,
            "* ----------------------------------------------------------------------------",
            ifelse(SD == ";", " ;", ""), enter, enter, enter, enter,
            "* There should be nothing to change below this line",
            ifelse(SD == ";", "                            ;", ""), enter,
            "* ----------------------------------------------------------------------------",
            ifelse(SD == ";", " ;", ""), enter, enter, enter, enter,         
            "log using \"`log_file'\", replace text",
            ifelse(SD == ";", " ;", ""), enter, enter,
            "insheet using \"`csvpath'\", comma names case",
            ifelse(SD == ";", " ;", ""), enter, enter,
            "* Note that some variables in the csv raw data file might be in lowercase",
            ifelse(SD == ";", "      ;", ""), enter,
            "* To ensure that the dataset contains only variable names in uppercase",
            ifelse(SD == ";", "         ;", ""), enter, enter,
            "foreach var of varlist _all {",
            ifelse(SD == ";", " ;", ""), enter,
            "    local newname = upper(\"`var'\")",
            ifelse(SD == ";", " ;", ""), enter,
            "    cap rename \`var\' \`newname\'",
            ifelse(SD == ";", " ;", ""), enter,
            "}",
            ifelse(SD == ";", " ;", ""), enter, enter, enter, sep="")
        
        
        if (any(unlist(stringvars))) {
            cat("* Recode string variables which have labels, to numeric variables", ifelse(SD == ";", " ;", ""), enter, enter, sep="")
            for (i in seq(length(stringvars))) {
                
                if (stringvars[[i]]) {
                
                    ## sort() is necessary because Stata automatically transforms
                    # character to numeric using the labels in alfabetical ascending order (TO VERIFY THAT!!)
                    vallabs <- internalbls2[[names(stringvars)[i]]]$values
                    
                    vals <- seq(length(vallabs))
                    names(vals) <- names(vallabs)
                    
                    # if (emptyvars[[i]]) {
                    #     cat("* Variable ", toupper(names(stringvars)[i]), " is completely empty, recoding command skipped",
                    #         ifelse(SD == ";", " ;", ""), enter, sep="")
                    # }
                    # else {
                        # if (any(grepl("^[0-9]", vallabs))) {
                            cat("generate TEMPVAR = .", ifelse(SD == ";", " ;", ""), enter, sep="")
                            for (j in vals) {
                                cat("replace TEMPVAR = ", j, " if ", toupper(names(stringvars)[i]), " == \"", vallabs[j], ifelse(SD == ";", "\" ;", "\""), enter, sep="")
                            }
                            cat("drop ", toupper(names(stringvars)[i]), ifelse(SD == ";", " ;", ""), enter,
                                "rename TEMPVAR ", toupper(names(stringvars)[i]), ifelse(SD == ";", " ;", ""), enter, sep="")
                        ## }
                        ## else {
                        ##     cat("encode (", toupper(names(stringvars)[i]), "), generate (TEMPVAR)", ifelse(SD == ";", " ;", ""), enter,
                        ##         "drop ", toupper(names(stringvars)[i]), ifelse(SD == ";", " ;", ""), enter,
                        ##         "rename TEMPVAR ", toupper(names(stringvars)[i]), ifelse(SD == ";", " ;", ""), enter, sep="")
                        ## }
                    # }
                    
                    internalbls2[[names(stringvars)[i]]]$values <- vals
                    cat(enter)
                }
            }
            cat(enter, enter, sep="")
        }
        
        
        
        if (length(numerical_with_strings) > 0) {
            cat("* Force variables as numeric", ifelse(SD == ";", " ;", ""), enter, enter, sep="")
            
            for (i in names(numerical_with_strings)) {
                
                tempvar <- csv[, i]
                
                for (j in sort(unique(tempvar[numerical_with_strings[[i]]]))) {
                    cat("replace ", toupper(i), " = \"\" if ", toupper(i), " == \"", j, ifelse(SD == ";", "\" ;", "\""), enter, sep="")
                }
                
                cat ("destring ", toupper(i), ", replace", ifelse(SD == ";", " ;", ""), enter, enter, sep="")
                
            }
            cat(enter)
        }
        
        
        
        maxchars <- max(nchar(names(internalbls2)))
        
        cat("* Definition of variable labels", ifelse(SD == ";", " ;", ""), enter, enter, sep="")
        
        # cat(maxchars, enter)
        
        for (n in names(internalbls)) {
            cat(paste("label variable ", toupper(n), paste(rep(" ", maxchars - nchar(n)), collapse=""), " \"", internalbls2[[n]]$label[1], "\"", ifelse(SD == ";", " ;", ""), enter, sep=""))
        }
        cat(enter, enter, sep="")
        
        cat("* Definition of category labels", ifelse(SD == ";", " ;", ""), enter, enter, sep="")
        
        for (i in seq(length(uniqueList))) {
            n <- uniqueList[[i]][1]
            
            headerlabel <- paste("label define LABELS_GROUP_", i, sep="")
            
            if (SD == ";") {
                cat(headerlabel, enter,
                    paste(paste(paste(internalbls2[[n]]$values, " \"", names(internalbls2[[n]]$values), "\"", sep=""),
                                collapse=enter), #paste("\n", paste(rep(" ", nchar(headerlabel)), collapse=""), sep="")),
                          " ;", enter, enter, sep=""),
                    sep="")
            }
            else if (SD == "///") {
                cat(headerlabel, " ///", enter,
                    paste(paste(paste(internalbls2[[n]]$values, " \"", names(internalbls2[[n]]$values), "\"", sep=""),
                                collapse=" ///", enter), enter, enter, sep=""), sep="")
            }
            else if (SD == "/*") {
                cat(headerlabel, " /*", enter,
                    paste("*/ ", paste(paste(internalbls2[[n]]$values, " \"", names(internalbls2[[n]]$values), "\"", sep=""),
                                collapse=paste(" /*", enter, "*/ ", sep="")),
                          enter, enter, sep=""),
                    sep="")
            }
            else {
                cat(paste("label define LABELS_GROUP_", i, " ", sep=""),
                    paste(paste(paste(internalbls2[[n]]$values, " \"", names(internalbls2[[n]]$values), "\"", sep=""),
                                collapse=" "),
                          enter, sep=""),
                    sep="")
            }
        }
        
        cat(enter, enter, sep="")
        cat("* Attachment of category labels to variables", ifelse(SD == ";", " ;", ""), enter, enter, sep="")
        
        for (i in seq(length(uniqueList))) {
            n <- uniqueList[[i]][1]
            for (j in uniqueList[[i]]) {
                cat(paste("label values ", toupper(j), paste(rep(" ", maxchars - nchar(j)), collapse=""), " LABELS_GROUP_", i, ifelse(SD == ";", " ;", ""), enter, sep=""))
            }
        }
        
        cat("\ncompress", ifelse(SD == ";", " ;", ""), enter,
            "save \"`statapath'\", replace", ifelse(SD == ";", " ;", ""), enter, enter,
            "log close", ifelse(SD == ";", " ;", ""), enter,
            "set more on", ifelse(SD == ";", "\n#delimit cr", ""), enter, sep="")
        
        sink()
        setwd(currentdir)
    }
    
    
    if (type == "SAS" | type == "all") {
        internalbls2 <- internalbls
        if (!file.exists("Setup files")) {
            dir.create("Setup files")
        }
        
        if (!file.exists(file.path("Setup files", "SAS"))) {
            dir.create(file.path("Setup files", "SAS"))
        }
        
        currentdir <- getwd()
        setwd(file.path("Setup files", "SAS"))
        sink(ifelse(length(grep("\\.sas", outfile)) > 0, outfile, paste(outfile, ".sas", sep="")))
        
        cat("* ------------------------------------------------------------------------------ ;", enter, enter,
            "* --- CONFIGURATION SECTION - START ---                                          ;", enter, enter, enter, sep="")                                            

        if (formats) {
            cat("* The following command should contain the complete path and                     ;", enter,
                "* name of the .csv file to be read (e.g. \"C:/CIS2008/Data/ALL.csv\")              ;", enter,
                "* Change CSV_DATA_PATH to your filename, below                                   ;", enter, enter,
                "FILENAME csvpath \"CSV_DATA_PATH\";", enter, enter, enter, sep="")
                      
           # cat("* It is assumed the data file was created under Windows (end of line is CRLF);", enter,
           #     "* If the csv file was created under Unix,  change eol=LF;", enter,
           #     "* If the csv file was created under MacOS, change eol=CR below;", enter, enter,
           #     "%LET eol=CRLF;", enter, enter, enter, sep="")
        }
        
        cat("* The following command should contain the complete path of the                  ;", enter,
            "* directory where the setup file will be saved (e.g. \"C:/CIS2008/Data\")          ;", enter,
            "* Change SAS_DATA_FOLDER to your directory name, below                           ;", enter, enter,
            "LIBNAME dirout \"SAS_DATA_FOLDER\";", enter, enter, enter, sep="")
                  
        cat("* The following command should contain the name of the output SAS file only      ;", enter,
            "* (without quotes, and without the .sas7bdat extension)                          ;", enter,
            "* Change SAS_FILE_NAME to your output file name, below                           ;", enter, enter,
            "%LET sasfile=SAS_FILE_NAME;", enter, enter, enter,
            "* --- CONFIGURATION SECTION -  END ---                                           ;", enter, enter,
            "* ------------------------------------------------------------------------------ ;", enter, enter, enter, enter,
            "* There should be nothing to change below this line;", enter,
            "* ------------------------------------------------------------------------------ ;", enter, enter, enter, enter, sep="")
        
        if (formats) {
            cat("* --- Read the raw data file ---                                                 ;", enter, enter,
                "DATA sasimport;", enter, enter,
                "INFILE csvpath", enter,
                "       DLM=", ifelse(delimiter == "\t", "'09'X", paste("\"", delimiter, "\"", sep="")), enter,
                "       FIRSTOBS=2", enter,
                #### "       TERMSTR=&eol", enter, #### line commented out
                "       DSD", enter,
                "       TRUNCOVER", enter,
                "       LRECL=512", enter,
                "       ;", enter, enter,
                
                "INPUT  ", toupper(csvnames[1]), ifelse(sasformats[1] == "$", " $", ""), enter, sep="")
            
            for (i in seq(2, length(csvnames))) {
                cat("       ", csvnames[i], ifelse(sasformats[i] == "$", " $", ""), enter, sep="")
            }
            cat("       ;", enter, sep="")
            cat("RUN;", enter, enter,
                "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep="")
        }
        
        if (any(unlist(stringvars))) {
            cat("* --- Recode string variables which have labels, to numeric variables ---        ;", enter, enter, sep="")
                
            for (i in names(stringvars)) {
                if (stringvars[[i]]) {
                    
                    x <- internalbls2[[i]]$values
                    vals <- seq(length(x))
                    names(vals) <- names(x)
                    
                    # if (emptyvars[[i]]) {
                    #     cat("* Variable ", toupper(i), " is completely empty, recoding command skipped ;", enter, sep="")
                    #     # TO DO: coerce empty variable from string ($) to numeric... ??
                    # }
                    # else {
                        cat("DATA sasimport;", enter, enter,
                            "    SET sasimport;", enter, enter, sep="")
                        
                        for (j in seq(length(vals))) {
                            cat("    IF (", i, " = '", x[j], "') THEN TEMPVAR = ", vals[j], ";", enter, sep="")
                        }
                        
                        cat(enter, "RUN;", enter, enter, "DATA sasimport;", enter, enter,
                            "    SET sasimport;", enter, enter,
                            "    DROP ", i, ";", enter,
                            "    RENAME TEMPVAR = ", i, ";", enter, enter,
                            "RUN;", enter, enter, sep="")
                    # }
                    
                    internalbls2[[i]]$values <- vals
                }
            }
            
            cat("* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep="")
        }
        
        
        
        
        if (length(numerical_with_strings) > 0) {
            
            cat("* --- Force variables as numeric ---                                             ;", enter, enter, sep="")
            
            for (i in toupper(names(numerical_with_strings))) {
                
                cat("DATA sasimport;", enter, enter,
                    "    SET sasimport;", enter, enter,
                    "    TEMPVAR = INPUT(", i, ", ?? best.);", enter, enter,
                    "RUN;", enter, enter, 
                    "DATA sasimport;", enter, enter,
                    "    SET sasimport;", enter, enter,
                    "    DROP ", i, ";", enter,
                    "    RENAME TEMPVAR = ", i, ";", enter, enter,
                    "RUN;", enter, enter, sep="")
            }
            
            cat("* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep="")
        }
        
        
        if (any(unlist(stringvars)) | length(numerical_with_strings) > 0) {
            cat("* --- Reorder the variables in their original positions ---                      ;", enter, enter,
                "DATA sasimport;", enter, enter,
                "    RETAIN ", gsub(",", "", splitrows(toupper(names(internalbls2)), enter, 70, "           ")), ";", enter, enter,
                "    SET sasimport;", enter, enter,
                "RUN;", enter, enter,
                "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep="")
        }
        
        
        
        cat("* --- Add variable labels ---                                                    ;", enter, enter,
            "DATA sasimport;", enter, enter,
            "    SET sasimport;", enter, enter, sep="")
        
        for (i in seq(length(varnames))) {
            cat("    LABEL ", varnames[i], paste(rep(" ", maxchars - nchar(varnames[i]) + 1), collapse=""), "=", " \"", internalbls2[[i]]$label[1], "\";", enter, sep="")
        }
        
        cat(enter, "RUN;", enter, enter,
            "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep="")
        
        cat("* --- Create value labels groups ---                                             ;", enter, enter,
            "PROC FORMAT;", enter, enter, sep="")
        
        for (i in seq(length(uniqueList))) {
            n <- uniqueList[[i]][1]
            cat(paste("VALUE LABELS_", i, "_GROUP", enter, sep=""),
                paste(paste(paste("      ", internalbls2[[n]]$values, "=\"", names(internalbls2[[n]]$values), "\"", sep=""),
                            collapse=enter), #paste("\n", paste(rep(" ", nchar(headerlabel)), collapse=""), sep="")),
                      ";", enter, enter, sep=""),
                sep="")
        }
		  
        cat("RUN;", enter, enter,
            "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep="")
        
        cat("* --- Format variables with value labels ---                                     ;", enter, enter,
            "DATA sasimport;", enter, enter,
            "    SET sasimport;", enter, enter, "    FORMAT", enter, sep="")
        
        for (i in seq(length(uniqueList))) {
            n <- uniqueList[[i]][1]
            for (j in uniqueList[[i]]) {
                cat("    ", toupper(j), paste(rep(" ", maxchars - nchar(j)), collapse=""), " LABELS_", i, "_GROUP", ".", enter, sep="")
            }
        }
          
        cat("    ;", enter, enter, "RUN;", enter, enter,
            "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep="")
        
                      
        cat("* --- Save data to a sas type file ---                                           ;", enter, enter,
            "DATA dirout.&sasfile;", enter, enter,
            "    SET sasimport;", enter, enter,
            "RUN;", enter, sep="")
        
        sink()
        setwd(currentdir)
    }
    
    
    
    if (type == "R" | type == "all") {
        internalbls2 <- internalbls
        printMISSING <- FALSE
        
        
        if (!file.exists("Setup files")) {
            dir.create("Setup files")
        }
        
        if (!file.exists(file.path("Setup files", "R"))) {
            dir.create(file.path("Setup files", "R"))
        }
        
        currentdir <- getwd()
        setwd(file.path("Setup files", "R"))
        
        sink(ifelse(length(grep("\\.R", outfile)) > 0, outfile, paste(outfile, ".R", sep="")))
        
        cat("# ------------------------------------------------------------------------------", enter, enter,
            "# --- CONFIGURATION SECTION - START ---", enter, enter, enter, sep="")

        # if (formats) {
            cat("# The following command should contain the complete path and", enter,
                "# name of the .csv file to be read (e.g. \"C:/CIS 2008/Data/ALL.csv\")", enter,
                "# Change CSV_DATA_PATH to your filename, below:", enter, enter,
                "csvpath <- \"CSV_DATA_PATH\"", enter, enter, enter, sep="")
        # }
        
        cat("# The following command should contain the complete path and", enter,
            "# name of the .Rdata file to be saved (e.g. \"C:/CIS 2008/Data/ALL.Rdata\")", enter,
            "# Change RDATA_PATH to your filename, below:", enter, enter,
            "rdatapath <- \"RDATA_PATH\"", enter, enter, enter, sep="")
        
                  
        # if (formats) {
            cat("# --- Read the raw data ---", enter, enter,
                "rdatafile <- read.csv(csvpath, sep = \"", ifelse(delimiter == "\t", "\\t", delimiter), "\")",
                enter, enter, "names(rdatafile) <- toupper(names(rdatafile))    # all variable names to upper case",
                enter, enter, enter, sep="")
        # }
        # else {
        #     cat("# \"rdatafile\" should be an R data.frame (usually read from a .csv file)\n\n")
        # }
        
        cat("# --- Specify the unique ID variable ---", enter, enter,
            "# The following command should contain the name of the unique ID variable", enter,
            "# in the .csv file (to identify missing observations in different variables).", enter,
            "# Change \"\" to your unique ID variable name (using upper case letters), below:", enter, enter,
            "uniqueid <- \"\"", enter, enter, enter, enter, sep = "")
        
        
        cat("# --- CONFIGURATION SECTION -  END  ---", enter, enter,
            "# There should be nothing to change below this line", enter,                                                            
            "# ------------------------------------------------------------------------------", enter, enter, enter, enter, sep="")
        
        
        if (any(unlist(stringvars))) {
            cat("# --- Recode string variables which have labels, to numeric variables ---", enter, enter, sep="")
            stringvars <- stringvars[unlist(stringvars)]
            for (i in names(stringvars)) {
                
                vals <- seq(length(internalbls2[[i]]$values))
                
                if (is.numeric(miss)) {
                    missvals <- !is.na(match(miss, internalbls2[[i]]$values))
                }
                else {
                    missvals <- !is.na(match(names(internalbls2[[i]]$values), miss))
                }
                
                if (any(missvals)) {
                    nummiss <- logical(length(missvals))
                    for (j in which(missvals)) {
                        missval <- suppressWarnings(as.numeric(internalbls2[[i]]$values[j]))
                        if (!is.na(missval)) {
                            if (!is.element(missval, vals)) {
                                vals[j] <- missval
                                nummiss[j] <- TRUE
                            }
                        }
                    }
                }
                
                
                x <- internalbls2[[i]]$values
                names(vals) <- names(x)
                
                cat("tempvar <- rep(NA, length(rdatafile$", toupper(i), "))", enter, enter, sep="")
                for (j in seq(length(vals))) {
                    cat("tempvar[rdatafile$", toupper(i), " == \"", x[j], "\"] <- ", vals[j], enter, sep="")
                }
                
                cat(enter, "rdatafile[ , \"", toupper(i), "\"] <- tempvar", enter, enter, sep="")
                
                internalbls2[[i]]$values <- vals
            }
        }
        
        
        
        if (length(numerical_with_strings) > 0) {
            
            cat("# --- Force variables as numeric ---", enter, enter, sep="")
            
            maxnchars <- max(nchar(names(numerical_with_strings)))
            
            for (i in toupper(names(numerical_with_strings))) {
                cat("rdatafile[ , \"", i, "\"]", paste(rep(" ", maxnchars - nchar(i)), collapse=""),
                    " <- suppressWarnings(as.numeric(rdatafile[ , \"", i, "\"]", "))", enter, sep="")
            }
        }
        
        cat(enter, enter,
            "# ------------------------------------------------------------------------------",
            enter, enter,
            "# --- Set the variable metadata attributes --- ", enter, enter, sep="")
        
        writeRlist(internalbls2, OS = OS, attr = TRUE, indent = indent)
        
        cat(enter, enter, sep = "")
        
        
        uniquevar <- unlist(lapply(internalbls2, function(x) {
            uniqueid <- FALSE
            if (any(is.element("uniqueid", names(x)))) {
                uniqueid <- x$uniqueid
            }
            return(uniqueid)
        }))
        
        if (sum(uniquevar) == 1) {
            uniqueid <- names(internalbls2)[uniquevar]
        }
        
        checkMISSING <- rep(FALSE, 3)
        checkMISSING[1] <- TRUE
        # checkMISSING[1] <- is.data.frame(csv)
        checkMISSING[2] <- !identical(miss, "")
        checkMISSING[3] <- !identical(uniqueid, "")
        
        
        if (all(checkMISSING)) {
        # if (is.data.frame(csv) & uniqueid != "") {
            
            if (is.numeric(miss)) {
                missvars <- lapply(internalbls2[hasvalues], function(x) !is.na(match(miss, x)))
                withmiss <- as.vector(unlist(lapply(missvars, any)))
                missvals <- lapply(missvars[withmiss], function(x) miss[x])
                names(missvals) <- names(internalbls2[hasvalues])[withmiss]
            }
            else {
                missvars <- lapply(internalbls2[hasvalues], function(x) !is.na(match(names(x$values), miss)))
                withmiss <- as.vector(unlist(lapply(missvars, any)))
                missvals <- lapply(which(withmiss), function(x) as.vector(internalbls2[hasvalues][[x]]$values[missvars[[x]]]))
                names(missvals) <- names(internalbls2[hasvalues])[withmiss]
            }
            
            uniqueid <- toupper(uniqueid)
            
            if (length(missvals) > 0) {
                
                cat("# ------------------------------------------------------------------------------",
                    enter, enter, "# --- Set the missing values attributes --- ", enter, enter,
                    "# only if the uniqueid variable really exists,", enter,
                    "# and it has no duplicate or missing values", enter, enter, 
                    "if (!identical(uniqueid, \"\")) {", enter, 
                    "    uniqueid <- rdatafile[, uniqueid]", sep = "")
                
                for (i in seq(length(missvals))) {
                    values <- internalbls2[[names(missvals)[i]]]$values
                    
                    cat(enter, enter, "    attr(rdatafile$", toupper(names(missvals)[i]),", \"metadata\")$misscases <- list(", enter, sep = "")
                    
                    for (j in seq(length(missvals[[i]]))) {
                        cat("        \"", missvals[[i]][j], "\" = sort(uniqueid[rdatafile$",
                            toupper(names(missvals)[i]), " == ", missvals[[i]][j], "])",
                            ifelse(j == length(missvals[[i]]), "", ","),
                            enter, sep = "")
                    }
                    cat("    )", enter, sep = "")
                    
                    # here, though, it is likely to have multiple missing values
                    # in the future, missvals[[i]] might be a list to accommodate for ranges
                    
                    if (length(unlist(missvals[[i]])) == 1) {
                        cat("    rdatafile$", toupper(names(missvals)[i]), "[rdatafile$",
                            toupper(names(missvals)[i]), " == ", unlist(missvals[[i]]),
                            "] <- NA", sep="")
                    }
                    else {
                        cat("    rdatafile$", toupper(names(missvals)[i]), "[is.element(rdatafile$",
                            toupper(names(missvals)[i]), ", c(", paste(unlist(missvals[[i]]), collapse = ", "),
                            "))] <- NA", sep = "")
                    }
                }
                
                cat(enter, "}", enter, enter, enter, enter,
                "# ------------------------------------------------------------------------------",
                enter, enter, sep="")
            }
        }
        
        
        cat("# --- Save the R data file --- ", enter, enter,
            "rfilename <- unlist(strsplit(basename(rdatapath), split=\"\\\\.\"))[1]", enter,
            "rdatapath <- file.path(dirname(rdatapath), paste(rfilename, \".Rdata\", sep=\"\"))", enter,
            "assign(rfilename, rdatafile)", enter,
            "eval(parse(text = paste(\"save(\", rfilename, \", file=rdatapath)\", sep=\"\")))",
            enter, enter, enter, enter,
            "# ------------------------------------------------------------------------------",
            enter, enter,
            "# --- Clean up the working space --- ", enter, enter,
            "rm(rfilename, rdatafile, csvpath, rdatapath, uniqueid", sep="")
        
        if (any(unlist(stringvars))) {
            cat(", tempvar, ", toupper(paste(names(stringvars)[unlist(stringvars)], collapse=", ")), sep="")
        }
        
        cat(")", enter, sep="")
        
        
        
        # finish writing and close the .R file
        sink()
        
        if (any(checkMISSING)) {
            if (!all(checkMISSING)) {
                if (!checkMISSING[1]) {
                    cat("    Data not found, check the \"csv\" argument or the \"data\" directory.\n")
                }
                if (!checkMISSING[2]) {
                    cat("    The \"miss\" argument needs to be specified.\n")
                }
                if (!checkMISSING[3]) {
                    cat("    The uniqueid variable has to be specified.\n")
                }
                cat("    Missing values not treated for the R setup file.\n\n")
            }
        }
        
        setwd(currentdir)
        
    }
}

