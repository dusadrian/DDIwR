treatPath <- function(path, type = "R", single = FALSE, check = TRUE) {
    if (length(path) > 1) {
        cat("\n")
        # if (type == "R") {
        #     stop("The <codeBook> argument should contain a single path to the list object.\n\n", call. = FALSE)
        # }
        if (type == "csv") {
            stop("The <csv> argument should contain a single path to the .csv file.\n\n", call. = FALSE)
        }
    }
    
    if (!is.character(path)) {
        cat("\n")
        stop("The path should be specified in a string.\n\n", call. = FALSE)
    }
    currdir <- getwd()
    
    ## PERHAPS IMPORTANT:
    # normalizePath() to deal with the symbolic links, relative paths and absolute paths
    
    ## file_test() determines if basename is a file or a folder
    # file_test("-d", basename(path))
    
    
    lastpart <- basename(path)
    pathname <- suppressWarnings(normalizePath(dirname(path), winslash = "/"))
    
    # check if a path exists, before the lastpart
    pathexists <- pathname != "."
    
    if (pathexists) {
        
        if (!file.exists(pathname)) {
            if (check) {
                cat("\n")
                stop(paste("Cannot find the path up to \"", pathname, "\".\n",
                        "Please check that path, or try changing the working directory.\n\n", sep=""), call. = FALSE)
            }
            else {
                pathname <- file.path(getwd(), pathname)
            }
        }
        
    }
    
    allfiles <- FALSE

    if (!file.exists(file.path(pathname, lastpart))) {
        filesplit <- unlist(strsplit(lastpart, split="\\."))
        
        if (length(filesplit) >= 2) {
            if (filesplit[1] == "*") {
                allfiles <- TRUE
            }
        }
        
        if (!allfiles & check) {
            cat("\n")
            stop(paste("There is no \"", lastpart, "\" in the directory \"", ifelse(pathname == ".", getwd(), pathname), "/\".\n\n", sep=""), call. = FALSE)
        }
    }
    
    completePath <- pathname
    
    fileobj <- "" # a default neutral value
    
    if (allfiles) {
        fileobj <- getFiles(dirpath = ".", type = filesplit[2])
        if (length(fileobj) > 1) { # otherwise it's just an error message from getFiles()
            files <- fileobj$files
            filenames <- fileobj$filenames
            fileext <- toupper(fileobj$fileext)
        }
    }
    else {
        
        if (file_test("-d", file.path(pathname, lastpart))) {
            # if it's a subfolder
            
            if (single) {
                cat("\n")
                stop(paste("A file name should be provided, not a directory.\n\n", sep=""), call. = FALSE)
            }
            
            completePath <- file.path(pathname, lastpart)
            
            fileobj <- getFiles(dirpath = completePath, type = type)
            
            if (length(fileobj) > 1) { # otherwise it's just an error message from getFiles()
                files <- fileobj$files
                filenames <- fileobj$filenames
                fileext <- toupper(fileobj$fileext)
            }
        }
        else {
            files <- lastpart
            filesplit <- unlist(strsplit(lastpart, split="\\."))
            filenames <- filesplit[1]
            fileext <- NA
            if (length(filesplit) >= 2) {
                fileext <- toupper(paste(filesplit[seq(2, length(filesplit))], collapse="."))
            }
        }
    }
    
    if (length(fileobj) == 1) {
        if (fileobj != "") {
            return(fileobj)
        }
    }
    
    return(list(
        completePath = completePath,
        files = files,
        filenames = filenames,
        fileext = fileext
        ))
}
