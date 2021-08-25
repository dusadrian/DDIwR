treatPath <- function(path, type = "*", single = FALSE, check = TRUE) {
    if (length(path) > 1) {
        cat("\n")
        # if (type == "R") {
        #     admisc::stopError("The <codeBook> argument should contain a single path to the list object.")
        # }
        if (type == "csv") {
            admisc::stopError("The argument \"csv\" should contain a single path to the .csv file.")
        }
    }
    
    if (!is.character(path)) {
        admisc::stopError("The path should be specified in a string.")
    }
    currdir <- getwd()


    lastpart <- basename(path)
    # normalizePath() deals with the symbolic links, relative paths and absolute paths
    pathname <- suppressWarnings(normalizePath(dirname(path), winslash = "/"))

    # check if a path exists, before the lastpart
    pathexists <- pathname != "."

    if (pathexists) {

        if (!file.exists(pathname)) {
            if (check) {
                admisc::stopError(
                    paste(
                        "Cannot find the path up to \"",
                        pathname,
                        "\".\n",
                        "Please check that path, or try changing the working directory.",
                        sep = ""
                    )
                )
            }
            else {
                pathname <- file.path(getwd(), pathname)
            }
        }

    }

    allfiles <- FALSE
    if (!file.exists(file.path(pathname, lastpart))) {
        # something like /path/to/*.R
        # where lastpart is *.R
        filesplit <- unlist(strsplit(lastpart, split = "\\."))

        if (length(filesplit) >= 2) {
            if (filesplit[1] == "*") {
                allfiles <- TRUE
                type <- filesplit[2]
                lastpart <- ""
            }
        }

        if (!allfiles & check) {
            admisc::stopError(
                paste(
                    "There is no \"",
                    lastpart,
                    "\" in the directory \"",
                    ifelse(
                        pathname == ".",
                        getwd(),
                        pathname
                    ),
                    "/\".",
                    sep = ""
                )
            )
        }

        fileobj <- list(
            completePath = pathname,
            files = lastpart,
            filenames = filesplit[1],
            fileext = toupper(filesplit[2])
        )
    }
    else {

        ## file_test() determines if a file or a directory
        if (file_test("-d", file.path(pathname, lastpart))) {
            if (single) {
                admisc::stopError("A file name should be provided, not a directory.")
            }
        }

        fileobj <- getFiles(path = file.path(pathname, lastpart), type = type)

    }

    return(fileobj)
}
