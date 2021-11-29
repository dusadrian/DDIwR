getFiles <- function(path = ".", type = "*", currdir) {
    
    lastpart <- basename(path)
    pathname <- suppressWarnings(normalizePath(dirname(path), winslash = "/"))
    path <- file.path(pathname, lastpart)

    # get all files
    if (file_test("-d", path)) {
        files <- list.files(path)
    }
    else {
        files <- lastpart
    }

    if (length(files) == 0) {
        return(
            paste(
                "The directory",
                path,
                "is empty."
            )
        )
    }

    fileext <- tools::file_ext(files)
    files <- files[fileext != ""]

    if (length(files) == 0) {
        return(
            paste(
                "The directory \"",
                path,
                "\" doesn't contain any known files.",
                sep = ""
            )
        )
    }

    fileext <- fileext[fileext != ""]
    if (length(wgz <- which(toupper(fileext) == "GZ")) > 0) {
        if (length(wcsv <- grepl("\\.CSV", toupper(files[wgz]))) > 0) {
            fcsv <- lapply(
                strsplit(
                    files[wgz][wcsv],
                    split = "\\."
                ), function(x) {

                lx <- length(x)
                return(
                    c(
                        paste(
                            x[seq(lx - 2)],
                            collapse = "."
                        ),
                        paste(
                            x[seq(lx - 1, lx)],
                            collapse = "."
                        )
                    )
                )
            })

            csvext <- unlist(lapply(fcsv, "[[", 2))
            # files[wgz][wcsv] <- unlist(lapply(fcsv, "[[", 1))

            fileext[wgz][wcsv] <- csvext
        }
    }


    if (type != "*") {
        # check if there is any file with the right extension
        fileidxs <- which(toupper(fileext) == toupper(type))
        if (toupper(type) == "CSV" & any(toupper(fileext) == "CSV.GZ")) {
            fileidxs <- which(is.element(toupper(fileext), c("CSV", "CSV.GZ")))
        }

        if (length(fileidxs) == 0) {
            return(
                paste(
                    "There is no .",
                    type,
                    " type file in the directory \"",
                    path,
                    "\"",
                    sep = ""
                )
            )
        }

        # if code survives this far, filter all the "right" files from all files
        files <- files[fileidxs]
        fileext <- fileext[fileidxs]
    }

    filenames <- files
    for (i in seq(length(files))) {
        filenames[i] <- gsub(paste(".", fileext[i], sep = ""), "", filenames[i])
    }

    return(
        list(
            completePath = pathname,
            files = files,
            filenames = filenames,
            fileext = toupper(fileext)
        )
    )

}
