#' @description Check function arguments
#' @noRd
`checkArgument` <- function(argvalue, argname, type = "c") {
    valid <- !is.null(argvalue) &&
            is.atomic(argvalue) &&
            length(argvalue) == 1

    if (type == "c") { # character
        valid <- valid & is.character(argvalue) &
                !admisc::possibleNumeric(argvalue)
    }

    if (!valid) {
        admisc::stopError(
            sprintf(
                "Argument '%s' should be a%s vector of length 1",
                argname,
                ifelse(type == "c", " character", "")
            )
        )
    }
}


#' @description Check the three dots ... argument
#' @details If the argument is not supplied via dots, returns a default
#' @noRd
`checkDots` <- function(dots, argname, default, type = "c") {
    if (is.element(argname, names(dots))) {
        dotsvalue <- dots[[argname]]
        checkArgument(dotsvalue, argname, type = type)
        # if the argument is alright (atomic vector of length 1), ok
        return(dotsvalue)
    }

    # if not, return the supplied default
    return(default)
}


#' @description Check if an element is a DDI Codebook element
#' @return Boolean.
#' @noRd
`checkElement` <- function(x) {
    if (is.null(x) || !is.atomic(x) || !is.character(x)) {
        admisc::stopError("'x' should be an atomic character input.")
    }

    if (!is.element(x, names(DDIC))) {
        admisc::stopError("This element is not part of the DDI Codebook list")
    }
}


#' @description Determine the variable type: categorical, numerical or mixed
#' @return A character scalar
#' @noRd
`checkType` <- function(x, labels = NULL, na_values = NULL, na_range = NULL) {

    xnumeric <- admisc::possibleNumeric(x)
    x <- declared::undeclare(x, drop = TRUE)
    uniquevals <- unique(x)
    all_nas <- declared:::all_missing_values(
        x = unclass(x),
        labels = labels,
        na_values = na_values,
        na_range = na_range
    )

    if (length(labels) > 0) {

        # possibly a categorical variable
        # but even numeric variables can have labels (for missing values)
        # check the unique values without the missing ones
        except_na <- setdiff(uniquevals, all_nas)

        if (all(is.element(labels, all_nas))) {
            if (xnumeric) {
                if (length(except_na) < 15) {
                    return("numcat")
                }
                return("num")
            }
            else {
                return("char")
            }
        }


        # verify if the number of unique values is (at most) equal to the number of labels

        if (all(is.element(except_na, labels))) {
            # surely a categorical variable, all values are labelled
            if (admisc::possibleNumeric(labels)) {
                return("cat")
            }
            return("catchar")
        }

        # some values do not have labels, possibly a ordinal variable (e.g. 1...7)
        # with only two labels (for 1 and for 7) or maybe a categorical variable
        # for which not all values are labeled

        # thinking of the smallest ordinal scale 1...7 that can be interpreted
        # as numeric
        return(ifelse(length(except_na) < 7, "cat", "catnum"))

        # TODO: what if a variable has very many numerical values (>15) but only
        # one or two labels?
        # this should be a coding mistake, should it trigger an error or a warning?
    }

    if (xnumeric) {
        # pure numerical variable with no labels at all
        if (length(uniquevals) < 15) {
            return("numcat")
        }
        else {
            return("num")
        }
    }

    return("char")
}


#' @description Rectify texts read from a metadata object
#' @return A character vector
#' @noRd
`cleanup` <- function(x, cdata = TRUE) {

    if (is.null(x)) return(NULL)

    x <- gsub("&amp;", "&", x)
    x <- gsub("&lt;", "<", x)
    x <- gsub("&gt;", ">", x)
    x <- gsub("^[[:space:]]+|[[:space:]]+$", "", x)
    x <- gsub("\"", "'", x)

    # replace backslash with a forward slash
    x <- gsub("\\\\", "/", x)

    if (cdata) {
        x <- gsub("<\\!\\[CDATA\\[|\\]\\]>", "", x)
    }

    x <- replaceTicks(x)
    return(x)
}


#' @description Collect metadata from a file or a dataframe object
#' @return A list containing variable level metadata information
#' @noRd
`collectMetadata` <- function(dataset, ...) {
    dots <- list(...)

    if (is.data.frame(dataset)) {
        error <- TRUE
        i <- 1
        while (i <= ncol(dataset) & error) {
            attrx <- attributes(dataset[[i]])
            if (any(is.element(
                c("label", "labels", "na_value", "na_range"),
                names(attrx)
            ))) {
                error <- FALSE
            }
            i <- i + 1
        }

        if (error && !isFALSE(dots$error_null)) {
            admisc::stopError(
                "The input does not seem to contain any metadata."
            )
        }
    }
    else {
        admisc::stopError(
            "The input should be a dataframe containing labelled variables."
        )
    }

    result <- lapply(dataset, function(x) {
        result <- list()

        label <- attr(x, "label", exact = TRUE)
        if (!is.null(label)) {
            result[["labl"]] <- cleanup(label)
        }

        values <- lbls <- attr(x, "labels", exact = TRUE)
        na_values <- attr(x, "na_values", exact = TRUE)
        na_range <- attr(x, "na_range", exact = TRUE)
        haslabels <- length(values) > 0
        tagged_values <- FALSE
        catgry <- NULL

        if (haslabels) {
            tagged_values <- haven::is_tagged_na(values)
        }

        if (is.null(na_values)) {
            tagged_x <- haven::is_tagged_na(x)
            if (any(tagged_values) | any(tagged_x)) {
                natags <- unique(haven::na_tag(c(unclass(x), unclass(lbls))))
                natags <- natags[!is.na(natags)]
                if (length(natags) > 0) {
                    na_values <- sort(natags)
                }
            }
        }
        else {
            # it should't have NA values, but just in case
            na_values <- na_values[!is.na(na_values)]
        }

        if (is.factor(x)) {
            haslabels <- TRUE
            xlevels <- levels(x)
            values <- setNames(seq_along(xlevels), xlevels)
            x <- as.numeric(x)
        }

        if (haslabels) {
            ismiss <- is.element(values, na_values)
            if (length(na_range) > 0) {
                ismiss <- ismiss | (values >= na_range[1] & values <= na_range[2])
            }

            labels <- cleanup(names(values))
            values <- unname(values)
            if (is.character(values)) {
                values <- cleanup(values)
            }

            catgry <- lapply(seq_along(values), function(i) {
                icatgry <- list(
                    labl = labels[i],
                    catValu = values[i]
                )

                if (ismiss[i]) {
                    # attr(icatgry, "missing") <- "Y"
                    icatgry[["missing"]] <- "Y"
                }

                return(icatgry)
            })

            names(catgry) <- values
            names(values) <- labels
        }

        if (length(na_range) > 0) {
            result[["invalrng"]] <- list(
                range = list(
                    min = na_range[1],
                    max = na_range[2]
                )
            )
        }

        result[["catgry"]] <- catgry
        result[["labels"]] <- values
        result[["na_values"]] <- na_values
        result[["na_range"]] <- na_range
        result[["type"]] <- checkType(x, values, na_values, na_range)
        result[["measurement"]] <- cleanup(attr(x, "measurement", exact = TRUE))

        format.spss <- attr(x, "format.spss", exact = TRUE)
        if (is.null(format.spss)) {
            format.spss <- getFormat(x, type = "SPSS")
        }

        format.stata <- attr(x, "format.stata", exact = TRUE)
        if (is.null(format.stata)) {
            format.stata <- getFormat(x, type = "Stata")
        }

        result[["varFormat"]] <- c(format.spss, format.stata)

        return(result)
    })

    return(result)
    attr(result, "element_name") <- name
}


#' @description Format an example from the DDI Codebook specification
#' @return Nothing
#' @noRd
`formatExample` <- function(xml_node, level = 0, indent = 2) {
    prespace <- repeatSpace(level, indent = indent)
    element <- paste0("<", xml2::xml_name(xml_node))
    attrs <- unlist(xml2::xml_attrs(xml_node))

    width <- getOption("width")

    if (length(attrs) > 0) {
        element <- paste(
            element,
            paste(
                paste0(names(attrs), "=\"", attrs, "\""),
                collapse = " "
            )
        )
    }

    children <- xml2::xml_children(xml_node)

    if (length(children) > 0) {
        element <- paste0(element, ">")
        cat(paste(
            strwrap(
                element,
                width = width,
                prefix = prespace
            ),
            collapse = "\n"
        ))
        cat("\n")
        for (child in children) {
            formatExample(child, level + 1)
            xml2::xml_remove(child)
        }
    }

    text <- xml2::xml_text(xml_node)
    if (nzchar(text)) {
        if (length(children) == 0) {
            element <- paste0(element, ">")
            cat(paste(
                strwrap(
                    element,
                    width = width,
                    prefix = prespace
                ),
                collapse = "\n"
            ))
            cat("\n")
        }
        prespaceplus <- repeatSpace(level + 1, indent = indent)
        writeLines(strwrap(
            text,
            width = width,
            prefix = prespaceplus
        ))
    }

    if (text == "" & length(children) == 0) {
        element <- paste0(element, "/>")
        cat(paste(
            strwrap(
                element,
                width = width,
                prefix = prespace
            ),
            collapse = "\n"
        ))
    }
    else {
        cat(strwrap(
            paste0("</", xml2::xml_name(xml_node), ">"),
            width = width,
            prefix = prespace
        ))
    }
    cat("\n")
}


#' @description Generate simple, custom unique ID codes
#' @return Character vector
#' @param x Number of ID values to return
#' @param nchars Number of characters for each ID
#' @noRd
`generateID` <- function(x, nchars = 16) {
    toreturn <- rep(NA, x)
    # the first character in the ID is _always_ a letter
    first <- sample(c(LETTERS, letters), x, replace = TRUE)
    for (i in seq(x)) {
        toreturn[i] <- paste(
            c(
                first[i],
                sample(c(LETTERS, letters, 0:9), nchars - 1, replace = TRUE)
            ),
            collapse = ""
        )
    }
    return(toreturn)
    # a safer way is to use unique() but it is highly unlikely this would be needed
    # toreturn <- unique(toreturn)
    # if (length(toreturn) == x) return(toreturn)
}


#' @description Guess the column delimiter from a text file
#' @return Character scalar
#' @noRd
`getDelimiter` <- function(x) {

    delimiter <- ","

    csvreadfile <- read.csv(x, as.is = TRUE)

    # if the delimiter is not a comma, there will be only one big column
    if (ncol(csvreadfile) == 1) { # try ";" separated
        delimiter <- ";"
        csvreadfile <- read.csv(x, sep = ";", as.is = TRUE)
    }

    # if still the delimiter is not the right one
    if (ncol(csvreadfile) == 1) { # try tab separated
        delimiter <- "\t"
        csvreadfile <- read.csv(x, sep = "\t", as.is = TRUE)
    }

    # finally, if it's still not the right delimiter stop and print an error message
    if (ncol(csvreadfile) == 1) {
        return("unknown")
    }

    return(delimiter)
}


#' @description Get the carriage return code from the current Operating System
#' @return Character scalar
#' @noRd
`getEnter` <- function(OS) {

    current_os <- Sys.info()[["sysname"]]
    target_os <- toupper(OS)

    if (target_os == "WINDOWS" | target_os == "WIN") {
        enter <- ifelse(current_os == "Windows", "\n", "\r\n")
    }
    else if (target_os == "LINUX") {
        enter <- "\n"
    }
    else if (
        target_os == "DARWIN" |
        target_os == "MACOS" |
        target_os == "APPLE" |
        target_os == "MAC"
    ) {
        enter <- ifelse(current_os == "Darwin", "\n", "\r")
    }
    else {
        admisc::stopError("The specified OS is not supported.")
    }

    return(enter)
}


#' @description Get information about the files in a given directory
#' @return A list with four components: the complete path, the files, the file names and the file extensions
#' @noRd
`getFiles` <- function(path = ".", type = "*", currdir) {

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


#' @description Determine the SPSS / Stata variable format
#' @return Character scalar
#' @noRd
`getFormat` <- function(x, type = c("SPSS", "Stata"), ...) {

    dots <- list(...)
    type <- toupper(match.arg(type))

    labels <- dots[["labels"]]
    if (is.null(labels) && (haven::is.labelled(x) | declared::is.declared(x))) {
        labels <- attr(x, "labels", exact = TRUE)
    }

    attributes(x) <- NULL
    attributes(labels) <- NULL

    pN <- TRUE
    allnax <- all(is.na(x))
    nullabels <- is.null(labels)
    if (!(allnax & nullabels)) {
        pN <- admisc::possibleNumeric(c(x, labels))
    }

    decimals <- 0
    if (pN & !allnax) {
        decimals <- min(3, admisc::numdec(x))
    }

    maxvarchar <- 0
    if (!allnax) {
        nofchars <- na.omit(nchar(x, allowNA = TRUE))

        if (length(nofchars) > 0) {
            maxvarchar <- max(nofchars, na.rm = TRUE)
        }
    }

    if (!nullabels & !pN) {
        maxvarchar <- max(maxvarchar, na.omit(nchar(labels, allowNA = TRUE)))
    }

    if (type == "SPSS") {
        return(
            sprintf(
                "%s%s%s%s",
                ifelse(pN, "F", "A"),
                max(1, maxvarchar),
                ifelse(pN, ".", ""),
                ifelse(pN, decimals, "")
            )
        )
    }
    else if (type == "STATA") {
        return(
            paste0("%",
                sprintf(
                    "%s%s%s%s",
                    max(1, maxvarchar),
                    ifelse(pN, ".", ""),
                    ifelse(pN, decimals, ""),
                    ifelse(pN, "g", "s")
                )
            )
        )
    }
}


#' @description Read the DDI XML file
#' @return An XML document
#' @noRd
`getXML` <- function(path) {
    tc <- admisc::tryCatchWEM(xml <- xml2::read_xml(path))

    if (is.null(tc$error)) {
        return(xml)
    }
    else {
        # xml <- readLines(path)
        # nms <- grepl("xmlns", xml[which(grepl("codeBook", xml))[1]])

        admisc::stopError("Unable to read the XML file.")
    }
}


#' @description Check if variables have missing labels
#' @return Boolean vector
#' @noRd
`hasMissingLabels` <- function(variables) {
    lapply(variables, function(x) {
        if (!is.element("labels", names(x))) return(FALSE)

        labels <- x[["labels"]]
        ismiss <- is.element(labels, x[["na_values"]])

        if (is.element("na_range", names(x)) && admisc::possibleNumeric(labels)) {
            na_range <- x[["na_range"]]
            labels <- as.numeric(labels)
            ismiss <- ismiss | (
                labels >= na_range[1] & labels <= na_range[2]
            )
        }

        return(ismiss)
    })
}


#' @description Coerce variables to labelled objects
#' @return A modified data frame.
#' @noRd
`makeLabelled` <- function(x, variables, declared = TRUE) {

    for (i in names(x)) {
        #------------------------------------------------------------------
        # attrx$label, if not existing, takes from attrx$labels
        # attrx[["label"]] is something like attr(x, "label", exact = TRUE)
        label <- variables[[i]][["labl"]]
        labels <- variables[[i]][["labels"]]
        #------------------------------------------------------------------

        na_values <- variables[[i]][["na_values"]]
        na_range <- variables[[i]][["na_range"]]
        measurement <- variables[[i]][["measurement"]]

        v <- x[[i]]
        attributes(v) <- NULL

        pN <- TRUE
        allnav <- all(is.na(v))
        nullabels <- is.null(labels)
        if (!(allnav & nullabels)) {
            pN <- admisc::possibleNumeric(c(v, unname(labels)))
        }

        if (pN) {
            v <- admisc::asNumeric(v)
        }
        else {
            v <- as.character(v)
            na_range <- NULL
        }

        if (!nullabels) {
            nms <- names(labels)
            if (pN) {
                labels <- setNames(admisc::asNumeric(labels), nms)
            }
            else {
                labels <- setNames(as.character(labels), nms)
            }
            # names(labels) <- nms
        }

        if (!is.null(na_values)) {
            if (admisc::possibleNumeric(na_values) & pN) {
                na_values <- admisc::asNumeric(na_values)
            }
            else {
                na_values <- as.character(na_values)
            }
        }

        if (all(sapply(list(labels, na_values, na_range, label), is.null))) {
            x[[i]] <- v
        } else {
            if (declared) {
                x[[i]] <- declared::declared(v, labels, na_values, na_range, label, measurement)
            }
            else {
                x[[i]] <- haven::labelled_spss(v, labels, na_values, na_range, label)
            }
        }



        # this is always about format.spss since both "declared" and "labelled_spss"
        # are not using Stata type extended missing values, and by consequence
        # not using the Stata format type
        attr(x[[i]], "format.spss") <- variables[[i]][["varFormat"]][1]

    }

    x[] <- lapply(x, function(x) {
        if (is.null(attr(x, "format.spss"))) {
            attr(x, "format.spss") <- getFormat(x, type = "SPSS")
        }
        return(x)
    })

    if (!declared) {
        class(x) <- c("tbl_df", "tbl", "data.frame")
    }

    return(x)
}


#' @description Prepares the missing values for the SPSS export syntax.
#' @return A vector of missing values representation.
#' @noRd
# for recoding into SPSS
`missingValuesSPSS` <- function(variables, range = FALSE, numvars = TRUE) {
    lapply(variables, function(x) {
        na_values <- NULL
        if (is.element("na_values", names(x))) {
            na_values <- sort(x[["na_values"]])
        }
        values <- x[["labels"]]
        na_range <- x[["na_range"]]
        if (length(na_range) > 0) {
            if (range) {
                if (na_range[1] == -Inf) na_range[1] <- "LO"
                if (na_range[2] == Inf) na_range[1] <- "HI"
                na_values <- c(
                    na_values,
                    paste(
                        na_range,
                        collapse = " THRU "
                    )
                )
            }
            else if (admisc::possibleNumeric(values)) {
                values <- as.numeric(values)
                na_values <- c(
                    na_values,
                    values[values >= na_range[1] & values <= na_range[2]]
                )
            }
        }
        return(na_values)
    })
}


#' @description Allows indentation of XML or HTML files
#' @return Character spaces
#' @noRd
`repeatSpace` <- function(times, indent) {
    paste(rep(" ", times*indent), collapse = "")
}


#' @description Replace certain characters, in preparation for XML export
#' @return Character vector
#' @noRd
`replaceChars` <- function(x) {
    x <- replaceTicks(x)
    x <- gsub(
        "<", "&lt;",
        gsub(
            ">", "&gt;",
            gsub(
                "&", "&amp;",
                x
            )
        )
    )

    tc <- admisc::tryCatchWEM({
        # use case: StatConverter in Electron, the terminal that opens R
        # probably doesn't have a suitable locale and it outputs an error
        # this does the same thing (using hexadecimal code) and is better

        # weird space character: gsub(rawToChar(as.raw(c(194, 160))), " ", x)
        x <- gsub("\u00a0", " ", x)
    })

    return(x)
}


#' @description Recode all tick characters with a single quote.
#' @return A recoded string.
#' @noRd
`replaceTicks` <- function(x) {

    # # weird A character sometimes from encoding a single tick quote
    # "\u00c2"
    # # forward and back ticks
    # c("\u00b4", "\u0060")

    tc <- admisc::tryCatchWEM({
        # use case: StatConverter in Electron, the terminal that opens R
        # probably doesn't have a suitable locale and it outputs an error
        # this does the same thing (using hexadecimal code) and is better
        achar <- "\u00c2"
        tick <- c("\u00b4", "\u0060")

        tick <- c(paste0(achar, "'"), paste0(achar, tick), tick)
        x <- gsub(paste(tick, collapse = "|"), "'", x)
    })

    return(x)
}


#' @description Split the written rows in the setup file.
#' @return A character vector.
#' @noRd
`splitrows` <- function(x, enter, y = 80, spacerep = "") {
    n <- x[1]
    command <- paste(toupper(x), collapse=", ")
    if (nchar(command) > y) {
        command <- precommand <- n
        for (ii in seq(2, length(x))) {
            if (nchar(precommand) > y) {
                precommand <- paste(toupper(x[ii]), ", ", sep = "")
                command <- paste(
                    command,
                    ",",
                    enter,
                    spacerep,
                    toupper(x[ii]),
                    sep = ""
                )
            }
            else {
                precommand <- paste(precommand, toupper(x[ii]), sep = ", ")
                command <- paste(command, toupper(x[ii]), sep = ", ")
            }

        }
    }
    return(command)
}


#' @description Determine which specific type of files are present in a certain
#' directory.
#' @return A list with four components: the complete path, the files, the file
#' names and the file extensions
#' @noRd
`treatPath` <- function(path, type = "*", single = FALSE, check = TRUE) {
    if (length(path) > 1) {
        # if (type == "R") {
        #     admisc::stopError("The <codeBook> argument should contain a single path to the list object.")
        # }
        if (type == "csv") {
            admisc::stopError(
                "The argument <csv> should contain a single path to the .csv file."
            )
        }
    }

    if (!is.character(path)) {
        admisc::stopError("A path should be specified in a string.")
    }
    currdir <- getwd()


    lastpart <- basename(path)
    # normalizePath() deals with the symbolic links, relative paths and
    # absolute paths
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
                        "Please check that path, or try changing the",
                        "working directory.",
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

        if (length(filesplit) > 2) {
            # multiple dots inside the file name
            filesplit <- c(
                paste(filesplit[-length(filesplit)], collapse = "."),
                filesplit[length(filesplit)]
            )
        }
        else if (length(filesplit) == 2) {
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
                admisc::stopError(
                    "A file name should be provided, not a directory."
                )
            }

            fileobj <- getFiles(
                path = file.path(pathname, lastpart),
                type = type
            )
        }
        else {

            if (
                type != "*" &&
                toupper(type) != toupper(
                    tools::file_ext(file.path(pathname, lastpart))
                )
            ) {
                return(paste0("Wrong file type, it should be ", type, "."))
            }

            fileobj <- getFiles(
                path = file.path(pathname, lastpart),
                type = type
            )
        }

    }

    return(fileobj)
}


#' @description Utility function to write the metadata part in the setup file.
#' @return Nothing.
#' @noRd
`writeMetadata` <- function(variables, OS = "", indent = 4) {
    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }
    enter <- getEnter(OS = OS)

    rs <- function(x) {
        paste(rep(" ", x * indent), collapse="")
    }

    #---------------------------------------------------------------------------
    # TODO: to verify if this is still needed
    if (is.element("dataDscr", names(variables))) {
        variables <- variables$dataDscr
    }

    if (is.element("var", names(variables))) {
        variables <- variables$var
    }
    #---------------------------------------------------------------------------

    for (i in seq(length(variables))) {
        if (is.element("labels", names(variables[[i]]))) {

            values <- variables[[i]][["labels"]]
            labels <- names(values)
            notNum <- any(is.na(suppressWarnings(as.numeric(values))))
            quote <- ifelse(notNum, "\"", "")

            valstring <- paste(
                paste("\"", labels, "\"", sep = ""),
                paste(quote, values, quote, sep = ""),
                sep = " = ",
                collapse = ",\n               "
            )

            cat(
                "rdatafile[[\"",
                names(variables)[i],
                "\"]] <- declared(rdatafile[[\"",
                names(variables)[i], "\"]],",
                enter,
                sep = ""
            )
            cat(paste0(rs(1), "labels = c(", valstring, "),", enter))

            if (is.element("na_values", names(variables[[i]]))) {
                cat(
                    paste0(
                        rs(1),
                        "na_values = c(",
                        paste(
                            quote,
                            variables[[i]]$na_values,
                            quote,
                            sep = "",
                            collapse = ", "
                        ),
                        "),",
                        enter
                    )
                )
            }

            if (is.element("na_range", names(variables[[i]]))) {
                cat(
                    paste0(
                        rs(1),
                        "na_range = c(",
                        paste(
                            quote,
                            variables[[i]]$na_range,
                            quote,
                            sep = "",
                            collapse = ", "
                        ),
                        "),",
                        enter
                    )
                )
            }

            cat(rs(1), "label = \"", variables[[i]][["label"]], "\"", sep = "")
            cat(paste0(enter, ")", enter, enter, enter))

        }
    }
}

##------------------------------------------------------------------------------
## TODO: modify this function according to DDIC

#' @description Write an .R file containing a metadata specific list.
#' @return Nothing.
#' @noRd
`writeRlist` <- function(
    variables, OS = "windows", indent = 4, dirpath = "", filename = ""
) {

    on.exit(suppressWarnings(sink()))

    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }
    enter <- getEnter(OS=OS)

    currentdir <- getwd()
    setwd(dirpath)

    sink(sprintf("%s.R", filename))
    cat("codeBook <- list(variables = list(var = list(", enter)

    rs <- function(x) {
        paste(rep(" ", x * indent), collapse="")
    }

    #---------------------------------------------------------------------------
    # TODO: to verify if this is still needed
    if (is.element("dataDscr", names(variables))) {
        variables <- variables$variables
    }

    if (is.element("var", names(variables))) {
        variables <- variables$var
    }
    #---------------------------------------------------------------------------

    for (i in seq(length(variables))) {

        cat(names(variables)[i], " = list(", enter, sep = "")

        cat(rs(1), "label = \"", variables[[i]][["label"]], "\"", sep = "")

        if (is.element("labels", names(variables[[i]]))) {
            cat(",", enter, rs(1), "labels = c(", enter, sep = "")

            values <- variables[[i]][["labels"]]
            names(values) <- cleanup(names(values))
            notNum <- any(is.na(suppressWarnings(as.numeric(values))))
            labl <- names(values)

            for (lbl in seq(length(values))) {
                cat(rs(2), "\"", labl[lbl], "\" = ", sep = "")
                quote <- ifelse(notNum, "\"", "")
                cat(quote, values[lbl], quote, sep = "")
                cat(
                    ifelse(
                        lbl < length(labl),
                        paste(",", enter, sep = ""),
                        paste(enter, rs(2), ")", sep = "")
                    )
                )
            }
        }

        if (is.element("na_values", names(variables[[i]]))) {
            na_values <- variables[[i]]$na_values
            notNum <- any(is.na(suppressWarnings(as.numeric(na_values))))
            cat(",", enter, sep = "")
            cat(
                rs(1),
                "na_values = ",
                ifelse(
                    length(na_values) > 1,
                    paste(
                        "c(",
                        paste(
                            na_values,
                            collapse = ifelse(
                                notNum,
                                "\", \"",
                                ", "
                            )
                        ),
                        ")",
                        sep = ""
                    ),
                    ifelse(
                        notNum,
                        paste("\"", na_values, "\"", sep = ""),
                        na_values
                    )
                ),
                sep = ""
            )
        }

        if (is.element("na_range", names(variables[[i]]))) {
            na_range <- variables[[i]]$na_range
            cat(",", enter, sep = "")
            cat(
                rs(1),
                "na_range = c(",
                paste(na_range, collapse = ", "),
                ")",
                sep = ""
            )
        }

        if (is.element("type", names(variables[[i]]))) {
            cat(",", enter, sep = "")
            cat(rs(1), "type = \"", variables[[i]]$type, "\"", sep = "")
        }

        if (is.element("measurement", names(variables[[i]]))) {
            cat(",", enter, sep = "")
            cat(
                rs(1),
                "measurement = \"",
                variables[[i]]$measurement,
                "\"",
                sep = ""
            )
        }

        # if (attr) {
        #     # close the variable specific list
        #     cat(enter, ")", enter, enter, sep = "")
        # }
        # else {
            cat(
                enter,
                ifelse(
                    i == length(variables),
                    ")",
                    "),"
                ),
                enter,
                sep = ""
            )
        # }
    }

    cat("))", enter)
    sink()
    setwd(currentdir)
}
##------------------------------------------------------------------------------








#### TODO: delete once the main writeXML function will be made

#' @description Write the study description part of the DDI XML file.
#' @return A character vector.
#' @noRd
`makeXML` <- function(x, space = 1, indent = 4, ns = "", enter = "\n") {
    sx <- paste(rep(" ", space*indent), collapse = "")
    result <- character(0)
    nmsx <- names(x)
    if (!is.null(nmsx)) {
        for (n in seq(length(nmsx))) {
            childnms <- names(x[[nmsx[n]]])
            attrx <- attributes(x[[nmsx[n]]])
            attrx$names <- NULL
            start <- paste(sx, "<", ns, nmsx[n], sep = "")

            if (length(attrx) > 0) {
                nms <- names(attrx)
                for (i in seq(length(attrx))) {
                    if (nms[i] == "lang") {
                        nms[i] <- "xml:lang"
                    }
                    start <- paste(
                        start,
                        paste(
                            nms[i],
                            paste("\"", attrx[[i]], "\"", sep = ""),
                            sep = "="
                        )
                    )
                }
            }

            start <- paste(
                start,
                ">",
                ifelse(is.null(childnms), "", enter),
                sep = ""
            )

            end <- paste(
                ifelse(is.null(childnms), "", sx),
                "</",
                ns,
                nmsx[n],
                ">",
                enter,
                sep = ""
            )

            result <- c(
                result,
                start,
                makeXML(x[[n]], space + 1),
                end
            )
        }
    }
    else {
        result <- c(result, x)
    }

    return(unlist(result))
}
