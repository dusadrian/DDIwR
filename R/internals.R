#' @title DDIwR internal functions
#' @description Functions to be used internally, only by developers and
#' contributors.
#' @name DDIwR_internal
#' @aliases dfm
NULL

#' @description `appendInfo`: Append information at variable level about
#' labels: `na_values`, `na_range`, `measurement`, `type`
#' etc. (taken from the XML file)
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`appendInfo` <- function(codeBook, data) {
    wdd <- indexChildren(codeBook, "dataDscr")

    if (length(wdd) == 0) {
        admisc::stopError("The codeBook does not contain any data description.")
    }

    if (length(wdd) > 1) {
        admisc::stopError("More than one data description found.")
    }

    found <- FALSE

    # in case there is more than one data description element
    for (i in seq_along(wdd)) {

        # children that are "var"(iables)
        wvar <- indexChildren(codeBook$children[[wdd[i]]], "var")

        # there snould be exactly as many "var" children as data columns!
        found <- length(wvar) == ncol(data)
        if (found) {
            variables <- codeBook$children[[wdd[i]]]$children[wvar]
            break;
        }
    }

    if (!found) {
        admisc::stopError("The codebook does not match the data.")
    }

    dnms <- names(data)
    for (v in seq_along(data)) {
        vattrs <- attributes(data[[v]])
        # the name of the variable is placed in the attributes of the
        # DDI <var> element, let's put it here as well, for convenience
        variables[[v]][["varname"]] <- dnms[v]

        # along with all other information
        variables[[v]][["label"]] <- getElement(vattrs, "label")
        variables[[v]][["labels"]] <- getElement(vattrs, "labels")
        variables[[v]][["na_values"]] <- getElement(vattrs, "na_values")
        variables[[v]][["na_range"]] <- getElement(vattrs, "na_range")
        variables[[v]][["type"]] <- checkType(getElement(data, v))

    }

    codeBook$children[[wdd]]$children[wvar] <- variables
    return(codeBook)
}


#' @description `checkArgument`: Check function arguments
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`checkArgument` <- function(argument, default, ...) {
    dots <- list(...)
    argname <- dots$argname
    if (is.null(argname)) {
        argname <- deparse(substitute(argument))
    }

    valid <- !is.null(argument) &&
            is.atomic(argument) &&
            length(argument) == 1

    type <- ""
    if (is.character(default)) {
        type <- " character"
        valid <- valid & is.character(argument)
    }
    else if (is.numeric(default)) {
        type <- " numeric"
        valid <- valid & is.numeric(argument)
    }

    if (!valid) {
        admisc::stopError(
            sprintf(
                "Argument '%s' should be a%s vector of length 1",
                argname,
                type
            )
        )
    }
}

#' @description `checkDots`: Check the three dots ... argument.
#' If the argument is not supplied via dots, returns a default
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`checkDots` <- function(dotsvalue, default) {
    argname <- admisc::getName(deparse(substitute(dotsvalue)))
    if (is.null(dotsvalue)) {
        return(default)
    }

    checkArgument(dotsvalue, default, argname = argname)
    return(dotsvalue)
}


#' @description `checkElement`: Check if an element is a DDI Codebook element
#' @return `checkElement`: Boolean.
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`checkElement` <- function(x) {
    if (is.null(x) || !is.atomic(x) || !is.character(x)) {
        admisc::stopError("'x' should be an atomic character input.")
    }

    if (!is.element(x, names(DDIC))) {
        admisc::stopError("This element is not part of the DDI Codebook list")
    }
}


#' @description `checkExisting`: Check if a certain (sub)element exists in a DDI Codebook.
#' @return `checkExisting`: Boolean.
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`checkExisting` <- function(xpath, inside, attribute = NULL) {

    if (
        missing(xpath) || is.null(xpath) || !is.atomic(xpath) ||
        !is.character(xpath)
    ) {
        admisc::stopError(
            "Argument 'xpath' should be a character vector."
        )
    }

    if (length(xpath) == 1) {
        xpath <- unlist(strsplit(xpath, split = "/"))
    }

    xpath <- setdiff(xpath, inside$name)

    if (hasChildren(inside, xpath[1])) {
        index <- indexChildren(inside, xpath[1])

        return(all(sapply(
            index,
            function(x) {
                if (length(xpath) > 1) {
                    return(checkExisting(xpath, inside$children[[x]], attribute))
                }

                return(ifelse(
                    is.null(attribute),
                    TRUE,
                    hasAttributes(inside$children[[x]], attribute)
                ))
            }
        )))
    }

    return(FALSE)
}


#' @description `checkvarFormat`: Makes sure all `var` elements have a single
#' (usually SPSS) value for the `varFormat` sub-element (if existing).
#' @return `checkvarFormat`: A potentially modified `codeBook` element.
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`checkvarFormat` <- function(codeBook) {
    variables <- getVariables(codeBook, name = FALSE)

    if (length(variables) > 0) {
        variables <- lapply(variables, function(x) {
            varFormat <- getChildren("varFormat", from = x, skiperror = TRUE)
            if (length(varFormat) > 0 && length(varFormat$content) > 0) {
                varFormat$content <- varFormat$content[1]
                removeChildren("varFormat", from = x)
                addChildren(varFormat, to = x)
            }
            return(x)
        })

        removeChildren("var", from = codeBook$children$dataDscr)
        addChildren(variables, to = codeBook$children$dataDscr)
    }

    return(codeBook)
}


#' @description `checkType`: Determine the variable type: categorical, numerical or mixed
#' @return `checkType`: A character scalar
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`checkType` <- function(x, labels = NULL, na_values = NULL, na_range = NULL) {

    xnumeric <- admisc::possibleNumeric(x)
    metadata <- inherits(x, "declared") | inherits(x, "haven_labelled_spss")
    if (metadata) {
        if (is.null(labels)) {
            labels <- attr(x, "labels", exact = TRUE)
        }

        if (is.null(na_values)) {
            na_values <- attr(x, "na_values")
        }

        if (is.null(na_range)) {
            na_range <- attr(x, "na_range")
        }
    }

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


#' @description `checkXMList`: Determine if an XML list is a DDI Codebook
#' @rdname DDIwR_internal
#' @export
`checkXMList` <- function(xmlist) {
    nms <- c()
    extractNames <- function(x) {
        if (is.list(x)) {
            nms <<- unique(c(nms, names(x)))
            eN <- lapply(x, extractNames)
        }
    }
    extractNames(xmlist)

    if (!all(is.element(setdiff(nms, ""), names(DDIC)))) {
        admisc::stopError(
            "This XML file contains elements that are not part of the DDI Codebook standard."
        )
    }
}


#' @description `cleanup`: Rectify texts read from a metadata object
#' @return `cleanup`: A character vector
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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


#' @description `collectMetadata`: Collect metadata from a file or a dataframe object
#' @return `collectMetadata`: A list containing variable level metadata information
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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
                paste(
                    "The input does not seem to contain any",
                    "metadata about values and labels."
                )
            )
        }
    }
    else {
        admisc::stopError(
            "The input should be a dataframe containing labelled variables."
        )
    }

    result <- lapply(names(dataset), function(colname) {
        x <- dataset[[colname]]
        variable <- makeElement("var")
        label <- cleanup(attr(x, "label", exact = TRUE))

        if (!is.null(label)) {
            addChildren(
                makeElement("labl", list(content = label)),
                to = variable
            )
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
                icatgry <- makeElement("catgry")

                if (ismiss[i]) {
                    addAttributes(list(missing = "Y"), to = icatgry)
                }

                addChildren(
                    list(
                        makeElement("labl", list(content = labels[i])),
                        makeElement("catValu", list(content = values[i]))
                    ),
                    to = icatgry
                )

                return(icatgry)
            })

            addChildren(catgry, to = variable)

            names(values) <- labels
        }

        if (length(na_range) > 0) {
            invalrng <- makeElement("invalrng")
            range <- makeElement("range")
            addAttributes(
                list(min = na_range[1], max = na_range[2]),
                to = range
            )

            addChildren(range, to = invalrng)
            addChildren(invalrng, to = variable)
        }

        addAttributes(list(name = colname), to = variable)

        variable[["varname"]] <- colname
        variable[["label"]] <- label
        variable[["labels"]] <- values
        variable[["na_values"]] <- na_values
        variable[["na_range"]] <- na_range
        variable[["type"]] <- checkType(x, values, na_values, na_range)
        variable[["measurement"]] <- cleanup(
            attr(x, "measurement", exact = TRUE)
        )

        spss <- attr(x, "format.spss", exact = TRUE)
        if (is.null(spss)) {
            spss <- getFormat(x, type = "SPSS")
        }

        stata <- attr(x, "format.stata", exact = TRUE)
        if (is.null(stata)) {
            stata <- getFormat(x, type = "Stata")
        }

        addChildren(
            makeElement("varFormat", list(content = c(spss, stata))),
            to = variable
        )

        return(variable)
    })

    names(result) <- names(dataset)

    return(result)
}


#' @description Collect metadata from a dataframe object
#' @return An R list containing variable level metadata information
#' @noRd
`collectRMetadata` <- function(dataset, ...) {
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
                paste(
                    "The input does not seem to contain any",
                    "metadata about values and labels."
                )
            )
        }
    }
    else {
        admisc::stopError(
            "The input should be a dataframe containing labelled variables."
        )
    }

    output <- lapply(dataset, function(x) {
        result <- list()

        label <- attr(x, "label", exact = TRUE)
        if (!is.null(label)) {
            result[["label"]] <- cleanup(label)
        }

        measurement <- attr(x, "measurement", exact = TRUE)
        if (!is.null(measurement)) {
            result[["measurement"]] <- cleanup(measurement)
        }

        tagged <- FALSE
        labels <- lbls <- attr(x, "labels", exact = TRUE)
        if (!is.null(labels)) {
            tagged <- haven::is_tagged_na(labels)
            # if (any(tagged)) {
            #     labels[tagged] <- haven::na_tag(labels[tagged])
            # }

            nms <- names(labels)
            if (is.character(labels)) {
                labels <- cleanup(labels)
            }
            names(labels) <- cleanup(nms)
            result[["labels"]] <- labels
        }
        else if (is.factor(x)) {
            xlevels <- levels(x)
            # labels <- seq(length(xlevels))
            # names(labels) <- xlevels
            # result[["labels"]] <- labels
            result[["labels"]] <- setNames(seq(length(xlevels)), xlevels)
            x <- as.numeric(x)
        }

        na_values <- attr(x, "na_values", exact = TRUE)
        if (is.null(na_values)) {
            xtagged <- haven::is_tagged_na(x)
            if (any(tagged) | any(xtagged)) {
                natags <- unique(haven::na_tag(c(unclass(x), unclass(lbls))))
                natags <- natags[!is.na(natags)]
                if (length(natags) > 0) {
                    result$na_values <- sort(natags)
                }
            }
        }
        else {
            # it should't have (tagged) NA values, but just in case
            na_values <- na_values[!is.na(na_values)]
            if (length(na_values) > 0) {
                result$na_values <- na_values
            }
        }

        result$na_range <- attr(x, "na_range", exact = TRUE)
        result$type <- checkType(
            x,
            labels,
            na_values,
            result$na_range
        )


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

    return(output)
}


#' @description `datanotes`: Prepare a notes element containing a
#' serialized and compressed R dataset
#' @return `datanotes`: A "notes" element to be added in the `fileDscr` element
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`datanotes` <- function(data) {
    enter <- getEnter(OS = Sys.info()[['sysname']])

    notes <- makeElement("notes")
    addContent(
        paste0(
            enter,
            repeatSpace(3, indent = 2),
            base64enc::base64encode(
                memCompress(serialize(data, NULL), type = "gzip"),
                linewidth = 500,
                newline = paste(enter, repeatSpace(3, indent = 2), sep = "")
            ),
            enter,
            repeatSpace(2, indent = 2)
        ),
        to = notes
    )

    addAttributes(
        list(
            ID = "rawdata",
            level = "file",
            subject = "R dataset, serialized gzip"
        ),
        to = notes
    )

    return(notes)
}


`extractData` <- function(xml) {
    dns <- getDNS(xml) # default name space
    xpath <- sprintf("/%scodeBook/%sfileDscr/%snotes", dns, dns, dns)
    notes <- xml2::xml_find_all(xml, xpath)

    if (length(notes) > 0) {
        attrs <- lapply(notes, xml2::xml_attrs)

        wdata <- which(sapply(notes, function(x) {
            xml_attr(x, "ID") == "rawdata" &&
            grepl("serialized", xml_attr(x, "subject"))
        }))

        if (length(wdata) > 0) {

            notes <- xml2::xml_text(notes[wdata])
            # this can only be possible from an XML, DDI Codebook
            # therefore the varFormat should always be of an SPSS type
            notes <- unlist(strsplit(notes, split = "\\n")) # enter...?
            data <- paste(
                admisc::trimstr(
                    notes[seq(2, length(notes) - 1)],
                    side = "left"
                ),
                collapse = "\n"
            )

            return(unserialize(
                memDecompress(
                    base64enc::base64decode(data),
                    type = "gzip"
                )
            ))
        }
    }

    return(NULL)
}


#' @description `formatExample`: Format an example from the DDI Codebook
#' specification
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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


#' @description `generateID`: Generate simple, custom unique ID codes
#' @return `generateID`: Character vector
#' @param x Number of ID values to return
#' @param nchars Number of characters for each ID
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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


#' @description `getDateTime`: Current date and time
#' @return `getDateTime`: Character vector
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`getDateTime` <- function() {
    time <- capture.output(print(Sys.time()))
    return(
        substr(
            gsub("\"", "", time),
            5,
            nchar(time)
        )
    )
}


#' @description `getDelimiter`: Guess the column delimiter from a text file
#' @return `getDelimiter`: Character scalar
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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


#' @description `getDNS`: Extracts the "D"efault "N"ame "Space" from an XML object
#' @return `getDNS`: Character scalar
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`getDNS` <- function(xml) {
    xmlns <- xml2::xml_ns(xml)
    wns <- which(xmlns == "ddi:codebook:2_5")
    if (length(wns) == 0) {
        admisc::stopError("The XML document does not contain a DDI namespace.")
    }
    return(paste0(names(xmlns)[wns[1]], ":"))
}


#' @description `getEnter`: Get the carriage return code from the current Operating System
#' @return `getEnter`: Character scalar
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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


#' @description `getFiles`: Get information about the files in a given directory
#' @return `getFiles`: A list with four components: the complete path, the files, the file names and the file extensions
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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


#' @description `getFormat`: Determine the SPSS / Stata variable format
#' @return `getFormat`: Character scalar
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`getFormat` <- function(x, type = c("SPSS", "Stata"), ...) {

    dots <- list(...)
    type <- toupper(match.arg(type))

    labels <- getElement(dots, "labels")
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


#' @description `getValues`: Extract values, labels and missing values from a `var` element
#' @return `getValues`: A list with two components: `labels` and `na_values`
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`getValues` <- function(variable) {
    labl <- character(0)
    labels <- NULL
    na_values <- NULL
    na_range <- NULL

    cnames <- names(variable$children)
    wcatgry <- which(cnames == "catgry")
    if (length(wcatgry) > 0) {
        for (wc in wcatgry) {
            value <- variable$children[[wc]]$children$catValu$content
            labels <- c(labels, value)
            attrs <- variable$children[[wc]]$attributes
            if (!is.null(attrs)) {
                if (!is.null(attrs$missing) && attrs$missing == "Y") {
                    na_values <- c(na_values, value)
                }
            }

            labl <- c(labl, variable$children[[wc]]$children$labl$content)
        }
    }

    if (any(cnames == "invalrng")) {
        invalrng <- variable$children[[which(cnames == "invalrng")]]
        na_range <- as.numeric(c(
            invalrng$children$attributes$min,
            invalrng$children$attributes$max
        ))
    }

    if (length(labels) > 0) {
        names(labels) <- labl
    }

    return(list(
        labels = labels,
        na_values = na_values,
        na_range = na_range
    ))
}


#' @description `getVariables`: Extract the "var" elements from a `dataDscr` element
#' By default, the children "var" elements are renamed with the variables names.
#' This can be deactivated through the argument "name".
#' @return `getVariables`: A list with "var" elements, renamed with the names of the variables
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`getVariables` <- function(codeBook, name = TRUE) {
    wdd <- indexChildren(codeBook, "dataDscr")

    if (length(wdd) == 0) {
        admisc::stopError("The codeBook does not contain any data description.")
    }

    if (length(wdd) > 1) {
        admisc::stopError("More than one data description found.")
    }

    wvar <- indexChildren(codeBook$children[[wdd[1]]], "var")

    if (length(wvar) == 0) {
        admisc::stopError(
            "The data description part of this codebook does not contain any \"var\" elements."
        )
    }

    variables <- codeBook$children[[wdd]]$children[wvar]
    if (isTRUE(name)) {
        names(variables) <- sapply(variables, function(x) x$attributes$name)
    }

    return(variables)
}


#' @description `getXML`: Read the DDI XML file, after testing if it can be loaded at all.
#' @return `getXML`: An XML document
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`getXML` <- function(path) {
    tc <- admisc::tryCatchWEM(
        xml <- xml2::read_xml(path)
    )

    if (is.null(tc$error)) {
        return(xml)
    }
    else {
        admisc::stopError(
            paste(
                "Unable to read the XML file",
                tc$error,
                sep = ", "
            )
        )
    }
}


#' @description `hasLabels`: Check if a dataset has (declared) labels
#' @return `hasLabels`: Boolean
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`hasLabels` <- function(x) {
    if (!is.data.frame(x)) {
        admisc::stopError("Argument 'x' has to be a data frame")
    }

    checked <- FALSE

    for (i in seq(ncol(x))) {
        checked <- any(is.element(
            c("label", "labels"),
            names(attributes(x[[i]]))
        ))
        if (checked) {
            break;
        }
    }

    return(checked)
}


#' @description `hasMissingLabels`: Check if variables have missing labels
#' @return `hasMissingLabels`: Boolean vector
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`hasMissingLabels` <- function(variables) {
    lapply(variables, function(x) {
        if (!is.element("labels", names(x))) return(FALSE)

        labels <- getElement(x, "labels")
        ismiss <- is.element(labels, getElement(x, "na_values"))

        if (is.element("na_range", names(x)) && admisc::possibleNumeric(labels)) {
            na_range <- getElement(x, "na_range")
            labels <- as.numeric(labels)
            ismiss <- ismiss | (
                labels >= na_range[1] & labels <= na_range[2]
            )
        }

        return(ismiss)
    })
}


#' @description `makeLabelled`: Coerce variables to labelled objects
#' @return `makeLabelled`: A modified data frame.
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`makeLabelled` <- function(x, variables, declared = TRUE) {

    for (i in names(x)) {
        #------------------------------------------------------------------
        # attrx$label, if not existing, takes from attrx$labels
        # attrx[["label"]] is something like attr(x, "label", exact = TRUE)
        label <- variables[[i]][["label"]]
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

`makeXMLdataDscr` <- function(variables, indent = 2, ...) {
    dots <- list(...)
    data <- dots$data
    dataDscr <- dots$dataDscr

    xmlang <- ""
    if (isFALSE(dots$monolang)) {
        xmlang <- paste0(
            " xml:lang=\"",
            checkDots(dots$xmlang, default = "en"),
            "\""
        )
    }

    on.exit(suppressWarnings(sink()))

    if (is.null(dataDscr)) {
        return(NULL)
    }

    s0 <- repeatSpace(0, indent = indent)
    s1 <- repeatSpace(1, indent = indent)
    s2 <- repeatSpace(2, indent = indent)
    s3 <- repeatSpace(3, indent = indent)
    s4 <- repeatSpace(4, indent = indent)
    s5 <- repeatSpace(5, indent = indent)

    pN <- logical(length(dataDscr))
    if (!is.null(data)) {
        pN <- sapply(data[names(dataDscr)], function(x) {
            admisc::possibleNumeric(unclass(x))
        })

        aN <- lapply(
            subset(
                data,
                select = is.element(
                    names(data),
                    names(pN)[pN]
                )
            ),
            # data[, names(pN)[pN], drop = FALSE],
            function(x) admisc::asNumeric(unclass(x))
        )
    }

    # uuid for all variables
    uuid <- generateID(length(dataDscr))
    varnames <- names(dataDscr)

    ns <- "" # namespace
    enter <- "\n"

    tmp <- tempdir()
    sink(file.path(tmp, "dataDscr.xml"))

    cat(paste(s1, "<", ns, "dataDscr>", enter, sep = ""))

    for (i in seq(length(dataDscr))) {
        dcml <- ""
        if (!is.null(data) && pN[i]) {
            dcml <- paste0(
                " dcml=\"",
                admisc::numdec(na.omit(aN[[varnames[i]]])),
                "\""
            )
        }

        nature <- ""
        if (is.element("measurement", names(dataDscr[[i]]))) {
            nature <- paste0(
                " nature=\"",
                gsub(
                    "categorical|quantitative|, ", "",
                    dataDscr[[i]]$measurement,
                ),
                "\""
            )
        }

        cat(paste0(
            s2, "<", ns, "var ID=\"", uuid[i], "\"",
            " name=\"", varnames[i], "\"",
            # " files=\"", fileDscrUUID, "\"",
            dcml, nature, ">",
            enter
        ))

        if (!is.null(dataDscr[[i]][["label"]])) {
            if (!is.na(dataDscr[[i]][["label"]])) {
                cat(paste(
                    s3, "<", ns, "labl", xmlang, ">",
                    replaceChars(
                        dataDscr[[i]][["label"]]
                    ),
                    "</", ns, "labl>",
                    enter,
                    sep = ""
                ))
            }
        }


        na_values <- NULL
        if (is.element("na_values", names(dataDscr[[i]]))) {
            na_values <- dataDscr[[i]]$na_values
        }

        na_range <- NULL
        if (is.element("na_range", names(dataDscr[[i]]))) {
            na_range <- dataDscr[[i]]$na_range
        }


        if (length(na_range) > 0) {
            cat(paste(s3, "<", ns, "invalrng>", enter, sep = ""))

            if (any(is.element(na_range, c(-Inf, Inf)))) {
                if (identical(na_range[1], -Inf)) {
                    cat(paste(
                        s4,
                        sprintf(
                            "<%srange UNITS=\"INT\" max=\"%s\"/>",
                            ns, na_range[2]
                        ),
                        enter,
                        sep = ""
                    ))
                }
                else {
                    cat(paste(
                        s4,
                        sprintf(
                            "<%srange UNITS=\"INT\" min=\"%s\"/>",
                            ns, na_range[1]
                        ),
                        enter,
                        sep = ""
                    ))
                }
            }
            else {
                cat(paste(
                    s4,
                    sprintf(
                        "<%srange UNITS=\"INT\" min=\"%s\" max=\"%s\"/>",
                        ns, na_range[1], na_range[2]
                    ),
                    enter,
                    sep = ""
                ))
            }

            cat(paste(s3, "</", ns, "invalrng>", enter, sep = ""))
        }

        lbls <- dataDscr[[i]][["labels"]]

        type <- dataDscr[[i]]$type

        # even if the data is not present, pN is FALSE for all variables
        if (pN[i]) {
            vals <- aN[[names(dataDscr)[i]]]

            if (!is.null(lbls)) {
                ismiss <- is.element(lbls, na_values)
                if (length(na_range) > 0) {
                    ismiss <- ismiss | (lbls >= na_range[1] & lbls <= na_range[2])
                }
                vals[is.element(vals, lbls[ismiss])] <- NA
            }

            vals <- na.omit(vals)

            # this is a test if a variable truly is numeric
            # (not just categorical using numbers)
            # if it has at least four(?) values different from the labels
            printnum <- length(setdiff(vals, lbls)) > 4

            if (!is.null(type)) {
                # at least two non missing values are needed to calculate sd()
                printnum <- printnum | (length(vals) > 2 & grepl("num", type))
            }

            if (printnum) { # numeric variable
                cat(paste(
                    s3,
                    "<", ns, "sumStat type=\"min\">",
                    format(
                        min(vals, na.rm = TRUE),
                        scientific = FALSE
                    ),
                    "</", ns, "sumStat>",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s3,
                    "<", ns, "sumStat type=\"max\">",
                    format(
                        max(vals, na.rm = TRUE),
                        scientific = FALSE
                    ),
                    "</", ns, "sumStat>",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s3,
                    "<", ns, "sumStat type=\"mean\">",
                    format(
                        mean(vals, na.rm = TRUE),
                        scientific = FALSE
                    ),
                    "</", ns, "sumStat>",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s3,
                    "<", ns, "sumStat type=\"medn\">",
                    format(
                        median(vals, na.rm = TRUE),
                        scientific = FALSE
                    ),
                    "</", ns, "sumStat>",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s3,
                    "<", ns, "sumStat type=\"stdev\">",
                    format(
                        sd(vals, na.rm = TRUE),
                        scientific = FALSE
                    ),
                    "</", ns, "sumStat>",
                    enter,
                    sep = ""
                ))

            }
        }


        if (!is.null(lbls)) {

            # what is the difference from data[[i]] ?
            tbl <- table(data[[names(dataDscr)[i]]])

            for (v in seq(length(lbls))) {

                ismiss <- is.element(lbls[v], na_values)
                if (length(na_range) > 0 & pN[i]) {
                    ismiss <- ismiss | (
                        lbls[v] >= na_range[1] & lbls[v] <= na_range[2]
                    )
                }

                cat(paste(
                    s3,
                    "<", ns, "catgry",
                    ifelse(ismiss, " missing=\"Y\"", ""), ">",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s4,
                    "<", ns, "catValu>",
                    replaceChars(lbls[v]),
                    "</", ns, "catValu>",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s4,
                    "<", ns, "labl", xmlang, ">",
                    replaceChars(names(lbls)[v]),
                    "</", ns, "labl>",
                    enter,
                    sep = ""
                ))

                if (!is.null(data)) {
                    freq <- tbl[match(lbls[v], names(tbl))]
                    cat(paste(
                        s4,
                        "<", ns, "catStat type=\"freq\">",
                        ifelse(
                            is.na(freq),
                            0,
                            format(freq, scientific = FALSE)
                        ),
                        "</", ns, "catStat>",
                        enter,
                        sep = ""
                    ))
                }

                cat(paste(s3, "</", ns, "catgry>", enter, sep = ""))
            }
        }

        if (any(grepl("type", names(dataDscr[[i]])))) {
            varFormat <- dataDscr[[i]]$varFormat[1] # SPSS
            cat(paste(
                s3,
                "<", ns, "varFormat type=\"",
                ifelse(
                    grepl("char", dataDscr[[i]]$type),
                    "character",
                    "numeric"
                ),
                # "\" schema=\"other\" formatname=\"",
                # substr(varFormat, 1, 1),
                "\">",
                varFormat,
                "</", ns, "varFormat>",
                enter,
                sep = ""
            ))
        }

        if (any(grepl("txt", names(dataDscr[[i]])))) {
            cat(paste(s3, "<", ns, "txt>", enter, sep = ""))
            cat(paste(
                s0,
                "<![CDATA[", dataDscr[[i]]$txt, "]]>",
                enter,
                sep = ""
            ))
            cat(paste(s3, "</", ns, "txt>", enter, sep = ""))
        }

        cat(paste(s2, "</", ns, "var>", enter, sep = ""))
    }

    cat(paste(s1, "</", ns, "dataDscr>", enter, sep = ""))

    sink()

    return(readLines(file.path(tmp, "dataDscr.xml")))
}


#' @description `missingValuesSPSS`: Prepares the missing values for the SPSS export syntax.
#' @return `missingValuesSPSS`: A vector of missing values representation.
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
# for recoding into SPSS
`missingValuesSPSS` <- function(variables, range = FALSE, numvars = TRUE) {
    lapply(variables, function(x) {
        na_values <- NULL
        if (is.element("na_values", names(x))) {
            na_values <- sort(getElement(x, "na_values"))
        }
        values <- getElement(x, "labels")
        na_range <- getElement(x, "na_range")
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


#' @description `prespace`: Prepend a text with a certain number of space characters.
#' @return `prespace`: A modified text.
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`prespace` <- function(text, indent = NULL) {
    if (is.null(indent)) {
        indent <- 4
    }
    nofspaces <- nchar(text) - nchar(gsub("^\\s+", "", text))
    return(
        paste0(
            repeatSpace(nofspaces / 2, indent),
            gsub("^\\s+", "", text)
        )
    )
}


#' @description `removeXMLang`: Remove the `xmlang` attribute from all elements.
#' @return `removeXMLang`: A modified `codeBook` element.
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`removeXMLang` <- function(x) {
    x <- removeAttributes("xmlang", from = x, overwrite = FALSE)

    if (anyChildren(x)) {
        for (i in seq(length(x$children))) {
            x$children[[i]] <- removeXMLang(x$children[[i]])
        }
    }

    return(x)
}


#' @description `repeatSpace`: Allows indentation of XML or HTML files
#' @return `repeatSpace`: Character spaces
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`repeatSpace` <- function(times, indent) {
    sapply(times, function(x) {
        paste(rep(" ", x * indent), collapse = "")
    })
}


#' @description `replaceChars`: Replace certain characters, in preparation for XML export
#' @return `replaceChars`: Character vector
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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


#' @description `replaceTicks`: Recode all tick characters with a single quote.
#' @return `replaceTicks`: A recoded string.
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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


#' @description `splitrows`: Split the written rows in the setup file.
#' @return `splitrows`: A character vector.
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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


#' @description `treatPath`: Determine which specific type of files are
#' present in a certain directory.
#' @return `treatPath`: A list with four components: the complete path, the
#' files, the file names and the file extensions
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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


#' @description `writeMetadata`: Utility function to write the metadata
#' part in the setup file.
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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

            values <- getElement(variables[[i]], "labels")
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

            cat(rs(1), "label = \"", getElement(variables[[i]], "label"), "\"", sep = "")
            cat(paste0(enter, ")", enter, enter, enter))

        }
    }
}


#' @description `XMLvariables`: Extract variable information from an XML Codebook
#' @return `XMLvariables`: An R list
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
`XMLvariables` <- function(xml) {
    children <- xml2::xml_children(xml)
    nms <- xml2::xml_name(children)
    dns <- getDNS(xml)
    xpath <- sprintf("/%scodeBook/%sdataDscr/%svar", dns, dns, dns)
    vars <- xml2::xml_find_all(xml, xpath)

    if (length(vars) == 0) {
        admisc::stopError("This DDI Codebook file does not contain any variable level metadata.")
    }

    varlab <- cleanup(
        xml2::xml_text(
            xml2::xml_find_first(vars, sprintf("%slabl", dns))
        )
    )

    dataDscr <- lapply(varlab, function(x) list(label = x))
    xpath <- sprintf("/%scodeBook/%sdataDscr/%svar/@name", dns, dns, dns)
    names(dataDscr) <- admisc::trimstr(
        xml2::xml_text(xml2::xml_find_all(xml, xpath))
    )

    for (i in seq(length(dataDscr))) {
        if (is.na(dataDscr[[i]][["label"]])) {
            dataDscr[[i]][["label"]] <- NULL
        }

        # nms <- xml_name(xml_contents(xml_find_all(xml, sprintf("/d1:codeBook/d1:dataDscr/d1:var[%s]", i))))

        # xpath <- sprintf("/%scodeBook/%sdataDscr/%svar[%s]", dns, dns, dns, i)
        # vars_i <- xml2::xml_find_first(xml, xpath)

        measurement <- xml2::xml_attr(vars[i], "nature")
        na_values <- NULL
        na_range <- NULL
        xpath <- sprintf("%sinvalrng/%srange", dns, dns)
        na_range[1] <- admisc::asNumeric(xml2::xml_attr(xml2::xml_find_first(vars[i], xpath), "min"))
        na_range[2] <- admisc::asNumeric(xml2::xml_attr(xml2::xml_find_first(vars[i], xpath), "max"))

        if (all(is.na(na_range))) {
            na_range <- NULL
        } else {
            if (is.na(na_range[1])) na_range[1] <- -Inf
            if (is.na(na_range[2])) na_range[2] <- Inf
        }

        xpath <- sprintf("%scatgry/%scatValu", dns, dns)
        values <- cleanup(xml2::xml_text(xml2::xml_find_all(vars[i], xpath)))

        xpath <- sprintf("%svarFormat", dns)
        vformat <- xml2::xml_find_first(vars[i], xpath)
        type <- xml2::xml_attr(vformat, "type")
        varFormat <- xml2::xml_text(vformat)

        if (length(values) > 0) {

            catgry <- xml2::xml_find_all(vars[i], sprintf("%scatgry", dns))

            na_values <- c(na_values, values[unlist(lapply(catgry, function(x) {
                grepl("Y", xml2::xml_attr(x, "missing"))
            }))])

            labl <- unlist(lapply(catgry, function(x) {
                xml2::xml_text(xml2::xml_find_first(x, sprintf("%slabl", dns)))
            }))

            values <- values[!is.na(labl)]
            labl <- cleanup(labl[!is.na(labl)])

            if (admisc::possibleNumeric(values)) {
                values <- admisc::asNumeric(values)
            }

            dataDscr[[i]][["labels"]] <- values
            names(dataDscr[[i]][["labels"]]) <- labl

            frequencies <- unlist(lapply(catgry, function(x) {
                xml2::xml_text(xml2::xml_find_first(x, sprintf("%scatStat", dns)))
            }))

            if (!all(is.na(frequencies))) {
                if (admisc::possibleNumeric(frequencies)) {
                    frequencies <- admisc::asNumeric(frequencies)
                }

                names(frequencies) <- labl
                dataDscr[[i]][["frequencies"]] <- frequencies
            }
        }

        if (length(na_values) > 0) {

            if (admisc::possibleNumeric(na_values) & admisc::possibleNumeric(values)) {
                na_values <- admisc::asNumeric(na_values)
            }

            na_values <- sort(unique(na_values))

            if (!is.null(na_range) && is.numeric(na_values)) {
                na_values <- na_values[na_values < na_range[1] | na_values > na_range[2]]
            }

            if (length(na_values) > 0) {
                dataDscr[[i]]$na_values <- na_values
            }
        }

        if (!is.null(na_range)) {
            dataDscr[[i]]$na_range <- na_range
        }

        if (is.na(measurement)) {
            if (!is.na(type)) {
                dataDscr[[i]]$type <- "num" # default

                if (type == "character") {
                    dataDscr[[i]]$type <- "char"
                }
                else if (length(values) > 0) {
                    if (length(setdiff(values, na_values)) > 0) {
                        dataDscr[[i]]$type <- "cat"
                    }
                }
            }
        }
        else {
            if (grepl("nominal|ordinal", measurement)) {
                dataDscr[[i]]$type <- "cat"
            }
            else if (grepl("interval|ratio", measurement)) {
                dataDscr[[i]]$type <- "num"
            }
            else if (!is.na(type)) {
                dataDscr[[i]]$type <- type
            }

            dataDscr[[i]]$measurement <- measurement
        }

        if (!is.na(vformat)) {
            dataDscr[[i]]$varFormat <- varFormat
        }

        if (identical(type, "character")) {
            xpath <- sprintf("%stxt", dns)
            txt <- cleanup(xml2::xml_text(xml2::xml_find_first(vars[i], xpath)))
            if (!is.na(txt)) {
                dataDscr[[i]]$txt <- txt
            }
        }
    }

    return(dataDscr)

}

##------------------------------------------------------------------------------
## TODO: modify this function according to DDIC

#' @description `writeRlist`: Write an .R file containing a metadata specific list.
#' @rdname DDIwR_internal
#' @keywords internal
#' @export
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

        cat(rs(1), "label = \"", getElement(variables[[i]], "label"), "\"", sep = "")

        if (is.element("labels", names(variables[[i]]))) {
            cat(",", enter, rs(1), "labels = c(", enter, sep = "")

            values <- getElement(variables[[i]], "labels")
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
