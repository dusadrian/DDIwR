#' @name getCodebook
#'
#' @title Extract metadata information
#'
#' @description
#' Extract a list containing the variable labels, value labels and any
#' available information about missing values.
#'
#' @details
#' This function extracts the metadata from an R dataset, or alternatively it
#' can read an XML file containing a DDI codebook version 2.6, or an
#' SPSS or Stata file and returns a list containing the variable labels, value
#' labels and information about the missing values.
#'
#' If the input is a dataset, it will extract the variable level metadata
#' (labels, missing values etc.). From a DDI XML file, it will import all
#' metadata elements, the most expensive being the data description.
#'
# It additionally attempts to automatically detect a type for each variable:
# \tabular{rl}{
#   **`cat`**: \tab categorical variable using numeric values\cr
#   **`catchar`**: \tab categorical variable using character values\cr
#   **`catnum`**: \tab categorical variable for which numerical summaries\cr
#   \tab can be calculated (ex. a 0...10 Likert response scale)\cr
#   **`num`**: \tab numerical\cr
#   **`numcat`**: \tab numerical variable with few enough values (ex. number of
# children)\cr
#   \tab for which a table of frequencies is possible in addition to
# frequencies
# }
#' For the moment, only DDI Codebook is supported, but DDI Lifecycle is planned
#' to be implemented.
#'
#' @examples
#' x <- data.frame(
#'     A = declared(
#'         c(1:5, -92),
#'         labels = c(Good = 1, Bad = 5, NR = -92),
#'         na_values = -92
#'     ),
#'     C = declared(
#'         c(1, -91, 3:5, -92),
#'         labels = c(DK = -91, NR = -92),
#'         na_values = c(-91, -92)
#'     )
#' )
#'
#' getCodebook(from = x)
#'
#' @return
#' An R list roughly equivalent to a DDI Codebook, containing all variables,
#' their corresponding variable labels and value labels, and (if applicable)
#' missing values if imported and found.
#'
#' @author Adrian Dusa
#'
#' @param from A path to a file, or a data frame object
#'
#' @param encoding The character encoding used to read a file
#'
#' @param ignore Character, ignore DDI elements when reading from an XML file
#'
#' @param ... Additional arguments for this function (internal use only)
#'
#' @export

`getCodebook` <- function(from = NULL, encoding = "UTF-8", ignore = NULL, ...) {

    # TODO: detect DDI version or ask the version through a dedicated argument
    # http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html

    funargs <- lapply(
        lapply(match.call(), deparse)[-1],
        function(x) gsub("'|\"|[[:space:]]", "", x)
    )

    DDIC <- get("DDIC", envir = cacheEnv)

    dots <- list(...)
    if (is.null(from) & !is.null(dots$x)) {
        from <- dots$x
    }

    data <- NULL

    print_processing <- !isFALSE(dots$print_processing)

    filetypes <- c("SPSS", "SPSS", "Stata", "SAS", "R", "DDI", "Excel", "Excel")
    fileexts <- c("SAV", "POR", "DTA", "SAS7BDAT", "RDS", "XML", "XLS", "XLSX")

    user_na <- !isFALSE(dots$user_na) # force reading the value labels
    embed <- isTRUE(dots$embed)

    if (is.null(from)) {
        admisc::stopError("`from` should be a path or a data frame.")
    } else {
        if (is.data.frame(from)) {

            if (!hasLabels(from)) {
                if (is.element("error_null", names(dots))) {
                    return(NULL)
                }

                admisc::stopError("The input does not seem to contain any metadata.")
            }
            else {
                codeBook <- makeElement("codeBook")
                fileDscr <- makeElement("fileDscr")
                fileTxt <- makeElement("fileTxt")

                fileName <- makeElement(
                    "fileName",
                    content = admisc::getName(funargs$from)
                )

                fileType <- makeElement("fileType", content = "R")
                addChildren(list(fileName, fileType), to = fileTxt)
                addChildren(fileTxt, to = fileDscr)

                # Recode extended missings (Stata/SAS style) to numeric codes
                # for a codebook-friendly representation, similar to convert()
                dots <- list(...)
                recode <- !isFALSE(dots$recode)
                data_input <- from
                if (recode) {
                    need_recode <- any(sapply(from, function(x) {
                        inherits(x, "haven_labelled") && !inherits(x, "haven_labelled_spss")
                    })) || any(sapply(from, function(x) {
                        any(haven::is_tagged_na(c(unclass(x), attr(x, "labels", exact = TRUE))))
                    }))

                    if (isTRUE(need_recode)) {
                        data_input <- recodeMissings(
                            dataset = from,
                            to = "SPSS",
                            dictionary = dots$dictionary
                        )
                    }
                }

                addChildren(fileDscr, to = codeBook)

                if (!isFALSE(dots$dataDscr)) {
                    addChildren(
                        collectMetadata(data_input, ... = ...),
                        to = codeBook
                    )
                }

                return(codeBook)
            }
        }
        else {
            if (!is.atomic(from) || !is.character(from) || length(from) != 1) {
                admisc::stopError("A path should be a character vector of length 1.")
            }
        }
    }

    fromsetupfile <- isTRUE(dots$fromsetupfile)

    tp <- treatPath(from, type = "*")

    singlefile <- length(tp$files) == 1

    if (print_processing & !fromsetupfile & !singlefile) {
        cat("Processing:\n")
    }

    data <- NULL

    result <- vector(mode = "list", length = length(tp$files))

    fromPublisher <- identical(ignore, "dataDscr") & isTRUE(dots$dataset)

    for (ff in seq(length(result))) {
        if (print_processing & !fromsetupfile & !singlefile) {
            cat(tp$files[ff], "\n")
        }

        if (tp$fileext[ff] == "XML") {

            xml <- getXML(file.path(tp$completePath, tp$files[ff]))
            monolang <- is.element("lang", names(xml2::xml_attrs(xml)))

            if (fromPublisher) {
                data <- extractData(xml)
                dns <- getDNS(xml) # default name space

                xpath <- sprintf("/%scodeBook/%sdataDscr/%svar", dns, dns, dns)
                xmlvars <- xml2::xml_find_all(xml, xpath)

                if (is.null(data)) {
                    csv <- NULL
                    csvexists <- FALSE
                    files <- getFiles(tp$completePath, "*")
                    csvfiles <- files$fileext == "CSV"

                    if (any(csvfiles)) {
                        csvexists <- is.element(
                            toupper(tp$filenames),
                            toupper(files$filenames[csvfiles])
                        )

                        csvfile <- files$files[csvfiles][
                            match(
                                toupper(tp$filenames),
                                toupper(files$filenames[csvfiles])
                            )
                        ]
                    }

                    if (csvexists) {
                        csv <- file.path(tp$completePath, csvfile)
                        callist <- list(file = csv)
                        for (f in names(formals(utils::read.csv))) {
                            if (is.element(f, names(dots))) {
                                callist[[f]] <- dots[[f]]
                            }
                        }

                        header <- ifelse(isFALSE(callist$header), FALSE, TRUE)
                        data <- do.call("read.csv", callist)

                        variables <- lapply(xmlvars, XMLtoRmetadata, dns = dns)

                        xpath <- sprintf("/%scodeBook/%sdataDscr/%svar/@name", dns, dns, dns)
                        names(variables) <- admisc::trimstr(
                            xml2::xml_text(xml2::xml_find_all(xml, xpath))
                        )

                        if (ncol(data) == length(variables)) {
                            if (header) {
                                if (!identical(names(data), names(variables))) {
                                    data <- NULL
                                }
                            }
                            else {
                                names(data) <- names(variables)
                            }
                        }

                        if (ncol(data) == length(variables) + 1) {
                            if (header) {
                                data <- NULL
                            }
                            else {
                                names(data) <- c("row_names_csv_file", names(variables))
                            }

                            if (!is.null(data)) {
                                rownames(data) <- data[, 1]
                                data <- subset(
                                    data,
                                    select = seq(2, ncol(data))
                                )
                                # data <- data[, -1, drop = FALSE]
                            }
                        }

                        if (!is.null(data)) {
                            data <- makeLabelled(data, variables)
                        }
                    }
                } else {
                    hashes <- attr(data, "hashes")
                    attr(data, "hashes") <- NULL

                    if (!is.null(hashes)) {
                        checkhashes <- getHashes(xmlvars)

                        if (!identical(hashes, checkhashes)) {
                            different <- which(hashes != checkhashes)

                            for (i in different) {
                                metadata <- XMLtoRmetadata(xmlvars[i], dns = dns)
                                for (att in c("label", "labels", "na_values", "na_range")) {
                                    attr(data[[i]], att) <- getElement(metadata, att)
                                }
                            }
                        }
                    }
                }

                if (is.null(data)) {
                    if (exists("dataset", envir = .GlobalEnv, inherits = FALSE)) {
                        rm("dataset", envir = .GlobalEnv)
                    }

                    ignore <- setdiff(ignore, "dataDscr")

                } else {
                    data <- declared::as.declared(data)
                    class(data) <- "data.frame"
                    assign("dataset", data, envir = .GlobalEnv)
                }
            }

            if (!is.null(ignore)) {
                if (
                    !is.atomic(ignore) || !is.character(ignore) ||
                    !all(is.element(ignore, unlist(DDIC$codeBook$children)))
                ) {
                    admisc::stopError("Argument 'ignore' should be a character vector of codeBook element names.")
                }

                children <- xml2::xml_children(xml)
                childnames <- xml2::xml_name(children)

                todelete <- which(is.element(childnames, ignore))

                if (length(todelete) > 0) {
                    for (d in todelete) {
                        xml2::xml_remove(children[d])
                    }
                }
            }

            # TODO: perhaps validate the codebook against the schema, first...!?
            xmlist <- xml2::as_list(xml)

            checkXMList(xmlist)
            codeBook <- coerceDDI(xmlist)
            codeBook$.extra$monolang <- monolang

            if (fromPublisher & is.null(data)) {
                codeBook$.extra$dataset_missing <- TRUE
            }
        }
        else { # not an XML file, needs importing
            codeBook <- makeElement("codeBook")
            fileDscr <- makeElement("fileDscr")

            if (tp$fileext[ff] == "SAV" | tp$fileext[ff] == "POR") {
                fargs <- names(formals(read_sav))
                arglist <- dots[is.element(names(dots), fargs)]
                arglist$file <- file.path(tp$completePath, tp$files[ff])
                arglist$user_na <- user_na
                if (tp$fileext[ff] == "SAV") {
                    arglist$encoding <- encoding
                }
                data <- do.call(
                    ifelse (
                        tp$fileext[ff] == "SAV",
                        haven::read_sav,
                        haven::read_por
                    ),
                    arglist
                )
            }
            else if (tp$fileext[ff] == "DTA") {
                fargs <- names(formals(read_dta))
                arglist <- dots[is.element(names(dots), fargs)]
                arglist$file <- file.path(tp$completePath, tp$files[ff])
                arglist$encoding <- encoding
                data <- do.call(haven::read_dta, arglist)

                # Ensure codebook uses numeric missing codes for Stata-style
                # tagged missings by recoding to SPSS style before metadata collection
                recode <- !isFALSE(dots$recode)
                if (recode) {
                    need_recode <- any(sapply(data, function(x) {
                        inherits(x, "haven_labelled") && !inherits(x, "haven_labelled_spss")
                    })) || any(sapply(data, function(x) {
                        any(haven::is_tagged_na(c(unclass(x), attr(x, "labels", exact = TRUE))))
                    }))

                    if (isTRUE(need_recode)) {
                        data <- recodeMissings(
                            dataset = data,
                            to = "SPSS",
                            dictionary = dots$dictionary
                        )
                    }
                }
            }
            else if (tp$fileext[ff] == "RDS") {
                data <- readRDS(file.path(tp$completePath, tp$files[ff]))
            }
            # not sure about SAS, as far as I understand the metadata is not
            # embedded in the datafile but it sits into a separate, catalog file
            # else if (tp$fileext[ff] == "SAS7BDAT") {
            #     data <- haven::read_sas(file.path(tp$completePath, tp$files[ff]))
            # }

            if (!is.element("dataDscr", ignore)) {
                addChildren(
                    collectMetadata(data, ... = ...), # dataDscr
                    to = codeBook
                )
            }

            if (isTRUE(dots$dataset)) {
                data <- declared::as.declared(data)
                class(data) <- "data.frame"
                assign("dataset", data, envir = .GlobalEnv)
            }

            fileName <- makeElement(
                "fileName",
                content = tp$files[ff]
            )

            fileType <- makeElement(
                "fileType",
                content = filetypes[which(fileexts == tp$fileext[ff])]
            )

            fileTxt <- makeElement("fileTxt")
            addChildren(list(fileName, fileType), to = fileTxt)
            addChildren(fileTxt, to = fileDscr)

            addChildren(fileDscr, to = codeBook)
        }

        if (singlefile) {
            return(codeBook)
        }
        else {
            result[[ff]] <- codeBook
        }
    }

    names(result) <- tp$filenames
    return(result)
}
