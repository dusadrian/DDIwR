#' @name getMetadata
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
#' getMetadata(from = x)
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

`getMetadata` <- function(from = NULL, encoding = "UTF-8", ignore = NULL, ...) {

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

                dataDscr <- collectMetadata(from, ... = ...)

                addChildren(list(dataDscr, fileDscr), to = codeBook)

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

    for (ff in seq(length(result))) {
        if (print_processing & !fromsetupfile & !singlefile) {
            cat(tp$files[ff], "\n")
        }

        if (tp$fileext[ff] == "XML") {

            xml <- getXML(file.path(tp$completePath, tp$files[ff]))
            monolang <- is.element("lang", names(xml2::xml_attrs(xml)))

            if (!is.null(ignore)) {
                if (
                    !is.atomic(ignore) || !is.character(ignore) ||
                    !all(is.element(ignore, unlist(DDIC$codeBook$children)))
                ) {
                    admisc::stopError("Argument 'ignore' should be a character vector of codeBook element names.")
                }

                children <- xml_children(xml)
                childnames <- xml_name(children)

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
        }
        else {
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
            }
            else if (tp$fileext[ff] == "RDS") {
                data <- readRDS(file.path(tp$completePath, tp$files[ff]))
            }
            # not sure about SAS, as far as I understand the metadata is not
            # embedded in the datafile but it sits into a separate, catalog file
            # else if (tp$fileext[ff] == "SAS7BDAT") {
            #     data <- haven::read_sas(file.path(tp$completePath, tp$files[ff]))
            # }

            addChildren(
                collectMetadata(data, ... = ...), # dataDscr
                to = codeBook
            )

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
