#' @name getMetadata
#'
#' @title Extract metadata information
#'
#' @description
#' Extract a list containing the variable labels, value labels and any
#' available information about missing values.
#'
#' @details
#' This function reads an XML file containing a DDI codebook version 2.6, or an
#' SPSS or Stata file and returns a list containing the variable labels, value
#' labels, plus some other useful information.
#'
#' If the input is a dataset, it will extract the variable level metadata
#' (labels, missing values etc.). From a DDI XML file, it will import all
#' metadata elements, the most expensive being the data description. This
#' element can be skipped by deactivating the `dataDscr` argument.
#'
#' It additionally attempts to automatically detect a type for each variable:
#' \tabular{rl}{
#'   **`cat`**: \tab categorical variable using numeric values\cr
#'   **`catchar`**: \tab categorical variable using character values\cr
#'   **`catnum`**: \tab categorical variable for which numerical summaries\cr
#'   \tab can be calculated (ex. a 0...10 Likert response scale)\cr
#'   **`num`**: \tab numerical\cr
#'   **`numcat`**: \tab numerical variable with few enough values (ex. number of
#' children)\cr
#'   \tab for which a table of frequencies is possible in addition to
#' frequencies
#' }
#'
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
#' getMetadata(x)
#'
#' @return
#' An R list roughly equivalent to a DDI Codebook, containing all variables,
#' their corresponding variable labels and value labels, and (if applicable)
#' missing values if imported and found.
#'
#' @author Adrian Dusa
#'
#' @param x A path to a file, or a data frame object
#'
#' @param encoding The character encoding used to read a file
#'
#' @param ... Additional arguments for this function (internal use only)
#'
#' @export

`getMetadata` <- function(x, encoding = "UTF-8", ...) {

    # TODO: detect DDI version or ask the version through a dedicated argument
    # http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html

    funargs <- lapply(
        lapply(match.call(), deparse)[-1],
        function(x) gsub("'|\"|[[:space:]]", "", x)
    )

    dots <- list(...)
    data <- NULL

    print_processing <- !isFALSE(dots$print_processing)

    filetypes <- c("SPSS", "SPSS", "Stata", "SAS", "R", "DDI", "Excel", "Excel")
    fileexts <- c("SAV", "POR", "DTA", "SAS7BDAT", "RDS", "XML", "XLS", "XLSX")

    user_na <- !isFALSE(dots$user_na) # force reading the value labels
    embed <- isTRUE(dots$embed)

    if (is.null(x)) {
        admisc::stopError("`x` should be a path or a data frame.")
    } else {
        if (is.data.frame(x)) {

            if (!hasLabels(x)) {
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
                    info = list(
                        content = admisc::getName(funargs$x)
                    )
                )

                fileType <- makeElement("fileType", info = list(content = "R"))
                addChildren(list(fileName, fileType), to = fileTxt)
                addChildren(fileTxt, to = fileDscr)

                dataDscr <- collectMetadata(x)

                addChildren(list(dataDscr, fileDscr), to = codeBook)

                return(codeBook)
            }
        }
        else {
            if (!is.atomic(x) || !is.character(x) || length(x) != 1) {
                admisc::stopError("A path should be a character vector of length 1.")
            }
        }
    }

    fromsetupfile <- isTRUE(dots$fromsetupfile)

    tp <- treatPath(x, type = "*")

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

            # TODO: perhaps validate the codebook against the schema, first...!?
            xmlist <- xml2::as_list(
                getXML(file.path(tp$completePath, tp$files[ff]))
            )

            checkXMList(xmlist)
            codeBook <- coerceDDI(xmlist)
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

            addChildren(collectMetadata(data), to = codeBook) # dataDscr

            fileName <- makeElement(
                "fileName",
                list(content = tp$files[ff])
            )

            fileType <- makeElement(
                "fileType",
                list(content = filetypes[which(fileexts == tp$fileext[ff])])
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
