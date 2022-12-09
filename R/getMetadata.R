#' @name getMetadata
#'
#' @title Extract metadata information
#'
#' @description
#' Extract a list containing the variable labels, value labels and any
#' available information about missing values.
#'
#' @details
#' This function reads an XML file containing a DDI codebook version 2.5, or an
#' SPSS or Stata file and returns a list containing the variable labels, value
#' labels, plus some other useful information.
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
#' By default, this function extracts the metadata into an R list object, but
#' when the argument `save` is activated, the argument `OS` (case insensitive)
#' can be either:\cr
#' `"Windows"` (default), or `"Win"`,\cr
#' `"MacOS"`, `"Darwin"`, `"Apple"`, `"Mac"`,\cr
#' `"Linux"`.\cr
#'
#' The end of line separator changes only when the target OS is different from
#' the running OS.
#'
#' For the moment, only DDI version 2.5 (Codebook) is supported, but DDI
#' Lifecycle is planned to be implemented.
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
#' getMetadata(x)$dataDscr
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
#' @param save Logical, save an .R file in the same directory
#'
#' @param declared Logical, embed the data as a declared object
#'
#' @param OS The target operating system, for the eol - end of line separator,
#' if saving the file
#'
#' @param encoding The character encoding used to read a file
#'
#' @param ... .Additional arguments for this function (internal uses only)
#'
#' @export

`getMetadata` <- function(
    x, save = FALSE, declared = TRUE, OS = "Windows", encoding = "UTF-8", ...
) {

    # TODO: detect DDI version or ask the version through a dedicated argument
    # http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html

    dots <- list(...)
    stdyDscr <- NULL

    user_na <- TRUE # force reading the value labels
    if (
        is.element("user_na", names(dots)) && is.atomic(dots$user_na) &&
        length(dots$user_na) == 1 && is.logical(dots$user_na)
    ) {
        user_na <- dots$user_na
    }

    embed <- isTRUE(dots$embed)

    if (is.data.frame(x)) {
        error <- TRUE
        i <- 1
        while (i <= ncol(x) & error) {
            attrx <- attributes(x[[i]])
            if (any(is.element(c("label", "labels", "na_value", "na_range"), names(attrx)))) {
                error <- FALSE
            }
            i <- i + 1
        }

        if (error) {
            if (is.element("error_null", names(dots))) {
                return(NULL)
            }

            admisc::stopError("The input does not seem to contain any metadata.")
        }
        else {
            codeBook <- list()
            codeBook$dataDscr <- collectMetadata(x)
            if (embed) {
                codeBook$fileDscr <- list(
                    datafile = x
                )
            }
            return(codeBook)
        }
    }
    else {
        if (!is.atomic(x) || !is.character(x) || length(x) != 1) {
            admisc::stopError("A path should be a string of length 1")
        }
    }

    enter <- getEnter(OS)

    fromsetupfile <- isTRUE(dots$fromsetupfile)

    tp <- treatPath(x, type = "*")

    singlefile <- length(tp$files) == 1
    notes <- NULL

    if (!fromsetupfile & !singlefile) {
        cat("Processing:\n")
    }

    data <- NULL

    result <- vector(mode = "list", length = length(tp$files))

    for (ff in seq(length(result))) {
        if (!fromsetupfile & !singlefile) {
            cat(tp$files[ff], "\n")
        }

        if (tp$fileext[ff] == "XML") {

            codeBook <- list()

            # xml <- getXML(file.path(tp$completePath, tp$files[ff]))
            tc <- admisc::tryCatchWEM(
                xml <- xml2::read_xml(file.path(tp$completePath, tp$files[ff]))
            )

            if (!is.null(tc$error)) {
                admisc::stopError(
                    paste(
                        "Unable to read the XML file",
                        tc$error,
                        sep = ", "
                    )
                )
            }

            children <- xml2::xml_children(xml)
            nms <- xml2::xml_name(children)
            if (is.element("stdyDscr", nms)) {
                stdyDscr <- xml2::as_list(children[[which(nms == "stdyDscr")]])
            }

            # lapply(xml_find_all(xml, "/d1:codeBook/d1:dataDscr/d1:var"), function(x) {
            #     list(label = admisc::trimstr(xml_text(xml_find_first(x, "d1:labl"))))
            # })

            xmlns <- xml2::xml_ns(xml)
            # d1  <-> ddi:codebook:2_5"
            # xsi <-> http://www.w3.org/2001/XMLSchema-instance
            # xsd <-> http://www.w3.org/2001/XMLSchema
            wns <- which(xmlns == "ddi:codebook:2_5")
            if (length(wns) == 0) {
                admisc::stopError("The XML document does not contain a DDI namespace.")
            }

            # <d>efault <n>ame <s>pace
            dns <- names(xmlns)[wns[1]]
            if (dns != "d1") {
                codeBook$xmlns <- dns
            }
            dns <- paste0(dns, ":")

            ### Unfortunately this does not work because some variables don't always have labels
            ### and we'll end up having a vector of labels that is shorter than the number of variables
            # xpath <- sprintf("/%scodeBook/%sdataDscr/%svar/%slabl", dns, dns, dns, dns)
            # varlab <- cleanup(xml2::xml_text(xml2::xml_find_all(xml, xpath)))
            ###

            xpath <- sprintf("/%scodeBook/%sdataDscr/%svar", dns, dns, dns)
            vars <- xml2::xml_find_all(xml, xpath)
            varlab <- cleanup(
                xml2::xml_text(
                    xml2::xml_find_first(vars, sprintf("%slabl", dns))
                )
            )

            xpath <- sprintf("/%scodeBook/%sfileDscr/%snotes", dns, dns, dns)
            notes <- xml2::xml_text(xml2::xml_find_all(xml, xpath))

            codeBook$dataDscr <- lapply(varlab, function(x) list(label = x))

            xpath <- sprintf("/%scodeBook/%sdataDscr/%svar/@name", dns, dns, dns)
            names(codeBook$dataDscr) <- admisc::trimstr(xml2::xml_text(xml2::xml_find_all(xml, xpath)))

            for (i in seq(length(codeBook$dataDscr))) {
                if (is.na(codeBook$dataDscr[[i]][["label"]])) {
                    codeBook$dataDscr[[i]][["label"]] <- NULL
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

                    codeBook$dataDscr[[i]][["labels"]] <- values
                    names(codeBook$dataDscr[[i]][["labels"]]) <- labl

                    frequencies <- unlist(lapply(catgry, function(x) {
                        xml2::xml_text(xml2::xml_find_first(x, sprintf("%scatStat", dns)))
                    }))

                    if (!all(is.na(frequencies))) {
                        if (admisc::possibleNumeric(frequencies)) {
                            frequencies <- admisc::asNumeric(frequencies)
                        }
                        
                        names(frequencies) <- labl
                        codeBook$dataDscr[[i]][["frequencies"]] <- frequencies
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
                        codeBook$dataDscr[[i]]$na_values <- na_values
                    }
                }

                if (!is.null(na_range)) {
                    codeBook$dataDscr[[i]]$na_range <- na_range
                }

                if (is.na(measurement)) {
                    if (!is.na(type)) {
                        codeBook$dataDscr[[i]]$type <- "num" # default

                        if (type == "character") {
                            codeBook$dataDscr[[i]]$type <- "char"
                        }
                        else if (length(values) > 0) {
                            if (length(setdiff(values, na_values)) > 0) {
                                codeBook$dataDscr[[i]]$type <- "cat"
                            }
                        }
                    }
                }
                else {
                    if (grepl("nominal|ordinal", measurement)) {
                        codeBook$dataDscr[[i]]$type <- "cat"
                    }
                    else if (grepl("interval|ratio", measurement)) {
                        codeBook$dataDscr[[i]]$type <- "num"
                    }
                    else if (!is.na(type)) {
                        codeBook$dataDscr[[i]]$type <- type
                    }

                    codeBook$dataDscr[[i]]$measurement <- measurement
                }

                if (!is.na(vformat)) {
                    codeBook$dataDscr[[i]]$varFormat <- varFormat
                }

                if (identical(type, "character")) {
                    xpath <- sprintf("%stxt", dns)
                    txt <- cleanup(xml2::xml_text(xml2::xml_find_first(vars[i], xpath)))
                    if (!is.na(txt)) {
                        codeBook$dataDscr[[i]]$txt <- txt
                    }
                }
            }
        }
        else {
            if (tp$fileext[ff] == "SAV") {
                fargs <- names(formals(read_sav))
                arglist <- dots[is.element(names(dots), fargs)]
                arglist$file <- file.path(tp$completePath, tp$files[ff])
                arglist$user_na <- user_na
                arglist$encoding <- encoding
                data <- do.call(haven::read_sav, arglist)
            }
            else if (tp$fileext[ff] == "POR") {
                fargs <- names(formals(read_sav))
                arglist <- dots[is.element(names(dots), fargs)]
                arglist$file <- file.path(tp$completePath, tp$files[ff])
                arglist$user_na <- user_na
                data <- do.call(haven::read_por, arglist)
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
            # not sure about SAS, as far as I understand the metadata is not embedded in the datafile
            # sometimes it might sit into a separate, catalog file or something (need to investigate)
            # else if (tp$fileext[ff] == "SAS7BDAT") {
            #     data <- haven::read_sas(file.path(tp$completePath, tp$files[ff]))
            # }

            codeBook <- list()
            codeBook$dataDscr <- collectMetadata(data)
        }

        codeBook$fileDscr$fileName <- tp$files[ff]

        filetypes <- c("SPSS", "SPSS", "Stata", "SAS", "R", "DDI", "Excel", "Excel")
        fileexts <- c("SAV", "POR", "DTA", "SAS7BDAT", "RDS", "XML", "XLS", "XLSX")

        codeBook$fileDscr$fileType <- filetypes[which(fileexts == tp$fileext[ff])]

        result[[ff]] <- codeBook

        if (save) {

            indent <- 4
            if (is.element("indent", names(dots))) {
                indent <- dots$indent
            }

            writeRlist(
                codeBook$dataDscr,
                OS = OS,
                indent = indent,
                dirpath = tp$completePath,
                filename = tp$filenames[ff]
            )

        }
    }

    names(result) <- tp$filenames

    if (singlefile) {
        if (length(notes) > 0) {
            wdata <- which(grepl("# start data #", notes))
            if (length(wdata) > 0) {
                notes <- notes[wdata]
                # this can only be possible from an XML, DDI Codebook
                # therefore the varFormat should always be of an SPSS type
                notes <- unlist(strsplit(notes, split = "\\n"))
                data <- admisc::trimstr(notes[
                    seq(
                        which(grepl("# start data #", notes)) + 1,
                        which(grepl("# end data #", notes)) - 1
                    )
                ], side = "left")

                tc <- admisc::tryCatchWEM(
                    data <- read.csv(text = paste(data, collapse = "\n"), as.is = TRUE)
                )

                if (!is.null(tc$error)) {
                    admisc::stopError("The <notes> tag does not contain a valid CSV dataset.")
                }

                
                # return(list(data, codeBook$dataDscr, declared = declared, spss = spss))

                # make_labelled is always and only about SPSS type of variables
                data <- make_labelled(
                    data,
                    codeBook$dataDscr,
                    declared = declared
                )

                embed <- TRUE
            }
        }

        if (embed & !is.null(data)) {
            codeBook$fileDscr$datafile <- data
        }

        if (!is.null(stdyDscr)) {
            codeBook$stdyDscr <- stdyDscr
        }

        return(codeBook)
    }
    else {
        return(result)
    }
}
