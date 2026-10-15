#' @name exportCodebook
#' @title
#' Export a DDI Codebook to an XML file.
#'
#' @description
#' Create a DDI Codebook version 2.6, XML file structure.
#'
#' @details
#' #' The information object is a `codeBook` DDI element having at least two
#' main children:
#'
#' - **`fileDscr`**, with the data provided as a sub-component named
#' **`datafile`**
#'
#' - **`dataDscr`**, having as many components as the number of variables in the
#' (meta)data.
#'
#' For the moment, only DDI codebook version 2.6 is exported, and DDI Lifecycle
#' is planned for future releases.
#'
#' A small number of required DDI specific elements and attributes have generic
#' default values, if not otherwise specified in the `codeBook` list object. For
#' the current version, these are: `monolang`, `xmlang`, `IDNo`, `titl`,
#' `agency`, `URI` (for the `holdings` element), `distrbtr`, `abstract` and
#' `level` (for the `otherMat` element).
#'
#' The finished XML is always validated against the bundled, pinned DDI Codebook
#' 2.6 schema and its local dependencies before writing. Invalid or incomplete
#' documents, or unavailable schemas, cause an error. Validation failure leaves
#' the destination and any companion CSV unchanged, and writes nothing to an
#' output connection. No network access is needed for validation. Most of these
#' arguments help create the mandatory element `stdyDscr`, which cannot be
#' harvested from the dataset. If this element is not already present, providing
#' any of these arguments via the three dots `...` gate, signal an automatic
#' creation and inclusion with the values provided.
#'
#' Argument **`xmlang`** expects a two letter ISO country coding, for instance
#' `"en"` to indicate English, or `"ro"` to indicate Romanian etc. The original
#' DDI Codebook attribute is called **`xml:lang`**, which for obvious reasons
#' had to be renamed into this R function.
#'
#' A logical argument `monolang` signal if the document is monolingual, in which
#' case the attribute `xmlang` is placed a single time for the entire document
#' in the `codeBook` element. For multilingual documents, `xmlang` should be
#' placed in the attributes of various other (child) elements, for instance
#' `abstract`, or the study title, name of the distributing institution,
#' variable labels etc.
#'
#' The argument **`OS`** can be either:\cr
#' `"windows"` (default), or `"Windows"`, `"Win"`, `"win"`,\cr
#' `"MacOS"`, `"Darwin"`, `"Apple"`, `"Mac"`, `"mac"`,\cr
#' `"Linux"`, `"linux"`.
#'
#' The end of line separator changes only when the target OS is different from
#' the running OS.
#'
#' The argument **`indent`** controls how many spaces will be used in the XML
#' file, to indent the different sub-elements.
#' Formatting and target line endings also apply when writing to a connection.
#'
#' @return
#' An XML file containing a DDI version 2.6 metadata.
#'
#' @seealso
#' \url{https://ddialliance.org/hubfs/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html}
#'
#' @examples
#' \dontrun{
#' exportCodebook(codeBook, to = "codebook.xml")
#'
#' # using a namespace
#' exportCodebook(codeBook, to = "codebook.xml", xmlns = "ddi")
#' }
#'
#' @author Adrian Dusa
#'
#' @param codeBook A standard element of class `"DDI"`.
#'
#' @param to
#' either a character string naming a file or a connection open for
#' writing ("" indicates output to the console)
#' Home-directory paths beginning with `~` are expanded before writing.
#'
#' @param OS
#' The target operating system, for the eol - end of line character(s)
#'
#' @param indent Indent width, in number of spaces
#'
#' @param ... Other arguments, mainly for internal use
#' @param session Optional session returned by [metadataSweepCapture()]. Its
#' complete variable description replaces `dataDscr` in `codeBook`, reusing
#' cached unweighted analysis. Study and document metadata remain supplied by
#' `codeBook`. Do not also supply `data` or `variables`. `embed = TRUE` includes
#' captured data with stable variable IDs; `csv = TRUE` requests a companion CSV.
#' Weighted export uses the existing weighted calculation path.
#' @param revision Current host revision, required when `session` is supplied.
#'
#' @export
`exportCodebook` <- function(codeBook, to = "", OS = "", indent = 2, ...,
    session = NULL, revision = NULL) {
    dots <- list(...)

    if (!is.null(session)) {
        .metadataSessionAssertRevision(session, revision)

        if (!is.null(dots$data) || !is.null(dots$variables)) {
            stop("Supply session export inputs through the session only.")
        }

        dots$dataDscr_directly_in_XML <- TRUE
        dots$embed <- isTRUE(dots$embed)
        dots$csv <- isTRUE(dots$csv)
        dots$variables <- .metadataSessionVariables(session, seq_len(session$columns))

        if (hasChildren(codeBook, "dataDscr")) {
            codeBook <- removeChildren("dataDscr", from = codeBook, overwrite = FALSE)
        }

        if (!hasChildren(codeBook, "fileDscr")) {
            codeBook <- addChildren(makeElement("fileDscr"),
                to = codeBook, overwrite = FALSE)
        }
    }

    if (identical(to, "") && !is.null(dots$file)) {
        to <- dots$file
    }

    if (is.character(to) && length(to) == 1L && nzchar(to)) {
        to <- path.expand(to)
    }

    if (codeBook$.extra$name == "codeBook") {
        if (any(
            is.element(
                c("IDNo", "titl", "agency", "URI", "distrbtr", "abstract", "level"),
                names(dots)
            )
        )) {
            if (!hasChildren(codeBook, "stdyDscr")) {
                codeBook <- addChildren(
                    makeElement("stdyDscr", fill = TRUE, ... = ...),
                    to = codeBook,
                    overwrite = FALSE
                )
            }

            if (!hasChildren(codeBook, "otherMat")) {
                codeBook <- addChildren(
                    makeElement("otherMat", fill = TRUE, ... = ...),
                    to = codeBook,
                    overwrite = FALSE
                )
            }
        }

        if (!hasChildren(codeBook, "docDscr")) {
            codeBook <- addChildren(
                makeElement("docDscr", fill = TRUE, ... = ...),
                to = codeBook,
                overwrite = FALSE
            )
        }
    }

    xmlang <- checkDots(dots$xmlang, default = "en")
    variables <- getElement(dots, "variables")
    varxmlang <- any(vapply(variables, function(x) {
        length(x$xmlang) > 0L
    }, logical(1)))

    if (!isFALSE(dots$monolang) & !varxmlang) {
        codeBook <- changeXMLang(codeBook, remove = TRUE)
        attr(codeBook, "xml:lang") <- xmlang
    } else {
        codeBook <- changeXMLang(codeBook)
    }


    if (isTRUE(dots$dataDscr_directly_in_XML)) {
        data <- dots$data

        if (!is.null(session)) {
            data <- .metadataSessionValues(session, seq_len(session$columns),
                attributes = TRUE)
        }
        embed <- dots$embed

        codeBook <- addChildren(
            makeElement("dataDscr"),
            to = codeBook,
            overwrite = FALSE
        )

        var_dots <- dots
        var_dots$variables <- NULL
        generator_inputs <- list(variables = dots$variables)

        if (!is.null(session)) {
            generator_inputs <- list(session = session, revision = revision)
        }

        var_info <- do.call(
            makeXMLvars,
            c(
                generator_inputs,
                list(
                    DDI = FALSE,
                    return_hashes = TRUE
                ),
                var_dots
            )
        )
        var_xml <- var_info$xml
        ns <- getElement(dots, "ns")

        if (is.null(ns)) {
            ns <- ""
        }

        if (nzchar(ns) && !grepl(":$", ns)) {
            ns <- paste0(ns, ":")
        }

        dataDscr_xml <- paste0(
            "  <", ns, "dataDscr>\n",
            paste(var_xml, collapse = ""),
            "  </", ns, "dataDscr>\n"
        )

        attr(data, "hashes") <- var_info$hashes

        if (embed) {
            uuid <- var_info$stats$id
            for (i in seq_along(uuid)) {
                attr(data[[i]], "ID") <- uuid[i]
            }
            codeBook$fileDscr <- addChildren(
                makeDataNotes(data),
                to = codeBook$fileDscr,
                overwrite = FALSE
            )
        }

        codeBook <- as_xml_document(
            list(codeBook = removeExtra(codeBook))
        )

        codeBook_xml <- as.character(codeBook)
        dataDscr_match <- regexpr(
            "(?m)^[ \\t]*<dataDscr\\s*/>\\r?\\n?",
            codeBook_xml,
            perl = TRUE
        )
        if (dataDscr_match[1] < 0) {
            admisc::stopError("Could not locate the <dataDscr/> placeholder in the codeBook XML.")
        }

        dataDscr_start <- dataDscr_match[1]
        dataDscr_end <- dataDscr_start + attr(dataDscr_match, "match.length") - 1L
        xml_prefix <- substr(codeBook_xml, 1L, dataDscr_start - 1L)
        xml_suffix <- substr(codeBook_xml, dataDscr_end + 1L, nchar(codeBook_xml))

        if (!identical(indent, 2) || !identical(OS, "")) {
            defaultOS <- Sys.info()[["sysname"]]
            checkArgument(indent, default = 2)
            checkArgument(OS, default = defaultOS)

            enter <- getEnter(OS = ifelse(OS == "", defaultOS, OS))
            xmlfile <- paste0(xml_prefix, dataDscr_xml, xml_suffix)
            xml_lines <- unlist(strsplit(xmlfile, "\n", fixed = TRUE))
            xmlfile <- paste(
                prespace(xml_lines, indent),
                collapse = enter
            )
        }
        else {
            xmlfile <- paste0(xml_prefix, dataDscr_xml, xml_suffix)
        }

        .validateCodebookXML(xmlfile)

        if (!embed && !isFALSE(dots$csv)) {
            tp_file <- treatPath(to, type = "*", single = TRUE, check = FALSE)
            write.table(
                undeclare(data, drop = TRUE),
                file = file.path(
                    tp_file$completePath,
                    paste(tp_file$filenames[1], "csv", sep = ".")
                ),
                sep = ",",
                na = "",
                row.names = FALSE
            )
        }

        if (is.character(to) && length(to) == 1 && nzchar(to)) {
            if (!identical(indent, 2) || !identical(OS, "")) {
                writeTextFileC(to, xmlfile)
            } else {
                writeTextFileChunksC(to, c(xml_prefix, dataDscr_xml, xml_suffix))
            }
        } else {
            if (!identical(indent, 2) || !identical(OS, "")) {
                writeLines(xmlfile, con = to, sep = enter, useBytes = TRUE)
            }
            else {
                xmlfile <- paste0(xml_prefix, dataDscr_xml, xml_suffix)
                writeLines(xmlfile, con = to, useBytes = TRUE)
            }
        }

    } else {
        codeBook <- xml2::as_xml_document(
            list(codeBook = removeExtra(codeBook))
        )

        if (!identical(indent, 2) || !identical(OS, "")) {
            xmlfile <- strsplit(as.character(codeBook), "\n", fixed = TRUE)[[1]]

            defaultOS <- Sys.info()[["sysname"]]
            checkArgument(indent, default = 2)
            checkArgument(OS, default = defaultOS)

            enter <- getEnter(OS = ifelse(OS == "", defaultOS, OS))

            xmlfile <- paste(
                prespace(xmlfile, indent),
                collapse = enter
            )

            .validateCodebookXML(xmlfile)
            writeLines(xmlfile, con = to, sep = enter, useBytes = TRUE)
        }
        else {
            .validateCodebookXML(as.character(codeBook))
            xml2::write_xml(codeBook, file = to)
        }
    }
}
