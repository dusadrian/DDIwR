#' @name exportDDI
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
#' The `codeBook` object is exported as provided, and it is the user's
#' responsibility to test its validity against the XML schema. Most of these
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
#'
#' @return
#' An XML file containing a DDI version 2.6 metadata.
#'
#' @seealso
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html}
#'
#' @examples
#'
#' \dontrun{
#' exportDDI(codeBook, file = "codebook.xml")
#'
#' # using a namespace
#' exportDDI(codeBook, file = "codebook.xml", xmlns = "ddi")
#' }
#'
#' @author Adrian Dusa
#'
#' @param codeBook A standard element of class `"DDI"`.
#'
#' @param file
#' either a character string naming a file or a connection open for
#' writing. "" indicates output to the console
#'
#' @param OS
#' The target operating system, for the eol - end of line character(s)
#'
#' @param indent Indent width, in number of spaces
#'
#' @param ... Other arguments, mainly for internal use
#'
#' @export
`exportDDI` <- function(codeBook, file = "", OS = "", indent = 2, ...) {

    # https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html

    # validation procedure:
    # https://ddialliance.org/Specification/DDI-Codebook/2.5/
    # schema <- read_xml("path/to/ddi_2_5_1/schemas/codebook.xsd")
    # doc <- read_xml("path/to/filetovalidate.xml")
    # xml_validate(doc, schema)
    # or using the CESSDA Metadata Validator
    # https://cmv.cessda.eu/#!validation
    # with its validation constraints
    # https://cmv.cessda.eu/documentation/constraints.html

    dots <- list(...)

    if (codeBook$.extra$name == "codeBook") {
        if (any(
            is.element(
                c("IDNo", "titl", "agency", "URI", "distrbtr", "abstract", "level"),
                names(dots)
            )
        )) {

            if (!hasChildren(codeBook, "stdyDscr")) {
                addChildren(
                    makeElement("stdyDscr", fill = TRUE, ... = ...),
                    to = codeBook
                )
            }

            if (!hasChildren(codeBook, "otherMat")) {
                addChildren(
                    makeElement("otherMat", fill = TRUE, ... = ...),
                    to = codeBook
                )
            }
        }

        if (!hasChildren(codeBook, "docDscr")) {
            addChildren(
                makeElement("docDscr", fill = TRUE, ... = ...),
                to = codeBook
            )
        }
    }

    xmlang <- checkDots(dots$xmlang, default = "en")
    variables <- getElement(dots, "variables")
    varxmlang <- ifelse(
        is.null(variables),
        FALSE,
        any(sapply(variables, function(x) {
            is.element("xmlang", names(x))
        }))
    )

    if (!isFALSE(dots$monolang) & !varxmlang) {
        codeBook <- changeXMLang(codeBook, remove = TRUE)
        attr(codeBook, "xml:lang") <- xmlang
    }
    else {
        codeBook <- changeXMLang(codeBook)
    }


    if (isTRUE(dots$dataDscr_directly_in_XML)) {
        data <- dots$data
        embed <- dots$embed
        
        addChildren(makeElement("dataDscr"), to = codeBook)

        XMLhashes <- makeXMLcodeBook(
            variables,
            data = data,
            DDI = FALSE,
            ... = ...
        )

        attr(data, "hashes") <- XMLhashes[[2]]

        if (embed) {
            addChildren(makeNotes(data), to = codeBook$fileDscr)
        }
        else {
            tp_file <- treatPath(file, type = "*", single = TRUE)
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
        
        codeBook <- as_xml_document(
            list(codeBook = removeExtra(codeBook))
        )
        dns <- getDNS(codeBook)

        xml2::xml_replace(
            xml_find_first(codeBook, sprintf("/%scodeBook/dataDscr", dns)),
            xml_find_first(XMLhashes[[1]], "/d1:codeBook/d1:dataDscr")
        )
    }
    else {
        codeBook <- xml2::as_xml_document(
            list(codeBook = removeExtra(codeBook))
        )
    }

    xml2::write_xml(codeBook, file = file)
    xmlfile <- readLines(file)
    xmlfile <- gsub(
        "<dataDscr xmlns=\"ddi:codebook:2_5\">",
        "<dataDscr>",
        xmlfile
    )

    if (!identical(indent, 2) || !identical(OS, "")) {
        # xmlfile <- readLines(file)

        defaultOS <- Sys.info()[["sysname"]]
        checkArgument(indent, default = 2)
        checkArgument(OS, default = defaultOS)

        enter <- getEnter(OS = ifelse(OS == "", defaultOS, OS))

        xmlfile <- paste(
            prespace(xmlfile, indent),
            collapse = enter
        )
    }

    writeLines(
        xmlfile,
        con = file
    )
}
