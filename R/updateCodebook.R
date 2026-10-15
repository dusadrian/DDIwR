#' @name updateCodebook
#'
#' @title
#' Update Codebook.
#'
#' @description
#' Update an XML file containing a DDI Codebook.
#'
#' @details
#' This function replaces entire Codebook sections. Any such section present in
#' the R object will replace the corresponding section from the XML document.
#' Sections are matched as XML elements, including in compact or prefixed XML,
#' and inserted in schema order. Repeated sections are replaced as a group.
#' The document namespace, root attributes and untouched sections are preserved.
#' Both inputs must have a `codeBook` root. Invalid root inputs are rejected
#' before the file is written.
#' The finished document must conform to the bundled DDI Codebook 2.6 schema.
#' Validation is mandatory and offline; failure leaves the original file
#' unchanged. Documents using another DDI namespace are rejected rather than
#' silently converted to 2.6.
#'
#' @author Adrian Dusa
#'
#' @param xmlfile A path to a DDI Codebook XML document.
#' @param with An R object containing a root `codeBook` element.
#' @param ... Other internal arguments.
#'
#' @export
`updateCodebook` <- function(xmlfile, with, ...) {
    if (
        !is.list(with) || !is.list(with$.extra) ||
        !identical(with$.extra$name, "codeBook")
    ) {
        admisc::stopError("The argument 'with' must be a standard codeBook element.")
    }

    checkXMList(with)
    DDIC <- get("DDIC", envir = cacheEnv)
    sections <- DDIC$codeBook$children
    dots <- list(...)
    indent <- checkDots(dots$indent, default = 2)
    enter <- checkDots(dots$enter,
        default = getEnter(OS = Sys.info()[["sysname"]]))
    xml <- getXML(xmlfile)
    root <- xml2::xml_root(xml)

    if (!identical(xml2::xml_name(root), "codeBook")) {
        admisc::stopError("The XML file must contain a codeBook root element.")
    }

    if (!all(is.element(xml2::xml_name(xml2::xml_children(root)), sections))) {
        admisc::stopError(
            "The XML file contains elements that are not Codebook sections."
        )
    }

    monolang <- isTRUE(with$.extra$monolang)
    codeBook <- removeExtra(changeXMLang(with, remove = monolang))
    namespace <- xml2::xml_find_chr(root, "namespace-uri(.)")
    attr(codeBook, "xmlns") <- namespace
    replacements <- xml2::xml_children(xml2::as_xml_document(
        list(codeBook = codeBook)
    ))
    replacement_names <- xml2::xml_name(replacements)

    # Work with elements rather than lines, including repeated and empty sections.
    for (section in unique(replacement_names)) {
        existing <- xml2::xml_children(root)
        xml2::xml_remove(existing[xml2::xml_name(existing) == section])
        following <- xml2::xml_children(root)
        following <- following[
            match(xml2::xml_name(following), sections) > match(section, sections)
        ]

        for (node in replacements[replacement_names == section]) {
            if (length(following) > 0L) {
                xml2::xml_add_sibling(following[[1]], node, .where = "before")
            }
            else {
                xml2::xml_add_child(root, node)
            }
        }
    }

    lines <- strsplit(as.character(xml), "\n", fixed = TRUE)[[1]]
    output <- paste(prespace(lines, indent), collapse = enter)
    .validateCodebookXML(output)
    writeLines(output, con = xmlfile, sep = enter, useBytes = TRUE)
}
