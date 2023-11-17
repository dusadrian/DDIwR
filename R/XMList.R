#' @name XMList
#'
#' @title
#' Convert to and from an XML list of class `"xml_document"`, as used by package
#' **xml2**.
#'
#' @description to add
#'
#' @return `fromXMList()` returns a standard list element of class `"DDI"`, and
#' `toXMList()` converts back to an XML list to be written into an ".xml" file.
#'
#' @param xmlist An R object of class `"xml_document"`
#' @param element A standard DDI Codebook list element.
#' @param ... Other arguments, mainly for internal use.
#'
#' @details The argument `xmlist` received an object created by the function
#' `read_xml()` in package **xml2**. Both functions are useful to import /
#' export from a DDI Codebook expressed in XML, to a standard R list of class
#' `"DDI"`.
#'
#' @author Adrian Dusa
#'
#' @export
`fromXMList` <- function(xmlist, ...) {
    dots <- list(...)
    parent <- dots$parent

    nms <- names(xmlist)
    wempty <- which(nms == "")
    if (length(wempty) > 0) {
        for (i in rev(wempty)) {
            text <- admisc::trimstr(xmlist[[i]])
            if (identical(text, "")) {
                xmlist <- xmlist[-i]
                nms <- nms[-i]
            }
            else {
                xmlist[[i]] <- text
            }
        }
    }

    if (is.null(nms)) {
        # reached a simple element
        parent$content <- cleanup(admisc::trimstr(unlist(xmlist)))
    }
    else {
        if (!all(is.element(setdiff(nms, ""), names(DDIC)))) {
            admisc::stopError(
                "This XML file contains elements that are not part of the DDI Codebook standard."
            )
        }

        for (i in seq_along(nms)) {
            if (nms[i] == "") {
                parent$content <- cleanup(admisc::trimstr(xmlist[[i]]))
            }
            else {
                child <- fromXMList(xmlist[[i]], parent = makeElement(nms[i]))
                attrs <- attributes(xmlist[[i]])
                if (length(attrs) > 0) {
                    attrs <- attrs[setdiff(names(attrs), "names")]
                    if (length(attrs) > 0) {
                        names(attrs)[names(attrs) == "lang"] <- "xmlang"
                        names(attrs)[names(attrs) == "schemaLocation"] <- "xsi:schemaLocation"
                        addAttributes(attrs, to = child)
                    }
                }

                if (is.null(parent)) {
                    parent <- child
                }
                else {
                    addChildren(child, to = parent)
                }
            }
        }
    }

    return(parent)
}


#' @rdname XMList
#' @export
`toXMList` <- function(element) {
    result <- list()

    if (length(element$content) > 0) {
        result <- append(result, list(element$content))
    }

    if (length(element$children) > 0) {
        nms <- names(element$children)
        result <- append(
            result,
            lapply(element$children, function(x) {
                toXMList(x)
            })
        )
    }

    attrs <- element$attributes

    if (!is.null(attrs)) {
        nms <- names(attrs)
        nmslang <- gsub("xmlang", "xml:lang", nms)
        for (i in seq_along(attrs)) {
            attr(result, nmslang[i]) <- attrs[[nms[i]]]
        }
    }

    return(result)
}
