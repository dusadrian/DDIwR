#' @name makeElement
#'
#' @title
#' Make a DDI Codebook element
#'
#' @description Creates a standard DDI element.
#'
#' @return A standard list element of class `"DDI"` with reserved component names.
#'
#' @param name Character, a DDI Codebook element name.
#' @param info A named list object containing information necessary to create
#' the element.
#' @param fill Logical, fill the element with arbitrary values for its
#' mandatory children and attributes
#' @param ... Other arguments, see Details.
#'
#' @details The structure of a DDI element in R follows the usual structure of
#' an XML node, as returned by the function `as_list()` from package **xml2**,
#' with one additional (first) component named ".extra" to accommodate any other
#' information that is not part of the DDI element.
#'
#' In the DDI Codebook, most elements and their attributes are optional, but
#' some are mandatory. In case of attributes, some become mandatory only if the
#' element itself is present. The mandatory elements need to be present in the
#' final version of the Codebook, to pass the validation against the XML schema.
#'
#' By activating the argument `fill`, this function creates DDI elements
#' containing all mandatory (sub)elements and (their) attributes, filled with
#' arbitrary values that can be changed later on. Some recommended elements are
#' also filled, as expected by the CESSDA Data Catalogue profile for DDI
#' Codebook.
#'
#' By default, the Codebook is assumed to have a single language for all
#' elements. This argument `monolang` can be deactivated through the `...` gate,
#' in which situation the appropriate elements will receive a default argument
#' `xmlang = "en"`. For other languages, that argument can also be provided
#' through the `...` gate.
#'
#' One such DDI Codebook element is the `stdyDscr` (Study Description), with the
#' associated mandatory children, for instance title, ID number, distributor,
#' citation, abstract etc.
#'
#' @author Adrian Dusa
#'
#' @seealso
#' \code{\link{addChildren}}
#' \code{\link{getChildren}}
#' \code{\link{showDetails}}
#'
#' @examples
#' stdyDscr <- makeElement("stdyDscr", fill = TRUE)
#'
#' # easier to extract with:
#' getChildren("stdyDscr/citation/titlStmt/titl", from = stdyDscr)
#'
#' @export
`makeElement` <- function(name, info = NULL, fill = FALSE, ...) {
    checkElement(name)

    dots <- list(...)
    prodDateTime <- getDateTime()
    version <- as.character(packageVersion("DDIwR"))

    result <- list(.extra = list(name = name))

    if (isTRUE(fill)) {
        IDNo <- checkDots(dots$IDNo, default = "S0000")
        titl <- checkDots(dots$titl, default = "Generic title")
        agency <- checkDots(dots$agency, default = "default")
        URI <- checkDots(dots$URI, default = "http://www.default.eu")
        distrbtr <- checkDots(dots$distrbtr, default = "Name of the distributing institution")
        abstract <- checkDots(dots$abstract, default = "Study abstract")
        level <- checkDots(dots$level, default = "0")
        xmlang <- checkDots(dots$xmlang, default = "en")
        monolang <- !isFALSE(dots$monolang)

        info <- NULL
        if (identical(name, "IDNo")) {
            IDNo <- makeElement("IDNo", list(
                content = IDNo
            ))
            addAttributes(list(agency = agency), to = IDNo)
            return(IDNo)
        }
        else if (identical(name, "titl")) {
            titl <- makeElement("titl", list(
                content = titl
            ))
            if (!monolang) {
                addAttributes(list(xmlang = xmlang), to = titl)
            }
            return(titl)
        }
        else if (identical(name, "titlStmt")) {
            titlStmt <- makeElement("titlStmt")
            addChildren(
                list(
                    makeElement("titl", fill = fill, ... = ...),
                    makeElement("IDNo", fill = fill, ... = ...)
                ),
                to = titlStmt
            )
            return(titlStmt)
        }
        else if (identical(name, "distrbtr")) {
            distrbtr <- makeElement("distrbtr", list(
                content = distrbtr
            ))
            addAttributes(list(xmlang = xmlang), to = distrbtr)
            return(distrbtr)
        }
        else if (identical(name, "distStmt")) {
            distStmt <- makeElement("distStmt")
            addChildren(
                makeElement("distrbtr", fill = fill, ... = ...),
                to = distStmt
            )
            return(distStmt)
        }
        else if (identical(name, "holdings")) {
            holdings <- makeElement("holdings", list(
                content = "Description of the study holdings"
            ))
            addAttributes(
                list(URI = URI),
                to = holdings
            )
            return(holdings)
        }
        else if (identical(name, "citation")) {
            citation <- makeElement("citation")
            addChildren(
                list(
                    makeElement("titlStmt", fill = fill, ... = ...),
                    makeElement("distStmt", fill = fill, ... = ...),
                    makeElement("holdings", fill = fill, ... = ...)
                ),
                to = citation
            )
            return(citation)
        }
        else if (identical(name, "abstract")) {
            abstract <- makeElement("abstract", list(
                content = "Study abstract"
            ))
            if (!monolang) {
                addAttributes(list(xmlang = xmlang), to = abstract)
            }
            return(abstract)
        }
        else if (identical(name, "stdyInfo")) {
            stdyInfo <- makeElement("stdyInfo")
            addChildren(
                makeElement("abstract", fill = fill, ... = ...),
                to = stdyInfo
            )
            return(stdyInfo)
        }
        else if (identical(name, "stdyDscr")) {
            stdyDscr <- makeElement("stdyDscr")
            addChildren(
                list(
                    makeElement("citation", fill = fill, ... = ...),
                    makeElement("stdyInfo", fill = fill, ... = ...)
                ),
                to = stdyDscr
            )
            return(stdyDscr)
        }
        else if (identical(name, "prodDate")) {
            prodDate <- makeElement("prodDate")
            addAttributes(
                list(date = substring(prodDateTime, 1, 19)),
                to = prodDate
            )
            addContent(prodDateTime, to = prodDate)
            return(prodDate)
        }
        else if (identical(name, "software")) {
            software <- makeElement("software")
            addAttributes(
                list(version = version),
                to = software
            )
            addContent("R package DDIwR", to = software)
            return(software)
        }
        else if (identical(name, "prodStmt")) {
            prodStmt <- makeElement("prodStmt")
            software <- makeElement("software", fill = fill, ... = ...)
            addAttributes(list(version = version), to = software)
            addChildren(
                list(
                    makeElement("prodDate", fill = fill, ... = ...),
                    software
                ),
                to = prodStmt
            )
            return(prodStmt)
        }
        else if (identical(name, "docDscr")) {
            docDscr <- makeElement("docDscr")
            addChildren(
                makeElement(
                    "citation",
                    info = list(
                        children = list(
                            makeElement("titlStmt", fill = fill, ... = ...),
                            makeElement("prodStmt", fill = fill, ... = ...)
                        )
                    )
                ),
                to = docDscr
            )
            return(docDscr)
        }
        else if (identical(name, "otherMat")) {
            otherMat <- makeElement("otherMat")
            addAttributes(list(level = level), to = otherMat)
            return(otherMat)
        }
        else {
            admisc::stopError("No default for this element.")
        }
    }

    if (!is.null(info)) {
        if (is.element("attributes", names(info))) {
            addAttributes(info$attributes, result)
        }

        if (is.element("children", names(info))) {
            addChildren(info$children, to = result)
        }

        if (is.element("content", names(info))) {
            addContent(info$content, result)
        }
    }

    if (name == "codeBook") {
        if (length(result$attributes) > 0) {
            attnms <- names(DDIC$codeBook$attributes)
            for (name in attnms) {
                if (
                    is.null(result$attributes[[name]]) &&
                    length(DDIC$codeBook$attributes$default) > 0
                ) {
                    result$attributes[[name]] <- DDIC$codeBook$attributes$default
                }
            }
        }
        else {
            # default attributes need to be added
            defaults <- lapply(DDIC$codeBook$attributes, function(x) x$default)

            addAttributes(
                defaults[sapply(defaults, length) > 0],
                to = result
            )
        }
    }

    return(result)
}

# which(sapply(DDIC, function(x) {
#     any(is.element(c(x$parents, x$children), names(x$attributes)))
#     # any(is.element(names(x$attributes), c(x$parents, x$children)))
# }))
#   backward derivation    forward       qstn        var
#         16         71        107        167        242
