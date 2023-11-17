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
#' an XML node: it has text content, attributes and possibly other child (sub)
#' elements. Although a typical R list perfectly fits this structure, this
#' package uses a different type of list, to accommodate any other requirements.
#'
#' The standard structure of a DDI element is an R list with separate components
#' for: "name", "content", "children" and "attributes". Instead of storing the
#' attributes in the usual R object attributes, the choice of using a regular
#' list component avoids the potential overlap of DDI element attribute names
#' over R's restricted attribute names such as `names` or `class`.
#'
#' This slightly over-complicates the access to the list's (sub)components,
#' instead of the usual `object$component$subcomponent`, these need to be
#' accessed via the "children" component:
#' `object$children$component$children$subcomponent`
#'
#' Dedicated functions are offered to make the manipulation of these children
#' components as smooth as possible. They all make use of the "name" component,
#' which is typically stored in the list component's name. Because each DDI
#' Codebook element has a unique structure, it is important to safeguard its
#' identification through the "name" component, especially if selecting a
#' particular component into a separate R object.
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
#' \code{\link{fromXMList}}
#' \code{\link{toXMList}}
#'
#' @examples
#' stdyDscr <- makeElement("stdyDscr", fill = fill)
#'
#' # the location of the R list sub-element "abstract":
#' stdyDscr$children$citation$children$titlStmt$children$titl
#'
#' # easier to extract with:
#' getChildren("stdyDscr/citation/titlStmt/titl", from = stdyDscr)
#'
#' @export
`makeElement` <- function(name, info = NULL, fill = FALSE, ...) {
    checkElement(name)

    dots <- list(...)

    result <- structure(list(name = name), class = "DDI")

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
            addChildren(info$children, result)
        }

        nms <- setdiff(names(info), c("attributes", "children"))
        if (length(nms) > 0) {
            for (n in nms) {
                result[[n]] <- info[[n]]
            }
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

# Application ID AA00CMJ0UZ
# CD2049820

# https://suchen.mobile.de/fahrzeuge/details.html?id=374269085&action=eyeCatcher&ecol=RED&fr=2021%3A&ft=PETROL&isSearchRequest=true&ml=%3A20000&ms=1900%3B25%3B%3B&od=up&ref=srp&refId=5de26c77-820e-92fa-144c-8c4e948f7786&s=Car&sb=p&searchId=5de26c77-820e-92fa-144c-8c4e948f7786&tr=MANUAL_GEAR&vc=Car
