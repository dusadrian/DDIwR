#' @name DDI-children
#'
#' @title
#' Add/remove/change one or more children or attributes from a DDI Codebook
#' attribute.
#'
#' @description
#' `addChildren()` adds one or more children to a standard DDI Codebook element
#' (see \code{\link{makeElement}}), `anyChildren()` checks if an element has any
#' children at all, `hasChildren()` checks if the element has specific children,
#' `indexChildren()` returns the positions of the children among all containing
#' children, and `getChildren()` extracts them. For attributes and content,
#' there are dedicated functions to `add*()`, `remove*()` and `change*()`.
#'
#' @details
#' Although an XML list generally allows for multiple contents, sometimes spread
#' between the children elements, it is preferable to maintain a single content
#' (eventually separated with carriage return characters for separate lines).
#'
#' Arguments are unique, and can be changed by simply referring to their names.
#'
#' Elements, however, can be repeated. For instance element `var` to describe
#' variables, within the `dataDscr` (data description) sub-element in the
#' `codeBook`. There are as many such `var` elements as the number of variables
#' in the dataset, in which case it is not possible to change a specific `var`
#' element by referring to its name. For this purpose, it is useful to extract
#' the positions of all `var` elements to iterate through, which is the purpose
#' of the function `indexChildren()`.
#'
#' Future versions will allow deep manipulations of child elements using the
#' `xpath` argument.
#'
#' @return An invisible standard DDI element. Functions `any*()` and `has*()`
#' return a logical (vector).
#'
#' @author Adrian Dusa
#'
#' @param children A standard element of class `"DDI"`, or a list of such elements.
#' @param to A standard element of class `"DDI"`.
#' @param from A standard element of class `"DDI"`.
#' @param element A standard element of class `"DDI"`.
#' @param content Character, the text content of a DDI element.
#' @param attrs A list of specific attribute names and values.
#' @param name Character, name(s) of specific child element / attribute.
#' @param overwrite Logical, overwrite the original object in the parent frame.
#' @param xpath Character, a path to a DDI Codebook element.
#' @param ... Other arguments, mainly for internal use.
#'
#' @details If more than one children, they should be grouped into a list.
#'
#' @export
`addChildren` <- function(children, to, overwrite = TRUE, ...) {
    objname <- deparse1(substitute(to))
    dots <- list(...)

    standard <- !missing(children) && is.list(children)
    if (standard) {
        if (is.element(".extra", names(children))) {
            children <- list(children)
        }

        standard <- all(sapply(
            children,
            function(x) is.element(".extra", names(x))
        ))
    }

    if (!standard) {
        admisc::stopError("The argument 'children' is not standard.")
    }

    if (missing(to) || !is.element(".extra", names(to))) {
        admisc::stopError("The argument 'to' is not standard.")
    }

    childnames <- sapply(children, function(x) x$.extra$name)
    names(children) <- childnames

    all_children <- DDIC[[to$.extra$name]]$children

    if (!all(is.element(childnames, all_children))) {
        admisc::stopError("One or more children do not belong to this element.")
    }

    uchildren <- unique(childnames)
    repeatable <- sapply(DDIC[uchildren], function(x) x$repeatable)
    tbl <- table(childnames)
    tbl <- tbl[tbl > 1 & !repeatable]

    if (length(tbl) > 0) {
        admisc::stopError(
            sprintf(
                "These children should not be repeated: %s.",
                paste(names(tbl), collapse = ", ")
            )
        )
    }
    
    uchildren <- intersect(uchildren[!repeatable], names(to))

    if (length(uchildren)) {
        admisc::stopError(
            sprintf(
                "These children already exist and should not be repeated: %s.",
                paste(uchildren, collapse = ", ")
            )
        )
    }

    attrbs <- attributes(to)
    to <- append(to, children)
    attrbs$names <- c(attrbs$names, childnames)

    corder <- order(match(
        attrbs$names,
        c("", all_children, ".extra")
    ))

    to <- to[corder]
    attrbs$names <- attrbs$names[corder]
    attributes(to) <- attrbs

    if (overwrite) {
        admisc::overwrite(objname, to, parent.frame())
    }

    return(invisible(to))
}


# Check if a DDI element has any children
#' @rdname DDI-children
#' @export
`anyChildren` <- function(element) {
    if (missing(element) || !is.list(element)) {
        admisc::stopError("The argument 'element' should be a list.")
    }

    return(
        length(setdiff(names(element), c(".extra", ""))) > 0
    )
}


#' @rdname DDI-children
#' @export
`getChildren` <- function(xpath, from, ...) {
    if (missing(from) || !is.element(".extra", names(from))) {
        admisc::stopError("The argument 'from' is not standard.")
    }

    if (
        missing(xpath) || is.null(xpath) || !is.atomic(xpath) ||
        !is.character(xpath) || length(xpath) != 1
    ) {
        admisc::stopError(
            "Argument 'xpath' should be a character vector of length 1."
        )
    }

    dots <- list(...)

    xpath <- gsub("\\$", "/", xpath)
    xpath <- unlist(strsplit(xpath, split = "/"))

    wextra <- which(xpath == from$.extra$name)
    if (length(wextra) > 0) {
        xpath <- xpath[-seq(wextra)]
    }

    if (length(xpath) == 0) {
        return(from)
    }

    if (!hasChildren(from, xpath[1])) {
        return(NULL)
    }

    if (length(xpath) >= 1) {
        index <- indexChildren(from, xpath[1])
        if (length(index) == 1) {
            return(getChildren(
                paste(xpath, collapse = "/"),
                from = from[[index]]
            ))
        }
        else {
            if (length(xpath) == 1) {
                return(from[index])
            }

            # e.g. codeBook/dataDscr/var/labl
            # and there are certainly, multiple "var" elements in the dataDscr

            admisc::stopError(sprintf(
                "Multiple '%s' elements to subtract '%s' children from.",
                xpath[1], xpath[2]
            ))
        }
    }
}


# Check if a DDI element has specific child(ren)
#' @rdname DDI-children
#' @export
`hasChildren` <- function(element, name) {
    if (
        missing(name) || is.null(name) ||
        !is.atomic(name) || !is.character(name)
    ) {
        admisc::stopError(
            "The argument 'name' should be a character vector."
        )
    }

    if (missing(element) || is.null(element) || !is.list(element)) {
        return(logical(length(name)))
    }

    is.element(name, setdiff(names(element), ".extra"))
}


#' @rdname DDI-children
#' @export
`indexChildren` <- function(element, name) {
    if (is.null(element) || !is.list(element)) {
        return(numeric(0))
    }

    which(names(element) == name)
}


#' @rdname DDI-children
#' @export
`removeChildren` <- function(name, from, overwrite = TRUE, ...) {
    objname <- deparse1(substitute(from))
    dots <- list(...)

    if (
        missing(name) || is.null(name) ||
        !is.atomic(name) || !is.character(name)
    ) {
        admisc::stopError("Argument 'name' should be a character vector.")
    }

    childnames <- names(from)

    if (missing(from) || !is.list(from) || is.null(childnames)) {
        admisc::stopError(
            "To remove children, the argument 'from' should be a named list."
        )
    }

    if (is.element(".extra", name)) {
        admisc::stopError(
            "The component '.extra' should not be removed."
        )
    }

    if (is.element("", name)) {
        admisc::stopError("Unknown child names to remove.")
    }

    childnames <- childnames[is.element(childnames, name)]

    if (length(childnames) > 0) {
        for (child in childnames) {
            from[[child]] <- NULL
        }

        if (overwrite) {
            admisc::overwrite(objname, from, parent.frame())
        }
    }

    return(invisible(from))
}


#' @rdname DDI-children
#' @export
`addContent` <- function(content, to, overwrite = TRUE) {
    objname <- deparse1(substitute(to))

    if (
        missing(content) ||
        is.null(content) ||
        !is.atomic(content) ||
        length(content) != 1
    ) {
        admisc::stopError("The content should be a vector of length 1.")
    }

    if (missing(to) || !is.element(".extra", names(to))) {
        admisc::stopError("The argument 'to' is not standard.")
    }

    nms <- names(to)
    if (any(nms == "")) {
        admisc::stopError("This element already has a content.")
    }

    attrbs <- attributes(to)
    to <- c(list(content), to)
    attrbs$names <- c("", attrbs$names)

    elorder <- order(
        match(
            attrbs$names,
            c(
                "",
                attrbs$names[!is.element(attrbs$names, c(".extra", ""))],
                ".extra"
            )
        )
    )

    to <- to[elorder]
    attrbs$names <- attrbs$names[elorder]
    attributes(to) <- attrbs

    if (overwrite) {
        admisc::overwrite(objname, to, parent.frame())
    }

    return(invisible(to))
}


#' @rdname DDI-children
#' @export
`changeContent` <- function(content, to, overwrite = TRUE) {
    objname <- deparse1(substitute(to))

    if (
        missing(content) ||
        is.null(content) ||
        !is.atomic(content) ||
        length(content) != 1
    ) {
        admisc::stopError("The content should be a vector of length 1.")
    }

    if (missing(to) || !is.element(".extra", names(to))) {
        admisc::stopError("The argument 'to' is not standard.")
    }

    nms <- names(to)
    wcontent <- which(nms == "")
    if (length(wcontent) == 0) {
        admisc::stopError("This element does not have a content to change.")
    }
    else if (length(wcontent) == 1) {
        to[[wcontent]] <- content
    }
    else {
        admisc::stopError("The argument 'to' has multiple content elements.")
    }

    if (overwrite) {
        admisc::overwrite(objname, to, parent.frame())
    }

    return(invisible(to))
}


#' @rdname DDI-children
#' @export
`removeContent` <- function(from, overwrite = TRUE) {
    objname <- deparse1(substitute(from))

    if (missing(from) || !is.element(".extra", names(from))) {
        admisc::stopError("The argument 'from' is not standard.")
    }

    nms <- names(from)
    attrbs <- attributes(from)
    wcontent <- which(nms == "")
    if (length(wcontent) > 0) {
        from <- from[-wcontent]
        nms <- nms[-wcontent]
        if (length(nms) == 0) nms <- NULL
        attrbs$names <- nms
        attributes(from) <- attrbs
    }

    if (overwrite) {
        admisc::overwrite(objname, from, parent.frame())
    }

    return(invisible(from))
}


#' @rdname DDI-children
#' @export
`addAttributes` <- function(attrs, to, overwrite = TRUE) {
    objname <- deparse1(substitute(to))
    attrnames <- names(attrs)

    if (missing(attrs) || is.null(attrnames) || !is.atomic(attrs)) {
        admisc::stopError(
            "The argument 'attrs' should be a vector of named values."
        )
    }

    if (missing(to) || !is.element(".extra", names(to))) {
        admisc::stopError("The argument 'to' is not standard.")
    }

    all_attributes <- unique(c(
        names(DDIC_global_attributes),
        names(DDIC[[to$.extra$name]]$attributes)
    ))

    if (is.null(attrnames) || !all(is.element(attrnames, all_attributes))) {
        admisc::stopError(
            "One or more attributes do not belong to this DDI element."
        )
    }

    attrbs <- attributes(to)
    attrbs <- c(attrbs, attrs)

    # make sure the children are arranged in exactly the order specified
    # by the standard (don't know if that matters, but just in case it does)
    corder <- order(match(names(attrbs), c("names", "class", all_attributes)))
    if (!identical(corder, seq(length(attrbs)))) {
        attrbs <- attrbs[corder]
    }


    attributes(to) <- attrbs

    if (overwrite) {
        admisc::overwrite(objname, to, parent.frame())
    }

    return(invisible(to))
}


# Check if a DDI element has any attributes
#' @rdname DDI-children
#' @export
`anyAttributes` <- function(element) {
    if (missing(element) || !is.list(element)) {
        return(FALSE)
    }

    length(
        setdiff(
            names(attributes(element)),
            c("names", "class")
        )
    ) > 0
}


#' @rdname DDI-children
#' @export
`changeAttributes` <- function(attrs, from, overwrite = TRUE) {
    objname <- deparse1(substitute(from))

    attrnames <- names(attrs)
    if (missing(attrs) || is.null(attrnames) || !is.atomic(attrs)) {
        admisc::stopError(
            "The argument 'attrs' should be a vector of named values."
        )
    }

    if (missing(from) || !is.element(".extra", names(from))) {
        admisc::stopError("The argument 'from' is not standard.")
    }

    all_attributes <- unique(c(
        names(DDIC_global_attributes),
        names(DDIC[[from$.extra$name]]$attributes)
    ))

    if (is.null(attrnames) || !all(is.element(attrnames, all_attributes))) {
        admisc::stopError("One or more attributes do not belong to this element.")
    }

    attrbs <- attributes(from)

    if (
        !all(is.element(attrnames, names(attrbs)))
    ) {
        admisc::stopError("Inexisting attribute(s) to change.")
    }

    attrbs[attrnames] <- attrs
    attributes(from) <- attrbs

    if (overwrite) {
        admisc::overwrite(objname, from, parent.frame())
    }

    return(invisible(from))
}


# Check if a DDI element has specific attribute(s)
#' @rdname DDI-children
#' @export
`hasAttributes` <- function(element, name) {
    if (missing(element) || !is.list(element)) {
        admisc::stopError("The argument 'element' should be a list.")
    }

    if (
        missing(name) || is.null(name) ||
        !is.atomic(name) || !is.character(name)
    ) {
        admisc::stopError(
            "The argument 'name' should be a character vector."
        )
    }

    is.element(name, names(attributes(element)))
}


#' @rdname DDI-children
#' @export
`removeAttributes` <- function(name, from, overwrite = TRUE) {
    objname <- deparse1(substitute(from))

    if (
        missing(name) ||
        is.null(name) ||
        !is.atomic(name) ||
        !is.character(name)
    ) {
        admisc::stopError("Argument 'name' should be a character vector of element name(s).")
    }

    all_attributes <- unique(c(
        names(DDIC_global_attributes),
        names(DDIC[[from$.extra$name]]$attributes)
    ))

    if (!all(is.element(name, all_attributes))) {
        admisc::stopError("One or more attributes do not belong to this element.")
    }

    if (missing(from) || !is.element(".extra", names(from))) {
        admisc::stopError("The argument 'from' is not standard.")
    }

    attrbs <- attributes(from)
    if (any(is.element(name, names(attrbs)))) {
        attrbs <- attributes(from)
        for (n in name) {
            attrbs[[n]] <- NULL
        }

        attributes(from) <- attrbs

        if (overwrite) {
            admisc::overwrite(objname, from, parent.frame())
        }
    }

    return(invisible(from))
}
