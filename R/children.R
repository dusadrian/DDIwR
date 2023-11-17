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
#' @param attributes A list of specific attributes and values.
#' @param name Character, name(s) of specific child element / attribute.
#' @param overwrite Logical, overwrite the original object in the parent frame.
#' @param xpath Character, a path to a DDI Codebook element.
#' @param ... Other arguments, mainly for internal use.
#'
#' @details If more than one children, they should be grouped into a list.
#'
#' @export
`addChildren` <- function(children, to, overwrite = TRUE, ...) {
    objname <- deparse(substitute(to))
    dots <- list(...)

    standard <- !missing(children) && is.list(children)
    if (standard) {
        if (inherits(children, "DDI")) {
            children <- list(children)
        }

        standard <- all(sapply(
            children,
            function(x) inherits(x, "DDI")
        ))
    }

    if (!standard) {
        admisc::stopError("The argument 'children' is not standard.")
    }

    if (missing(to) || !inherits(to, "DDI")) {
        admisc::stopError("The argument 'to' is not standard.")
    }

    all_children <- DDIC[[to$name]]$children
    childnames <- sapply(children, function(x) x$name)

    if (!all(is.element(childnames, all_children))) {
        admisc::stopError("One or more children do not belong to this element.")
    }

    if (is.null(to$children)) {
        # to$children <- lapply(children, unclass)
        to$children <- children
    }
    else {
        # to$children <- append(to$children, lapply(children, unclass))
        to$children <- append(to$children, children)
    }


    # just in case there were already, other children present
    childnames <- sapply(to$children, function(x) x$name)
    names(to$children) <- childnames

    # make sure the children are arranged in exactly the order specified
    # by the standard (don't know if that matters, but just in case it does)
    corder <- order(match(childnames, all_children))
    if (!identical(corder, seq(length(childnames)))) {
        to$children <- to$children[corder]
    }

    if (overwrite) {
        admisc::overwrite(objname, to, parent.frame())
    }

    return(invisible(to))
}


# Check if a DDI element has any children
#' @rdname DDI-children
#' @export
`anyChildren` <- function(element) {
    if (missing(element) || !inherits(element, "DDI")) {
        admisc::stopError("The argument 'element' is not standard.")
    }

    length(element$children) > 0
}


#' @rdname DDI-children
#' @export
`getChildren` <- function(xpath, from, ...) {
    if (missing(from) || !inherits(from, "DDI")) {
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

    if (any(xpath == from$name)) {
        xpath <- xpath[-seq(which(xpath == from$name))]
    }

    if (length(xpath) == 0) {
        return(from)
    }

    if (!hasChildren(from, xpath[1])) {
        if (isTRUE(dots$skiperror)) {
            return(NULL)
        }

        admisc::stopError(
            sprintf(
                "Inexisting (sub)element %s in the element %s.",
                xpath[1], from$name
            )
        )
    }

    if (length(xpath) >= 1) {
        index <- indexChildren(from, xpath[1])
        if (length(index) == 1) {
            return(getChildren(
                paste(xpath, collapse = "/"),
                from = from$children[[index]]
            ))
        }
        else {
            if (length(xpath) == 1) {
                return(from$children[index])
            }

            # e.g. codeBook/dataDscr/var/labl
            # and there are certainly, multiple "var" elements in the dataDscr

            admisc::stopError("Only single elements can be currently extracted.")
        }
    }
}


# Check if a DDI element has specific child(ren)
#' @rdname DDI-children
#' @export
`hasChildren` <- function(element, name) {
    if (missing(element) || !inherits(element, "DDI")) {
        admisc::stopError("The argument 'element' is not standard.")
    }

    if (
        missing(name) || is.null(name) ||
        !is.atomic(name) || !is.character(name)
    ) {
        admisc::stopError(
            "The argument 'name' should be a character vector."
        )
    }

    is.element(name, names(element$children))
}


#' @rdname DDI-children
#' @export
`indexChildren` <- function(element, name) {
    if (!hasChildren(element, name)) {
        return(numeric(0))
    }

    which(names(element$children) == name)
}


#' @rdname DDI-children
#' @export
`removeChildren` <- function(name, from, overwrite = TRUE, ...) {
    objname <- deparse(substitute(from))
    dots <- list(...)

    if (missing(name) || is.null(name) || !is.atomic(name) || !is.character(name)) {
        admisc::stopError("Argument 'name' should be a character vector.")
    }

    if (missing(from) || !inherits(from, "DDI")) {
        admisc::stopError("The argument 'from' is not standard.")
    }

    if (length(from$children) > 0) {
        childnames <- names(from$children)
        if (any(is.element(childnames, name))) {
            from$children <- from$children[-which(is.element(childnames, name))]
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
    objname <- deparse(substitute(to))

    if (
        missing(content) ||
        is.null(content) ||
        !is.atomic(content) ||
        length(content) != 1
    ) {
        admisc::stopError("The content should be a vector of length 1.")
    }

    if (missing(to) || !inherits(to, "DDI")) {
        admisc::stopError("The argument 'to' is not standard.")
    }

    if (is.null(to$content)) {
        to$content <- content
    }
    else {
        admisc::stopError("This element already has a content.")
    }

    if (overwrite) {
        admisc::overwrite(objname, to, parent.frame())
    }

    return(invisible(to))
}


#' @rdname DDI-children
#' @export
`changeContent` <- function(content, to, overwrite = TRUE) {
    objname <- deparse(substitute(to))

    if (
        missing(content) ||
        is.null(content) ||
        !is.atomic(content) ||
        length(content) != 1
    ) {
        admisc::stopError("The content should be a vector of length 1.")
    }

    if (missing(to) || !inherits(to, "DDI")) {
        admisc::stopError("The argument 'to' is not standard.")
    }

    if (is.null(to$content)) {
        admisc::stopError("This element does not have a content to change.")
    }
    else {
        to$content <- content
    }

    if (overwrite) {
        admisc::overwrite(objname, to, parent.frame())
    }

    return(invisible(to))
}


#' @rdname DDI-children
#' @export
`removeContent` <- function(from, overwrite = TRUE) {
    objname <- deparse(substitute(from))

    if (missing(from) || !inherits(from, "DDI")) {
        admisc::stopError("The argument 'from' is not standard.")
    }

    from$content <- NULL
    if (overwrite) {
        admisc::overwrite(objname, from, parent.frame())
    }

    return(invisible(from))
}


#' @rdname DDI-children
#' @export
`addAttributes` <- function(attributes, to, overwrite = TRUE) {
    objname <- deparse(substitute(to))
    # if (to$name == "codeBook") {
    #     print(to$attributes)
    # }

    if (missing(attributes) || !is.list(attributes)) {
        admisc::stopError("The argument 'attributes' should be a list of named values.")
    }

    if (missing(to) || !inherits(to, "DDI")) {
        admisc::stopError("The argument 'to' is not standard.")
    }

    all_attributes <- unique(c(
        names(DDIC_global_attributes),
        names(DDIC[[to$name]]$attributes)
    ))
    attrnames <- names(attributes)

    if (is.null(attrnames) || !all(is.element(attrnames, all_attributes))) {
        admisc::stopError("One or more attributes do not belong to this element.")
    }

    for (i in names(attributes)) {
        to$attributes[[i]] <- attributes[[i]]
    }

    # just in case there were already, other attributes present
    attrnames <- names(to$attributes)

    # make sure the children are arranged in exactly the order specified
    # by the standard (don't know if that matters, but just in case it does)
    corder <- order(match(attrnames, all_attributes))
    if (!identical(corder, seq(length(attrnames)))) {
        to$attributes <- to$attributes[corder]
    }

    if (overwrite) {
        admisc::overwrite(objname, to, parent.frame())
    }

    return(invisible(to))
}


# Check if a DDI element has any attributes
#' @rdname DDI-children
#' @export
`anyAttributes` <- function(element) {
    if (missing(element) || !inherits(element, "DDI")) {
        admisc::stopError("The argument 'element' is not standard.")
    }

    length(element$attributes) > 0
}


#' @rdname DDI-children
#' @export
`changeAttributes` <- function(attributes, from, overwrite = TRUE) {
    objname <- deparse(substitute(from))

    if (missing(attributes) || !is.list(attributes)) {
        admisc::stopError("The argument 'attributes' should be a list of named values.")
    }

    if (missing(from) || !inherits(from, "DDI")) {
        admisc::stopError("The argument 'from' is not standard.")
    }

    if (
        is.null(names(attributes)) ||
        !all(is.element(names(attributes), names(to$attributes)))
    ) {
        admisc::stopError("Inexisting attributes to change.")
    }

    for (i in names(attributes)) {
        to$attributes[[i]] <- attributes[[i]]
    }

    if (overwrite) {
        admisc::overwrite(objname, from, parent.frame())
    }

    return(invisible(to))
}


# Check if a DDI element has specific attribute(s)
#' @rdname DDI-children
#' @export
`hasAttributes` <- function(element, name) {
    if (missing(element) || !inherits(element, "DDI")) {
        admisc::stopError("The argument 'element' is not standard.")
    }

    if (
        missing(name) || is.null(name) ||
        !is.atomic(name) || !is.character(name)
    ) {
        admisc::stopError(
            "The argument 'name' should be a character vector."
        )
    }

    is.element(name, names(element$attributes))
}


#' @rdname DDI-children
#' @export
`removeAttributes` <- function(name, from, overwrite = TRUE) {
    objname <- deparse(substitute(from))

    if (
        missing(name) ||
        is.null(name) ||
        !is.atomic(name) ||
        !is.character(name)
    ) {
        admisc::stopError("Argument 'name' should be a character vector of element name(s).")
    }

    if (missing(from) || !inherits(from, "DDI")) {
        admisc::stopError("The argument 'from' is not standard.")
    }

    if (length(from$attributes) > 0) {
        attrnames <- names(from$attributes)
        if (any(is.element(attrnames, name))) {
            from$attributes <- from$attributes[-which(is.element(attrnames, name))]
        }
        if (overwrite) {
            admisc::overwrite(objname, from, parent.frame())
        }
    }

    return(invisible(from))
}
