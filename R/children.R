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
#
#' XPath resolution accepts indexed segments using `[n]`. When an index is
#' missing, the first matching element is selected.
#'
#' Arguments are unique, and can be changed by simply referring to their names.
#'
#' Elements can be repeated. For example, `dataDscr` contains one `var` element
#' per dataset variable. When multiple `var` elements exist, referring only to
#' the name is ambiguous. Use indexed xpaths like `var[3]` to target a specific
#' instance, or use `indexChildren()` to list all positions for iteration.
#'
#' @return An invisible standard DDI element. Functions `any*()` and `has*()`
#' return a logical (vector).
#'
#' @author Adrian Dusa
#'
#' @param children A standard element of class `"DDI"`, or a list of such elements.
#' @param to A standard element of class `"DDI"`, or an xpath string pointing
#' to a target element.
#' @param from A standard element of class `"DDI"`, or an xpath string pointing
#' to a target element.
#' @param element A standard element of class `"DDI"`.
#' @param content Character, the text content of a DDI element.
#' @param attrs A list of specific attribute names and values.
#' @param name Character, name(s) of specific child element / attribute.
#' @param overwrite Logical, overwrite the original object in the parent frame.
#' @param xpath Character, an xpath to a DDI Codebook element. Indexed segments
#' are supported using square brackets, e.g. `codeBook/dataDscr/var[3]`.
#' Missing indexes default to the first matching element.
#' @param ... Other arguments, mainly for internal use.
#'
#' @details If more than one children, they should be grouped into a list.
#' Functions `addContent`, `changeContent`, `removeContent`, `addAttributes`,
#' `changeAttributes`, and `removeAttributes` accept either a standard DDI element
#' or a character `xpath`. When an xpath is provided, the target element is
#' resolved and replaced in the root element.
#'
#' @export
`addChildren` <- function(children, to, overwrite = TRUE, ...) {

    DDIC <- get("DDIC", envir = cacheEnv)

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

    all_children <- unlist(DDIC[[to$.extra$name]]$children)

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

    nonrep <- intersect(uchildren[!repeatable], names(to))

    if (length(nonrep)) {
        admisc::stopError(
            sprintf(
                "These children already exist and should not be repeated: %s.",
                paste(nonrep, collapse = ", ")
            )
        )
    }

    choice <- NULL
    tochildren <- DDIC[[to$.extra$name]]$children
    if (!is.null(tochildren)) {
        choice <- tochildren$choice
    }


    if (!is.null(choice)) {
        existing <- names(to)
        uchildren <- setdiff(uchildren, existing)

        restriction <- FALSE

        if (identical(existing, ".extra")) {
            if (length(uchildren) > 1) {
                restriction <- TRUE
            }
        }
        else if (length(uchildren) && any(is.element(uchildren, choice))) {
            restriction <- TRUE
        }

        if (restriction) {
            admisc::stopError(
                sprintf(
                    "Choice restriction, only one of these children should be added: %s.",
                    paste(choice, collapse = ", ")
                )
            )
        }
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

    nms <- names(to)
    if (length(nms) > 0) {
        for (i in seq_along(nms)) {
            if (!identical(nms[i], ".extra") && !identical(nms[i], "")) {
                to[[i]]$.extra$index <- sum(nms[seq_len(i)] == nms[i])
            }
        }
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

    base_segs <- sub("\\[(\\d+)\\]$", "", xpath, perl = TRUE)

    wextra <- which(base_segs == from$.extra$name)
    if (length(wextra) > 0) {
        xpath <- xpath[-seq(wextra)]
    }

    if (length(xpath) == 0) {
        return(from)
    }

    parse_seg <- function(s) {
        m <- regexec("^([^\\[]+)(\\[(\\d+)\\])?$", s)
        r <- regmatches(s, m)[[1]]
        if (length(r) == 0) return(NULL)
        name <- r[2]
        idx <- NA_integer_
        if (length(r) >= 4 && nchar(r[4]) > 0) {
            idx <- as.integer(r[4])
        }
        list(name = name, index = idx)
    }

    info0 <- parse_seg(xpath[1])
    if (is.null(info0) || is.na(info0$name)) {
        admisc::stopError("Invalid xpath segment.")
    }

    if (!hasChildren(from, info0$name)) {
        return(NULL)
    }

    if (length(xpath) >= 1) {
        info <- info0
        index <- indexChildren(from, info$name)
        if (length(index) == 0) {
            return(NULL)
        }

        if (length(index) == 1) {
            return(getChildren(
                paste(xpath, collapse = "/"),
                from = from[[index]]
            ))
        }
        else {
            if (length(xpath) == 1) {
                if (!is.na(info$index)) {
                    if (info$index < 1 || info$index > length(index)) {
                        admisc::stopError("Index out of range for xpath segment.")
                    }
                    return(from[index[info$index]])
                }
                return(from[index])
            }

            if (is.na(info$index)) {
                # Default to the first element when index is missing
                return(getChildren(
                    paste(xpath[-1], collapse = "/"),
                    from = from[[index[1]]]
                ))
            }

            if (info$index < 1 || info$index > length(index)) {
                admisc::stopError("Index out of range for xpath segment.")
            }

            return(getChildren(
                paste(xpath[-1], collapse = "/"),
                from = from[[index[info$index]]]
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

    if (is.character(to) && length(to) == 1) {
        res <- resolve_xpath_target(to, env = parent.frame())
        node <- addContent(content, to = res$node, overwrite = FALSE)
        if (identical(res$canonical, res$rootname)) {
            if (overwrite) {
                admisc::overwrite(res$rootname, node, parent.frame())
            }
        } else if (overwrite) {
            replaceChild(res$canonical, with = node, overwrite = TRUE)
        }
        return(invisible(node))
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
`makePath` <- function(xpath, from, overwrite = TRUE, ...) {
    objname <- deparse1(substitute(from))

    if (
        missing(xpath) || is.null(xpath) || !is.atomic(xpath) ||
        !is.character(xpath) || length(xpath) != 1
    ) {
        admisc::stopError("Argument 'xpath' should be a character vector of length 1.")
    }

    if (missing(from) || !is.element(".extra", names(from))) {
        admisc::stopError("The argument 'from' is not standard.")
    }

    segs <- unlist(strsplit(xpath, split = "/"))
    segs <- segs[nzchar(segs)]

    if (length(segs) == 0) {
        return(invisible(from))
    }

    # Remove leading root if present
    if (identical(segs[1], from$.extra$name)) {
        segs <- segs[-1]
    }

    if (length(segs) == 0) {
        return(invisible(from))
    }

    parse_seg <- function(s) {
        m <- regexec("^([^\\[]+)(\\[(\\d+)\\])?$", s)
        r <- regmatches(s, m)[[1]]
        if (length(r) == 0) return(NULL)
        name <- r[2]
        idx <- 1L
        if (length(r) >= 4 && nchar(r[4]) > 0) {
            idx <- as.integer(r[4])
        }
        list(name = name, index = idx)
    }

    make_recursive <- function(el, rest) {
        if (length(rest) == 0) return(el)

        info <- parse_seg(rest[1])
        if (is.null(info) || is.na(info$name)) {
            admisc::stopError("Invalid xpath segment.")
        }

        existing <- indexChildren(el, info$name)
        if (length(existing) < info$index) {
            for (k in seq_len(info$index - length(existing))) {
                child <- makeElement(info$name)
                el <- addChildren(child, to = el, overwrite = FALSE)
            }
        }

        pos <- indexChildren(el, info$name)[info$index]
        child <- el[[pos]]
        child$.extra$index <- info$index
        child <- make_recursive(child, rest[-1])
        el[[pos]] <- child

        return(el)
    }

    result <- make_recursive(from, segs)

    if (overwrite) {
        admisc::overwrite(objname, result, parent.frame())
    }

    return(invisible(result))
}


#' @rdname DDI-children
#' @export
`moveChild` <- function(xpath, from, to, overwrite = TRUE, ...) {
    objname <- deparse1(substitute(xpath))

    if (
        missing(xpath) || is.null(xpath) || !is.atomic(xpath) ||
        !is.character(xpath) || length(xpath) != 1
    ) {
        admisc::stopError("Argument 'xpath' should be a character scalar.")
    }

    if (missing(to) || is.null(to) || length(to) != 1) {
        admisc::stopError("Argument 'to' should be a length 1 integer.")
    }

    to <- as.integer(to)

    segs <- unlist(strsplit(xpath, split = "/"))
    segs <- segs[nzchar(segs)]

    if (length(segs) < 2) {
        admisc::stopError("Argument 'xpath' should include a root and a child.")
    }

    rootname <- segs[1]
    if (!exists(rootname, envir = parent.frame())) {
        admisc::stopError("Could not resolve root element from xpath.")
    }

    element <- get(rootname, envir = parent.frame())
    if (!is.element(".extra", names(element))) {
        admisc::stopError("The resolved root element is not standard.")
    }

    segs <- segs[-1]

    parse_seg <- function(s) {
        m <- regexec("^([^\\[]+)(\\[(\\d+)\\])?$", s)
        r <- regmatches(s, m)[[1]]
        if (length(r) == 0) return(NULL)
        name <- r[2]
        idx <- NA_integer_
        if (length(r) >= 4 && nchar(r[4]) > 0) {
            idx <- as.integer(r[4])
        }
        list(name = name, index = idx)
    }

    resolve_stack <- function(root, parts) {
        cur <- root
        stack <- list()
        for (seg in parts) {
            info <- parse_seg(seg)
            if (is.null(info) || is.na(info$name)) {
                admisc::stopError("Invalid xpath segment.")
            }
            positions <- indexChildren(cur, info$name)
            if (length(positions) == 0) {
                admisc::stopError("No such child element in xpath.")
            }
            idx <- info$index
            if (is.na(idx)) {
                # Default to the first element when index is missing
                idx <- 1L
            }
            if (idx < 1 || idx > length(positions)) {
                admisc::stopError("Index out of range for xpath segment.")
            }
            stack[[length(stack) + 1]] <- list(parent = cur, positions = positions, idx = idx)
            cur <- cur[[positions[idx]]]
            cur$.extra$index <- idx
        }
        list(node = cur, stack = stack)
    }

    last <- segs[length(segs)]
    info_last <- parse_seg(last)
    if (is.null(info_last) || is.na(info_last$name)) {
        admisc::stopError("Invalid xpath segment.")
    }

    if (missing(from) || is.null(from)) {
        if (is.na(info_last$index)) {
            admisc::stopError("Argument 'from' is missing and no index was provided in xpath.")
        }
        from <- info_last$index
    } else {
        if (length(from) != 1) {
            admisc::stopError("Argument 'from' should be a length 1 integer.")
        }
        from <- as.integer(from)
        if (!is.na(info_last$index) && from != info_last$index) {
            admisc::stopError("Argument 'from' does not match the index provided in xpath.")
        }
    }

    parent_parts <- segs[-length(segs)]
    parent_res <- if (length(parent_parts)) resolve_stack(element, parent_parts) else list(node = element, stack = list())
    parent <- parent_res$node

    if (is.null(parent) || !is.element(".extra", names(parent))) {
        admisc::stopError("Could not resolve parent element from xpath.")
    }

    positions <- indexChildren(parent, info_last$name)
    if (length(positions) == 0) {
        admisc::stopError("No such child elements to move.")
    }

    if (from < 1 || from > length(positions) || to < 1 || to > length(positions)) {
        admisc::stopError("Indices out of range for the specified child name.")
    }

    new_order <- seq_along(positions)
    item <- new_order[from]
    new_order <- new_order[-from]
    new_order <- append(new_order, item, after = to - 1)

    parent_reordered <- parent
    parent_reordered[positions] <- parent[positions][new_order]

    attrbs <- attributes(parent_reordered)
    attrbs$names <- names(parent_reordered)
    attributes(parent_reordered) <- attrbs

    updated <- parent_reordered
    if (length(parent_res$stack)) {
        for (i in seq(length(parent_res$stack), 1)) {
            entry <- parent_res$stack[[i]]
            par <- entry$parent
            par[[entry$positions[entry$idx]]] <- updated
            attrbs2 <- attributes(par)
            attrbs2$names <- names(par)
            attributes(par) <- attrbs2
            updated <- par
        }
    }

    if (overwrite) {
        admisc::overwrite(rootname, updated, parent.frame())
    }

    return(invisible(updated))
}


#' @rdname DDI-children
#' @export
`replaceChild` <- function(xpath, with, overwrite = TRUE, ...) {
    objname <- deparse1(substitute(xpath))

    if (
        missing(xpath) || is.null(xpath) || !is.atomic(xpath) ||
        !is.character(xpath) || length(xpath) != 1
    ) {
        admisc::stopError("Argument 'xpath' should be a character scalar.")
    }

    if (missing(with) || !is.element(".extra", names(with))) {
        admisc::stopError("The argument 'with' is not standard.")
    }

    segs <- unlist(strsplit(xpath, split = "/"))
    segs <- segs[nzchar(segs)]

    if (length(segs) < 2) {
        admisc::stopError("Argument 'xpath' should include a root and a child.")
    }

    rootname <- segs[1]
    if (!exists(rootname, envir = parent.frame())) {
        admisc::stopError("Could not resolve root element from xpath.")
    }

    element <- get(rootname, envir = parent.frame())
    if (!is.element(".extra", names(element))) {
        admisc::stopError("The resolved root element is not standard.")
    }

    segs <- segs[-1]

    parse_seg <- function(s) {
        m <- regexec("^([^\\[]+)(\\[(\\d+)\\])?$", s)
        r <- regmatches(s, m)[[1]]
        if (length(r) == 0) return(NULL)
        name <- r[2]
        idx <- NA_integer_
        if (length(r) >= 4 && nchar(r[4]) > 0) {
            idx <- as.integer(r[4])
        }
        list(name = name, index = idx)
    }

    resolve_stack <- function(root, parts) {
        cur <- root
        stack <- list()
        for (seg in parts) {
            info <- parse_seg(seg)
            if (is.null(info) || is.na(info$name)) {
                admisc::stopError("Invalid xpath segment.")
            }
            positions <- indexChildren(cur, info$name)
            if (length(positions) == 0) {
                admisc::stopError("No such child element in xpath.")
            }
            idx <- info$index
            if (is.na(idx)) {
                # Default to the first element when index is missing
                idx <- 1L
            }
            if (idx < 1 || idx > length(positions)) {
                admisc::stopError("Index out of range for xpath segment.")
            }
            stack[[length(stack) + 1]] <- list(parent = cur, positions = positions, idx = idx)
            cur <- cur[[positions[idx]]]
            cur$.extra$index <- idx
        }
        list(node = cur, stack = stack)
    }

    last <- segs[length(segs)]
    info <- parse_seg(last)
    if (is.null(info) || is.na(info$name)) {
        admisc::stopError("Invalid xpath segment.")
    }

    if (is.na(info$index)) {
        admisc::stopError("Argument 'xpath' should include a child index like [n].")
    }

    parent_parts <- segs[-length(segs)]
    parent_res <- if (length(parent_parts)) resolve_stack(element, parent_parts) else list(node = element, stack = list())
    parent <- parent_res$node

    if (is.null(parent) || !is.element(".extra", names(parent))) {
        admisc::stopError("Could not resolve parent element from xpath.")
    }

    positions <- indexChildren(parent, info$name)
    if (length(positions) == 0) {
        admisc::stopError("No such child elements to replace.")
    }

    if (info$index < 1 || info$index > length(positions)) {
        admisc::stopError("Index out of range for the specified child name.")
    }

    parent[[positions[info$index]]] <- with
    attrbs <- attributes(parent)
    attrbs$names <- names(parent)
    attributes(parent) <- attrbs

    updated <- parent
    if (length(parent_res$stack)) {
        for (i in seq(length(parent_res$stack), 1)) {
            entry <- parent_res$stack[[i]]
            par <- entry$parent
            par[[entry$positions[entry$idx]]] <- updated
            attrbs2 <- attributes(par)
            attrbs2$names <- names(par)
            attributes(par) <- attrbs2
            updated <- par
        }
    }

    if (overwrite) {
        admisc::overwrite(rootname, updated, parent.frame())
    }

    return(invisible(updated))
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

    if (is.character(to) && length(to) == 1) {
        res <- resolve_xpath_target(to, env = parent.frame())
        node <- changeContent(content, to = res$node, overwrite = FALSE)
        if (identical(res$canonical, res$rootname)) {
            if (overwrite) {
                admisc::overwrite(res$rootname, node, parent.frame())
            }
        } else if (overwrite) {
            replaceChild(res$canonical, with = node, overwrite = TRUE)
        }
        return(invisible(node))
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

    if (is.character(from) && length(from) == 1) {
        res <- resolve_xpath_target(from, env = parent.frame())
        node <- removeContent(from = res$node, overwrite = FALSE)
        if (identical(res$canonical, res$rootname)) {
            if (overwrite) {
                admisc::overwrite(res$rootname, node, parent.frame())
            }
        } else if (overwrite) {
            replaceChild(res$canonical, with = node, overwrite = TRUE)
        }
        return(invisible(node))
    }

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

    if (is.character(to) && length(to) == 1) {
        res <- resolve_xpath_target(to, env = parent.frame())
        node <- addAttributes(attrs, to = res$node, overwrite = FALSE)
        if (identical(res$canonical, res$rootname)) {
            if (overwrite) {
                admisc::overwrite(res$rootname, node, parent.frame())
            }
        } else if (overwrite) {
            replaceChild(res$canonical, with = node, overwrite = TRUE)
        }
        return(invisible(node))
    }

    DDIC <- get("DDIC", envir = cacheEnv)
    DDIC_global_attributes <- get("DDIC_global_attributes", envir = cacheEnv)

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

    if (is.character(from) && length(from) == 1) {
        res <- resolve_xpath_target(from, env = parent.frame())
        node <- changeAttributes(attrs, from = res$node, overwrite = FALSE)
        if (identical(res$canonical, res$rootname)) {
            if (overwrite) {
                admisc::overwrite(res$rootname, node, parent.frame())
            }
        } else if (overwrite) {
            replaceChild(res$canonical, with = node, overwrite = TRUE)
        }
        return(invisible(node))
    }

    DDIC <- get("DDIC", envir = cacheEnv)
    DDIC_global_attributes <- get("DDIC_global_attributes", envir = cacheEnv)

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

    if (!all(is.element(attrnames, names(attrbs)))) {
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

    if (is.character(from) && length(from) == 1) {
        res <- resolve_xpath_target(from, env = parent.frame())
        node <- removeAttributes(name, from = res$node, overwrite = FALSE)
        if (identical(res$canonical, res$rootname)) {
            if (overwrite) {
                admisc::overwrite(res$rootname, node, parent.frame())
            }
        } else if (overwrite) {
            replaceChild(res$canonical, with = node, overwrite = TRUE)
        }
        return(invisible(node))
    }

    DDIC <- get("DDIC", envir = cacheEnv)
    DDIC_global_attributes <- get("DDIC_global_attributes", envir = cacheEnv)

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



# Internal helper: resolve xpath to target node and canonical indexed xpath
resolve_xpath_target <- function(xpath, env = parent.frame()) {
    if (missing(xpath) || is.null(xpath) || !is.atomic(xpath) || !is.character(xpath) || length(xpath) != 1) {
        admisc::stopError("Argument 'xpath' should be a character scalar.")
    }

    segs <- unlist(strsplit(xpath, split = "/"))
    segs <- segs[nzchar(segs)]
    if (length(segs) == 0) {
        admisc::stopError("Argument 'xpath' is empty.")
    }

    rootname <- segs[1]
    if (!exists(rootname, envir = env)) {
        admisc::stopError("Could not resolve root element from xpath.")
    }

    root <- get(rootname, envir = env)
    if (!is.element(".extra", names(root))) {
        admisc::stopError("The resolved root element is not standard.")
    }

    if (length(segs) == 1) {
        return(list(rootname = rootname, root = root, node = root, canonical = rootname))
    }

    parse_seg <- function(s) {
        m <- regexec("^([^\\[]+)(\\[(\\d+)\\])?$", s)
        r <- regmatches(s, m)[[1]]
        if (length(r) == 0) return(NULL)
        name <- r[2]
        idx <- NA_integer_
        if (length(r) >= 4 && nchar(r[4]) > 0) {
            idx <- as.integer(r[4])
        }
        list(name = name, index = idx)
    }

    last <- segs[length(segs)]
    info <- parse_seg(last)
    if (is.null(info) || is.na(info$name)) {
        admisc::stopError("Invalid xpath segment.")
    }

    parent_parts <- segs[-length(segs)]
    parent_path <- paste(parent_parts, collapse = "/")
    parent <- if (nzchar(parent_path)) {
        getChildren(parent_path, from = root)
    } else {
        root
    }

    if (is.null(parent) || !is.element(".extra", names(parent))) {
        admisc::stopError("Could not resolve parent element from xpath.")
    }

    positions <- indexChildren(parent, info$name)
    if (length(positions) == 0) {
        admisc::stopError("No such child element in xpath.")
    }

    idx <- info$index
    if (is.na(idx)) {
        # Default to the first element when index is missing
        idx <- 1L
    }

    if (idx < 1 || idx > length(positions)) {
        admisc::stopError("Index out of range for xpath segment.")
    }

    node <- parent[[positions[idx]]]
    canonical_last <- paste0(info$name, "[", idx, "]")

    # Avoid duplicating the root in canonical path
    if (length(parent_parts) > 0 && identical(parent_parts[1], rootname)) {
        parent_parts <- parent_parts[-1]
    }

    canonical <- if (length(parent_parts) > 0) {
        paste(rootname, paste(parent_parts, collapse = "/"), canonical_last, sep = "/")
    } else {
        paste(rootname, canonical_last, sep = "/")
    }

    list(rootname = rootname, root = root, node = node, canonical = canonical)
}
