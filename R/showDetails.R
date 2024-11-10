#' @name showDetails
#'
#' @title
#' Describe what a DDI element is
#'
#' @author Adrian Dusa
#'
#' @param x Character, a DDI Codebook element name.
#' @param ... Other arguments, mainly for internal use.
#'
#' @details All arguments having predefined values such as "(Y | N) : N" are
#' mandatory if the element is used
#'
#' @examples
#' showDetails("codeBook")
#'
#' showAttributes("catgry")
#'
#' showExamples("abstract")
#'
#' showLineages("titl")
#'
#' @export
`showDetails` <- function(x, ...) {
    description <- showDescription(x, return = TRUE)
    attributes <- showAttributes(x, return = TRUE)
    examples <- showExamples(x, return = TRUE)
    relations <- showRelations(x, return = TRUE)

    toprint <- character(0)
    noprint <- character(0)
    nodesc <- any(grepl("does not have a description", description))
    noatts <- any(grepl("does not have any specific attributes", attributes))
    noexls <- any(grepl("no examples for this element", examples))

    prefix <- FALSE

    if (nodesc) {
        noprint <- c(noprint, "This element does not have a description")
        prefix <- TRUE
    } else {
        toprint <- c(toprint, description)
    }

    if (noatts) {
        if (prefix) {
            noprint <- c(noprint, ifelse(noexls, ",", " and"), " it has no specific attributes")
        } else {
            noprint <- c(noprint, "This element does not have any specific attributes")
        }

        prefix <- TRUE
    } else {
        toprint <- c(toprint, attributes)
    }

    if (noexls) {
        if (prefix) {
            noprint <- c(noprint, " and it has no examples")
        } else {
            noprint <- c(noprint, "This element has no examples")
        }
    } else {
        toprint <- c(toprint, examples)
    }

    noprint <- c(noprint, ".\n\n")

    if (sum(c(nodesc, noatts, noexls)) == 0) {
        noprint <- character(0)
    }

    toprint <- c(toprint, relations, noprint)

    cat(paste(toprint, collapse = ""))
}

#' @rdname showDetails
#' @export
`showDescription` <- function(x, ...) {

    DDIC <- get("DDIC", envir = cacheEnv)

    checkElement(x)
    dots <- list(...)
    endWith <- ifelse(isTRUE(dots$return), "", "\n")

    toprint <- "\n"
    toprint <- c(
        toprint,
        paste(
            strwrap(
                paste0(
                    DDIC[[x]]$title,
                    " (",
                    ifelse (DDIC[[x]]$optional, "optional, ", "mandatory, "),
                    ifelse (DDIC[[x]]$repeatable, "repeatable", "non-repeatable"),
                    ")"
                )
            ),
            collapse = "\n"
        ),
        "\n"
    )

    if (identical(DDIC[[x]]$description, "")) {
        toprint <- c(toprint, "\nThis element does not have a description.\n")
    } else {
        toprint <- c(
            toprint,
            paste(
                strwrap(
                    unlist(strsplit(DDIC[[x]]$description, split = "\n"))
                ),
                collapse = "\n"
            ),
            "\n"
        )
    }

    toprint <- c(toprint, endWith)

    if (isTRUE(dots$return)) {
        return(toprint)
    } else {
        cat(paste(toprint, collapse = ""))
    }
}

#' @rdname showDetails
#' @param name Character, print only a specific element (name)
#' @export
`showAttributes` <- function(x, name = NULL, ...) {

    DDIC <- get("DDIC", envir = cacheEnv)
    toprint <- character(0)

    dots <- list(...)
    if (is.list(x)) {
        attrs <- attributes(x)
        attrs$names <- NULL
        nms <- names(attrs)
        for (n in nms) {
            toprint <- c(
                toprint,
                sprintf("%s: %s\n", n, attrs[[n]])
            )
        }
    }
    else {
        endWith <- ifelse(isTRUE(dots$return), "", "\n")

        if (identical(x, "_global_")) {
            attributes <- get("DDIC_global_attributes", envir = cacheEnv)
            message <- "\nGlobal attributes:\n"
        }
        else {
            checkElement(x)
            attributes <- DDIC[[x]]$attributes
            message <- "\nSpecific attributes, use globalAttributes() for the rest:\n"
        }

        if (length(attributes) > 0) {
            toprint <- c(toprint, message)

            if (is.null(name)) {
                name <- names(attributes)
            }
            else {
                if (any(!is.element(name, names(attributes)))) {
                    admisc::stopError("Innexisting attribute name(s).")
                }
            }
            for (n in name) {
                type <- gsub("xs\\:", "", attributes[[n]]$type)
                nm <- n
                if (attributes[[n]]$optional) {
                    if (attributes[[n]]$recommended) {
                        nm <- paste(nm, "(recommended)")
                    }
                    else {
                        nm <- paste(nm, "(optional)")
                    }
                }
                else {
                    nm <- paste(nm, "(mandatory)")
                }

                if (length(attributes[[n]]$values) > 0) {
                    type <- paste0(
                        "(",
                        paste(attributes[[n]]$values, collapse = " | "),
                        ")"
                    )
                    if (length(attributes[[n]]$default) > 0) {
                        type <- paste(type, attributes[[n]]$default, sep = " : ")
                    }
                }

                toprint <- c(
                    toprint,
                    paste(
                        strwrap(
                            paste0("- ", nm, ": ", type, "\n"),
                            exdent = 4
                        ),
                        collapse = "\n"
                    ),
                    "\n"
                )

                description <- attributes[[n]]$description

                for (i in seq(length(description))) {
                    if (nzchar(description[i]) & !is.na(description[i])) {
                        toprint <- c(
                            toprint,
                            paste(
                                strwrap(description[i], prefix = "  "),
                                collapse = "\n"
                            ),
                            "\n"
                        )
                    }
                }
            }
        }
        else {
            toprint <- c(toprint, "\nThis element does not have any specific attributes.\n")
        }

        toprint <- c(toprint, endWith)
    }

    if (isTRUE(dots$return)) {
        return(toprint)
    } else {
        cat(paste(toprint, collapse = ""))
    }
}

#' @rdname showDetails
#' @export
`globalAttributes` <- function() {
    showAttributes("_global_")
}

#' @rdname showDetails
#' @export
`showExamples` <- function(x, ...) {

    DDIC <- get("DDIC", envir = cacheEnv)

    checkElement(x)
    dots <- list(...)
    endWith <- ifelse(isTRUE(dots$return), "", "\n")
    examples <- DDIC[[x]]$examples


    if (length(examples) == 0) {
        toprint <- "\nThere are no examples for this element.\n"
    }
    else {
        toprint <- "\nExamples:\n"
        for (i in seq(length(examples))) {
            toprint <- c(
                toprint,
                formatExample(xml2::read_xml(examples[i]), ... = ...)
            )
        }
    }

    toprint <- c(toprint, endWith)

    if (isTRUE(dots$return)) {
        return(toprint)
    } else {
        cat(paste(toprint, collapse = ""))
    }
}

#' @rdname showDetails
#' @export
`showRelations` <- function(x, ...) {

    DDIC <- get("DDIC", envir = cacheEnv)

    checkElement(x)
    dots <- list(...)
    endWith <- ifelse(is.null(dots$endWith), "\n", dots$endWith)
    toprint <- "\n"
    parents <- DDIC[[x]]$parents
    parents2 <- parents
    children <- unlist(DDIC[[x]]$children)
    children2 <- children

    if (is.element(x, children)) {
        # recursive
        children2[children == x] <- paste(children[children == x], "(RECURSIVE)")
    }

    if (length(children) > 0) {
        toprint <- c(toprint,
            "Children:\n- ",
            paste(
                children2,
                sapply(
                    DDIC[children], function(x) {
                        return(x$title)
                    }
                ),
                sep = ": ",
                collapse = "\n- "
            ),
            "\n\n"
        )
    }

    if (is.element(x, parents)) {
        # recursive
        parents2[parents == x] <- paste(parents[parents == x], "(RECURSIVE)")
    }

    if (length(parents) > 0) {
        toprint <- c(toprint,
            "Parents:\n- ",
            paste(
                parents2,
                sapply(
                    DDIC[parents], function(x) {
                        return(x$title)
                    }
                ),
                sep = ": ",
                collapse = "\n- "
            ),
            "\n"
        )
    }

    toprint <- c(toprint, endWith)

    if (isTRUE(dots$return)) {
        return(toprint)
    } else {
        cat(paste(toprint, collapse = ""))
    }
}

#' @rdname showDetails
#' @export
`showLineages` <- function(x, ...) {

    DDIC <- get("DDIC", envir = cacheEnv)

    checkElement(x)

    if (identical(x, "codeBook")) {
        cat("\nThe codeBook element has no lineage, it is the first element.\n")
        invisible(return(NULL))
    }

    dots <- list(...)
    endWith <- ifelse(is.null(dots$endWith), "\n", dots$endWith)
    all_lineages <- list()

    `getLineage` <- function(x, lineage = character(0)) {
        parents <- setdiff(DDIC[[x]]$parents, x) # to avoid circular references
        lineage <- c(lineage, x)

        # print(parents)
        # print(lineage)
        # cat("---------------\n")

        if (is.element("codeBook", parents)) {
            all_lineages <<- append(all_lineages, list(rev(lineage)))
        }
        else if (length(parents) > 0) {
            for (parent in parents) {
                # print(parent)
                # cat("---\n")
                getLineage(parent, lineage)
            }
        }
    }

    getLineage(x)

    first <- sapply(all_lineages, "[[", 1)
    all_lineages <- lapply(all_lineages, function(x) {
        return(c("codeBook", x))
    })
    all_lineages <- all_lineages[order(match(first, unlist(DDIC$codeBook$children)))]

    x <- structure(unique(all_lineages), class = "lineage")
    attr(x, "endWith") <- endWith

    return(x)
}

#' @export
`print.lineage` <- function(x, ...) {
    cat("\n")
    writeLines(sapply(x, paste, collapse = "/"))
    cat(attr(x, "endWith"))
}
