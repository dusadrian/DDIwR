#' @name showDetails
#'
#' @title
#' Describe what a DDI element is

#' @return Nothing, it just prints the information on the console
#'
#' @author Adrian Dusa
#'
#' @param x Character, a DDI Codebook element name.
#' @param ... Other arguments, mainly for internal use.
#'
#' @details All arguments having predefined values such as "(Y | N) : N" are
#' mandatory if the element is used
#'
#' @export
`showDetails` <- function(x, ...) {
    showDescription(x, endWith = "")
    showAttributes(x, endWith = "")
    showExamples(x, endWith = "")
    showRelations(x)
}

#' @rdname showDetails
#' @export
`showDescription` <- function(x, ...) {
    checkElement(x)
    dots <- list(...)
    endWith <- ifelse(is.null(dots$endWith), "\n", dots$endWith)
    cat("\n")
    writeLines(strwrap(
        paste0(
            DDIC[[x]]$title,
            " (",
            ifelse (DDIC[[x]]$optional, "optional, ", "mandatory, "),
            ifelse (DDIC[[x]]$repeatable, "repeatable", "non-repeatable"),
            ")"
        )
    ))

    writeLines(strwrap(
        unlist(strsplit(DDIC[[x]]$description, split = "\n"))
    ))

    cat(endWith)
}

#' @rdname showDetails
#' @param name Character, print only a specific element (name)
#' @export
`showAttributes` <- function(x, name = NULL, ...) {
    dots <- list(...)
    endWith <- ifelse(is.null(dots$endWith), "\n", dots$endWith)

    if (identical(x, "_global_")) {
        attributes <- DDIC_global_attributes
        message <- "\nGlobal attributes:\n"
    }
    else {
        checkElement(x)
        attributes <- DDIC[[x]]$attributes
        message <- "\nSpecific attributes, use globalAttributes() for the rest:\n"
    }


    if (length(attributes) > 0) {
        cat(message)

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

            if (length(attributes[[n]]$values) > 0) {
                type <- paste0(
                    "(",
                    paste(attributes[[n]]$values, collapse = " | "),
                    ")"
                )
                if (attributes[[n]]$default != "") {
                    type <- paste(type, attributes[[n]]$default, sep = " : ")
                }
            }

            cat(paste0("- ", n, ": ", type, "\n"))

            description <- attributes[[n]]$description

            for (i in seq(length(description))) {
                if (nzchar(description[i]) & !is.na(description[i])) {
                    writeLines(strwrap(description[i], prefix = "  "))
                }
            }

        }
    }
    else {
        cat("\nThis element does not have any specific attributes.\n")
    }

    cat(endWith)
}

#' @rdname showDetails
#' @export
`globalAttributes` <- function() {
    showAttributes("_global_")
}

#' @rdname showDetails
#' @export
`showExamples` <- function(x, ...) {
    checkElement(x)
    dots <- list(...)
    endWith <- ifelse(is.null(dots$endWith), "\n", dots$endWith)
    cat("\n")
    examples <- DDIC[[x]]$examples

    cat("Examples:\n")

    if (length(examples) == 0) {
        cat("There are no examples for this element.\n")
    }
    else {
        for (i in seq(length(examples))) {
            formatExample(xml2::read_xml(examples[i]))
        }
    }

    cat(endWith)
}

#' @rdname showDetails
#' @export
`showRelations` <- function(x, ...) {
    checkElement(x)
    dots <- list(...)
    endWith <- ifelse(is.null(dots$endWith), "\n", dots$endWith)
    cat("\n")
    parents <- DDIC[[x]]$parents
    parents2 <- parents
    children <- DDIC[[x]]$children
    children2 <- children

    if (is.element(x, children)) {
        # recursive
        children2[children == x] <- paste(children[children == x], "(RECURSIVE)")
    }

    if (length(children) > 0) {
        cat(
            "Children:\n-",
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
        cat(
            "Parents:\n-",
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

    cat(endWith)
}

#' @rdname showDetails
#' @export
`showLineages` <- function(x, ...) {
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

    all_lineages <- lapply(all_lineages, function(x) {
        return(c("codeBook", x))
    })
    first <- sapply(all_lineages, "[[", 2)
    all_lineages <- all_lineages[order(match(first, DDIC$codeBook$children))]

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