#' @name testValid
#'
#' @title Validate a DDI element.
#'
#' @description Attempts a minimal validation of a DDI Codebook element, by
#' searching for mandatory elements and attributes.
#'
#' @param element A standard element of class `"DDI"`.
#' @param monolang Logical, the codebook file is monolingual
#'
#' @details This function currently attempts a minimal check for the absolute
#' most mandatory elements, such as the `stdyDscr`. An absolute bare version
#' of this element, filled with arbitrary default values, can be produced with
#' the function `makeElement()`, activating its attribute `fill`.

#' It also checks for chained expectations, that is element X is mandatory only
#' if the parent element is present.
#'
#' Future versions will implement more functionality for recommended elements
#' and attributes, with the intention to provide a 1:1 validation as offered by
#' the "CESSDA Metadata Validator".
#'
#' To ease the validation of the DDI Codebook XML files, the argument `monolang`
#' is activated by default. This means a single attribute `xmlang` in the main
#' `codeBook` element. For multi-language codebooks, an error is flagged if this
#' argument is missing where appropriate.
#'
#' @return A character vector of validation problems found.
#'
#' @author Adrian Dusa
#'
#' @seealso
#' \code{\link{makeElement}}
#'
#' @export
`testValid` <- function(element, monolang = TRUE) {

    DDIC <- get("DDIC", envir = cacheEnv)

    # mandatory elements
    melements <- setdiff(
        names(which(sapply(DDIC, function(x) !x$optional))),
        "codeBook"
    )

    # mandatory attributes
    mattributes <- setdiff(
        names(which(sapply(DDIC, function(x) {
            ifelse(
                length(x$attributes) > 0,
                any(sapply(x$attributes, function(a) !a$optional)),
                FALSE
            )
        }))),
        "codeBook"
    )

    output <- list(mandatory = c(), optional = c())

    xpaths_elements <- list()
    for (m in melements) {
        xpaths_elements <- c(xpaths_elements, showLineages(m))
    }

    second <- sapply(xpaths_elements, "[[", 2)
    xpaths_elements <- xpaths_elements[
        order(match(second, DDIC$codeBook$children))
    ]

    xpaths_attributes <- list()
    for (m in mattributes) {
        xpaths_attributes <- c(xpaths_attributes, showLineages(m))
    }

    second <- sapply(xpaths_attributes, "[[", 2)
    xpaths_attributes <- xpaths_attributes[
        order(match(second, DDIC$codeBook$children))
    ]

    xpaths_attributes <- setdiff(xpaths_attributes, xpaths_elements)


    selements <- sapply(xpaths_elements, function(x) {
        is.element(element$.extra$name, x)
    })


    if (length(xpaths_attributes) > 0) {
        sattributes <- sapply(xpaths_attributes, function(x) {
            is.element(element$.extra$name, x)
        })
    }
    else {
        sattributes <- FALSE
    }

    if (any(selements)) {
        xpaths_elements <- lapply(xpaths_elements[selements], function(x) {
            return(x[seq(which(x == element$.extra$name), length(x))])
        })

        result <- lapply(xpaths_elements, function(xpath) {
            last <- xpath[length(xpath)]
            xpath <- xpath[-length(xpath)]

            if (identical(xpath, element$.extra$name)) {
                if (!checkExisting(last, element)) {
                    return(sprintf(
                        "%s expects the mandatory child %s.",
                        element$.extra$name, last
                    ))
                }
            }

            if (!checkExisting(xpath, element)) {
                return(character(0))
            }

            if (!checkExisting(c(xpath, last), element)) {
                return(sprintf(
                    "%s expects the mandatory child %s.",
                    paste(xpath, collapse = "/"), last
                ))
            }

            if (length(DDIC[[last]]$attributes) > 0) {
                optional <- sapply(DDIC[[last]]$attributes, function(x) {
                    x$optional
                })

                if (any(!optional)) {
                    for (n in names(optional)[!optional]) {
                        attr <- checkExisting(c(xpath, last), element, attribute = n)

                        if (n == "xmlang") {
                            if (attr & isTRUE(monolang)) {
                                return(sprintf(
                                    "%s should not have an 'xmlang' attribute, in a monolang codeBook.",
                                    paste(c(xpath, last), collapse = "/")
                                ))
                            }

                            if (!attr & isFALSE(monolang)) {
                                return(sprintf(
                                    "%s should have an 'xmlang' attribute, when the codeBook is not monolang.",
                                    paste(c(xpath, last), collapse = "/")
                                ))
                            }
                        }
                        else if (!attr) {
                            return(sprintf(
                                "%s should have a mandatory attribute '%s'.",
                                paste(c(xpath, last), collapse = "/"), n
                            ))
                        }
                    }
                }
            }
        })

        problems <- sapply(result, length) > 0
        if (any(problems)) {
            output$mandatory <- unlist(result[problems])
        }
    }

    if (any(sattributes)) {
        xpaths_attributes <- lapply(xpaths_attributes[sattributes], function(x) {
            return(x[seq(which(x == element$.extra$name), length(x))])
        })

        result <- lapply(xpaths_attributes, function(xpath) {

            last <- xpath[length(xpath)]

            if (!checkExisting(xpath, element)) {
                return(character(0))
            }

            if (length(DDIC[[last]]$attributes) > 0) {
                optional <- sapply(DDIC[[last]]$attributes, function(x) {
                    x$optional
                })

                if (any(!optional)) {
                    for (n in names(optional)[!optional]) {
                        attr <- checkExisting(xpath, element, attribute = n)

                        if (n == "xmlang") {
                            if (attr & isTRUE(monolang)) {
                                return(sprintf(
                                    "%s should not have an 'xmlang' attribute, in a monolang codeBook.",
                                    paste(xpath, collapse = "/")
                                ))
                            }

                            if (!attr & isFALSE(monolang)) {
                                return(sprintf(
                                    "%s should have an 'xmlang' attribute, when the codeBook is not monolang.",
                                    paste(xpath, collapse = "/")
                                ))
                            }
                        }
                        else if (!attr) {
                            return(sprintf(
                                "%s should have a mandatory attribute '%s'.",
                                paste(xpath, collapse = "/"), n
                            ))
                        }
                    }
                }
            }
        })

        problems <- sapply(result, length) > 0
        if (any(problems)) {
            output$optional <- unlist(result[problems])
        }
    }

    return(structure(output, class = "validation"))
}


#' @export
print.validation <- function(x, ...) {
    cat("\n")
    if (length(x$mandatory) == 0 & length(x$optional) == 0) {
        cat("No validation problems.\n")
    }
    else {
        if (length(x$mandatory) > 0) {
            cat("Mandatory elements:\n")
            writeLines(strwrap(unclass(x$mandatory)))
        }
        if (length(x$optional) > 0) {
            if (length(x$mandatory) > 0) {
                cat("\n")
            }
            cat("Optional elements:\n")
            writeLines(strwrap(unclass(x$optional)))
        }
    }
    cat("\n")
}
