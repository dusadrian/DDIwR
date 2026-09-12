#' @name testValid
#'
#' @title Validate a DDI element.
#'
#' @description Checks parent-specific child occurrences, sequences, choices
#' and mandatory attributes throughout a DDI Codebook element.
#'
#' @param element A standard element of class `"DDI"`.
#' @param monolang Logical, the codebook file is monolingual
#'
#' @details Each existing instance is checked separately, including repeated
#' and recursively nested elements. Required children are checked only when
#' their parent exists. Repeated choices may use different alternatives.
#' An incomplete element can be built incrementally with `addChildren()`;
#' `testValid()` also checks the required minimum occurrences and child order.
#'
#' Validation follows the curated DDI child model. It does not expand imported
#' markup groups or validate all XML datatypes, ID references or mixed text.
#' Use XML Schema validation on exported XML for those additional checks.
#'
#' To ease the validation of the DDI Codebook XML files, the argument `monolang`
#' is activated by default. This means a single attribute `xmlang` in the main
#' `codeBook` element. For multi-language codebooks, an error is flagged if this
#' argument is missing where appropriate.
#'
#' @return A list of class `validation`, with character vectors `mandatory` and
#' `optional`. The latter contains missing required attributes of optional
#' children; both components report validation problems, not recommendations.
#'
#' @author Adrian Dusa
#'
#' @seealso
#' \code{\link{makeElement}}
#'
#' @export
`testValid` <- function(element, monolang = TRUE) {

    DDIC <- get("DDIC", envir = cacheEnv)
    if (
        !is.list(element) ||
        is.null(element$.extra$name) ||
        !is.element(element$.extra$name, names(DDIC))
    ) {
        admisc::stopError("The argument 'element' is not a standard DDI element.")
    }

    output <- list(mandatory = NULL, optional = NULL)

    visit <- function(node, path, optional = FALSE) {
        name <- node$.extra$name
        definition <- DDIC[[name]]
        model <- definition$contentModel
        if (is.null(model)) {
            stop("The schema has no content model for ", name, ".")
        }

        positions <- which(!is.element(names(node), c("", ".extra")))
        children <- names(node)[positions]
        output$mandatory <<- c(output$mandatory, ddiModelProblems(model, children, path))
        attributes <- definition$attributes

        for (key in names(attributes)) {
            if (isTRUE(attributes[[key]]$optional)) next
            present <- !is.null(attr(node, key, exact = TRUE))
            problem <- NULL

            if (key == "xmlang") {
                if (present && isTRUE(monolang)) problem <- sprintf(
                    "%s should not have an 'xmlang' attribute, in a monolang codeBook.", path
                )
                if (!present && isFALSE(monolang)) problem <- sprintf(
                    "%s should have an 'xmlang' attribute, when the codeBook is not monolang.", path
                )
            } else if (!present) {
                problem <- sprintf("%s should have a mandatory attribute '%s'.", path, key)
            }

            field <- if (optional) "optional" else "mandatory"
            output[[field]] <<- c(output[[field]], problem)
        }

        for (i in seq_along(positions)) {
            child <- node[[positions[i]]]
            childName <- children[i]
            index <- sum(children[seq_len(i)] == childName)
            childPath <- paste0(path, "/", childName, "[", index, "]")

            if (!is.list(child) || is.null(child$.extra$name) ||
                !identical(child$.extra$name, childName) ||
                !is.element(childName, names(DDIC))) {
                output$mandatory <<- c(output$mandatory, paste(childPath, "is not a standard DDI child."))
                next
            }

            bounds <- ddiModelBounds(model, childName)
            visit(child, childPath, optional = bounds[1] == 0)
        }
    }

    visit(element, element$.extra$name)

    structure(output, class = "validation")
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
