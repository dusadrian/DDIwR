#' @name searchFor
#'
#' @title Search for key words
#'
#' @description Search function to return elements that contain a certain
#' word of regular expression pattern.
#'
#' @param word Character, either a word or a regular expression.
#' @param where Character, in which sections to search for.
#'
#' @return Character vector of DDI element names.
#'
#' @author Adrian Dusa
#' @export
`searchFor` <- function(
    word, where = c("everywhere", "description", "examples", "attributes")
) {
    tryit <- admisc::tryCatchWEM(
        where <- match.arg(where, several.ok = TRUE)
    )

    if (!is.null(tryit$error)) {
        admisc::stopError(
            paste("Argument", gsub("arg", "where", tryit$error))
        )
    }

    if (is.element("everywhere", where)) {
        where <- c("description", "examples", "attributes")
    }

    DDIC <- get("DDIC", envir = cacheEnv)

    hasword <- sapply(DDIC, function(x) {
        result <- FALSE
        if (is.element("description", where)) {
            result <- result | any(grepl(word, x$description))
        }

        if (is.element("examples", where)) {
            result <- result | any(grepl(word, x$examples))
        }

        if (is.element("attributes", where)) {
            result <- result | any(sapply(x$attributes, function(a) {
                any(grepl(word, a$description))
            }))
        }
        return(result)
    })

    return(names(hasword)[hasword])
}
