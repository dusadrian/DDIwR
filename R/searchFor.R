#' @name searchFor
#'
#' @title Search for key words
#'
#' @description Search function to return elements that contain a certain
#' word or regular expression pattern.
#'
#' @param x Character, either word(s) or a regular expression.
#' @param where Character, in which section(s) to search for.
#' @param ... Other arguments to be passed to the grepl() function.
#'
#' @return Character vector of DDI element names.
#'
#' @author Adrian Dusa
#' @export
`searchFor` <- function(
    x,
    where = c("everywhere", "title", "description", "attributes", "examples"),
    ...
) {
    tryit <- admisc::tryCatchWEM(
        where <- match.arg(where, several.ok = TRUE)
    )

    `xmlcontent` <- function(x) {
        xml2::xml_text(
            xml2::xml_find_all(
                xml2::read_xml(paste0("<root>", x, "</root>")),
                ".//*"
            )
        )
    }

    if (!is.null(tryit$error)) {
        admisc::stopError(
            paste("Argument", gsub("arg", "where", tryit$error))
        )
    }

    if (is.element("everywhere", tolower(where))) {
        where <- c("title", "description", "attributes", "examples")
    }

    DDIC <- get("DDIC", envir = cacheEnv)

    hasword <- sapply(DDIC, function(element) {
        result <- FALSE
        if (is.element("title", tolower(where))) {
            result <- result | any(
                grepl(
                    paste(x, collapse = "|"),
                    element$title,
                    ... = ...
                )
            )
        }

        if (is.element("description", tolower(where))) {
            result <- result | any(
                grepl(
                    paste(x, collapse = "|"),
                    element$description,
                    ... = ...
                )
            )
        }

        if (is.element("attributes", tolower(where)) && length(element$attributes) > 0) {
            for (i in seq_along(element$attributes)) {
                result <- result | any(
                    grepl(
                        paste(x, collapse = "|"),
                        element$attributes[[i]]$description,
                        ... = ...
                    )
                )
            }
        }

        if (is.element("examples", tolower(where)) && length(element$examples) > 0) {
            for (i in seq_along(element$examples)) {
                result <- result | any(
                    grepl(
                        paste(x, collapse = "|"),
                        xmlcontent(element$examples[i]),
                        ... = ...
                    )
                )
            }
        }

        return(result)
    })

    return(names(hasword)[hasword])
}
