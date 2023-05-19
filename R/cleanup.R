#' @description Rectify texts read from a metadata object
#' @return A character vector
#' @noRd
`cleanup` <- function(x, cdata = TRUE) {

    x <- gsub("&amp;", "&", x)
    x <- gsub("&lt;", "<", x)
    x <- gsub("&gt;", ">", x)
    x <- gsub("^[[:space:]]+|[[:space:]]+$", "", x)
    x <- gsub("\"", "'", x)

    # replace backslash with a forward slash
    x <- gsub("\\\\", "/", x)

    if (cdata) {
        x <- gsub("<\\!\\[CDATA\\[|\\]\\]>", "", x)
    }

    x <- replaceTicks(x)
    return(x)
}
