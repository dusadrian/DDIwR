#' @description Rectify texts read from a metadata object
#' @return A character vector
#' @noRd
`cleanup` <- function(x, cdata = TRUE) {

    tc <- admisc::tryCatchWEM({
        # use case: StatConverter in Electron, the terminal that opens R
        # probably doesn't have a suitable locale and it outputs and error

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
    })

    return(x)
}
