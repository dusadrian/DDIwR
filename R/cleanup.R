`cleanup` <- function(x, cdata = TRUE) {

    x <- gsub("&amp;", "&", x)
    x <- gsub("&lt;", "<", x)
    x <- gsub("&gt;", ">", x)
    x <- gsub("^[[:space:]]+|[[:space:]]+$", "", x)
    x <- gsub("\"", "'", x)
    for (l in letters) {
        x <- gsub(sprintf("\\\\+%s", l), sprintf("/%s", l), x)
    }
    x <- gsub("\\\\", "/", x)
    if (cdata) {
        x <- gsub("<\\!\\[CDATA\\[|\\]\\]>", "", x)
    }

    x <- replaceTicks(x)
    
    return(x)
}
