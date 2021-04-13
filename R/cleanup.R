`cleanup` <- function(x, cdata = TRUE) {

    x <- gsub("^[[:space:]]+|[[:space:]]+$", "", x)
    x <- gsub("\"", "'", x)
    for (l in letters) {
        x <- gsub(sprintf("\\\\+%s", l), sprintf("/%s", l), x)
    }
    x <- gsub("\\\\", "/", x)
    if (cdata) {
        x <- gsub("<\\!\\[CDATA\\[|\\]\\]>", "", x)
    }

    irv <- c(194, 180)
    tick <- unlist(strsplit(rawToChar(as.raw(irv)), split = ""))
    x <- gsub(paste(tick, collapse = "|"), "'", x)
    
    return(x)
}
