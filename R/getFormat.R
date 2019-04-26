`getFormat` <- function(x) {
    string <- is.character(x)
    x2 <- as.character(x)

    while (TRUE) {
        nofchars <- tryCatch(nchar(x2), error = function(x) return(x))

        if (!is.list(nofchars)) break
        
        # if here, tryCatch caught an error, most likely a multibyte character
        error <- unlist(strsplit(nofchars[[1]], split = " "))
        x2 <- x2[-as.numeric(error[length(error)])]
        
    }

    if (length(nofchars) == 0) {
        nofchars <- 1
    }

    maxvarchar <- max(nofchars, na.rm = TRUE)

    if (is.labelled(x)) {
        labels <- attr(x, "labels")
        maxvarchar <- max(maxvarchar, nchar(labels))
        if (is.character(labels)) string <- TRUE
    }

    decimals <- FALSE
    if (is.numeric(x)) {
        decimals <- any(x - floor(x) > 0)
    }

    return(sprintf("%d%d%d%d", 
        ifelse(string, "A", "F"), 
        maxvarchar, 
        ifelse(string, "", "."),
        ifelse(decimals, 2, 0)))
}
