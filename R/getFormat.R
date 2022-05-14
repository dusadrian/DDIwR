`getFormat` <- function(x) {

    if (all(is.na(x))) {
        return("A0")
    }
    
    string <- is.character(x)
    x2 <- as.character(declared::undeclare(x, drop = TRUE))

    while (TRUE) {
        nofchars <- tryCatch(nchar(x2), error = function(x) return(x))

        if (!is.list(nofchars)) break

        # if here, tryCatch caught an error, most likely a multibyte character
        error <- unlist(strsplit(nofchars[[1]], split = " "))
        # remove the offending character
        x2 <- x2[-as.numeric(error[length(error)])]
        # and repeat the loop until no more problems appear
    }

    if (length(nofchars) == 0) {
        nofchars <- 1
    }

    maxvarchar <- max(nofchars, na.rm = TRUE)

    if (haven::is.labelled(x) | declared::is.declared(x)) {
        labels <- attr(x, "labels")
        if (is.character(labels)) {
            string <- TRUE
            maxvarchar <- max(maxvarchar, nchar(labels))
        }
    }

    decimals <- 0
    if (admisc::possibleNumeric(x)) {
        decimals <- admisc::numdec(x)
    }

    return(
        sprintf(
            "%s%s%s%d", 
            ifelse(string, "A", "F"),
            maxvarchar, 
            ifelse(string, "", "."),
            ifelse(string, "", decimals)
        )
    )
}
