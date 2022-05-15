`getFormat` <- function(x, ...) {

    dots <- list(...)
    labels <- dots[["labels"]]
    if (is.null(labels) && (haven::is.labelled(x) | declared::is.declared(x))) {
        labels <- attr(x, "labels", exact = TRUE)
    }

    pN <- TRUE
    allnax <- all(is.na(x))
    nullabels <- is.null(labels)
    if (!(allnax & nullabels)) {
        pN <- admisc::possibleNumeric(c(x, unname(labels)))
    }

    decimals <- 0
    if (pN & !allnax) {
        decimals <- admisc::numdec(x)
    }

    x <- as.character(declared::undeclare(x, drop = TRUE))

    if (allnax) {
        maxvarchar <- 0
    }
    else {
        while (TRUE) {
            nofchars <- tryCatch(nchar(x), error = function(x) return(x))

            if (!is.list(nofchars)) break

            # if here, tryCatch caught an error, most likely a multibyte character
            error <- unlist(strsplit(nofchars[[1]], split = " "))
            # remove the offending character
            x <- x[-as.numeric(error[length(error)])]
            # and repeat the loop until no more problems appear
        }

        if (length(nofchars) > 0) {
            maxvarchar <- max(nofchars, na.rm = TRUE)
        }
    }

    if (pN)

    if (!nullabels & !pN) {
        maxvarchar <- max(maxvarchar, nchar(labels))
    }    

    return(
        sprintf(
            "%s%s%s%s", 
            ifelse(pN, "F", "A"),
            maxvarchar, 
            ifelse(pN, ".", ""),
            ifelse(pN, decimals, "")
        )
    )
}
