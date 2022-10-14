#' @description Determine the SPSS / Stata variable format
#' @return Character scalar
#' @noRd
`getFormat` <- function(x, type = c("SPSS", "Stata"), ...) {

    dots <- list(...)
    type <- toupper(match.arg(type))

    labels <- dots[["labels"]]
    if (is.null(labels) && (haven::is.labelled(x) | declared::is.declared(x))) {
        labels <- attr(x, "labels", exact = TRUE)
    }

    attributes(x) <- NULL
    attributes(labels) <- NULL

    pN <- TRUE
    allnax <- all(is.na(x))
    nullabels <- is.null(labels)
    if (!(allnax & nullabels)) {
        pN <- admisc::possibleNumeric(c(x, labels))
    }

    decimals <- 0
    if (pN & !allnax) {
        decimals <- admisc::numdec(x)
    }

    x <- as.character(x)

    maxvarchar <- 0
    if (!allnax) {
        nofchars <- nchar(x[!grepl("[^!-~ ]", x)])

        if (length(nofchars) > 0) {
            maxvarchar <- max(nofchars, na.rm = TRUE)
        }
    }

    if (!nullabels & !pN) {
        maxvarchar <- max(maxvarchar, nchar(labels[!grepl("[^!-~ ]", labels)]))
    }    

    if (type == "SPSS") {
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
    else if (type == "STATA") {
        return(
            paste0("%",
                sprintf(
                    "%s%s%s%s", 
                    maxvarchar, 
                    ifelse(pN, ".", ""),
                    ifelse(pN, decimals, ""),
                    ifelse(pN, "g", "s")
                )
            )
        )
    }
}
