
`make_labelled` <- function(x, dataDscr, ...) {

    pN <- unlist(lapply(x, function(x) admisc::possibleNumeric(x)))
    
    for (i in names(x)) {
        #------------------------------------------------------------------
        # attrx$label, if not existing, takes from attrx$labels
        # attrx[["label"]] is something like attr(x, "label", exact = TRUE)
        label <- dataDscr[[i]][["label"]]
        labels <- dataDscr[[i]][["labels"]]
        #------------------------------------------------------------------

        na_values <- dataDscr[[i]][["na_values"]]
        na_range <- dataDscr[[i]][["na_range"]]

        v <- unname(unlist(unclass(x[, i])))
        if (pN[i]) {
            v <- admisc::asNumeric(v)
        }
        
        x[[i]] <- declared::declared(v, labels, na_values, na_range, label)

    }

    other.args <- list(...)
    if (is.element("spss", names(other.args))) {
        if (other.args$spss) {
            x[] <- lapply(x, function(x) {
                attr(x, "format.spss") <- getFormat(x)
                return(x)
            })
        }
    }

    return(x)
}
