
`convertibble` <- function(x, dataDscr, ...) {

    pN <- unlist(lapply(x, function(x) admisc::possibleNumeric(x)))
    
    for (i in names(x)) {
        
        if (is.element("labels", names(dataDscr[[i]]))) {

            na_values <- NULL
            if (is.element("na_values", names(dataDscr[[i]]))) {
                na_values <- dataDscr[[i]]$na_values
            }

            na_range <- NULL
            if (is.element("na_range", names(dataDscr[[i]]))) {
                na_range <- dataDscr[[i]]$na_range
            }

            labels <- dataDscr[[i]]$labels
            
            var <- unname(unlist(unclass(x[, i])))

            if (pN[i]) {
                var <- admisc::asNumeric(var)
            }
            else {
                var <- as.character(var)
                labels <- unlist(lapply(labels, as.character))
                na_values <- unlist(lapply(na_values, as.character))
                na_range <- unlist(lapply(na_range, as.character))
            }
            
            x[[i]] <- haven::labelled_spss(var, labels, na_values, na_range)
        }

        attr(x[[i]], "label") <- dataDscr[[i]]$label

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
