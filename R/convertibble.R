
`convertibble` <- function(x, dataDscr) {

    for (i in names(x)) {
        
        if (is.element("values", names(dataDscr[[i]]))) {

            na_values <- NULL
            if (is.element("missing", names(dataDscr[[i]]))) {
                na_values <- dataDscr[[i]]$missing
            }

            na_range <- NULL
            if (is.element("missrange", names(dataDscr[[i]]))) {
                na_range <- dataDscr[[i]]$missrange
            }

            labels <- dataDscr[[i]]$values
            
            var <- unname(unlist(unclass(x[, i])))

            if (admisc::possibleNumeric(var)) {
                var <- admisc::asNumeric(var)
            }
            else {
                var <- as.character(var)
            }
            
            x[[i]] <- haven::labelled_spss(var, labels, na_values, na_range)
        }

        attr(x[[i]], "label") <- dataDscr[[i]]$label

    }

    x[] <- lapply(x, function(x) {
                attr(x, "format.spss") <- getFormat(x)
                return(x)
            })

    return(x)
}
