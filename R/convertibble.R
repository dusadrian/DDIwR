
`convertibble` <- function(x, dataDscr) {

    `possibleNumeric` <- function(x) {
        # as.character converts everything (especially factors)
        return(!any(is.na(suppressWarnings(as.numeric(na.omit(as.character(x)))))) & !all(is.na(x)))
    }
    
    `asNumeric` <- function(x) {
        return(suppressWarnings(as.numeric(as.character(x))))
    }

    # integer numbers, but not necessarily declared as integers
    `wholeNumber` <- function(x) {
        all(floor(x) == x, na.rm = TRUE)
    }

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

            if (possibleNumeric(var)) {
                var <- asNumeric(var)
                if (wholeNumber(var)) {
                    var <- as.integer(var)
                }
            }
            else {
                var <- as.character(var)
            }
            
            x[[i]] <- haven::labelled_spss(var, labels, na_values, na_range)
        }

        attr(x[[i]], "label") <- dataDscr[[i]]$label

    }

    return(x)
}
