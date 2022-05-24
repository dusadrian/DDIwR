`checkType` <- function(x, labels, na_values) {
    
    xnumeric <- admisc::possibleNumeric(x)
    uniquevals <- unique(x)
    
    if (length(labels) > 0) {
        
        # possibly a categorical variable
        # but even numeric variables can have labels (for missing values)
        # unique values excepting the missing values
        except_na <- setdiff(uniquevals, na_values)
        
        if (all(is.element(labels, na_values))) {
            if (xnumeric) {
                if (length(except_na) < 15) {
                    return("numcat")
                }
                return("num")
            }
            else {
                return("char")
            }
        }
        
        
        # verify if the number of unique values is (at most) equal to the number of labels
        
        if (all(is.element(except_na, labels))) {
            # surely a categorical variable, all values are labelled
            if (admisc::possibleNumeric(labels)) {
                return("cat")
            }
            return("catchar")
        }
            
        # some values do not have labels, possibly a ordinal variable (e.g. 1...7)
        # with only two labels (for 1 and for 7) or maybe a categorical variable
        # for which not all values are labeled
        
        # thinking of the smallest ordinal scale 1...7 that can be interpreted
        return(ifelse(length(except_na) < 7, "cat", "catnum"))

        # TODO: what if a variable has very many numerical values (>15) but only one or two labels?
        # this should be a coding mistake, should it trigger an error or a warning?
    }

    if (xnumeric) {
        # pure numerical variable with no labels at all
        if (length(uniquevals) < 15) {
            return("numcat")
        }
        else {
            return("num")
        }
    }
    
    return("char")
}
