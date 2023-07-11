#' @description Determine the variable type: categorical, numerical or mixed
#' @return A character scalar
#' @noRd
`checkType` <- function(x, labels = NULL, na_values = NULL, na_range = NULL) {

    xnumeric <- admisc::possibleNumeric(x)
    x <- declared::undeclare(x, drop = TRUE)
    uniquevals <- unique(x)
    all_nas <- declared:::all_missing_values(
        x = unclass(x),
        labels = labels,
        na_values = na_values,
        na_range = na_range
    )

    if (length(labels) > 0) {

        # possibly a categorical variable
        # but even numeric variables can have labels (for missing values)
        # check the unique values without the missing ones
        except_na <- setdiff(uniquevals, all_nas)

        if (all(is.element(labels, all_nas))) {
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
        # as numeric
        return(ifelse(length(except_na) < 7, "cat", "catnum"))

        # TODO: what if a variable has very many numerical values (>15) but only
        # one or two labels?
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
