`checkType` <- function(x, labels, xnumeric) {
    
    if (length(labels) > 0) {
        
        # possibly a categorical variable
        # but even numeric variables can have labels (for missing values)
        
        # verify if the number of unique values is (at most) equal to the number of labels
        uniquevals <- unique(x)
        
        if (length(uniquevals) <= length(labels)) {
            # surely a categorical variable
            return("cat")
        }
        else {
            
            # the number of unique values is greater than the number of labels
            # possibly a numeric variable (e.g. 1...10) with only two labels (for 1 and for 10)
            # or maybe a categorical variable for which not all values are labeled
            
            # unique values without labels
            nolabels <- setdiff(uniquevals, labels)
            
            # 5 and 8 are arbitrary numbers,
            # thinking of the smallest ordinal scale 1...7 that can be interpreted as "numeric"
            # and an even bigger numerical scale with labels, such as 1...10
            
            if (length(nolabels) < 5) {
                return("cat")
            }
            else if (length(nolabels) < 9) {
                return("numcat")
            }
            else {
                return("num")
            }
        }
    }
    
    return(ifelse(xnumeric, "num", "char"))
}
