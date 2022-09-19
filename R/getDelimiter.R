#' @description Guess the column delimiter from a text file
#' @return Character scalar
#' @noRd
`getDelimiter` <- function(x) {
    
    delimiter <- ","
        
    csvreadfile <- read.csv(x, as.is = TRUE)
    
    # if the delimiter is not a comma, there will be only one big column
    if (ncol(csvreadfile) == 1) { # try ";" separated
        delimiter <- ";"
        csvreadfile <- read.csv(x, sep = ";", as.is = TRUE)
    }
    
    # if still the delimiter is not the right one
    if (ncol(csvreadfile) == 1) { # try tab separated
        delimiter <- "\t"
        csvreadfile <- read.csv(x, sep = "\t", as.is = TRUE)
    }
    
    # finally, if it's still not the right delimiter stop and print an error message
    if (ncol(csvreadfile) == 1) {
        return("unknown")
    }
    
    return(delimiter)
        
}
