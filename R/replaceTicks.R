#' @description Recode all tick characters with a single quote.
#' @return A recoded string.
#' @noRd
`replaceTicks` <- function(x) {

    # # weird A character sometimes from encoding a single tick quote
    # "\u00c2"
    # # forward and back ticks
    # c("\u00b4", "\u0060")

    tc <- admisc::tryCatchWEM({
        # use case: StatConverter in Electron, the terminal that opens R
        # probably doesn't have a suitable locale and it outputs an error
        # this does the same thing (using hexadecimal code) and is better
        achar <- "\u00c2"
        tick <- c("\u00b4", "\u0060")
        
        tick <- c(paste0(achar, "'"), paste0(achar, tick), tick)
        x <- gsub(paste(tick, collapse = "|"), "'", x)
    })
    
    return(x)
}
