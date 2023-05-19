#' @description Recode all tick characters with a single quote.
#' @return A recoded string.
#' @noRd
`replaceTicks` <- function(x) {

    # # weird A character sometimes from encoding a single tick quote
    # achar <- rawToChar(as.raw(c(195, 130)))
    # # forward and back ticks
    # irv <- c(194, 180, 96)
    # tick <- unlist(strsplit(rawToChar(as.raw(irv)), split = ""))

    # use case: StatConverter in Electron, the terminal that opens R
    # probably doesn't have a suitable locale and it outputs and error
    # this does the same thing (using hexadecimal code) and is better
    achar <- "\uc2"
    tick <- c("\ub4", "\u60")
    
    tick <- c(paste0(achar, "'"), paste0(achar, tick), tick)
    x <- gsub(paste(tick, collapse = "|"), "'", x)
    
    return(x)
}
