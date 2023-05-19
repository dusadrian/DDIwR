#' @description Recode all tick characters with a single quote.
#' @return A recoded string.
#' @noRd
`replaceTicks` <- function(x) {
    tc <- admisc::tryCatchWEM({
        # weird A character sometimes from encoding a single tick quote
        achar <- rawToChar(as.raw(c(195, 130)))
        # forward and back ticks
        irv <- c(194, 180, 96)
        tick <- unlist(strsplit(rawToChar(as.raw(irv)), split = ""))
        tick <- c(paste0(achar, "'"), paste0(achar, tick), tick)
        x <- gsub(paste(tick, collapse = "|"), "'", x)
    })
    
    return(x)
}
