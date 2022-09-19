#' @description Get the number of decimals from a given number
#' @return Numeric scalar
#' @noRd
`getDecimals` <- function(number) {
    decs <- unique(nchar(format(abs(number), scientific = FALSE)) - (trunc(log10(max(1, trunc(abs(number))))) + 1) - 1)
    return(ifelse(decs < 0, 0, decs))
}
