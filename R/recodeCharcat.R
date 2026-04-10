#' @name recodeCharcat
#'
#' @title Recode character categorical variables
#'
#' @description
#' Recodes a character categorical variables to a numerical categorical
#' variable.
#'
#' @details
#' For this function, a categorical variable is something else than a base
#' factor. It should be an object of class `"declared"` with a specific
#' attribute called `"labels"` that stores the value labels.
#'
#' @return
#' A numeric categorical variable of the same class as the input.
#'
#' @examples
#' x <- declared(
#'     c(letters[1:5], -91),
#'     labels = c(Good = "a", Bad = "e", NR = -91),
#'     na_values = -91
#' )
#'
#' recodeCharcat(x)
#'
#' @author Adrian Dusa
#'
#' @param x A character categorical variable
#' @param ... Other internal arguments
#'
#' @export

`recodeCharcat` <- function(x, ...) {
    if (!is.character(x)) {
        return(x)
    }

    dots <- list(...)
    metadata <- dots$metadata

    if (is.null(metadata)) {
        metadata <- attributes(x)
    }

    labels <- getElement(metadata, "labels")
    xdeclared <- inherits(x, "declared")


    # only character _categorical_ variables should be recoded
    if (
        is.null(labels) &
        !xdeclared
    ) {
        # nothing to recode, no information about categories
        return(x)
    }


    x <- declared::undeclare(x, drop = TRUE)

    label <- getElement(metadata, "label")
    na_values <- getElement(metadata, "na_values")

    x[x == ""] <- NA

    # TODO: make sure the values of missing codes are way outside the
    # range of the normal values

    labels <- sort(labels)
    ux <- unique(c(unname(labels), na_values, x[!is.na(x)]))
    pnux <- admisc::possibleNumeric(ux, each = TRUE)

    numeric_map <- suppressWarnings(as.numeric(ux[pnux]))
    names(numeric_map) <- as.character(ux[pnux])

    if (any(!pnux)) {
        cux <- as.character(ux[!pnux])
        next_code <- 1L
        if (length(numeric_map) > 0) {
            while (next_code %in% numeric_map) {
                next_code <- next_code + 1L
            }
        }

        char_map <- integer(length(cux))
        for (i in seq_along(cux)) {
            char_map[i] <- next_code
            next_code <- next_code + 1L
            while (next_code %in% numeric_map || next_code %in% char_map[seq_len(i)]) {
                next_code <- next_code + 1L
            }
        }
        names(char_map) <- cux

        all_map <- c(numeric_map, char_map)
    }
    else {
        all_map <- numeric_map
    }

    x_chr <- as.character(x)
    x <- suppressWarnings(as.numeric(all_map[x_chr]))
    if (length(labels) > 0) {
        labels_chr <- as.character(labels)
        labels <- setNames(
            suppressWarnings(as.numeric(all_map[labels_chr])),
            names(labels)
        )
    }
    if (length(na_values) > 0) {
        na_values <- suppressWarnings(as.numeric(all_map[as.character(na_values)]))
        na_values <- na_values[!is.na(na_values)]
    }
    else {
        na_values <- NULL
    }

    if (xdeclared) {
        return(declared::declared(
            x,
            label = label,
            labels = labels,
            na_values = na_values
        ))
    }

    return(declared::declared(
        x,
        label = label,
        labels = labels,
        na_values = na_values
    ))
}
