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
#' Distinct character codes receive distinct numbers, including alternate
#' spellings such as `01` and `1`.
#'
#' Variable labels, display widths, custom SPSS attributes and Stata
#' characteristics are retained. String formats and storage widths are removed
#' when the values become numeric; subsequent exports derive numeric formats.
#' The `spss_metadata`, `stata_metadata` and `sas_metadata` lists are also
#' retained, except for their string storage fields (`format`, `width`, SAS
#' `informat` and Stata `type`).
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

    template <- x
    x <- declared::undeclare(x, drop = TRUE)

    label <- getElement(metadata, "label")
    na_values <- getElement(metadata, "na_values")

    x[x == ""] <- NA

    labels <- sort(labels)
    keys <- unique(c(unname(labels), na_values, x[!is.na(x)]))
    keys <- as.character(keys[!is.na(keys)])
    numeric_keys <- which(admisc::possibleNumeric(keys, each = TRUE))
    codes <- rep(NA_real_, length(keys))
    parsed <- suppressWarnings(as.numeric(keys[numeric_keys]))

    # Prefer the standard spelling when several distinct strings denote one number.
    priority <- order(keys[numeric_keys] != as.character(parsed))
    retained <- priority[!duplicated(parsed[priority])]
    codes[numeric_keys[retained]] <- parsed[retained]

    available <- seq_along(keys)
    available <- available[!is.element(available, codes)]
    allocate <- is.na(codes)
    codes[allocate] <- available[seq_len(sum(allocate))]
    all_map <- setNames(codes, keys)

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

    out <- declared::declared(
        x,
        label = label,
        labels = labels,
        na_values = na_values
    )

    return(restore_variable_metadata(out, template))
}
