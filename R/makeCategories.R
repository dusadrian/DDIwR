#' @name makeCategories
#'
#' @title
#' Create the `catgry` elements for a particular variable
#'
#' @description Utility function to create the `catgry` elements, as well as all
#' necessary sub-elements (e.g. `catValu`, `labl`, `varFormat`) along with their
#' associated XML attributes.

#' @return A list of standard `catgry` DDI elements.
#'
#' @author Adrian Dusa
#'
#' @param metadata A list of two or three components: `labels`,
#' `na_values` and/or `na_range`
#'
#' @export
`makeCategories` <- function(metadata) {
    if (!is.list(metadata) || is.null(names(metadata))) {
        admisc::stopError("The argument 'metadata' should be a names list.")
    }

    nms <- names(metadata)

    if (!is.element("labels", nms)) {
        admisc::stopError("Values and labels are necessary to create the `catgry` element")
    }

    values <- unname(getElement(metadata, "labels"))
    labels <- names(getElement(metadata, "labels"))
    ismiss <- logical(length(values))

    na_values <- metadata$na_values
    if (!is.null(na_values)) {
        ismiss <- is.element(values, na_values)
    }
    na_range <- metadata$na_range
    if (!is.null(na_range)) {
        ismiss <- ismiss | (values >= na_range[1] & values <= na_range[2])
    }

    return(lapply(seq_along(values), function(i) {
        icatgry <- makeElement("catgry")

        if (ismiss[i]) {
            addAttributes(c(missing = "Y"), to = icatgry)
        }

        addChildren(
            list(
                makeElement("labl", content = labels[i]),
                makeElement("catValu", content = values[i])
            ),
            to = icatgry
        )

        return(icatgry)
    }))
}
