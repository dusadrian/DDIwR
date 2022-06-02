`recodeCharcat` <- function(x, ...) {
    if (!is.character(x)) {
        return(x)
    }

    dots <- list(...)
    metadata <- dots$metadata

    attrx <- attributes(x)
    x <- declared::undeclare(x)
    attributes(x) <- NULL

    if (is.null(metadata)) {
        metadata <- attrx
    }
    
    label <- metadata[["label"]]
    labels <- metadata[["labels"]]
    na_values <- metadata[["na_values"]]
    
    # only character _categorical_ variables should be recoded
    if (is.null(labels)) {
        # nothing to recode, no information about categories
        attributes(x) <- attrx
        return(x)
    }

    x[x == ""] <- NA

    ### TODO: make sure the values of missing codes are way outside the range of the normal values

    labels <- sort(labels)
    ux <- unique(c(unname(labels), na_values, x[!is.na(x)]))
    pnux <- admisc::possibleNumeric(ux, each = TRUE)

    nums <- c()
    if (any(pnux)) {
        nums <- as.numeric(ux[pnux])
    }

    if (any(!pnux)) {
        nms_l <- names(labels)

        cux <- ux[!pnux]
        lux <- length(cux)
        n <- l <- 1
        
        while (l <= lux) {
            if (!is.element(n, nums)) {
                x[x == cux[l]] <- n
                labels[labels == cux[l]] <- n
                na_values[na_values == cux[l]] <- n
                l <- l + 1
            }

            n <- n + 1
        }

        labels <- setNames(as.numeric(labels), nms_l)
    }

    x <- as.numeric(x)
    if (length(na_values) > 0) {
        na_values <- as.numeric(na_values)
        na_values <- na_values[!is.na(na_values)]
    }
    else {
        na_values <- NULL
    }

    if (is.element("declared", attrx$class)) {
        return(declared::declared(
            x,
            label = label,
            labels = labels,
            na_values = na_values
        ))
    }
    
    return(haven::labelled_spss(
        x,
        label = label,
        labels = labels,
        na_values = na_values
    ))
}