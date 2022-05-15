
`make_labelled` <- function(x, dataDscr, declared = TRUE, ...) {

    for (i in names(x)) {
        #------------------------------------------------------------------
        # attrx$label, if not existing, takes from attrx$labels
        # attrx[["label"]] is something like attr(x, "label", exact = TRUE)
        label <- dataDscr[[i]][["label"]]
        labels <- dataDscr[[i]][["labels"]]
        #------------------------------------------------------------------

        na_values <- dataDscr[[i]][["na_values"]]
        na_range <- dataDscr[[i]][["na_range"]]

        v <- x[[i]]
        attributes(v) <- NULL

        pN <- TRUE
        allnav <- all(is.na(v))
        nullabels <- is.null(labels)
        if (!(allnav & nullabels)) {
            pN <- admisc::possibleNumeric(c(v, unname(labels)))
        }

        if (pN) {
            v <- admisc::asNumeric(v)
        }
        else {
            v <- as.character(v)
            na_range <- NULL
        }

        if (!nullabels) {
            nms <- names(labels)
            if (pN) {
                labels <- admisc::asNumeric(labels)
            }
            else {
                labels <- as.character(labels)
            }
            names(labels) <- nms
        }

        if (!is.null(na_values)) {
            if (admisc::possibleNumeric(na_values) & pN) {
                na_values <- admisc::asNumeric(na_values)
            }
            else {
                na_values <- as.character(na_values)
            }
        }
        
        
        if (declared) {
            x[[i]] <- declared::declared(v, labels, na_values, na_range, label)
        }
        else {
            x[[i]] <- haven::labelled_spss(v, labels, na_values, na_range, label)
        }

    }

    dots <- list(...)
    if (is.element("spss", names(dots))) {
        if (dots$spss) {
            x[] <- lapply(x, function(x) {
                attr(x, "format.spss") <- getFormat(x)
                return(x)
            })
        }
    }

    return(x)
}
