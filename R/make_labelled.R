
`make_labelled` <- function(x, dataDscr, declared = TRUE) {

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
                labels <- setNames(admisc::asNumeric(labels), nms)
            }
            else {
                labels <- setNames(as.character(labels), nms)
            }
            # names(labels) <- nms
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

        # this is always about format.spss since both "declared" and "labelled_spss"
        # are not using Stata type extended missing values, and by consequence
        # not using the Stata format type
        attr(x[[i]], "format.spss") <- dataDscr[[i]][["varFormat"]][1]

    }

    x[] <- lapply(x, function(x) {
        if (is.null(attr(x, "format.spss"))) {
            attr(x, "format.spss") <- getFormat(x, type = "SPSS")
        }
        return(x)
    })

    if (!declared) {
        class(x) <- c("tbl_df", "tbl", "data.frame")
    }

    return(x)
}
