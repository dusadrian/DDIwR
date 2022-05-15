
`make_labelled` <- function(x, dataDscr, declared = TRUE, ...) {

    pN <- sapply(x, admisc::possibleNumeric)
    
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
        allnav <- all(is.na(v))

        if (is.null(labels)) {
            if (allnav) {
                pN[i] <- TRUE
            }
        }
        else {
            if (admisc::possibleNumeric(labels) && (pN[i] | allnav)) {
                labels <- admisc::asNumeric(labels)
            }
            else {
                v <- as.character(v)
                nms <- names(labels)
                labels <- as.character(labels)
                names(labels) <- nms
                pN[i] <- FALSE
                na_range <- NULL
            }
        }

        if (!is.null(na_values)) {
            if (admisc::possibleNumeric(na_values) & (pN[i] | allnav)) {
                na_values <- admisc::asNumeric(na_values)
            }
            else {
                na_values <- as.character(na_values)
            }
        }

        if (pN[i] || (allnav & is.numeric(labels))) {
            v <- admisc::asNumeric(v)
        }
        else {
            v <- as.character(v)
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
