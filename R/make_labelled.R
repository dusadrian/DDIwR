
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

        if (!is.null(labels)) {
            if (admisc::possibleNumeric(labels) && pN[i]) {
                labels <- admisc::asNumeric(labels)
            }
            else {
                x[[i]] <- as.character(x[[i]])
                nms <- names(labels)
                labels <- as.character(labels)
                names(labels) <- nms
                pN[i] <- FALSE
                na_range <- NULL
            }
        }

        if (!is.null(na_values)) {
            if (admisc::possibleNumeric(na_values) & pN[i]) {
                na_values <- admisc::asNumeric(na_values)
            }
            else {
                na_values <- as.character(na_values)
            }
        }

        if (pN[i]) {
            x[[i]] <- admisc::asNumeric(x[[i]])
        }
        else {
            x[[i]] <- as.character(x[[i]])
        }
        
        v <- unname(unlist(unclass(x[, i])))
        
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
