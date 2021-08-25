`collectMetadata` <- function(dataset, ...) {
    dots <- list(...)

    error_null <- TRUE
    if (is.element("error_null", names(dots))) {
        error_null <- dots$error_null
    }

    if (is.data.frame(dataset)) {
        error <- TRUE
        i <- 1
        while (i <= ncol(dataset) & error) {
            attrx <- attributes(dataset[[i]])
            if (any(is.element(
                c("label", "labels", "na_value", "na_range"),
                names(attrx)
            ))) {
                error <- FALSE
            }
            i <- i + 1
        }

        if (error && error_null) {
            admisc::stopError(
                "The input does not seem to contain any metadata."
            )
        }
    }
    else {
        admisc::stopError(
            "The input should be a dataframe containing labelled variables."
        )
    }

    output <- lapply(dataset, function(x) {
        result <- list()
        
        label <- attr(x, "label", exact = TRUE)
        if (!is.null(label)) {
            result[["label"]] <- DDIwR::cleanup(label)
        }


        labels <- attr(x, "labels", exact = TRUE)
        if (!is.null(labels)) {
            tagged <- logical(length(labels))
            if (is.double(labels)) {
                tagged <- haven::is_tagged_na(labels)
            }

            labels <- labels[!is.na(labels) | tagged]
            if (length(labels) > 0) {
                names(labels) <- DDIwR::cleanup(names(labels))
                result[["labels"]] <- labels
            }
        }
        
        na_values <- attr(x, "na_values", exact = TRUE)
        if (is.null(na_values)) {
            if (is.double(x)) {
                natags <- unique(haven::na_tag(c(unclass(x), unclass(labels))))
                natags <- natags[!is.na(natags)]
                if (length(natags) > 0) {
                    result$na_values <- sort(natags)
                }
            }
        }
        else {
            # it should't have (tagged) NA values, but just in case
            na_values <- na_values[!is.na(na_values)]
            if (length(na_values) > 0) {
                result$na_values <- na_values
            }
        }
        
        result$na_range <- attr(x, "na_range", exact = TRUE)
        result$type <- DDIwR::checkType(
            x,
            labels,
            admisc::possibleNumeric(x)
        )

        return(result)
    })

    return(output)
}
