#' @description Collect metadata from a file or a dataframe object
#' @return A list containing variable level metadata information
#' @noRd
`collectMetadata` <- function(dataset, ...) {
    dots <- list(...)

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

        if (error && !isFALSE(dots$error_null)) {
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
            result[["label"]] <- cleanup(label)
        }

        measurement <- attr(x, "measurement", exact = TRUE)
        if (!is.null(measurement)) {
            result[["measurement"]] <- cleanup(measurement)
        }

        tagged <- FALSE
        labels <- lbls <- attr(x, "labels", exact = TRUE)
        if (!is.null(labels)) {
            tagged <- haven::is_tagged_na(labels)
            # if (any(tagged)) {
            #     labels[tagged] <- haven::na_tag(labels[tagged])
            # }

            nms <- names(labels)
            if (is.character(labels)) {
                labels <- cleanup(labels)
            }
            names(labels) <- cleanup(nms)
            result[["labels"]] <- labels
        }
        else if (is.factor(x)) {
            xlevels <- levels(x)
            # labels <- seq(length(xlevels))
            # names(labels) <- xlevels
            # result[["labels"]] <- labels
            result[["labels"]] <- setNames(seq(length(xlevels)), xlevels)
            x <- as.numeric(x)
        }

        na_values <- attr(x, "na_values", exact = TRUE)
        if (is.null(na_values)) {
            xtagged <- haven::is_tagged_na(x)
            if (any(tagged) | any(xtagged)) {
                natags <- unique(haven::na_tag(c(unclass(x), unclass(lbls))))
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
        result$type <- checkType(
            x,
            labels,
            na_values,
            result$na_range
        )


        format.spss <- attr(x, "format.spss", exact = TRUE)
        if (is.null(format.spss)) {
            format.spss <- getFormat(x, type = "SPSS")
        }

        format.stata <- attr(x, "format.stata", exact = TRUE)
        if (is.null(format.stata)) {
            format.stata <- getFormat(x, type = "Stata")
        }

        result[["varFormat"]] <- c(format.spss, format.stata)

        return(result)
    })

    return(output)
}
