makeXML <- function(x, space = 1, indent = 4, ns = "", enter = "\n") {
    sx <- paste(rep(" ", space*indent), collapse = "")
    result <- character(0)
    nmsx <- names(x)
    if (!is.null(nmsx)) {
        for (n in seq(length(nmsx))) {
            childnms <- names(x[[nmsx[n]]])
            attrx <- attributes(x[[nmsx[n]]])
            attrx$names <- NULL
            start <- paste(sx, "<", ns, nmsx[n], sep = "")
            
            if (length(attrx) > 0) {
                nms <- names(attrx)
                for (i in seq(length(attrx))) {
                    if (nms[i] == "lang") {
                        nms[i] <- "xml:lang"
                    }
                    start <- paste(
                        start,
                        paste(
                            nms[i],
                            paste("\"", attrx[[i]], "\"", sep = ""),
                            sep = "="
                        )
                    )
                }
            }

            start <- paste(
                start,
                ">",
                ifelse(is.null(childnms), "", enter),
                sep = ""
            )

            end <- paste(
                ifelse(is.null(childnms), "", sx),
                "</",
                ns,
                nmsx[n],
                ">",
                enter,
                sep = ""
            )

            result <- c(
                result,
                start,
                makeXML(x[[n]], space + 1),
                end
            )
        }
    }
    else {
        result <- c(result, x)
    }
    
    return(unlist(result))
}
