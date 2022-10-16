#' @description Write an .R file containing a metadata specific list.
#' @return Nothing.
#' @noRd
`writeRlist` <- function(
    dataDscr, OS = "windows", indent = 4, dirpath = "", filename = ""
) {
    
    on.exit(suppressWarnings(sink()))
    
    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }
    enter <- getEnter(OS=OS)

    currentdir <- getwd()
    setwd(dirpath)

    sink(sprintf("%s.R", filename))
    cat("codeBook <- list(dataDscr = list(", enter)
    
    rs <- function(x) {
        paste(rep(" ", x * indent), collapse="")
    }
    
    if (is.element("dataDscr", names(dataDscr))) {
        dataDscr <- dataDscr$dataDscr
    }
    
    for (i in seq(length(dataDscr))) {
        
        cat(names(dataDscr)[i], " = list(", enter, sep = "")
        
        cat(rs(1), "label = \"", dataDscr[[i]][["label"]], "\"", sep = "") 
        
        if (is.element("labels", names(dataDscr[[i]]))) {
            cat(",", enter, rs(1), "labels = c(", enter, sep = "")
            
            values <- dataDscr[[i]][["labels"]]
            names(values) <- cleanup(names(values))
            notNum <- any(is.na(suppressWarnings(as.numeric(values))))
            labl <- names(values)
            
            for (lbl in seq(length(values))) {
                cat(rs(2), "\"", labl[lbl], "\" = ", sep = "")
                quote <- ifelse(notNum, "\"", "")
                cat(quote, values[lbl], quote, sep = "")
                cat(
                    ifelse(
                        lbl < length(labl),
                        paste(",", enter, sep = ""),
                        paste(enter, rs(2), ")", sep = "")
                    )
                )
            }
        }
        
        if (is.element("na_values", names(dataDscr[[i]]))) {
            na_values <- dataDscr[[i]]$na_values
            notNum <- any(is.na(suppressWarnings(as.numeric(na_values))))
            cat(",", enter, sep = "")
            cat(
                rs(1),
                "na_values = ",
                ifelse(
                    length(na_values) > 1,
                    paste(
                        "c(",
                        paste(
                            na_values,
                            collapse = ifelse(
                                notNum,
                                "\", \"",
                                ", "
                            )
                        ),
                        ")",
                        sep = ""
                    ),
                    ifelse(
                        notNum,
                        paste("\"", na_values, "\"", sep = ""),
                        na_values
                    )
                ),
                sep = ""
            )
        }
        
        if (is.element("na_range", names(dataDscr[[i]]))) {
            na_range <- dataDscr[[i]]$na_range
            cat(",", enter, sep = "")
            cat(
                rs(1),
                "na_range = c(",
                paste(na_range, collapse = ", "),
                ")",
                sep = ""
            )
        }
        
        if (is.element("type", names(dataDscr[[i]]))) {
            cat(",", enter, sep = "")
            cat(rs(1), "type = \"", dataDscr[[i]]$type, "\"", sep = "")
        }
        
        if (is.element("measurement", names(dataDscr[[i]]))) {
            cat(",", enter, sep = "")
            cat(
                rs(1),
                "measurement = \"",
                dataDscr[[i]]$measurement,
                "\"",
                sep = ""
            )
        }
            
        # if (attr) {
        #     # close the variable specific list
        #     cat(enter, ")", enter, enter, sep = "")
        # }
        # else {
            cat(
                enter,
                ifelse(
                    i == length(dataDscr),
                    ")",
                    "),"
                ),
                enter,
                sep = ""
            )
        # }
    }

    cat("))", enter)
    sink()
    setwd(currentdir)
}
