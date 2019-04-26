`tibbleMetadata` <- function(dataDscr, OS = "", indent = 4) {
    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }
    enter <- getEnter(OS = OS)
    
    rs <- function(x) {
        paste(rep(" ", x*indent), collapse="")
    }
    
    if (is.element("dataDscr", names(dataDscr))) {
        dataDscr <- dataDscr$dataDscr
    }
    
    for (i in seq(length(dataDscr))) {
        if (is.element("values", names(dataDscr[[i]]))) {

            values <- dataDscr[[i]]$values
            labl <- names(values)
            notNum <- any(is.na(suppressWarnings(as.numeric(values))))
            quote <- ifelse(notNum, "\"", "")
            
            valstring <- paste(paste("\"", labl, "\"", sep = ""),
                               paste(quote, values, quote, sep = ""),
                               sep = " = ", collapse = ",\n               ")
            cat("rdatafile[[\"", names(dataDscr)[i], "\"]] <- haven::labelled_spss(rdatafile[[\"", names(dataDscr)[i], "\"]],", enter, sep="")
            cat(rs(1), "labels = c(", valstring, ")", sep = "")
            if (is.element("missing", names(dataDscr[[i]]))) {
                cat(",", enter, rs(1), "na_values = c(", paste(quote, dataDscr[[i]]$missing, quote, sep = "", collapse = ", "), ")", sep = "")
            }
            cat(enter, ")", enter, sep = "")
        }
        cat("attr(rdatafile[[\"", names(dataDscr)[i], "\"]], \"label\") <- \"", dataDscr[[i]]$label, "\"", enter, enter, sep="")
    }
}
