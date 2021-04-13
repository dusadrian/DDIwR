`tibbleMetadata` <- function(dataDscr, OS = "", indent = 4) {
    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }
    enter <- getEnter(OS = OS)
    
    rs <- function(x) {
        paste(rep(" ", x * indent), collapse="")
    }
    
    if (is.element("dataDscr", names(dataDscr))) {
        dataDscr <- dataDscr$dataDscr
    }
    
    for (i in seq(length(dataDscr))) {
        if (is.element("labels", names(dataDscr[[i]]))) {

            values <- dataDscr[[i]]$labels
            labels <- names(values)
            notNum <- any(is.na(suppressWarnings(as.numeric(values))))
            quote <- ifelse(notNum, "\"", "")
            
            valstring <- paste(paste("\"", labels, "\"", sep = ""),
                               paste(quote, values, quote, sep = ""),
                               sep = " = ", collapse = ",\n               ")
            cat("rdatafile[[\"", names(dataDscr)[i], "\"]] <- haven::labelled(rdatafile[[\"", names(dataDscr)[i], "\"]],", enter, sep="")
            cat(rs(1), "labels = c(", valstring, "),", enter, sep = "")
            cat(rs(1), "label = \"", dataDscr[[i]]$label, "\"", sep = "")
            cat(enter, ")", enter, enter, sep = "")
            if (is.element("na_values", names(dataDscr[[i]]))) {
                cat(enter, "na_values(rdatafile[[\"", names(dataDscr)[i], "\"]]) <- c(", paste(quote, dataDscr[[i]]$na_values, quote, sep = "", collapse = ", "), ")", sep = "")
            }
        }
    }
}
