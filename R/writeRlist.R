`writeRlist` <- function(dataDscr, OS = "windows", indent = 4) {
    
    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }
    enter <- getEnter(OS=OS)
    
    rs <- function(x) {
        paste(rep(" ", x*indent), collapse="")
    }
    
    if (is.element("dataDscr", names(dataDscr))) {
        dataDscr <- dataDscr$dataDscr
    }
    
    for (i in seq(length(dataDscr))) {
        
        cat(names(dataDscr)[i], " = list(", enter, sep = "")
        
        cat(rs(1), "label = \"", dataDscr[[i]]$label, "\"", sep = "") 
        
        if (is.element("values", names(dataDscr[[i]]))) {
            cat(",", enter, rs(1), "values = c(", enter, sep = "")
            
            values <- dataDscr[[i]]$values
            notNum <- any(is.na(suppressWarnings(as.numeric(values))))
            labl <- names(values)
            
            for (lbl in seq(length(values))) {
                cat(rs(2), "\"", labl[lbl], "\" = ", sep = "")
                quote <- ifelse(notNum, "\"", "")
                cat(quote, values[lbl], quote, sep = "")
                cat(ifelse(lbl < length(labl), paste(",", enter, sep = ""), paste(enter, rs(2), ")", sep = "")))
            }
        }
        
        if (is.element("missing", names(dataDscr[[i]]))) {
            missng <- dataDscr[[i]]$missing
            notNum <- any(is.na(suppressWarnings(as.numeric(missng))))
            cat(",", enter, sep = "")
            cat(rs(1), "missing = ", ifelse(length(missng) > 1,
                paste("c(", paste(missng, collapse = ifelse(notNum, "\", \"", ", ")), ")", sep = ""),
                ifelse(notNum, paste("\"", missng, "\"", sep = ""), missng)), sep = "")
        }
        
        if (is.element("missrange", names(dataDscr[[i]]))) {
            missrange <- dataDscr[[i]]$missrange
            cat(",", enter, sep = "")
            cat(rs(1), "missrange = c(", paste(missrange, collapse = ", "), ")", sep = "")
        }
        
        if (is.element("type", names(dataDscr[[i]]))) {
            cat(",", enter, sep = "")
            cat(rs(1), "type = \"", dataDscr[[i]]$type, "\"", sep = "")
        }
        
        if (is.element("measurement", names(dataDscr[[i]]))) {
            cat(",", enter, sep = "")
            cat(rs(1), "measurement = \"", dataDscr[[i]]$measurement, "\"", sep = "")
        }
            
        if (attr) {
            cat(enter, ")", enter, enter, sep = "") # close the variable specific list
        }
        else {
            cat(enter, ifelse(i == length(dataDscr), ")", "),"), enter, sep = "")
        }
    }
}

