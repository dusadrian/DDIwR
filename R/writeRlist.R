writeRlist <- function(Rlist, OS = "windows", attr = FALSE, indent) {
    
    if (missing(indent)) {
        indent <- 4
    }
    
    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }
    enter <- getEnter(OS=OS)
    
    rs <- function(x) {
        paste(rep(" ", x*indent), collapse="")
    }
    
    
    
    for (i in seq(length(Rlist))) {
        
        if (attr) {
            cat("attr(rdatafile$", names(Rlist)[i], ", \"metadata\") <- list(", enter, sep = "")
        }
        else {
            cat("metadata$", names(Rlist)[i], " <- list(", enter, sep = "")
        }
        
        cat(rs(1), "label = \"", Rlist[[i]]$label, "\"", sep = "") 
        
        if (is.element("values", names(Rlist[[i]]))) {
            cat(",", enter, rs(1), "values = c(", enter, sep = "")
            
            values <- Rlist[[i]]$values
            notNum <- any(is.na(suppressWarnings(as.numeric(values))))
            labl <- names(values)
            
            for (lbl in seq(length(values))) {
                cat(rs(2), "\"", labl[lbl], "\" = ", sep = "")
                
                if (notNum) {
                    cat("\"", values[lbl], "\"", sep = "")
                }
                else {
                    cat(values[lbl])
                }
                
                cat(ifelse(lbl < length(labl), paste(",", enter, sep = ""), ")"), sep = "")
            }
        }
        
        if (is.element("missing", names(Rlist[[i]]))) {
            missng <- Rlist[[i]]$missing
            notNum <- any(is.na(suppressWarnings(as.numeric(missng))))
            cat(",", enter, sep = "")
            # here, if more feature will be added (like type or measurement), enter should be eliminated
            cat(rs(1), "missing = ", ifelse(length(missng) > 1,
                paste("c(", paste(missng, collapse = ifelse(notNum, "\", \"", ", ")), ")", sep = ""),
                ifelse(notNum, paste("\"", missng, "\"", sep = ""), missng)), sep = "")
        }
            
        
        cat(enter, ")", enter, enter, sep = "") # close the variable specific list
    }
}
