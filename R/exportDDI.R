exportDDI <- function(obj, file = "", indent = 4, OS = "") {
    
    if (!identical(file, "")) {
        sink(file)
    }
    
    # other.args <- list(...)
    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }
    enter <- getEnter(OS=OS)
    
    rs <- function(x) {
        paste(rep(" ", x*indent), collapse="")
    }
    
    varnames <- names(obj)
    cat("<?xml version='1.0' encoding='UTF-8'?>", enter, sep = "")
    cat("<codeBook xmlns=\"ddi:codebook:2_5\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"ddi:codebook:2_5 http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd\" version=\"2.5\">", enter, sep = "")
    cat(rs(1), "<dataDscr>", enter, sep = "")
    for (i in seq(length(obj))) {
        intrvl <- "" # if numeric, whether "discrete" or "contin"
        # if (any(grepl("type", names(obj[[i]])))) {
        #     if (grepl("num", obj[[i]]$type)) {
        
        #     }
        # }
        
        nature <- ""
        if (any(grepl("measurement", names(obj[[i]])))) {
            nature <- paste(" nature=\"", obj[[i]]$measurement, "\"", sep = "")
        }
        
        reptype <- ""
        if (any(grepl("type", names(obj[[i]])))) {
            if (identical("char", obj[[i]]$type))
            reptype <- " representationType=\"text\""
        }
        
        cat(rs(2), "<var name=\"", varnames[i], "\"", nature, intrvl, reptype, ">", enter, sep = "")
            
        if (any(grepl("type", names(obj[[i]])))) {
            cat(rs(3), "<varFormat type=\"", ifelse(grepl("char", obj[[i]]$type), "character", "numeric"), "\"/>", enter, sep = "")
        }
        
        cat(rs(3), "<labl>", obj[[i]]$label, "</labl>", enter, sep = "")
        
        if (any(grepl("values", names(obj[[i]])))) {
            miss <- NULL
            if (any(grepl("missing", names(obj[[i]])))) {
                miss <- obj[[i]]$missing
            }
            
            for (v in seq(length(obj[[i]]$values))) {
                lbls <- obj[[i]]$values
                cat(rs(3), "<catgry", ifelse(is.element(lbls[v], miss), " missing=\"Yes\"", ""), ">", enter, sep = "")
                
                cat(rs(4), "<catValu>",  lbls[v],  "</catValu>", enter, sep = "")
                
                cat(rs(4), "<labl>",  names(lbls)[v],  "</labl>", enter, sep = "")
                
                cat(rs(3), "</catgry>", enter, sep = "")
            }
        }
        
        if (any(grepl("txt", names(obj[[i]])))) {
            cat(rs(3), "<txt>", enter, sep = "")
            cat("<![CDATA[", obj[[i]]$txt, "]]>", enter, sep = "")
            cat(rs(3), "</txt>", enter, sep = "")
        }
        
        cat(rs(2), "</var>", enter, sep = "")
    }
    
    
    cat(rs(1), "</dataDscr>", enter, sep = "")
    cat("</codeBook>", enter, sep = "")
    
    if (!identical(file, "")) {
        sink()
    }
}
