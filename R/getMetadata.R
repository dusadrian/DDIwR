`getMetadata` <- 
function(xmlpath, OS = "windows", saveFile = FALSE, ...) {
    
    # TODO: detect DDI version or ask the version through a dedicated argument
    
    possibleNumeric <- function(x) {
        # as.character converts everything (especially factors)
        return(!any(is.na(suppressWarnings(as.numeric(na.omit(as.character(x)))))) & !all(is.na(x)))
    }
    
    asNumeric <- function(x) {
        return(suppressWarnings(as.numeric(as.character(x))))
    }
    
    trimstr <- function(x, side = "both") {
        pattern <- switch(side,
            both = paste("^", "[[:space:]]", "+|", "[[:space:]]", "+$", sep = ""),
            left = paste("^", "[[:space:]]", "+", sep = ""),
            right = paste("[[:space:]]", "+$", sep = "")
        )
        x <- gsub(pattern, "", x)
        gsub("\\n", "", x)
    }

    other.args <- list(...)
    
    enter <- getEnter(OS)
    
    fromsetupfile <- FALSE
    if ("fromsetupfile" %in% names(other.args)) {
        fromsetupfile <- other.args$fromsetupfile
    }
    
    
    tp <- treatPath(xmlpath, type = "XML")
    
    singlefile <- length(tp$files) == 1
    
    if (!fromsetupfile & !singlefile) {
        cat("Processing:\n")
    }
    
    for (ff in seq(length(tp$files))) {
        if (!fromsetupfile & !singlefile) {
            cat(tp$files[ff], "\n")
        }
        
        xml <- read_xml(file.path(tp$completePath, tp$files[ff]))
        
        nms <- xml_name(xml_children(xml))
        
        dd <- xml_children(xml_children(xml)[[which(nms == "dataDscr")]])
        xmlVarNames <- xml_attr(dd, "name")
        
        metadata <- vector(mode = "list", length = length(dd))
        names(metadata) <- xmlVarNames
        
        for (i in seq(length(dd))) {
            nms <- xml_name(xml_children(dd[[i]]))
            varlab <- trimstr(xml_contents(xml_children(dd[[i]])[[which(nms == "labl")]]))
            varlab <- gsub("\"", "'", varlab)
            varlab <- gsub("\\\\", "/", varlab)
            varlab <- gsub("<\\!\\[CDATA\\[|\\]\\]>", "", varlab)
            metadata[[i]]$label <- varlab
            
            missng <- NULL
            
            if (any(nms == "invalrng")) {
                inval <- xml_children(xml_children(dd[[i]])[[which(nms == "invalrng")]])
                for (j in seq(length(inval))) {
                    missng <- c(missng, xml_attr(inval[[j]], "VALUE"))
                }
            }
            
            if (any(nms == "catgry")) {
                vallabs <- xml_children(dd[[i]])[which(nms == "catgry")]
                values <- unlist(lapply(vallabs, function(x) {
                    nms <- xml_name(xml_children(x))
                    trimstr(xml_contents(xml_children(x)[[which(nms == "catValu")]]))
                }))
                values <- gsub("\"", "'", values)
                values <- gsub("\\\\", "/", values)
                
                labl <- unlist(lapply(vallabs, function(x) {
                    nms <- xml_name(xml_children(x))
                    
                    if (any(nms == "labl")) {
                        trimstr(xml_contents(xml_children(x)[[which(nms == "labl")]]))
                    }
                    else {
                        return(NA)
                    }
                }))
                
                values <- values[!is.na(labl)]
                labl <- labl[!is.na(labl)]
                
                missng <- c(missng, values[unlist(lapply(vallabs, function(x) {
                    attrs <- xml_attrs(x)
                    if (any(names(attrs) == "missing")) {
                        return(grepl("Y", xml_attr(x, "missing")))
                    }
                    else {
                        return(FALSE)
                    }
                }))])
                
                if (possibleNumeric(values)) {
                    values <- asNumeric(values)
                }
                
                metadata[[i]]$values <- values
                names(metadata[[i]]$values) <- labl
            }
            
            if (length(missng) > 0) {
                if (possibleNumeric(missng)) {
                    missng <- asNumeric(missng)
                }
                metadata[[i]]$missing <- sort(unique(missng))
            }
        }
        
        if (saveFile) {
            currentdir <- getwd()
            setwd(tp$completePath)
            
            indent <- 4
            if ("indent" %in% names(other.args)) {
                indent <- other.args$indent
            }
            
            sink(paste(tp$filenames[ff], "R", sep = "."))
            cat("metadata <- list()", enter)
            writeRlist(metadata, OS = OS, indent = indent)
            sink()
            setwd(currentdir)
        }
    }
    
    
    if (singlefile) {
        return(invisible(metadata))
    }
}

