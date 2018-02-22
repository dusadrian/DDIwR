require(XML)
getMetadata <- function(xmlpath, OS = "windows", saveFile = FALSE, ...) {
    
    # TODO: detect DDI version or ask the version through a dedicated argument
    
    
    other.args <- list(...)
    
    enter <- getEnter(OS)
    
    fromsetupfile <- FALSE
    if ("fromsetupfile" %in% names(other.args)) {
        fromsetupfile <- other.args$fromsetupfile
    }
    
    indent <- 4
    if ("indent" %in% names(other.args)) {
        indent <- other.args$indent
    }
    
    tp <- treatPath(xmlpath, type = "XML")
    
    currentdir <- getwd()
    # if (saveFile) {
        setwd(tp$completePath)
    # }
    
    singlefile <- length(tp$files) == 1
    
    if (!fromsetupfile) {
        cat("Processing:\n")
    }
    
    for (ff in seq(length(tp$files))) {
        if (!fromsetupfile) {
            cat(tp$files[ff], "\n")
        }
        
        
        dd <- xmlTreeParse(tp$files[ff])$doc$children$codeBook
        
        
        #### !!! ####
        # NEVER use getNodeSet() it's toooooo slooooow!!!
        # use instead xmlElementsByTagName()
    
        dd <- xmlElementsByTagName(dd, "dataDscr")[[1]]
        dd <- xmlElementsByTagName(dd, "var")
              
        xmlVarNames <- as.vector(sapply(dd, xmlGetAttr, "name"))
        # return(drop(xmlVarNames))
        
        metadata <- vector(mode = "list", length = length(dd))
        names(metadata) <- xmlVarNames
        
        
        for (i in seq(length(dd))) {
            
            varlab <- xmlValue(xmlElementsByTagName(dd[[i]], "labl")[[1]])
            varlab <- gsub("\"", "'", varlab)
            varlab <- gsub("\\\\", "/", varlab)
            
            metadata[[xmlVarNames[i]]]$label <- varlab
            
            
            #vallabs <- unlist(lapply(getNodeSet(dd[[i]], "//labl[@level='category']"), xmlValue))
            vallabs <- xmlElementsByTagName(dd[[i]], "catgry")
            
            missng <- NULL
            
            if (length(vallabs) > 0) {
                
                # metadata$vallab[[xmlVarNames[i]]] <- unlist(lapply(getNodeSet(dd[[i]], "//catValu"), xmlValue))
                values <- unname(unlist(lapply(vallabs, function(x) {
                    xmlValue(xmlElementsByTagName(x, "catValu")[[1]][[1]])
                })))
                values <- gsub("\"", "'", values)
                values <- gsub("\\\\", "/", values)
                
                labl <- lapply(vallabs, function(x) {
                    xmlValue(xmlElementsByTagName(x, "labl")[[1]][[1]])
                })
                
                haslabel <- unlist(lapply(labl, length)) > 0
                
                values <- values[haslabel]
                labl <- unname(unlist(labl[haslabel]))
                
                missng <- unname(unlist(lapply(vallabs, function(x) {
                    missng <- xmlAttrs(x)
                    if (is.element("missing", names(missng))) {
                        if (toupper(missng["missing"]) == "YES") {
                            return(xmlValue(xmlElementsByTagName(x, "catValu")[[1]][[1]]))
                        }
                    }
                })))
                
                if (length(values) > 0) {
                    metadata[[xmlVarNames[i]]]$values <- values
                    names(metadata[[xmlVarNames[i]]]$values) <- labl
                }
            }
        }
        
        if (saveFile) {
            sink(paste(tp$filenames[ff], "R", sep = "."))
            cat("metadata <- list()", enter)
            writeRlist(metadata, OS = OS, indent = indent)
            sink()
        }
    }
    
    setwd(currentdir)
    if (singlefile) {
        return(invisible(metadata))
    }
}

