`getXML` <- function(path) {
    xml <- tryCatch(xml2::read_xml(path), error = identity, warning = identity)
            
    if (any(class(xml) == "error")) {
        xml <- readLines(path)
        
        nms <- grepl("xmlns", xml[which(grepl("codeBook", xml))[1]])
        error <- "Unknown error reading the XML file"
        
        if (!nms & any(grepl("&", xml))) {
            error <- "Invalid XML file: \"&\" character(s) present without a namespace"
        }
        
        cat("\n")
        stop(error, call. = FALSE)
    }

    return(xml)
}
