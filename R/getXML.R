`getXML` <- function(path) {
    xml <- tryCatch(xml2::read_xml(path), error = identity, warning = identity)
            
    if (any(class(xml) == "error")) {
        xml <- readLines(path)
        
        nms <- grepl("xmlns", xml[which(grepl("codeBook", xml))[1]])
        error <- "Unable to read the XML file"
        
        cat("\n")
        stop(error, call. = FALSE)
    }

    return(xml)
}
