`getXML` <- function(path) {
    xml <- tryCatch(xml2::read_xml(path), error = identity, warning = identity)

    if (any(class(xml) == "error")) {
        # xml <- readLines(path)
        # nms <- grepl("xmlns", xml[which(grepl("codeBook", xml))[1]])

        admisc::stopError("Unable to read the XML file.")
    }

    return(xml)
}
