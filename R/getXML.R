#' @description Read the DDI XML file
#' @return An XML document
#' @noRd
`getXML` <- function(path) {
    tc <- admisc::tryCatchWEM(xml <- xml2::read_xml(path))

    if (is.null(tc$error)) {
        return(xml)
    }
    else {
        # xml <- readLines(path)
        # nms <- grepl("xmlns", xml[which(grepl("codeBook", xml))[1]])

        admisc::stopError("Unable to read the XML file.")
    }
}
