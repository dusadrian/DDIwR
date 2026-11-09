library(xml2)
cat("xml2:", as.character(packageVersion("xml2")), "libxml2:", as.character(xml2:::libxml2_version()), "\n")
local({
    path <- tempfile(fileext = ".xml")
    on.exit(unlink(path), add = TRUE)
    writeLines("<root><child></root>", path)

    for (iteration in seq_len(100L)) {
        result <- tryCatch(read_xml(path), error = identity)
        stopifnot(inherits(result, "error"))
    }
})
gc()
gc()
