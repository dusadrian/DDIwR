.validateCodebookXML <- function(output, schema_path = system.file(
    "schemas", "ddi-codebook-2.6", "codebook.xsd", package = "DDIwR"
)) {
    if (!nzchar(schema_path) || !file.exists(schema_path)) {
        stop("Cannot validate DDI Codebook XML: the bundled DDI 2.6 schema is unavailable.",
            call. = FALSE)
    }

    result <- tryCatch({
        schema <- xml2::read_xml(schema_path, options = "NONET")
        document <- xml2::read_xml(charToRaw(enc2utf8(output)),
            options = c("NONET", "HUGE"))
        valid <- xml2::xml_validate(document, schema)
        diagnostics <- attr(valid, "errors")

        if (!isTRUE(valid) || length(diagnostics) > 0L) {
            if (length(diagnostics) == 0L) {
                diagnostics <- "The document does not conform to the schema."
            }

            stop(paste(diagnostics, collapse = "\n"), call. = FALSE)
        }

        TRUE
    }, error = function(error) {
        conditionMessage(error)
    })

    if (!isTRUE(result)) {
        stop(paste0("DDI Codebook 2.6 XML validation failed:\n", result),
            call. = FALSE)
    }

    return(invisible(TRUE))
}
