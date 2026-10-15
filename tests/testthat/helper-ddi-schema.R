expect_ddi_26_schema <- function(output, info = NULL) {
    schema <- xml2::read_xml(system.file(
        "schemas", "ddi-codebook-2.6", "codebook.xsd",
        package = "DDIwR", mustWork = TRUE
    ), options = "NONET")
    document <- xml2::read_xml(output, options = "NONET")
    valid <- xml2::xml_validate(document, schema)
    diagnostics <- attr(valid, "errors")

    testthat::expect_true(isTRUE(valid),
        info = paste(c(info, diagnostics), collapse = "\n"))
    testthat::expect_length(diagnostics, 0L)

    return(invisible(document))
}
