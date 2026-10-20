test_that("vendor accessors resolve legacy fallback and explicit NULL clearing", {
    x <- structure(c(1, 2), format.sas = "BEST8", width = 8L)
    expect_identical(vendorMetadata(x, "sas", "format"), "BEST8")
    vendorMetadata(x, "SAS", "format") <- "BEST12"
    vendorMetadata(x, "SAS", "informat") <- "BEST32"
    expect_identical(vendorMetadata(x, "SAS", "format"), "BEST12")
    vendorMetadata(x, "SAS", "format") <- NULL
    expect_null(vendorMetadata(x, "SAS", "format"))
    expect_true(is.element("format", names(vendorMetadata(x, "SAS"))))
    expect_identical(vendorMetadata(x, "SAS", "informat"), "BEST32")
    expect_identical(attr(x, "format.sas"), "BEST8")
    vendorMetadata(x, "SAS") <- NULL
    expect_identical(vendorMetadata(x, "SAS", "format"), "BEST8")
    expect_null(vendorMetadata(x, "SPSS", "format"))
    expect_identical(vendorMetadata(x, "Stata", "width"), 8L)
})

test_that("vendor metadata replacement preserves values and unrelated attributes", {
    x <- structure(as.Date(c("2020-01-01", "2020-01-02")), label = "Date",
        spss_metadata = list(format = "DATE11", custom = NULL))
    original <- x
    vendorMetadata(x, "SAS") <- list(format = "DATE9", informat = "DATE9")
    expect_identical(unclass(x)[1:2], unclass(original)[1:2])
    expect_identical(class(x), class(original))
    expect_identical(attr(x, "label"), "Date")
    expect_identical(attr(x, "spss_metadata"), attr(original, "spss_metadata"))
    data <- data.frame(x = x)
    vendorMetadata(data, "SPSS", "notes") <- c("one", "two")
    expect_identical(data$x, x)
    expect_identical(vendorMetadata(data, "SPSS", "notes"), c("one", "two"))
    expect_null(attr(original, "sas_metadata"))
})

test_that("vendor metadata accessors reject malformed arguments and lists", {
    expect_error(vendorMetadata(1, "other"), "vendor")
    expect_error(vendorMetadata(1, "SAS", ""), "field")
    expect_error(vendorMetadata(environment(), "SAS"), "vector")
    x <- 1
    expect_error(vendorMetadata(x, "SAS") <- list(1), "nonempty, unique")
    expect_error(vendorMetadata(x, "SAS") <- list(a = 1, a = 2), "unique")
    expect_null(attr(x, "sas_metadata"))
})

test_that("accessor edits reach writers without reactivating legacy values", {
    data <- data.frame(x = c(1, 2))
    attr(data$x, "format.spss") <- "F16.4"
    vendorMetadata(data$x, "SPSS", "format") <- "F9.2"
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    convert(data, path)
    imported <- convert(path, declared = FALSE)
    expect_identical(vendorMetadata(imported$x, "SPSS", "format"), "F9.2")
    vendorMetadata(data$x, "SPSS", "format") <- NULL
    convert(data, path)
    imported <- convert(path, declared = FALSE)
    expect_false(identical(vendorMetadata(imported$x, "SPSS", "format"), "F16.4"))
})
