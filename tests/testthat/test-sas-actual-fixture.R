test_that("actual SAS 9.4 output retains temporal meaning and native missing labels", {
    path <- test_path("fixtures", "readstat", "sas94-m9-reference.sas7bdat")
    catalog <- test_path("fixtures", "readstat", "sas94-m9-reference.sas7bcat")
    data <- read_sas(path, catalog_file = catalog)
    expected_labels <- c("Before SAS epoch" = -3654, "SAS epoch" = -3653,
        "R epoch" = 0, "System missing" = NA_real_,
        "Underscore missing" = sas_label_underscore(), "Missing A" = -91, "Missing Z" = -116)
    expected_tags <- c("", "", "", ".", "_", "A", "Z")

    expect_s3_class(data$day, "Date")
    expect_identical(as.numeric(data$day), c(0, rep(NA_real_, 4L)))
    expect_identical(attr(data$day, "labels"), expected_labels)
    expect_identical(writeBin(unname(attr(data$day, "labels")), raw()),
        writeBin(unname(expected_labels), raw()))
    expect_identical(attr(data$day, "na_index"), c("_" = 3L, "-91" = 4L, "-116" = 5L))
    expect_identical(attr(data$day, "sas_metadata")$informat, "DATE9")
    expect_identical(attr(data$day, "sas_metadata")$format, "REFDATE")
    expect_identical(attr(data$day, "sas_metadata")$label_tags, expected_tags)
    expect_identical(as.character(plain_values(data$text)), c("01", "", " A", "é", "漢字"))
    expect_identical(attr(data$text, "labels"),
        c(ZeroOne = "01", Leading = " A", Accent = "é", Unicode = "漢字", Blank = ""))

    for (limit in c(0L, 1L)) {
        window <- read_sas(path, catalog_file = catalog, n_max = limit)
        expect_identical(attr(window$day, "labels"), expected_labels)
        expect_identical(attr(window$day, "sas_metadata")$label_tags, expected_tags)
    }

    window <- read_sas(path, catalog_file = catalog, skip = 2L, n_max = 2L)
    expect_identical(attr(window$day, "na_index"), c("_" = 1L, "-91" = 2L))
    inspected <- inspectFile(path, catalog_file = catalog)
    expect_identical(inspected$row_count, 5)
    expect_identical(inspected$columns$metadata[[2L]]$labels, expected_labels)
    expect_identical(inspected$columns$metadata[[2L]]$sas_metadata$label_tags, expected_tags)
})
