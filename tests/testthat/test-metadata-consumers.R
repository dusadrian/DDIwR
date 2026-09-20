test_that("progressive metadata is cheap and supports ordered named requests", {
    fixture <- dget(test_path("fixtures", "dialogr-metadata.R"))
    session <- metadataSweepCapture(fixture$data, "v1")
    on.exit(metadataSweepClose(session))
    expect_false(any(session$metadata_ready))

    first <- metadataSweepRead(session, "v1", columns = c("factor", "missing", "factor"))
    expect_identical(first$columns, c(3L, 10L, 3L))
    expect_identical(first$completed_fields,
        c("identity", "annotations", "category_metadata"))
    expect_false(any(session$values_ready))
    expect_identical(first$session$analysis_scans, 0L)
    expect_identical(first$session$normalized_columns, c(3L, 10L))
    expect_identical(first$projection$category_metadata[[2]]$categories,
        fixture$expected[[10]]$categories)
    expect_identical(first$projection$category_metadata[[2]]$missingRange,
        fixture$expected[[10]]$missingRange)
    expect_identical(first$projection$identity$ID[[1]],
        first$projection$identity$ID[[3]])
    expect_error(metadataSweepRead(session, "v1", "absent"), "positions or")
    expect_error(metadataSweepRead(session, "v1", profile = "unknown"), "profile")
    expect_error(metadataSweepRead(session, "v1", xml_options = list(.analysis = 1)),
        "replace")
})

test_that("DialogR interpretation matches frozen actual runtime output and is cached", {
    fixture <- dget(test_path("fixtures", "dialogr-metadata.R"))
    session <- metadataSweepCapture(fixture$data, "v1")
    on.exit(metadataSweepClose(session))

    result <- metadataSweepRead(session, "v1", profile = "dialogr")

    for (index in seq_along(fixture$expected)) {
        expected <- fixture$expected[[index]]
        expect_equal(result$projection$dialogr[[index]][names(expected)], expected)
    }

    expect_identical(result$session$analysis_scans, 0L)
    expect_identical(result$session$normalized_columns, integer())
    expect_identical(result$session$dialog_columns, seq_len(ncol(fixture$data)))
    expect_null(session$source)
    testthat::local_mocked_bindings(
        .metadataDialogVariable = function(...) stop("Repeated interpretation"),
        .package = "DDIwR"
    )
    repeated <- metadataSweepRead(session, "v1", c(12, 3, 12), profile = "dialogr")
    expect_identical(repeated$projection$dialogr, result$projection$dialogr[c(12, 3, 12)])
    expect_silent(metadataSweepRead(session, "v1", 1:2, fields = "summaries"))
})

test_that("progressive, dialog and XML consumers share stable IDs and analysis", {
    data <- data.frame(number = c(1, 2, 3, NA), text = c("a", "b", "a", NA))
    attr(data$number, "labels") <- c(one = 1, two = 2)
    session <- metadataSweepCapture(data, "v1")
    on.exit(metadataSweepClose(session))
    identity <- metadataSweepRead(session, "v1")$projection$identity

    metadataSweepRead(session, "v1", 1, fields = "summaries")
    metadataSweepRead(session, "v1", 1, profile = "dialogr")
    first_xml <- metadataSweepRead(session, "v1", 1, profile = "codebook")
    complete <- metadataSweepRead(session, "v1", profile = "codebook")
    repeated <- metadataSweepRead(session, "v1", c(2, 1, 2), profile = "codebook")

    expect_identical(first_xml$projection$xml, complete$projection$xml[1])
    expect_identical(repeated$projection$xml, complete$projection$xml[c(2, 1, 2)])
    expect_identical(complete$projection$xml_metadata$id, unname(unlist(identity$ID)))
    expect_identical(complete$session$analysis_scans, 2L)
    expect_identical(repeated$session$analysis_scans, 2L)
    expect_identical(repeated$session$analysed_columns, 2L)
})

test_that("complete Codebook export consumes cached sessions including embedded data", {
    fixture <- dget(test_path("fixtures", "dialogr-metadata.R"))
    data <- fixture$data[c("numeric", "text", "factor", "date", "missing")]
    attr(data, "label") <- "Original dataset"
    session <- metadataSweepCapture(data, "v1")
    on.exit(metadataSweepClose(session))
    metadataSweepRead(session, "v1", 1:2, profile = "codebook")
    metadataSweepRead(session, "v1", profile = "dialogr")
    ids <- metadataSweepRead(session, "v1")$projection$identity$ID

    for (index in seq_along(data)) {
        attr(data[[index]], "ID") <- ids[[index]]
    }

    book <- makeElement("codeBook", children = list(
        makeElement("stdyDscr", fill = TRUE),
        makeElement("fileDscr"),
        makeElement("dataDscr")
    ))
    target <- tempfile(fileext = ".xml")
    on.exit(unlink(target), add = TRUE)
    exportCodebook(book, target, session = session, revision = "v1", embed = TRUE)
    document <- xml2::read_xml(target)
    variables <- xml2::xml_find_all(document, "//*[local-name()='var']")

    expect_identical(xml2::xml_attr(variables, "ID"), unname(unlist(ids)))
    expect_length(xml2::xml_find_all(document, "//*[local-name()='dataDscr']"), 1L)
    expect_identical(session$analysed_columns, ncol(data))
    expect_identical(session$analysis_scans, 2L)
    expected <- collectMetadata(data)
    actual <- collectMetadata(session = session, revision = "v1")
    expect_equal(actual, expected)
    embedded <- extractData(document)
    expect_equal(embedded, data, ignore_attr = TRUE)
    expect_identical(lapply(embedded, attr, "ID", exact = TRUE), ids)
    expect_identical(attr(embedded, "label"), "Original dataset")
    expect_identical(attr(embedded, "hashes"),
        getMetadataHashes(.metadataSessionVariables(session, seq_along(data))))
    expect_error(exportCodebook(book, target, session = session, revision = "v1",
        data = data), "session only")

    testthat::local_mocked_bindings(
        collectDataDscrStatsC = function(...) stop("Repeated scan"),
        .package = "DDIwR"
    )
    expect_silent(exportCodebook(book, target, session = session, revision = "v1"))
    expect_error(exportCodebook(book, target, session = session, revision = "v2"),
        "revision does not match")
})

test_that("weighted session XML preserves the existing export path", {
    data <- data.frame(x = c(1, 2, 2, 3, NA), weight = c(1, 2, 3, 1, 1))
    attr(data$x, "labels") <- c(one = 1, two = 2)
    attr(data$x, "ID") <- "x-id"
    attr(data$weight, "ID") <- "weight-id"
    session <- metadataSweepCapture(data, 1L)
    on.exit(metadataSweepClose(session))

    expected <- makeXMLvars(data = data, wt = "weight")
    actual <- makeXMLvars(session = session, revision = 1L, wt = "weight")
    expect_equal(actual, expected)
    expect_identical(session$analysis_scans, 0L)
})

test_that("the session API is exported and survives ordinary source edits", {
    expect_true(all(is.element(c("metadataSweepCapture", "metadataSweepRead",
        "metadataSweepInvalidate", "metadataSweepClose"), getNamespaceExports("DDIwR"))))
    data <- data.frame(x = c(1, 2, 3))
    attr(data$x, "labels") <- c(one = 1, two = 2)
    session <- metadataSweepCapture(data, 1L)
    on.exit(metadataSweepClose(session))
    data$x[1] <- 99
    attr(data$x, "labels") <- c(other = 99)

    metadata <- metadataSweepRead(session, 1L)
    expect_identical(metadata$projection$category_metadata[[1]]$labels,
        c(one = 1, two = 2))
    result <- metadataSweepRead(session, 1L, fields = "summaries")
    expect_equal(result$projection$summaries$value_maximum, 3)
    expect_error(collectMetadata(data, session = session, revision = 1L),
        "either 'from'")
    metadataSweepInvalidate(session, 2L)
    expect_error(metadataSweepRead(session, 1L), "stale")
})

test_that("session profiles cover empty and duplicate-name selections", {
    session <- metadataSweepCapture(data.frame(), 1L)
    on.exit(metadataSweepClose(session))

    for (profile in c("progressive", "dialogr", "codebook")) {
        expect_identical(metadataSweepRead(session, 1L, profile = profile)$columns,
            integer())
    }

    target <- tempfile(fileext = ".xml")
    on.exit(unlink(target), add = TRUE)
    exportCodebook(makeElement("codeBook"), target, session = session,
        revision = 1L, embed = TRUE)
    expect_equal(ncol(extractData(xml2::read_xml(target))), 0L)
    expect_error(metadataSweepCapture(data.frame(), new.env()), "Revision")

    data <- data.frame(x = 1:3, y = 4:6)
    names(data) <- c("same", "same")
    duplicate <- metadataSweepCapture(data, 1L)
    on.exit(metadataSweepClose(duplicate), add = TRUE)
    expect_error(metadataSweepRead(duplicate, 1L, "same"), "duplicate")
    expect_length(metadataSweepRead(duplicate, 1L, c(2, 1, 2),
        profile = "codebook")$projection$xml, 3L)
    expect_length(metadataSweepRead(duplicate, 1L, integer(),
        profile = "dialogr")$projection$dialogr, 0L)
})
