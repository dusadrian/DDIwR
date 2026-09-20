test_that("metadata session delays and caches value analysis", {
    data <- data.frame(
        number = c(1, 2, 2, NA_real_),
        category = factor(
            c("b", "a", "b", NA_character_),
            levels = c("a", "b")
        ),
        text = c("x", "y", NA_character_, "x"),
        check.names = FALSE
    )
    names(data) <- c("same", "same", "text")
    attr(data[[1]], "label") <- "Number"
    attr(data[[1]], "ID") <- "number-id"
    attr(data[[1]], "labels") <- c(one = 1, two = 2)

    session <- metadataSweepCapture(data, revision = "revision-1")
    on.exit(metadataSweepClose(session))

    metadata <- metadataSweepRead(
        session,
        revision = "revision-1",
        columns = c(2, 1, 2),
        fields = c("identity", "annotations")
    )

    expect_identical(metadata$projection$identity$position, c(2L, 1L, 2L))
    expect_identical(metadata$projection$identity$name, c("same", "same", "same"))
    expect_identical(metadata$projection$identity$storage,
        c("integer", "double", "integer"))
    expect_identical(metadata$projection$identity$length, c(4L, 4L, 4L))
    expect_identical(metadata$session$analysis_scans, 0L)
    expect_identical(metadata$session$cached_columns, integer())

    first <- metadataSweepRead(
        session,
        revision = "revision-1",
        columns = c(2, 1, 2),
        fields = c("classification", "summaries", "categories")
    )
    expected <- metadataProjection(
        data,
        columns = c(2, 1, 2),
        fields = c("classification", "summaries", "categories"),
        revision = "revision-1"
    )

    expect_identical(first$session$analysis_scans, 1L)
    expect_identical(first$session$analysed_columns, 2L)
    expect_identical(first$session$cached_columns, c(1L, 2L))
    expect_identical(first$projection$classification[[1]],
        first$projection$classification[[3]])
    expect_identical(first$projection$categories$counts, c(2L, 2L, 2L))
    expect_identical(first$projection, expected$projection)

    repeated <- metadataSweepRead(
        session,
        revision = "revision-1",
        columns = c(1, 2),
        fields = c("classification", "weight_signal", "summaries")
    )

    expect_identical(repeated$session$analysis_scans, 1L)
    expect_identical(repeated$session$analysed_columns, 2L)

    remaining <- metadataSweepRead(
        session,
        revision = "revision-1",
        columns = 3,
        fields = "classification"
    )

    expect_identical(remaining$session$analysis_scans, 2L)
    expect_identical(remaining$session$analysed_columns, 3L)
    expect_null(session$source)
})

test_that("metadata session rejects stale revisions and closes safely", {
    session <- metadataSweepCapture(
        data.frame(x = 1:3),
        revision = 1L
    )

    expect_error(
        metadataSweepRead(session, revision = 2L),
        "does not match"
    )
    expect_error(
        metadataSweepRead(session, revision = 1L),
        "stale"
    )

    metadataSweepClose(session)
    expect_silent(metadataSweepClose(session))
    expect_error(
        metadataSweepRead(session, revision = 1L),
        "closed"
    )
})

test_that("metadata session supports explicit invalidation", {
    session <- metadataSweepCapture(
        data.frame(x = 1:3),
        revision = "before-edit"
    )
    on.exit(metadataSweepClose(session))

    metadataSweepInvalidate(session, revision = "after-edit")

    expect_error(
        metadataSweepRead(session, revision = "before-edit"),
        "stale"
    )
})

test_that("metadata session XML reuses cached analysis", {
    data <- data.frame(
        number = c(1, 2, 2, NA_real_),
        text = c("a", "b", NA_character_, "a"),
        date = as.Date(c("2026-01-01", "2026-01-02", NA, "2026-01-04"))
    )
    attr(data$number, "ID") <- "number-id"
    attr(data$text, "ID") <- "text-id"
    attr(data$date, "ID") <- "date-id"
    attr(data$number, "labels") <- c(one = 1, two = 2)

    expected <- makeXMLvars(data = data, MetadataPublisher = TRUE)$xml
    session <- metadataSweepCapture(data, revision = "xml-1")
    on.exit(metadataSweepClose(session))

    metadataSweepRead(
        session,
        revision = "xml-1",
        fields = c("classification", "summaries", "categories")
    )
    result <- metadataSweepRead(
        session,
        revision = "xml-1",
        fields = "xml",
        xml_options = list(MetadataPublisher = TRUE)
    )

    expect_identical(result$projection$xml, expected)
    expect_identical(result$session$analysis_scans, 1L)
    expect_identical(result$session$analysed_columns, 3L)
})

test_that("metadata session validates revisions and unsupported values", {
    expect_error(
        metadataSweepCapture(data.frame(x = 1:3), revision = NULL),
        "Revision"
    )
    expect_error(
        metadataSweepCapture(data.frame(x = 1:3), revision = NA_character_),
        "Revision"
    )

    data <- data.frame(x = I(list(1, 2, 3)))
    session <- metadataSweepCapture(data, revision = "list-column")
    on.exit(metadataSweepClose(session))

    expect_silent(metadataSweepRead(
        session,
        revision = "list-column",
        fields = c("identity", "annotations")
    ))
    expect_error(
        metadataSweepRead(
            session,
            revision = "list-column",
            fields = "classification"
        ),
        "Unsupported value storage"
    )
})
