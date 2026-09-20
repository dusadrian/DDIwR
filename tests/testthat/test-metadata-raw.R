test_that("raw capture preserves stored metadata without inference", {
    data <- data.frame(x = ordered(c("low", "high"), levels = c("low", "high")),
        y = c(1, 2))
    attr(data$y, "label") <- "  Raw label  "
    attr(data$y, "measurement") <- "Unspecified, Ratio"
    attr(data$y, "nature") <- "ordinal"
    attr(data$y, "na_values") <- numeric()
    snapshot <- metadataSnapshotRaw(data)
    on.exit(metadataSnapshotClose(snapshot))
    records <- metadataSnapshotRead(snapshot)

    expect_identical(records$x$classes, c("ordered", "factor"))
    expect_identical(records$x$levels, c("low", "high"))
    expect_null(records$x$labels)
    expect_identical(records$y$label, "  Raw label  ")
    expect_identical(records$y$measurement, "Unspecified, Ratio")
    expect_identical(records$y$nature, "ordinal")
    expect_identical(records$y$na_values, numeric())
    expect_null(records$x$na_values)
    expect_identical(records$y$storage, "double")
    expect_identical(records$y$length, 2)
    expect_identical(records$y$position, 2)
    attr(data$y, "label") <- "changed"
    expect_identical(metadataSnapshotRead(snapshot), records)
})

test_that("selective capture skips unrequested unsupported attributes", {
    data <- data.frame(x = 1:3)
    attr(data$x, "labels") <- environment()
    snapshot <- metadataSnapshotRaw(data, c("storage", "label"))
    on.exit(metadataSnapshotClose(snapshot))
    expect_identical(metadataSnapshotRead(snapshot),
        list(x = list(storage = "integer", label = NULL)))
    expect_error(metadataSnapshotRead(snapshot, fields = "labels"), "supported")
    expect_error(metadataSnapshotRaw(data, "labels"))
    expect_error(metadataSnapshotRaw(data, "bogus"), "supported")
    expect_error(metadataSnapshotRaw(data, c("label", "label")), "distinct")
})

test_that("raw capture preserves dates and language attributes without scanning values", {
    data <- data.frame(x = as.POSIXct(c("2020-01-01", "2020-01-02"), tz = "UTC"))
    attr(data$x, "xmlang") <- "ro"
    attr(data$x, "format.spss") <- "DATETIME20"
    snapshot <- metadataSnapshotRaw(data, c("classes", "tzone", "xmlang", "format.spss"))
    on.exit(metadataSnapshotClose(snapshot))
    expect_identical(metadataSnapshotRead(snapshot), list(x = list(
        classes = c("POSIXct", "POSIXt"), tzone = "UTC", xmlang = "ro",
        format.spss = "DATETIME20")))
})
