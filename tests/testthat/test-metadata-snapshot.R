test_that("owned snapshot preserves metadata and ordered batch projections", {
    data <- data.frame(number = c(1, NA_real_, NaN),
        category = factor(c("a", "b", NA), levels = c("b", "a")),
        text = c("é", NA, ""), date = as.Date(c("2020-01-01", NA, "2020-02-01")))
    attr(data$number, "labels") <- c(yes = 1, missing = NA_real_, nan = NaN)
    attr(data$number, "na_values") <- c(99, 98)
    attr(data$number, "na_range") <- c(90, 100)
    attr(data$number, "measurement") <- "ratio"
    attr(data$text, "label") <- "Étiquette"
    attr(data$text, "xmlang") <- "fr"
    attr(data$text, "ID") <- "v3"
    names(data) <- c("same", "same", "text", "date")
    expected <- collectRMetadata(data, infer_type = FALSE, include_formats = FALSE)
    snapshot <- metadataSnapshot(data)
    on.exit(metadataSnapshotClose(snapshot))

    expect_identical(metadataSnapshotRead(snapshot), expected)
    expect_identical(metadataSnapshotRead(snapshot, c(3, 1, 3)), expected[c(3, 1, 3)])
    expect_identical(metadataSnapshotRead(snapshot, integer()), expected[integer()])
    expect_identical(metadataSnapshotRead(snapshot, 3, c("ID", "label")),
        lapply(expected[3], function(record) record[c("ID", "label")]))

    attr(data[[3]], "label") <- "changed"
    rm(data)
    gc()
    expect_identical(metadataSnapshotRead(snapshot), expected)
    copy <- metadataSnapshotRead(snapshot)
    copy[[3]]$label <- "changed again"
    expect_identical(metadataSnapshotRead(snapshot), expected)
})

test_that("snapshot validates requests and has idempotent close", {
    snapshot <- metadataSnapshot(data.frame(x = 1:3))
    expect_error(metadataSnapshotRead(snapshot, NA_integer_), "integer positions")
    expect_error(metadataSnapshotRead(snapshot, 1.5), "integer positions")
    expect_error(metadataSnapshotRead(snapshot, 2), "integer positions")
    expect_error(metadataSnapshotRead(snapshot, fields = "type"), "supported")
    expect_error(metadataSnapshotRead(snapshot, fields = c("ID", "ID")), "distinct")
    metadataSnapshotClose(snapshot)
    expect_silent(metadataSnapshotClose(snapshot))
    expect_error(metadataSnapshotRead(snapshot), "closed or invalid")
})

test_that("C capture preserves string encodings and fails explicitly on unsupported data", {
    text <- iconv("café", to = "latin1")
    Encoding(text) <- "latin1"
    records <- list(x = list(label = c(text, NA_character_, ""),
        labels = c(a = NA_real_, b = NaN)))
    pointer <- .Call("metadata_snapshot_create", records, PACKAGE = "DDIwR")
    on.exit(.Call("metadata_snapshot_close", pointer, PACKAGE = "DDIwR"))
    expect_identical(.Call("metadata_snapshot_read", pointer, 1L, NULL, PACKAGE = "DDIwR"), records)
    expect_error(.Call("metadata_snapshot_create", list(x = list(label = environment())),
        PACKAGE = "DDIwR"))
    expect_error(.Call("metadata_snapshot_create", list(x = structure(1, class = "custom")),
        PACKAGE = "DDIwR"), "Unsupported metadata attribute")
})

test_that("empty snapshots remain readable", {
    snapshot <- metadataSnapshot(data.frame())
    on.exit(metadataSnapshotClose(snapshot))
    expect_length(metadataSnapshotRead(snapshot), 0)
})
