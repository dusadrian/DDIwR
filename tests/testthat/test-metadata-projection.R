test_that("metadata projection returns ordered structured records", {
    data <- data.frame(
        number = c(1, 2, 2, NA_real_),
        category = factor(c("b", "a", "b", NA_character_), levels = c("a", "b")),
        text = c("x", "y", NA_character_, "x"),
        check.names = FALSE
    )
    names(data) <- c("same", "same", "text")
    attr(data[[1]], "label") <- "Number"
    attr(data[[1]], "ID") <- "number-id"
    attr(data[[1]], "labels") <- c(one = 1, two = 2)

    result <- metadataProjection(
        data,
        columns = c(2, 1, 2),
        revision = "revision-7"
    )

    expect_identical(result$abi, 1L)
    expect_identical(result$layout, "columnar")
    expect_identical(result$revision, "revision-7")
    expect_identical(result$columns, c(2L, 1L, 2L))
    expect_identical(result$projection$identity$name, c("same", "same", "same"))
    expect_identical(result$projection$identity$position, c(2L, 1L, 2L))
    expect_identical(result$projection$identity$ID[[2]], "number-id")
    expect_identical(result$projection$annotations$label[[2]], "Number")
    expect_identical(result$projection$classification[2], checkType(
        data[[1]],
        attr(data[[1]], "labels", exact = TRUE)
    ))
    expect_identical(result$projection$categories$counts, c(2L, 2L, 2L))
    expect_identical(result$projection$categories$values[1:2], c("1", "2"))
    expect_identical(result$projection$categories$labels[1:2], c("a", "b"))
    expect_identical(result$projection$categories$frequencies[1:2], c(1, 2))
    expect_identical(result$projection$classification[3],
        result$projection$classification[1])
})

test_that("metadata projection avoids value analysis for metadata-only fields", {
    data <- data.frame(x = 1:3)
    result <- metadataProjection(data, fields = c("identity", "annotations"))

    expect_identical(result$completed_fields, c("identity", "annotations"))
    expect_identical(names(result$projection), c("identity", "annotations"))
    expect_identical(names(result$projection$identity),
        c("position", "name", "ID", "storage", "classes", "length"))
    expect_identical(names(result$projection$annotations),
        c("label", "measurement", "xmlang"))
    expect_null(result$projection$classification)
    expect_null(result$projection$summaries)
})

test_that("metadata projection validates ordered requests", {
    data <- data.frame(x = 1:3)

    expect_error(metadataProjection(data, columns = 0), "integer positions")
    expect_error(metadataProjection(data, columns = 1.5), "integer positions")
    expect_error(metadataProjection(data, fields = "formats"), "supported projection")
    expect_error(metadataProjection(data, fields = c("identity", "identity")), "distinct")
})

test_that("metadata projection supports empty datasets and selections", {
    empty_data <- data.frame()
    empty_result <- metadataProjection(empty_data)
    empty_xml <- metadataProjection(
        empty_data,
        fields = "xml",
        xml_options = list(MetadataPublisher = TRUE)
    )
    selected_result <- metadataProjection(data.frame(x = 1:3), columns = integer())

    expect_identical(empty_result$columns, integer())
    expect_identical(empty_result$projection$identity$name, character())
    expect_identical(empty_result$projection$classification, character())
    expect_identical(empty_result$projection$categories$counts, integer())
    expect_identical(empty_xml$projection$xml, character())
    expect_identical(selected_result$columns, integer())
    expect_identical(selected_result$projection$identity$position, integer())
})

test_that("XML projection reuses the structured analysis", {
    data <- data.frame(
        number = c(1, 2, 2, NA_real_),
        text = c("a", "b", NA_character_, "a")
    )
    attr(data$number, "ID") <- "number-id"
    attr(data$text, "ID") <- "text-id"
    attr(data$number, "labels") <- c(one = 1, two = 2)

    expected <- makeXMLvars(data = data, MetadataPublisher = TRUE)$xml
    result <- metadataProjection(
        data,
        fields = "xml",
        xml_options = list(MetadataPublisher = TRUE)
    )

    expect_identical(result$projection$xml, expected)
    expect_identical(names(result$projection), c("xml", "xml_metadata"))
    expect_identical(result$projection$xml_metadata$id, c("number-id", "text-id"))
    expect_identical(result$projection$xml_metadata$display_type, c("Numeric", "String"))
    expect_error(metadataProjection(data, fields = "xml", xml_options = list(data = data)),
        "cannot replace")
    expect_error(metadataProjection(data, fields = "xml", xml_options = list(TRUE)),
        "named list")
})
