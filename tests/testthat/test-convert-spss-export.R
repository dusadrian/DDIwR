test_that("public ZSAV export preserves data and SPSS metadata", {
    path <- tempfile(fileext = ".zsav")
    copy <- tempfile(fileext = ".ZSAV")
    on.exit(unlink(c(path, copy)), add = TRUE)
    data <- data.frame(
        answer = declared::declared(c(1, 9, 2), label = "Answer",
            labels = c(Yes = 1, Missing = 9, No = 2), na_values = 9),
        text = c("one", "été", "three"),
        day = as.Date("2001-01-01") + 0:2
    )
    attr(data, "label") <- "Survey"
    attr(data, "spss.attributes") <- list(Source = "Archive")
    attr(data$answer, "spss.attributes") <- list(Review = c("first", "second"))
    before <- data

    expect_no_error(convert(data, path, label = "Exported survey"))
    expect_identical(readChar(path, 4L, useBytes = TRUE), "$FL3")
    result <- convert(path)

    expect_identical(as.numeric(result$answer), as.numeric(data$answer))
    expect_identical(as.character(result$text), data$text)
    expect_equal(result$day, data$day, ignore_attr = TRUE)
    expect_identical(attr(result$answer, "label"), "Answer")
    expect_identical(attr(result$answer, "labels"), attr(data$answer, "labels"))
    expect_identical(attr(result$answer, "na_values"), 9)
    expect_identical(attr(result, "label"), "Exported survey")
    expect_identical(attr(result, "spss.attributes"), attr(data, "spss.attributes"))
    expect_identical(attr(result$answer, "spss.attributes"),
        attr(data$answer, "spss.attributes"))
    expect_identical(data, before)

    expect_no_error(convert(path, copy))
    expect_identical(readChar(copy, 4L, useBytes = TRUE), "$FL3")
    expect_identical(as.numeric(convert(copy)$answer), as.numeric(data$answer))

    expect_no_error(convert(data, copy, compress = "none"))
    expect_identical(readChar(copy, 4L, useBytes = TRUE), "$FL2")
})

test_that("public SPSS export honors explicit compression and keeps the SAV default", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(answer = c(1, 2, NA_real_), text = c("one", "two", "three"))
    modes <- list(NULL, "none", "byte", "zsav", FALSE, TRUE)
    expected_compression <- c(1L, 0L, 1L, 2L, 0L, 2L)

    for (i in seq_along(modes)) {
        arguments <- list(from = data, to = path)

        if (!is.null(modes[[i]])) {
            arguments$compress <- modes[[i]]
        }

        expect_no_error(do.call(convert, arguments))
        header <- readBin(path, "raw", n = 76L)

        # Compression is the 32-bit SAV header field at byte offset 72.
        compression <- readBin(header[73:76], "integer", n = 1L, size = 4L,
            endian = .Platform$endian)
        expect_identical(compression, expected_compression[i])
        result <- convert(path, recode = FALSE, declared = FALSE)
        expect_identical(as.numeric(result$answer), data$answer)
        expect_identical(as.character(result$text), data$text)
    }
})
