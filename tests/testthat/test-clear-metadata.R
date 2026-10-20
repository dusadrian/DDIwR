clearing_metadata_vector <- function() {
    x <- declared::declared(c(1, 9, 2), labels = c(Yes = 1, Missing = 9),
        na_values = 9, label = "Answer")
    names(x) <- c("first", "missing", "last")
    attr(x, "format.spss") <- "F12.3"
    attr(x, "format.stata") <- "%12.3f"
    attr(x, "format.sas") <- "F12.3"
    attr(x, "width") <- 8L
    attr(x, "display_width") <- 30L
    attr(x, "spss_metadata") <- list(format = "F16.4", width = 8L,
        display_width = 40L, measure = "ordinal", alignment = "center", future = NULL)
    attr(x, "stata_metadata") <- list(format = "%16.4f", type = "double", future = NULL)
    attr(x, "sas_metadata") <- list(format = "F16.4", width = 6L,
        informat = "F20.5", future = list(value = "retain"))
    attr(x, "spss.attributes") <- list(format = "Custom format", width = "Custom width")
    attr(x, "stata.characteristics") <- list(format = "Custom format", width = "Custom width")
    attr(x, "custom") <- list(source = "Archive", empty = NULL)

    return(x)
}

test_that("format clearing retains widths and unrelated metadata", {
    x <- clearing_metadata_vector()
    before <- x
    result <- clearFormats(x)
    unchanged <- setdiff(names(attributes(x)), c("format.spss", "format.stata",
        "format.sas", "spss_metadata", "stata_metadata", "sas_metadata"))

    expect_identical(x, before)
    expect_identical(as.vector(result), as.vector(x))
    expect_identical(attributes(result)[unchanged], attributes(x)[unchanged])
    expect_null(attr(result, "format.spss", exact = TRUE))
    expect_null(attr(result, "format.stata", exact = TRUE))
    expect_null(attr(result, "format.sas", exact = TRUE))
    expect_identical(attr(result, "spss_metadata"), list(width = 8L,
        display_width = 40L, measure = "ordinal", alignment = "center", future = NULL))
    expect_identical(attr(result, "stata_metadata"), list(type = "double", future = NULL))
    expect_identical(attr(result, "sas_metadata"), list(width = 6L,
        informat = "F20.5", future = list(value = "retain")))
    expect_identical(clearFormats(result), result)
})

test_that("width clearing retains formats, informats and Stata storage types", {
    x <- clearing_metadata_vector()
    before <- x
    result <- clearWidths(x)
    unchanged <- setdiff(names(attributes(x)),
        c("width", "display_width", "spss_metadata", "sas_metadata"))

    expect_identical(x, before)
    expect_identical(as.vector(result), as.vector(x))
    expect_identical(attributes(result)[unchanged], attributes(x)[unchanged])
    expect_null(attr(result, "width", exact = TRUE))
    expect_null(attr(result, "display_width", exact = TRUE))
    expect_identical(attr(result, "spss_metadata"), list(format = "F16.4",
        measure = "ordinal", alignment = "center", future = NULL))
    expect_identical(attr(result, "sas_metadata"), list(format = "F16.4",
        informat = "F20.5", future = list(value = "retain")))
    expect_identical(clearWidths(result), result)
    expect_identical(clearFormats(result), clearWidths(clearFormats(x)))
})

test_that("clearing removes NULL fields and prevents legacy fallback", {
    for (value in list(NULL, "override")) {
        x <- structure(1:2, format.spss = "F12.3", format.stata = "%12.3f",
            format.sas = "F12.3", width = 24L, display_width = 30L)
        attr(x, "spss_metadata") <- list(format = value, width = value,
            display_width = value)
        attr(x, "stata_metadata") <- list(format = value)
        attr(x, "sas_metadata") <- list(format = value, width = value)
        result <- clearWidths(clearFormats(x))

        expect_identical(result, 1:2)
        for (target in c("sav", "dta", "sas7bdat", "xpt")) {
            prepared <- prepare_vendor_metadata(data.frame(number = result), target)$data
            expect_null(attr(prepared$number, "width", exact = TRUE))
            expect_null(attr(prepared$number, "display_width", exact = TRUE))
        }
    }

    legacy <- structure(1:2, format.spss = "F12.3", format.stata = "%12.3f",
        format.sas = "F12.3", width = 24L, display_width = 30L)
    expect_identical(clearWidths(clearFormats(legacy)), 1:2)

    for (x in list(NULL, numeric(), character(), 1:2, list(a = 1, b = NULL))) {
        expect_identical(clearFormats(x), x)
        expect_identical(clearWidths(x), x)
    }

    x <- structure(1:2, spss_metadata = list(), sas_metadata = list())
    expect_identical(clearFormats(x), x)
    expect_identical(clearWidths(x), x)

    x <- structure(1:2,
        spss_metadata = list(form = NULL, widths = 40L, display = 20L),
        sas_metadata = list(informat = "F12.3", widths = 6L),
        stata_metadata = list(type = "double", formatting = "%10.2f"))
    expect_identical(clearFormats(x), x)
    expect_identical(clearWidths(x), x)
})

test_that("data frame clearing preserves dataset attributes and column structure", {
    data <- data.frame(answer = clearing_metadata_vector(),
        day = as.Date("2000-01-01") + 0:2,
        moment = as.POSIXct("2000-01-01", tz = "UTC") + 0:2,
        elapsed = as.difftime(0:2, units = "hours"),
        group = factor(c("yes", "no", "yes")))
    data$items <- list(clearing_metadata_vector(), NULL, list(width = "content"))
    attr(data$items, "width") <- 40L
    attr(data$items, "sas_metadata") <- list(format = "$40", width = 40L)

    for (name in c("day", "moment", "elapsed", "group")) {
        attr(data[[name]], "format.spss") <- "F12.3"
        attr(data[[name]], "width") <- 8L
    }

    attr(data, "label") <- "Dataset"
    attr(data, "notes") <- c("Keep", "These")
    attr(data, "spss_metadata") <- list(notes = NULL, mr_sets = list())
    attr(data, "sas_metadata") <- list(table_name = "DATA", version = 8L)
    attr(data, "stata_metadata") <- list(version = 14L)
    class(data) <- c("study", class(data))
    before <- data
    result <- clearWidths(clearFormats(data))

    expect_identical(data, before)
    expect_identical(attributes(result), attributes(data))
    expect_identical(result$answer, clearWidths(clearFormats(data$answer)))
    expect_identical(result$items, list(data$items[[1]], NULL, data$items[[3]]))

    for (name in c("day", "moment", "elapsed", "group")) {
        expected <- data[[name]]
        attr(expected, "format.spss") <- NULL
        attr(expected, "width") <- NULL
        expect_identical(result[[name]], expected)
    }

    empty <- data[FALSE, , drop = FALSE]
    expect_identical(dim(clearFormats(empty)), dim(empty))
    expect_identical(dim(clearWidths(empty)), dim(empty))
    expect_identical(clearFormats(data.frame()), data.frame())
    expect_identical(clearWidths(data.frame()), data.frame())
})

test_that("malformed examined metadata fails without changing inputs", {
    malformed <- list("format", list("format"), list(format = 1, format = 2),
        setNames(list(1), ""), setNames(list(1), NA_character_),
        structure(list(format = 1), class = "custom_metadata"))

    for (metadata in malformed) {
        data <- data.frame(first = clearing_metadata_vector(), second = 1:3)
        attr(data$second, "spss_metadata") <- metadata
        before <- data
        expect_error(clearFormats(data), "Variable 'second'.*spss_metadata")
        expect_error(clearWidths(data), "Variable 'second'.*spss_metadata")
        expect_identical(data, before)
    }

    for (x in list(new.env(), identity)) {
        expect_error(clearFormats(x), "vector or a data frame")
        expect_error(clearWidths(x), "vector or a data frame")
    }
})

test_that("cleared formats export with defaults rather than legacy fallback", {
    path <- tempfile()
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(number = c(1.25, 2.5), day = as.Date("2000-01-01") + 0:1)
    writers <- list(sav = write_sav, dta = write_dta, sas7bdat = write_sas, xpt = write_xpt)
    readers <- list(sav = read_sav, dta = read_dta, sas7bdat = read_sas, xpt = read_xpt)
    attributes <- c(sav = "format.spss", dta = "format.stata",
        sas7bdat = "format.sas", xpt = "format.sas")

    for (target in names(writers)) {
        writers[[target]](data, path)
        expected <- readers[[target]](path)
        metadata <- switch(target, sav = "spss_metadata", dta = "stata_metadata",
            sas7bdat = "sas_metadata", xpt = "sas_metadata")

        for (field in list(list(), list(format = NULL), list(format = "invalid old format"))) {
            modified <- data
            for (name in names(modified)) {
                attr(modified[[name]], attributes[[target]]) <- "invalid legacy format"
                attr(modified[[name]], metadata) <- field
            }

            writers[[target]](clearFormats(modified), path)
            result <- readers[[target]](path)
            for (name in names(data)) {
                expect_identical(attr(result[[name]], metadata)$format,
                    attr(expected[[name]], metadata)$format)
                expect_identical(as.numeric(result[[name]]), as.numeric(data[[name]]))
                expect_identical(class(result[[name]]), class(expected[[name]]))
            }
        }
    }
})

test_that("cleared widths export with default storage and display widths", {
    path <- tempfile()
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(text = c("one", "two"), number = c(1 + 2^-13, 8193))
    attr(data$text, "width") <- 18L
    attr(data$text, "display_width") <- 19L
    attr(data$text, "spss_metadata") <- list(width = 24L, display_width = 27L)
    attr(data$text, "sas_metadata") <- list(width = 32L)
    attr(data$number, "width") <- 6L
    attr(data$number, "sas_metadata") <- list(width = 3L)
    attr(data$number, "stata_metadata") <- list(type = "double")

    writers <- list(sav = write_sav, dta = write_dta, sas7bdat = write_sas, xpt = write_xpt)
    readers <- list(sav = read_sav, dta = read_dta, sas7bdat = read_sas, xpt = read_xpt)
    attributes <- c(sav = "spss_metadata", dta = "stata_metadata",
        sas7bdat = "sas_metadata", xpt = "sas_metadata")
    expected_widths <- c(sav = 24L, dta = 18L, sas7bdat = 32L, xpt = 32L)
    default_widths <- c(sav = 3L, dta = 3L, sas7bdat = 3L, xpt = 3L)
    cleared <- clearWidths(data)

    for (target in names(writers)) {
        writers[[target]](data, path)
        expect_identical(attr(readers[[target]](path)$text, attributes[[target]])$width, expected_widths[[target]])

        writers[[target]](cleared, path)
        result <- readers[[target]](path)
        expect_identical(attr(result$text, attributes[[target]])$width, default_widths[[target]])
        expect_identical(as.character(result$text), as.vector(data$text))
        expect_identical(as.numeric(result$number), data$number |> as.vector())

        if (target == "sav") {
            expect_null(attr(result$text, "spss_metadata")$display_width)
        }
        else if (target == "dta") {
            expect_identical(attr(result$number, "stata_metadata")$type, "double")
        }
        else {
            expect_identical(attr(result$number, "sas_metadata")$width, 8L)
        }
    }
})
