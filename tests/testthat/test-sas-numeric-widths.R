sas_truncated_numeric <- function(values, width) {
    positions <- which(is.finite(values) & values != 0)
    magnitude <- abs(values[positions])
    exponent <- pmax(floor(log2(magnitude)), -1022)
    step <- 2^(exponent - (8 * width - 12))
    values[positions] <- sign(values[positions]) * floor(magnitude / step) * step

    return(values)
}

test_that("native SAS numeric widths pack mixed columns across data pages", {
    path <- tempfile(fileext = ".sas7bdat")
    repeated <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(c(path, repeated)), add = TRUE)
    values <- rep(c(-8192, -31, 0, 1.25, 8192, NA_real_), 400L)
    data <- data.frame(
        first = rep(c("before", "start"), 1200L),
        w3 = as.integer(values), w4 = values,
        middle = rep(c("middle", "text"), 1200L),
        w5 = rep(c(TRUE, FALSE, NA), 800L),
        w6 = values, w7 = values, w8 = values,
        last = rep(c("after", "end"), 1200L)
    )
    attr(data, "label") <- "Reduced storage"
    attr(data, "sas_metadata") <- list(table_name = "WIDTHS")

    for (width in 3:8) {
        name <- paste0("w", width)
        attr(data[[name]], "sas_metadata") <- list(width = width, informat = "F12.3")
        attr(data[[name]], "label") <- paste("Width", width)
        attr(data[[name]], "format.sas") <- "F10.2"
    }

    before <- data
    write_sas(data, path)
    imported <- read_sas(path)
    window <- read_sas(path, skip = 399L, n_max = 7L)
    info <- inspectFile(path)
    expect_identical(data, before)
    expect_identical(names(imported), names(data))
    expect_identical(info$row_count, 2400)
    expect_identical(attr(imported, "label"), "Reduced storage")
    expect_identical(attr(imported, "sas_metadata")$table_name, "WIDTHS")

    for (name in names(data)) {
        if (is.character(data[[name]])) {
            expect_identical(as.character(imported[[name]]), data[[name]])
            expect_identical(as.character(window[[name]]), data[[name]][400:406])
        }
        else {
            width <- attr(data[[name]], "sas_metadata")$width
            expect_identical(as.numeric(imported[[name]]), as.numeric(data[[name]]))
            expect_identical(as.numeric(window[[name]]), as.numeric(data[[name]][400:406]))
            expect_identical(attr(imported[[name]], "sas_metadata"),
                list(format = "F10.2", informat = "F12.3", width = width))
            expect_identical(info$columns$width[match(name, info$columns$name)], as.numeric(width))
            expect_identical(attr(imported[[name]], "sas_metadata")$format, "F10.2")
            expect_identical(attr(imported[[name]], "label"), paste("Width", width))
        }
    }

    write_sas(imported, repeated)
    reimported <- read_sas(repeated)
    for (name in names(data)) {
        expect_identical(reimported[[name]], imported[[name]])
    }

    full <- data
    for (width in 3:8) {
        attr(full[[paste0("w", width)]], "sas_metadata")$width <- 8L
    }

    write_sas(full, repeated)
    expect_lt(file.info(path)$size, file.info(repeated)$size)
})

test_that("native SAS truncates mantissa bytes rather than rounding or using float32", {
    path <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(path), add = TRUE)
    values <- c(8193, -8195, 1 + sum(2^(-c(13, 21, 29, 37, 45, 52))),
        -pi, 0, -0, 1.25, 2^50 + 1, 2^100, 2^-100,
        .Machine$double.xmin, 2^-1074, NA_real_)

    for (width in 3:8) {
        data <- data.frame(number = values)
        attr(data$number, "sas_metadata") <- list(width = width)
        write_sas(data, path)
        imported <- as.numeric(read_sas(path)$number)
        expected <- sas_truncated_numeric(values, width)

        expect_identical(imported, expected, info = paste("width", width))
        expect_identical(1 / imported[5:6], c(Inf, -Inf))
    }
})

test_that("reduced SAS widths retain system missing and every SAS missing tag", {
    path <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(path), add = TRUE)
    values <- c(0L, NA_integer_, NA_integer_, rep(NA_integer_, 27L))
    expected_index <- structure(4:30, names = c("_", as.character(-91:-116)))
    data <- data.frame(
        number = as.numeric(values), integer = values,
        day = structure(values - 3653L, class = "Date"),
        moment = structure(values - 315619200L,
            class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )

    for (name in names(data)) {
        attr(data[[name]], "na_index") <- expected_index
        attr(data[[name]], "na_values") <- as.numeric(-91:-116)
    }

    for (width in 3:8) {
        for (name in names(data)) {
            attr(data[[name]], "sas_metadata") <- list(width = width)
        }

        write_sas(data, path)
        imported <- read_sas(path)

        for (name in names(data)) {
            expect_identical(as.numeric(imported[[name]]), as.numeric(data[[name]]))
            expect_identical(attr(imported[[name]], "na_index"), expected_index)
            expect_identical(attr(imported[[name]], "na_values"), as.numeric(-91:-116))
            expect_identical(attr(imported[[name]], "sas_metadata")$width, width)
        }

        expect_s3_class(imported$day, "Date")
        expect_s3_class(imported$moment, "POSIXct")
    }
})

test_that("SAS temporal widths apply after conversion to SAS units", {
    path <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(
        day = structure(c(0, 20454, -3653, 0.125, NA_real_), class = "Date"),
        moment = structure(c(0, 1767225600.125, -315619200, 0.125, NA_real_),
            class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )

    for (width in 3:8) {
        attr(data$day, "sas_metadata") <- list(width = width)
        attr(data$moment, "sas_metadata") <- list(width = width)
        write_sas(data, path)
        imported <- read_sas(path)

        expect_identical(as.numeric(imported$day),
            sas_truncated_numeric(as.numeric(data$day) + 3653, width) - 3653)
        expect_identical(as.numeric(imported$moment),
            sas_truncated_numeric(as.numeric(data$moment) + 315619200, width) - 315619200)
        expect_s3_class(imported$day, "Date")
        expect_s3_class(imported$moment, "POSIXct")
    }
})

test_that("SAS numeric widths respect list precedence and legacy attributes", {
    path <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(number = c(1.25, 8192))
    scenarios <- list(
        list(legacy = NULL, metadata = NULL, width = 8L),
        list(legacy = 3L, metadata = NULL, width = 3L),
        list(legacy = 5L, metadata = list(informat = "F10.2"), width = 5L),
        list(legacy = 2L, metadata = list(width = 4L), width = 4L),
        list(legacy = 2L, metadata = list(width = NULL), width = 8L),
        list(legacy = 3L, metadata = list(width = 0L), width = 8L),
        list(legacy = 0L, metadata = NULL, width = 8L)
    )

    for (scenario in scenarios) {
        attr(data$number, "width") <- scenario$legacy
        attr(data$number, "sas_metadata") <- scenario$metadata
        before <- data
        write_sas(data, path)
        expect_identical(data, before)
        expect_identical(as.numeric(read_sas(path)$number), c(1.25, 8192))
        expect_identical(inspectFile(path)$columns$width, as.numeric(scenario$width))

        empty <- data.frame(number = numeric())
        attributes(empty$number) <- attributes(data$number)
        write_sas(empty, path)
        expect_identical(inspectFile(path)$columns$width, as.numeric(scenario$width))
        expect_identical(inspectFile(path)$row_count, 0)
        expect_length(read_sas(path)$number, 0L)
    }
})

test_that("invalid SAS numeric widths fail before opening the destination", {
    path <- tempfile(fileext = ".sas7bdat")
    absent <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(c(path, absent)), add = TRUE)
    expected <- charToRaw("Preserve the existing destination")
    writeBin(expected, path)
    invalid <- list(-1L, 1L, 2L, 9L, 3.5, NA_integer_, NaN, Inf,
        .Machine$integer.max + 1, numeric(), c(3L, 4L), "4", TRUE,
        structure(4, class = "Date"))

    for (width in invalid) {
        for (attribute in c("sas_metadata", "width")) {
            data <- data.frame(number = c(1, 2))

            if (attribute == "sas_metadata") {
                attr(data$number, attribute) <- list(width = width)
            }
            else {
                attr(data$number, attribute) <- width
            }

            expect_error(write_sas(data, path), "width")
            expect_identical(readBin(path, "raw", n = length(expected)), expected)
            expect_identical(file.info(path)$size, as.numeric(length(expected)))
            expect_error(write_sas(data, absent), "width")
            expect_false(file.exists(absent))
        }
    }

    # Native validation also guards direct callers that bypass R normalization.
    for (width in c(2L, 9L)) {
        data <- data.frame(number = c(1, 2))
        attr(data$number, "width") <- width
        expect_error(.Call("declared_write_sas_", data, path, NULL, PACKAGE = "DDIwR"),
            "Failed to create numeric column")
        expect_identical(readBin(path, "raw", n = length(expected)), expected)
    }
})
