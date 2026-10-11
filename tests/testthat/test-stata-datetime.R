# Inspect one numeric column independently of the ReadStat reader.
stata_numeric_storage <- function(path) {
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    endian <- "big"

    if (length(grepRaw(charToRaw("<byteorder>LSF</byteorder>"), bytes, fixed = TRUE)) == 1L) {
        endian <- "little"
    }

    type_start <- grepRaw(charToRaw("<variable_types>"), bytes, fixed = TRUE) + 16L
    type <- readBin(bytes[type_start + 0:1], "integer", n = 1L, size = 2L, signed = FALSE, endian = endian)
    sizes <- c(`65530` = 1L, `65529` = 2L, `65528` = 4L, `65526` = 8L)
    size <- unname(sizes[as.character(type)])
    stopifnot(length(size) == 1L, !is.na(size))

    data_start <- grepRaw(charToRaw("<data>"), bytes, fixed = TRUE) + 6L
    what <- "integer"

    if (size == 8L) {
        what <- "double"
    }

    value <- readBin(bytes[data_start + seq_len(size) - 1L], what, n = 1L, size = size, endian = endian)

    return(list(type = type, value = as.double(value)))
}

test_that("Stata date and timestamp epochs survive public conversion", {
    data <- data.frame(
        day = as.Date(c("2026-01-01", "1970-01-01", "1960-01-01", "1959-12-31", NA)),
        moment = as.POSIXct(c("2026-01-01 12:34:56", "1970-01-01 00:00:00", "1960-01-01 00:00:00", "1959-12-31 00:00:00", NA), tz = "UTC")
    )
    attr(data$day, "label") <- "Calendar date"
    attr(data$moment, "label") <- "Timestamp"
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (version in c(8L, 12L, 13L, 14L, 15L)) {
        path <- tempfile(fileext = ".dta")
        repeated_path <- tempfile(fileext = ".dta")
        paths <- c(paths, path, repeated_path)
        convert(data, to = path, version = version)
        imported <- convert(path, num_threads = 1L)

        expect_equal(as.numeric(imported$day), as.numeric(data$day))
        expect_equal(as.numeric(imported$moment), as.numeric(data$moment))

        native <- read_dta(path, num_threads = 1L)
        expect_s3_class(native$day, "Date")
        expect_s3_class(native$moment, "POSIXct")

        convert(imported, to = repeated_path, version = version)
        repeated <- convert(repeated_path, num_threads = 1L)
        expect_equal(as.numeric(repeated$day), as.numeric(data$day))
        expect_equal(as.numeric(repeated$moment), as.numeric(data$moment))
    }
})

test_that("Stata date storage uses converted values at integer boundaries", {
    # Stata reserves values above these positive bounds for missing codes.
    values <- c(-127, 100, -128, 101, -32767, 32740, -32768, 32741,
        -2147483647, 2147483620, -2147483648, 2147483621)
    types <- c(65530L, 65530L, 65529L, 65529L, 65529L, 65529L,
        65528L, 65528L, 65528L, 65528L, 65526L, 65526L)
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (version in c(8L, 12L, 13L, 14L, 15L)) {
        for (index in seq_along(values)) {
            day <- structure(c(values[index] - 3653, NA_real_), class = "Date")
            path <- tempfile(fileext = ".dta")
            paths <- c(paths, path)
            write_dta(data.frame(day = day), path, version = version)
            imported <- read_dta(path, num_threads = 1L)

            expect_equal(as.numeric(imported$day), as.numeric(day))

            if (version >= 13L) {
                storage <- stata_numeric_storage(path)
                expect_identical(storage$type, types[index])
                expect_identical(storage$value, values[index])
            }
        }
    }
})

test_that("Stata timestamp storage uses milliseconds and retains fractions", {
    values <- c(0, -1000, 1000, -32000, 32000, -33000, 33000,
        -2147483000, 2147483000, -2147484000, 2147484000,
        315619200125, 2082888000125, 315619200000.125)
    types <- c(65530L, rep(65529L, 4L), rep(65528L, 4L), rep(65526L, 5L))
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (version in c(8L, 12L, 13L, 14L, 15L)) {
        for (index in seq_along(values)) {
            moment <- structure(c(values[index] / 1000 - 315619200, NA_real_), class = c("POSIXct", "POSIXt"), tzone = "UTC")
            path <- tempfile(fileext = ".dta")
            paths <- c(paths, path)
            write_dta(data.frame(moment = moment), path, version = version, adjust_tz = FALSE)
            imported <- read_dta(path, num_threads = 1L)

            expect_equal(as.numeric(imported$moment) - as.numeric(moment), c(0, NA_real_), tolerance = 1e-7)

            if (version >= 13L) {
                storage <- stata_numeric_storage(path)
                expect_identical(storage$type, types[index])
                expect_identical(storage$value, values[index])
            }
        }
    }
})

test_that("Stata temporal columns retain system and extended missing values", {
    data <- data.frame(
        day = structure(c(-3653, NA_real_, NA_real_, NA_real_, NaN), class = "Date"),
        wide_day = structure(c(20454, NA_real_, NA_real_, NA_real_, NaN), class = "Date"),
        long_day = structure(c(40000, NA_real_, NA_real_, NA_real_, NaN), class = "Date"),
        moment = structure(c(1767225600.125, NA_real_, NA_real_, NA_real_, NaN), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
        empty_day = structure(rep(NA_real_, 5L), class = "Date"),
        empty_moment = structure(rep(NA_real_, 5L), class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
    missing_index <- structure(c(3L, 4L), names = c("a", "z"))
    # DDIwR represents imported .a and .z as its conventional numeric codes.
    imported_index <- structure(c(3L, 4L), names = c("-91", "-116"))

    for (name in names(data)) {
        attr(data[[name]], "na_index") <- missing_index
    }

    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (version in c(8L, 12L, 13L, 14L, 15L)) {
        path <- tempfile(fileext = ".dta")
        paths <- c(paths, path)
        write_dta(data, path, version = version, adjust_tz = FALSE)
        imported <- read_dta(path, num_threads = 1L)

        for (name in names(data)) {
            expected <- as.numeric(data[[name]])
            expected[is.na(expected)] <- NA_real_
            expect_equal(as.numeric(imported[[name]]), expected)
            expect_identical(attr(imported[[name]], "na_index"), imported_index)
        }
    }
})

test_that("Stata temporal values agree in serial and parallel imports", {
    skip_if_not(readstat_parallel_supported())
    data <- data.frame(
        day = structure(rep(c(20454, -3653, NA_real_), length.out = 20001L), class = "Date"),
        moment = structure(rep(c(1767225600.125, -315619200, NA_real_), length.out = 20001L), class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)
    write_dta(data, path, adjust_tz = FALSE)

    serial <- read_dta(path, num_threads = 1L)
    parallel <- read_dta(path, num_threads = 2L)

    expect_identical(serial, parallel)
    expect_identical(as.numeric(parallel$day), as.numeric(data$day))
    expect_identical(as.numeric(parallel$moment), as.numeric(data$moment))
})

test_that("integer-backed temporal columns widen storage after conversion", {
    day <- structure(c(2147480000L, 2147483647L, NA_integer_), class = "Date")
    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)
    write_dta(data.frame(day = day), path)
    imported <- read_dta(path, num_threads = 1L)

    expect_equal(as.numeric(imported$day), as.numeric(day))
    expect_identical(stata_numeric_storage(path)$type, 65526L)
    expect_equal(stata_numeric_storage(path)$value, 2147483653)

    moment <- structure(c(0L, 2147483647L, NA_integer_), class = c("POSIXct", "POSIXt"), tzone = "UTC")
    write_dta(data.frame(moment = moment), path, adjust_tz = FALSE)
    imported <- read_dta(path, num_threads = 1L)

    expect_identical(as.numeric(imported$moment), as.numeric(moment))
    expect_identical(stata_numeric_storage(path)$type, 65526L)
    expect_identical(stata_numeric_storage(path)$value, 315619200000)
})
