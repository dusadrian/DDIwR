datetime_file_io <- function(format) {
    if (startsWith(format, "spss-")) {
        compression <- sub("spss-", "", format, fixed = TRUE)

        return(list(
            vendor = "SPSS",
            format_attribute = "format.spss",
            write = function(data, path) {
                write_sav(data, path, compress = compression, adjust_tz = FALSE)
            },
            read = function(path) {
                read_sav(path, user_na = TRUE)
            }
        ))
    }

    if (identical(format, "sas7bdat")) {
        return(list(
            vendor = "SAS",
            format_attribute = "format.sas",
            write = write_sas,
            read = read_sas
        ))
    }

    version <- as.integer(sub("xpt-", "", format, fixed = TRUE))
    stopifnot(is.element(version, c(5L, 8L)))

    return(list(
        vendor = "SAS",
        format_attribute = "format.sas",
        write = function(data, path) {
            write_xpt(data, path, version = version, name = "DATES", adjust_tz = FALSE)
        },
        read = read_xpt
    ))
}

test_that("SPSS and SAS preserve date and timestamp epochs and fractions", {
    old_options <- options(DDIwR.readstat_threads = 1L)
    on.exit(options(old_options), add = TRUE)
    data <- data.frame(
        day = as.Date(c("2026-01-01", "1970-01-01", "1960-01-01", "1582-10-14", NA)),
        moment = structure(c(1767225600.125, 0, -315619200, -12219379200, NA_real_),
            class = c("POSIXct", "POSIXt"), tzone = "UTC"),
        emptyday = structure(rep(NA_real_, 5L), class = "Date"),
        emptydt = structure(rep(NA_real_, 5L), class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (format in c("spss-none", "spss-byte", "spss-zsav", "sas7bdat", "xpt-5", "xpt-8")) {
        io <- datetime_file_io(format)
        path <- tempfile()
        paths <- c(paths, path)
        io$write(data, path)
        imported <- io$read(path)

        for (name in names(data)) {
            expect_identical(as.numeric(imported[[name]]), as.numeric(data[[name]]), info = format)
        }

        expect_s3_class(imported$day, "Date")
        expect_s3_class(imported$moment, "POSIXct")

        # A numeric display format disables the reader's epoch conversion.
        raw_data <- data

        for (name in names(raw_data)) {
            attr(raw_data[[name]], io$format_attribute) <- "F20.3"
        }

        io$write(raw_data, path)
        encoded <- io$read(path)

        if (identical(io$vendor, "SPSS")) {
            expect_identical(as.numeric(encoded$day), (as.numeric(data$day) + 141428) * 86400)
            expect_identical(as.numeric(encoded$moment), as.numeric(data$moment) + 12219379200)
        }
        else {
            expect_identical(as.numeric(encoded$day), as.numeric(data$day) + 3653)
            expect_identical(as.numeric(encoded$moment), as.numeric(data$moment) + 315619200)
        }
    }
})

test_that("SPSS and SAS temporal storage crosses integer boundaries safely", {
    old_options <- options(DDIwR.readstat_threads = 1L)
    on.exit(options(old_options), add = TRUE)
    values <- c(-2147483648, -2147483647, -32768, -128, 0, 127, 32767,
        2147483646, 2147483647, 2147483648, NA_real_)
    data <- data.frame(
        day = structure(values, class = "Date"),
        moment = structure(values + 0.125, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
    integer_data <- data.frame(
        day = structure(c(-2147483647L, -3653L, 0L, 20454L, 2147483647L, NA_integer_), class = "Date"),
        moment = structure(c(-2147483647L, -315619200L, 0L, 1767225600L, 2147483647L, NA_integer_),
            class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (format in c("spss-none", "spss-byte", "spss-zsav", "sas7bdat", "xpt-5", "xpt-8")) {
        io <- datetime_file_io(format)
        path <- tempfile()
        paths <- c(paths, path)

        for (source in list(data, integer_data)) {
            io$write(source, path)
            imported <- io$read(path)

            expect_identical(as.numeric(imported$day), as.numeric(source$day), info = format)
            expect_identical(as.numeric(imported$moment), as.numeric(source$moment), info = format)
        }
    }
})

test_that("SPSS temporal columns retain discrete and ranged user missings", {
    old_options <- options(DDIwR.readstat_threads = 1L)
    on.exit(options(old_options), add = TRUE)
    data <- data.frame(
        day = structure(c(20454, NA_real_, NA_real_, NA_real_, NaN), class = "Date"),
        moment = structure(c(1767225600.125, NA_real_, NA_real_, NA_real_, NaN), class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
    attr(data$day, "na_values") <- c(-91, -92)
    attr(data$day, "na_index") <- structure(3:4, names = c("-91", "-92"))
    attr(data$day, "labels") <- c(DK = -91, Refusal = -92)
    attr(data$moment, "na_range") <- c(-99, -90)
    attr(data$moment, "na_index") <- structure(3:4, names = c("-95", "-94"))
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (format in c("spss-none", "spss-byte", "spss-zsav")) {
        io <- datetime_file_io(format)
        path <- tempfile()
        repeated_path <- tempfile()
        paths <- c(paths, path, repeated_path)
        io$write(data, path)
        imported <- io$read(path)

        for (name in names(data)) {
            expected <- as.numeric(data[[name]])
            expected[is.na(expected)] <- NA_real_
            expect_identical(as.numeric(imported[[name]]), expected)
            expect_identical(attr(imported[[name]], "na_index"), attr(data[[name]], "na_index"))
        }

        expect_identical(attr(imported$day, "na_values"), attr(data$day, "na_values"))
        expect_identical(attr(imported$day, "labels"), attr(data$day, "labels"))
        expect_identical(attr(imported$moment, "na_range"), attr(data$moment, "na_range"))

        io$write(imported, repeated_path)
        repeated <- io$read(repeated_path)
        expect_identical(repeated, imported)

        discarded <- read_sav(path, user_na = FALSE)
        expect_identical(as.numeric(discarded$day), as.numeric(imported$day))
        expect_identical(as.numeric(discarded$moment), as.numeric(imported$moment))
        expect_null(attr(discarded$day, "na_values"))
        expect_null(attr(discarded$moment, "na_range"))
    }
})

test_that("SAS temporal columns retain extended and system missings", {
    data <- data.frame(
        day = structure(c(20454, NA_real_, NA_real_, NA_real_, NaN), class = "Date"),
        moment = structure(c(1767225600.125, NA_real_, NA_real_, NA_real_, NaN), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
        emptyday = structure(rep(NA_real_, 5L), class = "Date"),
        emptydt = structure(rep(NA_real_, 5L), class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )

    for (name in names(data)) {
        attr(data[[name]], "na_index") <- structure(3:4, names = c("A", "Z"))
    }

    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (format in c("sas7bdat", "xpt-5", "xpt-8")) {
        io <- datetime_file_io(format)
        path <- tempfile()
        paths <- c(paths, path)
        io$write(data, path)
        imported <- io$read(path)

        for (name in names(data)) {
            expected <- as.numeric(data[[name]])
            expected[is.na(expected)] <- NA_real_
            expect_identical(as.numeric(imported[[name]]), expected)
            expect_identical(attr(imported[[name]], "na_index"), structure(3:4, names = c("-91", "-116")))
        }
    }
})

test_that("independent SPSS temporal missings agree in serial and parallel reads", {
    old_options <- options(DDIwR.readstat_threads = 1L)
    on.exit(options(old_options), add = TRUE)
    # Numeric source values and explicit formats bypass Date/POSIXct export.
    data <- data.frame(
        day = rep(c(13986604800, NA_real_, -91, -92), length.out = 20001L),
        moment = rep(c(13986604800.125, NA_real_, -95, -94), length.out = 20001L)
    )
    attr(data$day, "format.spss") <- "DATE11"
    attr(data$day, "na_values") <- c(-91, -92)
    attr(data$moment, "format.spss") <- "DATETIME20.3"
    attr(data$moment, "na_range") <- c(-99, -90)
    expected_day <- rep(c(20454, NA_real_, NA_real_, NA_real_), length.out = 20001L)
    expected_moment <- rep(c(1767225600.125, NA_real_, NA_real_, NA_real_), length.out = 20001L)
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (compression in c("none", "byte", "zsav")) {
        path <- tempfile(fileext = ".sav")
        paths <- c(paths, path)
        write_sav(data, path, compress = compression)
        serial <- read_sav(path, user_na = TRUE)

        expect_identical(as.numeric(serial$day), expected_day)
        expect_identical(as.numeric(serial$moment), expected_moment)
        expect_identical(unname(attr(serial$day, "na_index")), which(is.element(data$day, c(-91, -92))))

        discarded <- read_sav(path, user_na = FALSE)
        expect_identical(as.numeric(discarded$day), expected_day)
        expect_identical(as.numeric(discarded$moment), expected_moment)

        window <- read_sav(path, user_na = TRUE, skip = 1L, n_max = 3L)
        expect_identical(attr(window$day, "na_index"), structure(2:3, names = c("-91", "-92")))

        converted <- convert(path, user_na = TRUE)
        expect_identical(as.numeric(converted$day), expected_day)
        expect_identical(as.numeric(converted$moment), expected_moment)
        expect_identical(attr(converted$day, "na_index"), attr(serial$day, "na_index"))

        if (readstat_parallel_supported() && identical(compression, "byte")) {
            parallel <- read_sav_parallel_prototype(path, user_na = TRUE, num_threads = 2L)
            expect_identical(parallel, serial)
            parallel_discarded <- read_sav_parallel_prototype(path, user_na = FALSE, num_threads = 2L)
            expect_identical(parallel_discarded, discarded)
        }
    }
})
