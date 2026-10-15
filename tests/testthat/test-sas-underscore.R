sas_missing_io <- function(format) {
    if (identical(format, "sas7bdat")) {
        return(list(write = write_sas, read = read_sas))
    }

    version <- as.integer(sub("xpt-", "", format, fixed = TRUE))

    return(list(
        write = function(data, path, dictionary = NULL) {
            write_xpt(data, path, version = version, name = "MISSING",
                adjust_tz = FALSE, dictionary = dictionary)
        },
        read = read_xpt
    ))
}

sas_missing_bytes <- function(format, tag) {
    if (identical(format, "sas7bdat")) {
        bytes <- writeBin(NaN, raw(), size = 8L, endian = .Platform$endian)
        tag_byte <- if (.Platform$endian == "little") { 6L } else { 3L }
        bytes[tag_byte] <- as.raw(bitwXor(utf8ToInt(tag), 255L))

        return(bytes)
    }

    return(c(charToRaw(tag), raw(7L)))
}

test_that("SAS readers distinguish underscore tags from system missing values", {
    data <- data.frame(
        number = c(0, NA_real_, NA_real_, NaN),
        day = structure(c(0, NA_real_, NA_real_, NaN), class = "Date"),
        moment = structure(c(0, NA_real_, NA_real_, NaN),
            class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (format in c("sas7bdat", "xpt-5", "xpt-8")) {
        io <- sas_missing_io(format)
        path <- tempfile()
        paths <- c(paths, path)
        io$write(data, path)
        bytes <- readBin(path, "raw", n = file.info(path)$size)
        positions <- grepRaw(sas_missing_bytes(format, "."), bytes, fixed = TRUE, all = TRUE)
        expect_length(positions, 9L)

        # Inject the on-disk ._ encoding independently of DDIwR's tag writer.
        for (position in head(positions, 3L)) {
            bytes[position + 0:7] <- sas_missing_bytes(format, "_")
        }

        writeBin(bytes, path)
        imported <- io$read(path)
        window <- io$read(path, skip = 1L, n_max = 2L)

        for (name in names(data)) {
            expected <- as.numeric(data[[name]])
            expected[is.na(expected)] <- NA_real_
            expect_identical(as.numeric(imported[[name]]), expected, info = format)
            expect_identical(attr(imported[[name]], "na_index"), c("_" = 2L), info = format)
            expect_identical(attr(window[[name]], "na_index"), c("_" = 1L), info = format)
        }

        expect_s3_class(imported$day, "Date")
        expect_s3_class(imported$moment, "POSIXct")
    }
})

test_that("SAS writers preserve underscore alongside all 26 letter tags", {
    codes <- c("_", as.character(-91:-116))
    values <- c(0, NA_real_, rep(NA_real_, length(codes)))
    data <- data.frame(
        number = values,
        day = structure(values, class = "Date"),
        moment = structure(values, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
    expected_index <- structure(seq.int(3L, length(values)), names = codes)

    for (name in names(data)) {
        attr(data[[name]], "na_index") <- expected_index
        attr(data[[name]], "na_values") <- as.numeric(-91:-116)
    }

    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (format in c("sas7bdat", "xpt-5", "xpt-8")) {
        io <- sas_missing_io(format)
        path <- tempfile()
        repeated_path <- tempfile()
        paths <- c(paths, path, repeated_path)

        for (dictionary in list(NULL, buildDictionary(data, to = "SAS"))) {
            io$write(data, path, dictionary = dictionary)
            bytes <- readBin(path, "raw", n = file.info(path)$size)
            expect_length(grepRaw(sas_missing_bytes(format, "_"), bytes, fixed = TRUE, all = TRUE), 3L)
            expect_length(grepRaw(sas_missing_bytes(format, "."), bytes, fixed = TRUE, all = TRUE), 3L)

            for (tag in LETTERS) {
                expect_length(grepRaw(sas_missing_bytes(format, tag), bytes, fixed = TRUE, all = TRUE), 3L)
            }

            imported <- io$read(path)

            for (name in names(data)) {
                expect_identical(as.numeric(imported[[name]]), values, info = format)
                expect_identical(attr(imported[[name]], "na_index"), expected_index, info = format)
                expect_identical(attr(imported[[name]], "na_values"), as.numeric(-91:-116))
            }

            io$write(imported, repeated_path, dictionary = dictionary)
            expect_identical(io$read(repeated_path), imported, info = format)
        }
    }
})

test_that("convert preserves SAS underscore tags through R and XPT output", {
    data <- data.frame(
        number = c(0, NA_real_, NA_real_, NA_real_),
        day = structure(c(0, NA_real_, NA_real_, NA_real_), class = "Date"),
        moment = structure(c(0, NA_real_, NA_real_, NA_real_),
            class = c("POSIXct", "POSIXt"), tzone = "UTC"),
        empty = rep(NA_real_, 4L)
    )
    expected_index <- structure(3:4, names = c("_", "-91"))

    for (name in names(data)) {
        attr(data[[name]], "na_index") <- expected_index
    }

    # Also exercise a column with ._ as its only tagged missing category.
    attr(data$empty, "na_index") <- c("_" = 3L)
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)

    for (format in c("sas7bdat", "xpt-5", "xpt-8")) {
        io <- sas_missing_io(format)
        extension <- ".xpt"

        if (identical(format, "sas7bdat")) {
            extension <- ".sas7bdat"
        }

        path <- file.path(directory, paste0("source", extension))
        only_path <- file.path(directory, paste0("only", extension))
        rds_path <- file.path(directory, "saved.rds")
        xpt_path <- file.path(directory, "repeated.xpt")
        io$write(data, path)
        imported <- convert(path)
        io$write(data["empty"], only_path)
        only_underscore <- convert(only_path)
        expect_identical(attr(only_underscore$empty, "na_index"), c("_" = 3L))
        convert(path, to = rds_path)
        write_xpt(readRDS(rds_path), xpt_path, name = "MISSING", adjust_tz = FALSE)

        for (result in list(imported, readRDS(rds_path), read_xpt(xpt_path))) {
            for (name in names(data)) {
                expected <- expected_index

                if (identical(name, "empty")) {
                    expected <- c("_" = 3L)
                }

                expect_identical(as.numeric(result[[name]]), as.numeric(data[[name]]), info = format)
                expect_identical(attr(result[[name]], "na_index"), expected, info = format)
            }

            expect_s3_class(result$day, "Date")
            expect_s3_class(result$moment, "POSIXct")
        }
    }
})

test_that("SAS writers retain underscore in integer-backed numeric and temporal columns", {
    values <- c(0L, NA_integer_, NA_integer_, NA_integer_, NA_integer_)
    data <- data.frame(
        number = values,
        day = structure(values, class = "Date"),
        moment = structure(values, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )

    for (name in names(data)) {
        attr(data[[name]], "na_index") <- structure(3:5, names = c("_", "A", "Z"))
    }

    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (format in c("sas7bdat", "xpt-5", "xpt-8")) {
        io <- sas_missing_io(format)
        path <- tempfile()
        paths <- c(paths, path)
        io$write(data, path)
        imported <- io$read(path)

        for (name in names(data)) {
            expect_identical(as.numeric(imported[[name]]), as.numeric(values), info = format)
            expect_identical(attr(imported[[name]], "na_index"),
                structure(3:5, names = c("_", "-91", "-116")), info = format)
        }
    }
})
