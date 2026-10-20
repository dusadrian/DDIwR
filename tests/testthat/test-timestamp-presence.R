timestamp_presence_utc <- function(code) {
    previous <- Sys.getenv("TZ", unset = NA_character_)
    on.exit({
        if (is.na(previous)) {
            Sys.unsetenv("TZ")
        }
        else {
            Sys.setenv(TZ = previous)
        }
    }, add = TRUE)
    Sys.setenv(TZ = "UTC")

    return(force(code))
}

timestamp_presence_blank_dta <- function(bytes, version) {
    if (version < 13L) {
        # Formats 113 through 115 have an 81-byte label and an 18-byte stamp.
        bytes[92:109] <- raw(18L)

        return(bytes)
    }

    length_position <- grepRaw(charToRaw("<timestamp>"), bytes, fixed = TRUE) + 11L
    stopifnot(length(length_position) == 1L)
    stamp_length <- as.integer(bytes[length_position])
    bytes[length_position] <- as.raw(0L)
    bytes <- bytes[-(length_position + seq_len(stamp_length))]
    map_start <- grepRaw(charToRaw("<map>"), bytes, fixed = TRUE) + 5L
    little <- length(grepRaw(charToRaw("<byteorder>LSF</byteorder>"),
        bytes, fixed = TRUE)) == 1L

    # Removing the optional stamp shifts the map and all following sections.
    for (index in 1:13) {
        positions <- map_start + index * 8L + 0:7
        digits <- as.integer(bytes[positions])

        if (!little) {
            digits <- rev(digits)
        }

        value <- sum(digits * 256^(0:7)) - stamp_length
        replacement <- as.raw(floor(value / 256^(0:7)) %% 256)

        if (!little) {
            replacement <- rev(replacement)
        }

        bytes[positions] <- replacement
    }

    return(bytes)
}

test_that("XPT timestamp presence follows independent member and library fields", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)

    timestamp_presence_utc({
        path <- file.path(directory, "data.xpt")
        rds <- file.path(directory, "data.rds")
        zero <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")
        later <- as.POSIXct("2001-02-03 04:05:06", tz = "UTC")
        stamp <- "01JAN70:00:00:00"
        fields <- list(145:160, 161:176, 465:480, 481:496)
        cases <- list(
            list(stamps = c("", "", "", ""), expected = setNames(list(), character())),
            list(stamps = c(stamp, "", "", ""), expected = list(created = zero)),
            list(stamps = c("", stamp, "", ""), expected = list(modified = zero)),
            list(stamps = c("", "", stamp, ""), expected = list(created = zero)),
            list(stamps = c("", "", "", stamp), expected = list(modified = zero)),
            list(stamps = c(stamp, stamp, "03FEB01:04:05:06", ""),
                expected = list(created = later, modified = zero)),
            list(stamps = c(stamp, stamp, "", "03FEB01:04:05:06"),
                expected = list(created = zero, modified = later))
        )
        data <- data.frame(answer = declared::declared(c(1, 9, 2),
            labels = c(Yes = 1, Missing = 9), na_values = 9, label = "Answer"))

        for (version in c(5L, 8L)) {
            write_xpt(data, path, version = version, name = "DATA")
            original <- readBin(path, "raw", n = file.info(path)$size)

            for (case in cases) {
                bytes <- original

                for (index in seq_along(fields)) {
                    text <- case$stamps[index]
                    bytes[fields[[index]]] <- charToRaw(paste0(text, strrep(" ", 16L - nchar(text))))
                }

                writeBin(bytes, path)

                for (window in list(list(), list(n_max = 0L), list(n_max = 1L, skip = 1L))) {
                    result <- do.call(convert, c(list(from = path, recode = FALSE), window))
                    metadata <- attr(result, "sas_metadata")
                    present <- intersect(c("created", "modified"), names(metadata))
                    expect_identical(present, as.character(names(case$expected)))
                    expect_identical(metadata[present], case$expected)
                }

                convert(path, rds, recode = FALSE)
                metadata <- attr(readRDS(rds), "sas_metadata")
                present <- intersect(c("created", "modified"), names(metadata))
                expect_identical(metadata[present], case$expected)
                expect_identical(attr(convert(readRDS(rds)), "sas_metadata"), metadata)
            }
        }
    })
})

test_that("SAV distinguishes absent creation stamps from epoch zero without inventing modification", {
    path <- tempfile(fileext = ".sav")
    copy <- tempfile(fileext = ".sav")
    rds <- tempfile(fileext = ".rds")
    on.exit(unlink(c(path, copy, rds)), add = TRUE)
    previous <- options(DDIwR.readstat_parallel_sav = FALSE)
    on.exit(options(previous), add = TRUE)

    timestamp_presence_utc({
        zero <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")
        data <- data.frame(answer = structure(rep(1:3, length.out = 20001L), label = "Answer"))
        attr(data, "spss_metadata") <- list(created = zero)

        for (compression in c("none", "byte", "zsav")) {
            write_sav(data, path, compress = compression)
            original <- readBin(path, "raw", n = file.info(path)$size)
            variants <- list(original, original, original, original, original)
            variants[[2]][93:109] <- charToRaw(strrep(" ", 17L))
            variants[[3]][93:109] <- raw(17L)
            variants[[4]][102:109] <- charToRaw(strrep(" ", 8L))
            variants[[5]][93:101] <- charToRaw(strrep(" ", 9L))

            for (index in seq_along(variants)) {
                writeBin(variants[[index]], path)
                expected <- setNames(list(), character())

                if (index == 1L) {
                    expected <- list(created = zero)
                }

                serial <- read_sav(path)
                expect_identical(attr(serial, "spss_metadata"), expected)
                convert(path, rds)
                expect_identical(attr(readRDS(rds), "spss_metadata"), expected)

                if (index == 1L) {
                    write_sav(serial, copy, compress = compression)
                    expect_identical(attr(read_sav(copy), "spss_metadata"), expected)
                }

                for (window in list(list(n_max = 0L), list(n_max = 1L, skip = 1L))) {
                    result <- do.call(convert, c(list(from = path), window))
                    expect_identical(attr(result, "spss_metadata"), expected)
                }

                if (compression == "byte" && readstat_parallel_supported()) {
                    parallel <- read_sav_parallel_prototype(path, num_threads = 2L)
                    expect_identical(attr(parallel, "spss_metadata"), expected)
                    expect_identical(parallel, serial)
                }
            }
        }
    })
})

test_that("Stata legacy and modern optional stamps retain presence in serial and parallel imports", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)

    timestamp_presence_utc({
        path <- file.path(directory, "data.dta")
        copy <- file.path(directory, "copy.dta")
        rds <- file.path(directory, "data.rds")
        zero <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")
        data <- data.frame(answer = structure(rep(1:3, length.out = 20001L), label = "Answer"))
        attr(data, "stata_metadata") <- list(created = zero)

        for (version in c(8L, 12L, 13L, 14L, 15L)) {
            write_dta(data, path, version = version)
            original <- readBin(path, "raw", n = file.info(path)$size)
            present <- read_dta(path, num_threads = 1L)
            expect_identical(attr(present, "stata_metadata")$created, zero)
            expect_false(is.element("modified", names(attr(present, "stata_metadata"))))
            write_dta(present, copy)
            expect_identical(attr(read_dta(copy), "stata_metadata")$created, zero)

            for (bytes in list(original, timestamp_presence_blank_dta(original, version))) {
                writeBin(bytes, path)
                expected <- attr(read_dta(path, num_threads = 1L), "stata_metadata")

                if (!identical(bytes, original)) {
                    expect_false(any(is.element(c("created", "modified"), names(expected))))
                }

                for (window in list(list(), list(n_max = 0L), list(n_max = 1L, skip = 1L))) {
                    result <- do.call(convert, c(list(from = path, recode = FALSE), window))
                    expect_identical(attr(result, "stata_metadata"), expected)
                }

                if (readstat_parallel_supported()) {
                    expect_identical(attr(read_dta(path, num_threads = 2L), "stata_metadata"), expected)
                }

                convert(path, rds, recode = FALSE)
                expect_identical(attr(readRDS(rds), "stata_metadata"), expected)
            }
        }
    })
})

test_that("SAS7BDAT retains independent finite timestamps including both epoch origins", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)

    timestamp_presence_utc({
        path <- file.path(directory, "data.sas7bdat")
        copy <- file.path(directory, "copy.sas7bdat")
        rds <- file.path(directory, "data.rds")
        zero <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")
        sas_epoch <- as.POSIXct("1960-01-01", tz = "UTC")
        data <- data.frame(answer = structure(1:3, label = "Answer"))
        attr(data, "sas_metadata") <- list(created = zero, modified = zero)
        write_sas(data, path)
        original <- readBin(path, "raw", n = file.info(path)$size)
        padding <- 0L
        endian <- "big"

        if (original[36L] == as.raw(0x33)) {
            padding <- 4L
        }

        if (original[38L] == as.raw(0x01)) {
            endian <- "little"
        }

        for (times in list(c(315619200, 315619200), c(NaN, 315619200),
            c(315619200, NaN), c(NaN, NaN), c(0, 0))) {
            bytes <- original
            bytes[165:180 + padding] <- writeBin(times, raw(), size = 8L, endian = endian)
            writeBin(bytes, path)
            expected <- setNames(list(), character())

            if (is.finite(times[1])) {
                expected$created <- zero

                if (times[1] == 0) {
                    expected$created <- sas_epoch
                }
            }

            if (is.finite(times[2])) {
                expected$modified <- zero

                if (times[2] == 0) {
                    expected$modified <- sas_epoch
                }
            }

            for (window in list(list(), list(n_max = 0L), list(n_max = 1L, skip = 1L))) {
                result <- do.call(convert, c(list(from = path, recode = FALSE), window))
                metadata <- attr(result, "sas_metadata")
                fields <- intersect(c("created", "modified"), names(metadata))
                expect_identical(metadata[fields], expected)
            }

            convert(path, rds, recode = FALSE)
            metadata <- attr(readRDS(rds), "sas_metadata")
            fields <- intersect(c("created", "modified"), names(metadata))
            expect_identical(metadata[fields], expected)

            if (all(is.finite(times))) {
                write_sas(readRDS(rds), copy)
                metadata <- attr(read_sas(copy), "sas_metadata")
                expect_identical(metadata[c("created", "modified")], expected)
            }
        }
    })
})

test_that("POR creation at epoch zero is present without a modification timestamp", {
    path <- test_path("fixtures/readstat/timestamp_epoch.por")
    rds <- tempfile(fileext = ".rds")
    on.exit(unlink(rds), add = TRUE)

    timestamp_presence_utc({
        zero <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")

        for (window in list(list(), list(n_max = 0L), list(n_max = 1L, skip = 1L))) {
            result <- do.call(convert, c(list(from = path), window))
            expect_identical(attr(result, "spss_metadata"), list(created = zero))
        }

        convert(path, rds)
        expect_identical(attr(readRDS(rds), "spss_metadata"), list(created = zero))
        expect_identical(as.vector(read_por(path)$ANSWER), c(1, 2))
    })
})
