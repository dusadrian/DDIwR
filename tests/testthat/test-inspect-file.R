inspect_file_uint64 <- function(value, endian = .Platform$endian) {
    bytes <- as.raw(floor(value / 256^(0:7)) %% 256)

    if (endian == "big") {
        bytes <- rev(bytes)
    }

    return(bytes)
}

test_that("inspection returns column definitions and the metadata of full imports", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(
        answer = declared::declared(c(1, 9, 2), labels = c(Yes = 1, Missing = 9),
            na_values = 9, label = "Answer"),
        other = structure(c(1, 2, 1), label = "Other"),
        text = structure(c("été", "one", "two"), label = "Text", width = 20L),
        day = as.Date(c("2001-01-01", "2001-01-02", "2001-01-03"))
    )
    attr(data, "label") <- "Dataset été"
    attr(data, "notes") <- "Archive note"
    attr(data, "spss.attributes") <- list(Source = "Archive")
    attr(data$answer, "spss.attributes") <- list(Review = c("first", "second"))
    attr(data, "stata.characteristics") <- list(Source = "Archive")
    attr(data$answer, "stata.characteristics") <- list(Review = "checked")
    attr(data, "spss_metadata") <- list(mr_sets = list(
        `$choices` = list(type = "C", variables = c("answer", "other"))
    ))
    attr(data$answer, "spss_metadata") <- list(measure = "ordinal", alignment = "left")
    attr(data$answer, "sas_metadata") <- list(informat = "F12.3")
    attr(data, "sas_metadata") <- list(table_name = "DATA")
    before <- data
    writers <- list(
        sav_none = function(path) {
            write_sav(data, path, compress = "none")
        },
        sav_byte = function(path) {
            write_sav(data, path, compress = "byte")
        },
        sav_zsav = function(path) {
            write_sav(data, path, compress = "zsav")
        },
        dta = function(path) {
            write_dta(data, path)
        },
        sas7bdat = function(path) {
            write_sas(data, path)
        },
        xpt5 = function(path) {
            write_xpt(data, path, version = 5L)
        },
        xpt8 = function(path) {
            write_xpt(data, path, version = 8L)
        }
    )

    for (format in names(writers)) {
        extension <- sub("_[a-z]+$", "", format)

        if (startsWith(format, "xpt")) {
            extension <- "xpt"
        }
        else if (format == "sav_zsav") {
            extension <- "zsav"
        }

        path <- file.path(directory, paste0("data.", extension))
        writers[[format]](path)
        vendors <- c(sav = "spss_metadata", zsav = "spss_metadata",
            por = "spss_metadata", dta = "stata_metadata",
            sas7bdat = "sas_metadata", xpt = "sas_metadata")
        imported <- convert(path, recode = FALSE)
        expected <- attributes(imported)
        expected <- expected[setdiff(names(expected), c("names", "row.names", "class"))]
        result <- inspectFile(path)
        expect_identical(result$column_count, 4L)
        expect_identical(result$columns$name, names(data))
        expect_identical(nrow(result$columns), 4L)
        expect_type(result$columns$width, "double")
        expect_true(all(result$columns$width > 0))
        expect_setequal(names(result$metadata), names(expected))
        expect_identical(result$metadata[names(expected)], expected)
        expect_identical(result$columns$metadata[[1]]$label, "Answer")
        expect_identical(result$columns$metadata[[3]][[vendors[[extension]]]]$width,
            attr(imported$text, vendors[[extension]])$width)
        expect_identical(result$columns$metadata[[4]]$class, class(imported$day))

        if (format == "xpt5") {
            expect_identical(result$row_count, NA_real_)
        }
        else {
            expect_identical(result$row_count, 3)
        }

        if (startsWith(format, "sav")) {
            expect_identical(result$columns$metadata[[1]]$na_values, 9)
            expect_identical(result$columns$metadata[[1]]$labels, c(Yes = 1, Missing = 9))
            expect_identical(result$columns$metadata[[1]]$spss.attributes,
                attr(data$answer, "spss.attributes"))
        }

        if (format == "dta") {
            expect_identical(result$columns$metadata[[1]]$labels, attr(imported$answer, "labels"))
            expect_identical(result$columns$metadata[[1]]$stata.characteristics,
                attr(data$answer, "stata.characteristics"))
        }
    }

    expect_identical(data, before)
})

test_that("SAV inspection succeeds with no observation payload under every compression", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(answer = declared::declared(c(1, 9, 2),
        labels = c(Yes = 1, Missing = 9), na_values = 9, label = "Answer"))

    for (compression in c("none", "byte", "zsav")) {
        write_sav(data, path, compress = compression)
        expected <- inspectFile(path)
        bytes <- readBin(path, "raw", n = file.info(path)$size)
        terminator <- grepRaw(writeBin(c(999L, 0L), raw(), size = 4L), bytes, fixed = TRUE)
        expect_length(terminator, 1L)
        writeBin(bytes[seq_len(terminator + 7L)], path)
        expect_identical(inspectFile(path), expected)
        expect_error(read_sav(path, n_max = 1L), "Failed to parse")
    }
})

test_that("large and unknown SAV counts come from metadata without allocating rows", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    write_sav(data.frame(answer = 1:3), path, compress = "none")
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    terminator <- grepRaw(writeBin(c(999L, 0L), raw(), size = 4L), bytes, fixed = TRUE)
    bytes <- bytes[seq_len(terminator + 7L)]
    record <- grepRaw(writeBin(c(7L, 16L, 8L, 2L), raw(), size = 4L), bytes, fixed = TRUE)
    expect_length(record, 1L)
    bytes[81:84] <- writeBin(-1L, raw(), size = 4L)
    large <- bytes
    large[record + 24:31] <- inspect_file_uint64(3e9)
    writeBin(large, path)
    result <- inspectFile(path)
    expect_identical(result$row_count, 3e9)
    expect_identical(result$column_count, 1L)
    expect_lt(as.numeric(object.size(result)), 1e5)

    writeBin(bytes[-(record + 0:31)], path)
    expect_identical(inspectFile(path)$row_count, NA_real_)
})

test_that("Stata inspection skips strL data while retaining later value labels", {
    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(answer = declared::declared(1:3, labels = c(Yes = 1, No = 2)),
        text = c("first long string", "second long string", "third long string"))
    write_dta(data, path, strl_threshold = 4L)
    expected <- inspectFile(path)
    expect_identical(expected$columns$metadata[[1]]$labels, c(Yes = 1, No = 2))
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    strls <- grepRaw(charToRaw("<strls>"), bytes, fixed = TRUE)
    expect_length(strls, 1L)
    bytes[strls + 7:9] <- charToRaw("BAD")
    writeBin(bytes, path)
    expect_identical(inspectFile(path), expected)
    expect_error(read_dta(path, num_threads = 1L), "Failed to parse")

    # A header count larger than R's integer range must never size data vectors.
    count <- grepRaw(charToRaw("<N>"), bytes, fixed = TRUE) + 3L
    bytes[count + 0:7] <- inspect_file_uint64(3e9)
    writeBin(bytes, path)
    result <- inspectFile(path)
    expect_identical(result$row_count, 3e9)
    expect_identical(result$columns, expected$columns)
    expect_lt(as.numeric(object.size(result)), 1e5)
})

test_that("XPT and POR inspection stops at observation headers", {
    path <- tempfile(fileext = ".xpt")
    portable <- tempfile(fileext = ".por")
    on.exit(unlink(c(path, portable)), add = TRUE)
    data <- data.frame(answer = structure(1:3, label = strrep("long label ", 6L)))

    for (version in c(5L, 8L)) {
        write_xpt(data, path, version = version, name = "DATA")
        expected <- inspectFile(path)
        bytes <- readBin(path, "raw", n = file.info(path)$size)
        header <- "HEADER RECORD*******OBS     "

        if (version == 8L) {
            header <- "HEADER RECORD*******OBSV8   "
        }

        position <- grepRaw(charToRaw(header), bytes, fixed = TRUE)
        expect_length(position, 1L)
        writeBin(bytes[seq_len(position + 79L)], path)
        expect_identical(inspectFile(path), expected)

        if (version == 8L) {
            bytes <- bytes[seq_len(position + 79L)]
            bytes[position + 48:62] <- charToRaw("000000000000000")
            writeBin(bytes, path)
            expect_identical(inspectFile(path)$row_count, NA_real_)
        }
    }

    bytes <- readBin(test_path("fixtures/readstat/timestamp_epoch.por"), "raw", n = 1000L)
    position <- grepRaw(charToRaw("F1/2/"), bytes, fixed = TRUE)
    expect_length(position, 1L)
    writeBin(bytes[seq_len(position)], portable)
    result <- inspectFile(portable)
    expect_identical(result$row_count, NA_real_)
    expect_identical(result$columns$name, "ANSWER")
    expect_identical(result$columns$metadata[[1]]$label, "Answer")
    expect_error(read_por(portable), "Failed to parse")
})

test_that("known empty files have zero counts and preserve definitions", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(answer = structure(numeric(), label = "Answer"), text = character())

    for (format in c("sav", "dta", "sas7bdat", "xpt")) {
        path <- file.path(directory, paste0("data.", format))
        writer <- switch(format, sav = write_sav, dta = write_dta,
            sas7bdat = write_sas, xpt = write_xpt)
        writer(data, path)
        result <- inspectFile(path)
        expect_identical(result$row_count, 0)
        expect_identical(result$columns$name, names(data))
        expect_identical(result$columns$metadata[[1]]$label, "Answer")
    }
})

test_that("SAS inspection skips changed and missing observation payloads", {
    path <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(path), add = TRUE)
    write_sas(data.frame(text = structure(rep("valid", 20000L), label = "Text")), path)
    expected <- inspectFile(path)
    expect_identical(expected$row_count, 20000)
    original <- readBin(path, "raw", n = file.info(path)$size)
    padding <- 0L
    endian <- "big"

    if (original[36L] == as.raw(0x33)) {
        padding <- 4L
    }

    if (original[38L] == as.raw(1L)) {
        endian <- "little"
    }

    u64 <- original[33L] == as.raw(0x33)
    sizes <- readBin(original[197:204 + padding], "integer", n = 2L, size = 4L, endian = endian)
    header_size <- sizes[1]
    page_size <- sizes[2]
    page_header_size <- 24L

    if (u64) {
        page_header_size <- 40L
    }

    starts <- seq.int(header_size, length(original) - page_size, by = page_size)
    types <- vapply(starts, function(start) {
        return(readBin(original[start + page_header_size - 7:6], "integer",
            n = 1L, size = 2L, endian = endian))
    }, integer(1))
    data_pages <- starts[bitwAnd(types, 0x0f00L) == 0x0100L]
    expect_gt(length(data_pages), 1L)
    bytes <- original
    bytes[data_pages[1] + page_header_size + 1L] <- charToRaw("X")
    writeBin(bytes, path)
    expect_identical(inspectFile(path), expected)
    observed <- as.character(read_sas(path)$text)
    expect_false(identical(observed, rep("valid", 20000L)))

    # Retain the final page header, but remove its complete observation payload.
    writeBin(original[seq_len(tail(data_pages, 1L) + page_header_size)], path)
    expect_identical(inspectFile(path), expected)
    expect_error(read_sas(path), "Failed to parse")

    # Counts are metadata, including deleted cases, even when no rows exist.
    bytes <- original
    row_size <- grepRaw(as.raw(rep(0xf7, 4L)), bytes, fixed = TRUE)
    count_offset <- 24L
    deleted_offset <- 28L
    count_width <- 4L
    count <- 3e9

    if (u64) {
        count_offset <- 48L
        deleted_offset <- 56L
        count_width <- 8L
        count <- 2^33 + 1
    }

    encode <- function(value) {
        encoded <- inspect_file_uint64(value, endian)

        if (endian == "big") {
            return(tail(encoded, count_width))
        }

        return(head(encoded, count_width))
    }

    bytes[row_size + count_offset + seq_len(count_width) - 1L] <- encode(count)
    bytes[row_size + deleted_offset + seq_len(count_width) - 1L] <- encode(2)
    writeBin(bytes, path)
    result <- inspectFile(path)
    expect_identical(result$row_count, count - 2)
    expect_identical(result$columns, expected$columns)
    expect_lt(as.numeric(object.size(result)), 1e5)
})

test_that("SAS inspection reads optional catalog metadata and preserves encoding precedence", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    data <- data.frame(number = structure(c(0, 1), format.sas = "MISSNUM", label = "Number"))
    write_sas(data, path)
    sas_label_catalog(catalog, encoding = 0L)
    imported <- read_sas(path, catalog_file = catalog, catalog_encoding = "UTF-8")
    result <- inspectFile(path, catalog_file = catalog)
    expect_identical(result$row_count, 2)
    expect_identical(result$columns$metadata[[1]]$labels, attr(imported$number, "labels"))
    expect_null(inspectFile(path)$columns$metadata[[1]]$labels)

    sas_label_catalog(catalog, encoding = 0L, text_encoding = "latin1", label_suffix = "é label")
    imported <- read_sas(path, catalog_file = catalog, catalog_encoding = "latin1")
    result <- inspectFile(path, catalog_file = catalog, catalog_encoding = "latin1")
    expect_identical(result$columns$metadata[[1]]$labels, attr(imported$number, "labels"))
    expect_error(inspectFile(path, catalog_file = paste0(catalog, ".missing")), "catalog_file")
    writeLines("invalid catalog", catalog)
    expect_error(inspectFile(path, catalog_file = catalog), "Failed to parse.*sas7bcat")
})

test_that("SAS inspection never decompresses compressed observations", {
    source <- test_path("fixtures/readstat/compression_type_0x06.sas7bdat")
    path <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(path), add = TRUE)
    expected <- inspectFile(source)
    expect_identical(expected$row_count, 5)
    expect_identical(nrow(read_sas(source)), 5L)
    bytes <- readBin(source, "raw", n = file.info(source)$size)

    # The pinned fixture's first compressed row pointer is at byte offset 4376.
    expect_identical(bytes[4393L], as.raw(4L))
    offset <- sum(as.integer(bytes[4377:4384]) * 256^(0:7))
    size <- sum(as.integer(bytes[4385:4392]) * 256^(0:7))
    bytes[4096L + offset + seq_len(size)] <- as.raw(0xff)
    writeBin(bytes, path)
    expect_identical(inspectFile(path), expected)
    expect_error(read_sas(path), "Failed to parse")
})

test_that("inspection validates arguments and rejects malformed metadata", {
    path <- tempfile(fileext = ".sav")
    unknown <- tempfile(fileext = ".csv")
    on.exit(unlink(c(path, unknown)), add = TRUE)
    write_sav(data.frame(answer = 1:3), path)
    writeLines("not a statistical file", unknown)

    for (file in list(NULL, NA_character_, c(path, path), 1, tempdir(), paste0(path, ".missing"))) {
        expect_error(inspectFile(file), "file")
    }

    expect_error(inspectFile(unknown), "Unsupported.*extension")
    expect_error(inspectFile(path, encoding = NA_character_), "encoding")
    expect_error(inspectFile(path, catalog_encoding = c("UTF-8", "latin1")), "catalog_encoding")
    expect_error(inspectFile(path, catalog_file = path), "only for SAS7BDAT")
    writeLines("invalid header", path)
    expect_error(inspectFile(path), "Failed to parse")
})
