test_that("paired SAS reads retain data-file metadata and catalog labels", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    data <- data.frame(answer = c(1, 2, 1), text = c("one", "two", "three"))
    labels <- c(Yes = 1, No = 2)
    attr(data$answer, "labels") <- labels
    attr(data, "label") <- "Data file metadata"
    write_sas(data, path, catalog_file = catalog)

    for (limit in c(0L, 3L)) {
        result <- read_sas(path, catalog_file = catalog, n_max = limit)

        expect_identical(names(result), names(data))
        expect_identical(nrow(result), limit)
        expect_identical(attr(result, "label"), attr(data, "label"))
        expect_identical(attr(result$answer, "labels"), labels)
        expect_equal(result$answer, data$answer[seq_len(limit)], ignore_attr = TRUE)
        expect_equal(result$text, data$text[seq_len(limit)], ignore_attr = TRUE)
    }

    unlink(path)
    expect_error(read_sas(path, catalog_file = catalog))
})

test_that("native SAS catalogs include the structural records required by SAS", {
    skip_if(.Platform$endian != "little", "Native byte fixtures use the Linux little-endian layout.")
    fixture <- test_path("fixtures", "readstat", "sas94-m9-reference.sas7bcat")
    reference <- readBin(fixture, "raw", n = file.info(fixture)$size)
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    data <- data.frame(number = 1)
    attr(data$number, "labels") <- c(One = 1)
    attr(data$number, "sas_metadata") <- list(format = "SIMPLE")
    write_sas(data, path, catalog_file = catalog)
    bytes <- readBin(catalog, "raw", n = file.info(catalog)$size)

    # SAS catalogs require the native host layout; CEDA does not translate them.
    expect_identical(bytes[33:41], reference[33:41])
    expect_identical(bytes[201:204], reference[201:204])
    expect_length(grepRaw(charToRaw("XLCH"), bytes, fixed = TRUE, all = TRUE), 1L)
    expect_length(grepRaw(charToRaw("PGBITMAP"), bytes, fixed = TRUE, all = TRUE), 1L)
    expect_length(grepRaw(charToRaw("FORMAT  SIMPLE"), bytes, fixed = TRUE, all = TRUE), 1L)
})

test_that("catalog directory entries match SAS-native name and location fields", {
    skip_if(.Platform$endian != "little", "Native byte fixtures use the Linux little-endian layout.")
    fixture <- test_path("fixtures", "readstat", "sas94-m9-simple.sas7bcat")
    reference <- readBin(fixture, "raw", n = file.info(fixture)$size)
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    data <- data.frame(number = 1)
    attr(data$number, "labels") <- c(One = 1)
    attr(data$number, "sas_metadata") <- list(format = "SIMPLE")
    write_sas(data, path, catalog_file = catalog)
    bytes <- readBin(catalog, "raw", n = file.info(catalog)$size)
    marker <- charToRaw("FORMAT  SIMPLE")
    actual <- grepRaw(marker, bytes, fixed = TRUE, all = TRUE)
    expected <- grepRaw(marker, reference, fixed = TRUE, all = TRUE)

    expect_length(actual, 1L)
    expect_length(expected, 1L)
    expect_identical(bytes[actual + 0:55], reference[expected + 0:55])
    # The native XLSR descriptor stores widths as hexadecimal text.
    expect_identical(bytes[12288L + 1296L + 79:96],
        reference[12288L + 1296L + 79:96])
    expect_identical(bytes[8192L + 33:40], reference[8192L + 33:40])
    expect_identical(sas_export_catalog_records(catalog), sas_export_catalog_records(fixture))
})

test_that("SAS-native long-key and many-format catalogs survive paired export", {
    skip_if(.Platform$endian != "little", "Native byte fixtures use the Linux little-endian layout.")
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    source <- file.path(directory, "source.sas7bdat")
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    fixture <- test_path("fixtures", "readstat", "sas94-m9-long-keys.sas7bcat")
    data <- data.frame(text = c("", " A", "abcdefghijklmnopqrstuv", "é"))
    attr(data$text, "sas_metadata") <- list(format = "$CHARACTER_LONG_NAME")
    write_sas(data, source)
    imported <- read_sas(source, catalog_file = fixture)
    expected <- c(Blank = "", Lead = " A", Long = "abcdefghijklmnopqrstuv", Accent = "é")
    expect_identical(attr(imported$text, "labels"), expected)
    write_sas(imported, path, catalog_file = catalog)
    expect_identical(sas_export_catalog_records(catalog), sas_export_catalog_records(fixture))
    expect_identical(attr(read_sas(path, catalog_file = catalog)$text, "labels"), expected)
    bytes <- readBin(catalog, "raw", n = file.info(catalog)$size)
    native <- readBin(fixture, "raw", n = file.info(fixture)$size)
    # These four native variable-length key records include six padding bytes.
    key_start <- 8192L + 3L * 4096L + 64L + 190L
    expect_identical(bytes[key_start + seq_len(143L)], native[key_start + seq_len(143L)])
    # These allocation sizes include eight-byte alignment of each record body.
    block_start <- 8192L + 3L * 4096L + 64L
    expect_identical(bytes[block_start + 87:102], native[block_start + 87:102])

    fixture <- test_path("fixtures", "readstat", "sas94-m9-many.sas7bcat")
    data <- setNames(as.data.frame(rep(list(c(1, 2)), 80L)), paste0("v", seq_len(80L)))

    for (index in seq_along(data)) {
        attr(data[[index]], "sas_metadata") <- list(format = sprintf("FORM%03dF", index))
    }

    write_sas(data, source)
    imported <- read_sas(source, catalog_file = fixture)
    expect_length(attr(imported[[1]], "labels"), 1000L)
    expect_identical(attr(imported[[80]], "labels"), c(Label1 = 1, Label2 = 2))
    write_sas(imported, path, catalog_file = catalog)
    expect_identical(sas_export_catalog_records(catalog), sas_export_catalog_records(fixture))
    bytes <- readBin(catalog, "raw", n = file.info(catalog)$size)
    native <- readBin(fixture, "raw", n = file.info(fixture)$size)
    endian <- if (bytes[38L] == as.raw(1L)) { "little" } else { "big" }
    offsets <- readBin(bytes[8192L + 49:52], "integer", n = 2L, size = 2L,
        signed = FALSE, endian = endian)
    native_offsets <- readBin(native[8192L + 49:52], "integer", n = 2L,
        size = 2L, signed = FALSE, endian = "little")

    # SAS derives each branch key's length from decreasing record offsets.
    expect_true(all(diff(native_offsets) < 0L))
    expect_true(all(diff(offsets) < 0L))
    expect_identical(readBin(bytes[8192L + 41:44], "integer", n = 1L,
        size = 4L, endian = endian), 1L)
    expect_identical(bytes[8192L + offsets[2L] + seq_len(40L)], rep(as.raw(255L), 40L))
    expect_length(grepRaw(charToRaw("XLSR"), bytes, fixed = TRUE, all = TRUE), 81L)
    children <- vapply(offsets, function(offset) {
        return(readBin(bytes[8192L + offset + 41:44], "integer", n = 1L,
            size = 4L, endian = endian))
    }, integer(1))
    native_ends <- c(4096L, native_offsets[1L])
    native_children <- vapply(native_ends, function(end) {
        return(readBin(native[8192L + end - 8L + 1:4], "integer", n = 1L,
            size = 4L, endian = "little"))
    }, integer(1))

    # Enumeration walks from the lowest leaf to the preceding higher leaf.
    expect_identical(readBin(native[8192L + (native_children[1L] - 1L) * 4096L + 17:20],
        "integer", n = 1L, size = 4L, endian = "little"), native_children[2L])
    expect_identical(readBin(bytes[8192L + (children[1L] - 1L) * 4096L + 17:20],
        "integer", n = 1L, size = 4L, endian = endian), children[2L])
    expect_identical(bytes[8192L + (children[1L] - 1L) * 4096L + 33:34],
        native[8192L + (native_children[1L] - 1L) * 4096L + 33:34])
    result <- read_sas(path, catalog_file = catalog)

    for (index in seq_along(imported)) {
        expect_identical(attr(result[[index]], "labels"), attr(imported[[index]], "labels"))
    }
})
