test_that("SAS creation and modification timestamps survive native and public round trips", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    created <- as.POSIXct("2001-01-15 12:34:56", tz = "UTC")
    modified <- as.POSIXct("2001-07-15 13:45:12", tz = "UTC")
    data <- data.frame(answer = declared::declared(c(1, 9, 2),
        labels = c(Yes = 1, Missing = 9), na_values = 9, label = "Answer"))
    metadata <- list(created = created, modified = modified, table_name = "ORIGINAL")
    attr(data, "sas_metadata") <- metadata
    before <- data
    writers <- list(sas7bdat = write_sas,
        xpt5 = function(data, path) {
            write_xpt(data, path, version = 5L)
        },
        xpt8 = function(data, path) {
            write_xpt(data, path, version = 8L)
        })

    for (format in names(writers)) {
        extension <- "xpt"

        if (format == "sas7bdat") {
            extension <- format
        }

        path <- file.path(directory, paste0(format, ".", extension))
        copy <- file.path(directory, paste0("copy.", extension))

        if (format == "sas7bdat") {
            write_sas(data, path)
        }
        else {
            version <- as.integer(sub("xpt", "", format))
            convert(data, path, version = version)
        }

        for (window in list(list(), list(n_max = 0L), list(n_max = 1L, skip = 1L))) {
            result <- do.call(convert, c(list(from = path), window))
            expect_identical(attr(result, "sas_metadata")$created, created)
            expect_identical(attr(result, "sas_metadata")$modified, modified)
            expect_identical(attr(result, "sas_metadata")$table_name, "ORIGINAL")
        }

        rds <- file.path(directory, "copy.rds")
        convert(path, rds)
        expect_identical(attr(readRDS(rds), "sas_metadata")$modified, modified)

        writers[[format]](readRDS(rds), copy)

        expect_identical(attr(convert(copy), "sas_metadata")$created, created)
        expect_identical(attr(convert(copy), "sas_metadata")$modified, modified)

        empty <- convert(path, n_max = 0L)
        writers[[format]](empty, copy)
        expect_identical(attr(convert(copy), "sas_metadata")$modified, modified)
        expect_identical(nrow(convert(copy)), 0L)
    }

    expect_identical(data, before)
})

test_that("SAS7BDAT stores distinct UTC offsets for creation and modification", {
    previous <- Sys.getenv("TZ", unset = NA_character_)
    on.exit({
        if (is.na(previous)) {
            Sys.unsetenv("TZ")
        }
        else {
            Sys.setenv(TZ = previous)
        }
    }, add = TRUE)
    Sys.setenv(TZ = "America/Los_Angeles")
    path <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(path), add = TRUE)
    created <- as.POSIXct("2001-01-15 12:34:56", tz = "UTC")
    modified <- as.POSIXct("2001-07-15 13:45:12", tz = "UTC")
    data <- data.frame(answer = 1:3)
    attr(data, "sas_metadata") <- list(created = created, modified = modified)
    write_sas(data, path)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    padding <- 0L
    endian <- "big"

    if (bytes[36L] == as.raw(0x33)) {
        padding <- 4L
    }

    if (bytes[38L] == as.raw(0x01)) {
        endian <- "little"
    }

    # Two timestamp doubles followed by their independent local-minus-UTC offsets.
    header <- readBin(bytes[165:196 + padding], "double", n = 4L, size = 8L, endian = endian)
    epoch <- as.numeric(as.POSIXct("1960-01-01", tz = "UTC"))
    expect_identical(header[3:4], c(-28800, -25200))
    expect_equal(header[1:2] - header[3:4] + epoch, as.numeric(c(created, modified)))
    expect_identical(attr(read_sas(path), "sas_metadata")$created, created)
    expect_identical(attr(read_sas(path), "sas_metadata")$modified, modified)

    # SAS7BDAT records enough offset information to read in a different timezone.
    Sys.setenv(TZ = "UTC")
    expect_identical(attr(read_sas(path), "sas_metadata")$created, created)
    expect_identical(attr(read_sas(path), "sas_metadata")$modified, modified)
})

test_that("XPT imports member timestamps and writes the documented header positions", {
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
    paths <- c(tempfile(fileext = ".xpt"), tempfile(fileext = ".xpt"))
    on.exit(unlink(paths), add = TRUE)
    created <- as.POSIXct("2001-01-15 12:34:56", tz = "UTC")
    modified <- as.POSIXct("2001-07-15 13:45:12", tz = "UTC")
    library_created <- "01FEB00:01:02:03"
    library_modified <- "02FEB00:02:03:04"
    member_created <- "15JAN01:12:34:56"
    member_modified <- "15JUL01:13:45:12"
    data <- data.frame(answer = 1:3)
    attr(data, "sas_metadata") <- list(created = created, modified = modified)

    for (version in c(5L, 8L)) {
        write_xpt(data, paths[1], version = version, name = "DATASET")
        bytes <- readBin(paths[1], "raw", n = file.info(paths[1])$size)

        # SAS TS-140: library stamps in records 2/3, member stamps in records 6/7.
        expect_identical(rawToChar(bytes[145:160]), member_created)
        expect_identical(rawToChar(bytes[161:176]), member_modified)
        expect_identical(rawToChar(bytes[465:480]), member_created)
        expect_identical(rawToChar(bytes[481:496]), member_modified)

        bytes[145:160] <- charToRaw(library_created)
        bytes[161:176] <- charToRaw(library_modified)
        writeBin(bytes, paths[1])
        result <- convert(paths[1], recode = FALSE)
        expect_identical(attr(result, "sas_metadata")$created, created)
        expect_identical(attr(result, "sas_metadata")$modified, modified)
        write_xpt(result, paths[2])
        expect_identical(attr(read_xpt(paths[2]), "sas_metadata")$modified, modified)

        # Missing member stamps fall back independently to the library pair.
        blank <- charToRaw(strrep(" ", 16L))
        bytes[465:480] <- blank
        bytes[481:496] <- blank
        writeBin(bytes, paths[1])
        fallback <- attr(read_xpt(paths[1]), "sas_metadata")
        expect_identical(fallback$created, as.POSIXct("2000-02-01 01:02:03", tz = "UTC"))
        expect_identical(fallback$modified, as.POSIXct("2000-02-02 02:03:04", tz = "UTC"))

        for (invalid in c("15XXX01:13:45:12", "31FEB01:13:45:12", "15JUL01:99:45:12")) {
            bytes[481:496] <- charToRaw(invalid)
            writeBin(bytes, paths[1])
            expect_error(read_xpt(paths[1]), "timestamp|date|time")
        }
    }
})

test_that("missing and NULL modification times retain defaults and epoch zero is explicit", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(answer = 1:3)
    created <- as.POSIXct("2001-01-15 12:34:56", tz = "UTC")
    zero <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")
    writers <- list(sas7bdat = write_sas, xpt = write_xpt)
    readers <- list(sas7bdat = read_sas, xpt = read_xpt)

    for (extension in names(writers)) {
        path <- file.path(directory, paste0("data.", extension))

        for (metadata in list(list(created = created), list(created = created, modified = NULL))) {
            attr(data, "sas_metadata") <- metadata
            writers[[extension]](data, path)
            result <- attr(readers[[extension]](path), "sas_metadata")
            expect_identical(result$created, created)
            expect_identical(result$modified, created)
        }

        attr(data, "sas_metadata") <- list(created = created, modified = zero)
        writers[[extension]](data, path)
        expect_identical(attr(readers[[extension]](path), "sas_metadata")$modified, zero)

        attr(data, "sas_metadata") <- list(modified = zero)
        start <- floor(as.numeric(Sys.time()))
        writers[[extension]](data, path)
        result <- attr(readers[[extension]](path), "sas_metadata")
        expect_identical(result$modified, zero)
        expect_gte(as.numeric(result$created), start)
        expect_lte(as.numeric(result$created), ceiling(as.numeric(Sys.time())))
    }
})

test_that("invalid supported modification timestamps preserve destination bytes", {
    path <- tempfile()
    on.exit(unlink(path), add = TRUE)
    original <- charToRaw("existing destination")
    data <- data.frame(answer = 1:3)
    invalid <- list(Sys.Date(), 1, "2001-01-01", as.POSIXct(NA),
        as.POSIXct(c("2001-01-01", "2002-01-01"), tz = "UTC"),
        structure(Inf, class = c("POSIXct", "POSIXt")),
        structure(1e30, class = c("POSIXct", "POSIXt")),
        as.POSIXct("2001-01-01", tz = "UTC") + 0.25)

    for (writer in list(write_sas, write_xpt)) {
        for (value in invalid) {
            writeBin(original, path)
            attr(data, "sas_metadata") <- list(modified = value)
            expect_error(writer(data, path), "sas_metadata\\$modified")
            expect_identical(readBin(path, "raw", n = file.info(path)$size), original)
        }
    }

    for (year in c("1959-06-01", "2060-06-01")) {
        attr(data, "sas_metadata") <- list(modified = as.POSIXct(year, tz = "UTC"))
        expect_error(write_xpt(data, path), "modified.*1960 through 2059")
        expect_identical(readBin(path, "raw", n = file.info(path)$size), original)
    }
})
