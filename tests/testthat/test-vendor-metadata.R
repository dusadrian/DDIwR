test_that("vendor lists survive declared conversion, recoding and RDS storage", {
    data <- data.frame(answer = structure(
        c(1, 9), label = "Answer", labels = c(Yes = 1, Missing = 9), na_values = 9
    ))
    metadata <- list(
        spss_metadata = list(format = "F12.3", display_width = 24L, future = list(a = 1)),
        sas_metadata = list(format = "F10.2", informat = "BEST32"),
        stata_metadata = list(format = "%9.2f", type = "double")
    )

    for (name in names(metadata)) {
        attr(data$answer, name) <- metadata[[name]]
        attr(data, name) <- list(created = as.POSIXct("2000-01-01", tz = "UTC"))
    }

    converted <- convert(data)
    recoded <- recodeMissings(converted, dictionary = data.frame(old = 9, new = -91))
    path <- tempfile(fileext = ".rds")
    on.exit(unlink(path), add = TRUE)
    convert(recoded, path)

    for (result in list(converted, recoded, readRDS(path),
        as_declared(data), as_declared(data, trusted = TRUE))) {
        for (name in names(metadata)) {
            expect_identical(attr(result$answer, name), metadata[[name]])
            expect_identical(attr(result, name), attr(data, name))
        }
    }

    expect_identical(names(attr(recoded$answer, "na_index")), "-91")
})

test_that("only the target vendor format overrides its legacy attribute", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(answer = c(1, 2))
    attr(data$answer, "label") <- "Answer"
    attr(data$answer, "labels") <- c(Yes = 1, No = 2)
    attr(data$answer, "format.spss") <- "F8.1"
    attr(data$answer, "format.stata") <- "%8.1f"
    attr(data$answer, "format.sas") <- "F8.1"
    attr(data$answer, "spss_metadata") <- list(format = "F14.3")
    attr(data$answer, "stata_metadata") <- list(format = "%15.4f")
    attr(data$answer, "sas_metadata") <- list(format = "F16.5")
    before <- data

    formats <- c(sav = "F14.3", dta = "%15.4f", xpt = "F16.5")
    attributes <- c(sav = "format.spss", dta = "format.stata", xpt = "format.sas")
    vendors <- c(sav = "spss_metadata", dta = "stata_metadata", xpt = "sas_metadata")

    for (extension in names(formats)) {
        path <- file.path(directory, paste0("data.", extension))
        convert(data, path)
        result <- convert(path, declared = FALSE, recode = FALSE)

        expect_identical(attr(result$answer, vendors[[extension]])$format, formats[[extension]])
        expect_identical(as.vector(result$answer), data$answer |> as.vector())
    }

    path <- file.path(directory, "data.sas7bdat")
    written <- write_sas(data, path)
    expect_s3_class(written$answer, "declared")
    expect_identical(attr(read_sas(path)$answer, "sas_metadata")$format, "F16.5")
    expect_identical(data, before)
})

test_that("haven-prefixed attributes are not aliases for DDIwR metadata", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(answer = structure(
        c(1, 2), label = "Answer", labels = c(Yes = 1, No = 2)
    ))
    formats <- c(sav = "F8.1", dta = "%8.1f", xpt = "F8.1")
    attributes <- c(sav = "format.spss", dta = "format.stata", xpt = "format.sas")
    vendors <- c(sav = "spss_metadata", dta = "stata_metadata", xpt = "sas_metadata")

    for (extension in names(formats)) {
        attr(data$answer, attributes[[extension]]) <- formats[[extension]]
    }

    attr(data$answer, "haven_spss") <- list(format = "F14.3")
    attr(data$answer, "haven_stata") <- list(format = "%15.4f")
    attr(data$answer, "haven_sas") <- list(format = "F16.5")
    attr(data, "notes") <- "Legacy note"
    attr(data, "haven_spss") <- list(notes = "Ignored note")
    attr(data, "haven_stata") <- list(notes = "Ignored note", version = 13L)
    attr(data, "haven_sas") <- list(table_name = "IGNORED", version = 8L)
    before <- data

    for (extension in names(formats)) {
        path <- file.path(directory, paste0("DATA.", extension))
        expect_no_warning(convert(data, path))
        result <- convert(path, declared = FALSE, recode = FALSE)
        expect_identical(attr(result$answer, vendors[[extension]])$format, formats[[extension]])
    }

    path <- file.path(directory, "DATA.dta")
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    expect_length(grepRaw("<release>118</release>", bytes, fixed = TRUE), 1L)
    expect_identical(attr(read_dta(path), "stata_metadata")$notes, "Legacy note")

    path <- file.path(directory, "DATA.xpt")
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    expect_length(grepRaw("LIBRARY", bytes, fixed = TRUE), 1L)
    expect_length(grepRaw("IGNORED", bytes, fixed = TRUE), 0L)

    converted <- convert(data)

    for (name in c("spss_metadata", "stata_metadata", "sas_metadata")) {
        expect_null(attr(converted$answer, name))
        expect_null(attr(converted, name))
    }

    expect_identical(data, before)
})

test_that("widths use target list fields with legacy fallback and explicit clearing", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(text = c("one", "two"))
    attr(data$text, "width") <- 18L
    attr(data$text, "display_width") <- 19L
    attr(data$text, "sas_metadata") <- list(width = 100L)
    attr(data$text, "spss_metadata") <- list(width = 24L)
    write_sav(data, path)
    result <- read_sav(path)
    expect_identical(attr(result$text, "spss_metadata")$width, 24L)
    expect_identical(attr(result$text, "spss_metadata")$display_width, 19L)

    attr(data$text, "spss_metadata") <- list(display_width = 27L)
    write_sav(data, path)
    result <- read_sav(path)
    expect_identical(attr(result$text, "spss_metadata")$width, 18L)
    expect_identical(attr(result$text, "spss_metadata")$display_width, 27L)

    attr(data$text, "spss_metadata") <- list(width = NULL, display_width = NULL, format = NULL)
    write_sav(data, path)
    result <- read_sav(path)
    expect_identical(attr(result$text, "spss_metadata")$width, 3L)
    expect_null(attr(result$text, "spss_metadata")$display_width)

    attr(data$text, "spss_metadata") <- NULL
    write_sav(data, path)
    result <- read_sav(path)
    expect_identical(attr(result$text, "spss_metadata")$width, 18L)

    expect_identical(attr(result$text, "spss_metadata")$format, "A18")
    attr(result$text, "spss_metadata")$display_width <- 31L
    convert(result, path)
    expect_identical(attr(read_sav(path)$text, "spss_metadata")$display_width, 31L)
})

test_that("dataset lists supply defaults and explicit arguments take precedence", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(answer = c(1, 2))
    attr(data$answer, "label") <- "Answer"
    attr(data$answer, "labels") <- c(Yes = 1, No = 2)
    attr(data, "notes") <- "Legacy note"
    attr(data, "stata.characteristics") <- list(note0 = "1", note1 = "Raw note", Source = "Archive")
    attr(data, "stata_metadata") <- list(notes = c("New note", "Second note"), version = 13L)
    attr(data, "sas_metadata") <- list(table_name = "ORIGINAL", version = 8L)
    before <- data
    path <- file.path(directory, "data.dta")
    convert(data, path)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    result <- read_dta(path)

    expect_length(grepRaw("<release>117</release>", bytes, fixed = TRUE), 1L)
    expect_identical(attr(result, "stata_metadata")$notes, c("New note", "Second note"))
    expect_identical(attr(result, "stata.characteristics")$Source, "Archive")

    convert(data, path, version = 14L)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    expect_length(grepRaw("<release>118</release>", bytes, fixed = TRUE), 1L)

    convert(data, path, version = NULL)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    expect_length(grepRaw("<release>118</release>", bytes, fixed = TRUE), 1L)

    path <- file.path(directory, "data.xpt")
    convert(data, path)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    expect_length(grepRaw("LIBV8", bytes, fixed = TRUE), 1L)
    expect_gt(length(grepRaw("ORIGINAL", bytes, fixed = TRUE)), 0L)

    convert(data, path, version = 5L, name = "EXPLICIT")
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    expect_length(grepRaw("LIBRARY", bytes, fixed = TRUE), 1L)
    expect_gt(length(grepRaw("EXPLICIT", bytes, fixed = TRUE)), 0L)
    expect_length(grepRaw("ORIGINAL", bytes, fixed = TRUE), 0L)

    convert(data, path, version = NULL)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    expect_length(grepRaw("LIBRARY", bytes, fixed = TRUE), 1L)

    attr(data, "sas_metadata") <- list(table_name = "ORIGINAL")
    convert(data, path)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    expect_length(grepRaw("LIBRARY", bytes, fixed = TRUE), 1L)

    attr(data, "sas_metadata") <- attr(before, "sas_metadata")
    expect_identical(data, before)
})

test_that("invalid effective metadata is rejected before opening the destination", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    writeBin(charToRaw("keep this file"), path)
    expected <- readBin(path, "raw", n = file.info(path)$size)
    data <- data.frame(answer = c(1, 2))
    invalid <- list(
        1, list("F8.2"), list(format = "F8.2", format = "F9.3"),
        setNames(list("F8.2"), NA_character_), data.frame(format = "F8.2"),
        list(format = NA_character_), list(format = c("F8.2", "F9.3")),
        list(display_width = -1), list(display_width = 2.5),
        list(width = Inf), list(width = .Machine$integer.max + 1),
        list(width = 2 + 1i), list(width = as.Date("2020-01-01"))
    )

    for (metadata in invalid) {
        attr(data$answer, "spss_metadata") <- metadata
        expect_error(convert(data, path), "spss_metadata")
        expect_identical(readBin(path, "raw", n = file.info(path)$size), expected)
    }

    attr(data$answer, "spss_metadata") <- list(format = "F12.3")
    attr(data$answer, "sas_metadata") <- "Ignored foreign metadata"
    expect_no_warning(convert(data, path))
    expect_identical(attr(read_sav(path)$answer, "spss_metadata")$format, "F12.3")
})

test_that("dataset fields fall back, clear explicitly, and respect writer arguments", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.dta")
    data <- data.frame(answer = c(1, 2))
    attr(data, "notes") <- "Legacy note"
    attr(data, "stata.characteristics") <- list(note0 = "1", note1 = "Raw note", Source = "Archive")
    attr(data, "stata_metadata") <- list(version = 14L)
    write_dta(data, path)
    expect_identical(attr(read_dta(path), "stata_metadata")$notes, "Raw note")

    for (notes in list(NULL, character())) {
        attr(data, "stata_metadata") <- list(notes = notes)
        write_dta(data, path)
        result <- read_dta(path)
        expect_length(attr(result, "stata_metadata")$notes, 0L)
        expect_identical(attr(result, "stata.characteristics"), list(Source = "Archive"))
    }

    attr(data, "stata.characteristics") <- NULL
    attr(data, "stata_metadata") <- list()
    write_dta(data, path)
    expect_identical(attr(read_dta(path), "stata_metadata")$notes, "Legacy note")

    # An overridden list field is not used or validated.
    attr(data, "stata_metadata") <- list(version = "invalid")
    expect_no_warning(convert(data, path, version = 14L))
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    expect_length(grepRaw("<release>118</release>", bytes, fixed = TRUE), 1L)

    attr(data, "sas_metadata") <- list(version = "invalid", table_name = c("invalid", "name"))
    path <- file.path(directory, "DATA.xpt")
    expect_no_warning(write_xpt(data, path, version = 5L, name = NULL))
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    expect_length(grepRaw("LIBRARY", bytes, fixed = TRUE), 1L)
    expect_gt(length(grepRaw("DATA", bytes, fixed = TRUE)), 0L)
})

test_that("invalid dataset list fields leave existing files intact", {
    path <- tempfile()
    on.exit(unlink(path), add = TRUE)
    writeBin(charToRaw("keep this file"), path)
    expected <- readBin(path, "raw", n = file.info(path)$size)
    data <- data.frame(answer = c(1, 2))

    for (metadata in list(list(notes = NA_character_), list(notes = 1),
        list(version = 16), list(version = c(13, 14)), list(version = 13.5))) {
        attr(data, "stata_metadata") <- metadata
        expect_error(write_dta(data, path), "stata_metadata")
        expect_identical(readBin(path, "raw", n = file.info(path)$size), expected)
    }

    for (metadata in list(list(version = 7), list(table_name = NA_character_),
        list(table_name = ""))) {
        attr(data, "sas_metadata") <- metadata
        expect_error(write_xpt(data, path), "sas_metadata")
        expect_identical(readBin(path, "raw", n = file.info(path)$size), expected)
    }
})

test_that("SAS lists supply widths and unsupported fields are never partially matched", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(text = c("one", "two"), number = c(1, 2))
    attr(data$text, "width") <- 18L
    attr(data$text, "sas_metadata") <- list(width = 24L)
    attr(data$number, "sas_metadata") <- list(width = 4L)
    path <- file.path(directory, "data.xpt")
    write_xpt(data, path)
    result <- read_xpt(path)
    expect_identical(attr(result$text, "sas_metadata")$width, 24L)
    expect_identical(as.vector(result$number), c(1, 2))

    bytes <- readBin(path, "raw", n = file.info(path)$size)
    # Check the XPT NAMESTR record independently of the reader's width metadata.
    numeric_header <- c(as.raw(c(0, 1, 0, 0, 0, 4, 0, 2)), charToRaw("number  "))
    expect_length(grepRaw(numeric_header, bytes, fixed = TRUE), 1L)

    attr(data$number, "sas_metadata") <- list(width = 2L)
    expect_error(write_xpt(data, path), "sas_metadata.*width")
    expect_identical(readBin(path, "raw", n = file.info(path)$size), bytes)

    path <- file.path(directory, "data.sas7bdat")
    expect_error(write_sas(data, path), "sas_metadata.*width")
    expect_false(file.exists(path))
    attr(data$number, "sas_metadata") <- list(width = 4L)
    write_sas(data, path)
    expect_identical(as.vector(read_sas(path)$number), c(1, 2))
    expect_identical(attr(read_sas(path)$number, "sas_metadata")$width, 4L)

    attr(data$number, "sas_metadata") <- NULL
    write_sas(data, path)
    expect_identical(attr(read_sas(path)$text, "sas_metadata")$width, 24L)

    attr(data$number, "format.spss") <- "F10.2"
    attr(data$number, "spss_metadata") <- list(form = "F12.3", display = 30L)
    path <- file.path(directory, "data.sav")
    expect_warning(write_sav(data, path), "not yet supported.*form.*display")
    expect_identical(attr(read_sav(path)$number, "spss_metadata")$format, "F10.2")
})

test_that("unsupported target fields are retained in R and reported on export", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(answer = c(1, 2))
    attr(data$answer, "spss_metadata") <- list(format = "F12.3", future = "Keep")
    before <- data

    expect_warning(convert(data, path), "not yet supported.*future")
    expect_identical(data, before)
    expect_identical(attr(read_sav(path)$answer, "spss_metadata")$format, "F12.3")
})

test_that("character recoding clears storage fields inside vendor lists", {
    x <- declared::declared(c("yes", "no"), labels = c(Yes = "yes", No = "no"))
    attr(x, "spss_metadata") <- list(format = "A20", width = 20L, display_width = 25L)
    attr(x, "sas_metadata") <- list(format = "$20", informat = "$20", width = 20L, future = "Keep")
    attr(x, "stata_metadata") <- list(format = "%20s", width = 20L, type = "strL", future = "Keep")
    result <- recodeCharcat(x)

    expect_identical(attr(result, "spss_metadata"), list(display_width = 25L))
    expect_identical(attr(result, "sas_metadata"), list(future = "Keep"))
    expect_identical(attr(result, "stata_metadata"), list(future = "Keep"))
    expect_identical(names(attr(result, "labels")), c("No", "Yes"))
    expect_identical(as.vector(result), c(2, 1))
})
