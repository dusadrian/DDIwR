test_that("creation timestamps and dataset identifiers survive native round trips", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    created <- as.POSIXct("2001-02-03 04:05:36", tz = "UTC")
    data <- data.frame(answer = structure(
        c(1, 9, 2), label = "Answer", labels = c(Yes = 1, Missing = 9), na_values = 9
    ))
    vendors <- c(sav = "spss_metadata", dta = "stata_metadata",
        sas7bdat = "sas_metadata", xpt = "sas_metadata")
    writers <- list(sav = write_sav, dta = write_dta, sas7bdat = write_sas, xpt = write_xpt)

    for (extension in names(vendors)) {
        attribute <- vendors[[extension]]
        metadata <- list(created = created, modified = created + 86400)

        if (attribute == "sas_metadata") {
            metadata$table_name <- "ORIGINAL"
        }

        attr(data, attribute) <- metadata
        path <- file.path(directory, paste0("data.", extension))
        writers[[extension]](data, path)
        result <- convert(path, recode = FALSE)
        observed <- attr(result, attribute)
        expected <- created

        if (extension == "dta") {
            expected <- created - 36
            expect_identical(observed$version, 14L)
        }
        else if (extension == "xpt") {
            expect_identical(observed$version, 8L)
        }

        expect_identical(observed$created, expected)

        if (attribute == "sas_metadata") {
            expect_identical(observed$modified, created + 86400)
        }
        else {
            expect_false(is.element("modified", names(observed)))
        }

        if (attribute == "sas_metadata") {
            expect_identical(observed$table_name, "ORIGINAL")
        }

        expect_no_warning(writers[[extension]](result, path))
        expect_identical(attr(convert(path, recode = FALSE), attribute), observed)
        rds <- file.path(directory, "data.rds")
        convert(result, rds)
        expect_identical(attr(readRDS(rds), attribute), observed)
    }
})

test_that("SAS modification timestamps remain distinct from creation timestamps", {
    path <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(path), add = TRUE)
    created <- as.POSIXct("2001-02-03 04:05:36", tz = "UTC")
    data <- data.frame(answer = 1:3)
    attr(data, "sas_metadata") <- list(created = created)
    write_sas(data, path)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    padding <- 0L
    endian <- "big"

    # The SAS header stores two consecutive doubles after its alignment pad.
    if (bytes[36L] == as.raw(0x33)) {
        padding <- 4L
    }

    if (bytes[38L] == as.raw(0x01)) {
        endian <- "little"
    }

    indices <- 173:180 + padding
    modified <- readBin(bytes[indices], "double", n = 1L, size = 8L, endian = endian)
    bytes[indices] <- writeBin(modified + 86400, raw(), size = 8L, endian = endian)
    writeBin(bytes, path)
    result <- convert(path, recode = FALSE)

    expect_identical(attr(result, "sas_metadata")$created, created)
    expect_identical(attr(result, "sas_metadata")$modified, created + 86400)
})

test_that("SPSS measurement, alignment and notes survive compression and parallel import", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    previous <- options(DDIwR.readstat_parallel_sav = FALSE)
    on.exit(options(previous), add = TRUE)
    data <- data.frame(
        answer = rep(c(1, 2), length.out = 20001L),
        rank = rep(c(1L, 2L), length.out = 20001L),
        text = rep(c("yes", "no"), length.out = 20001L)
    )
    metadata <- list(
        answer = list(measure = "nominal", alignment = "center"),
        rank = list(measure = "ordinal", alignment = "left"),
        text = list(measure = "nominal", alignment = "right")
    )

    for (name in names(metadata)) {
        attr(data[[name]], "spss_metadata") <- metadata[[name]]
    }

    notes <- c("First note", "", "été", strrep("a", 80))
    attr(data, "notes") <- "Legacy note"
    attr(data, "spss_metadata") <- list(notes = notes)

    for (compression in c("byte", "none", "zsav")) {
        path <- file.path(directory, paste0(compression, ".sav"))
        write_sav(data, path, compress = compression)
        serial <- read_sav(path)
        expect_identical(attr(serial, "spss_metadata")$notes, notes)

        for (name in names(metadata)) {
            expect_identical(attr(serial[[name]], "spss_metadata")[names(metadata[[name]])], metadata[[name]])
            expect_equal(as.vector(serial[[name]]), as.vector(data[[name]]))
        }

        if (compression == "byte" && readstat_parallel_supported()) {
            parallel <- read_sav_parallel_prototype(path, num_threads = 2L)
            expect_identical(parallel, serial)
        }

        # Edit the canonical note field after import.
        attr(serial, "spss_metadata")$notes <- "Edited list note"
        convert(serial, path)
        expect_identical(attr(read_sav(path), "spss_metadata")$notes, "Edited list note")
        expect_identical(attr(read_sav(path, n_max = 0)$rank, "spss_metadata")[names(metadata$rank)], metadata$rank)
    }

    attr(data, "spss_metadata") <- list(notes = NULL)
    write_sav(data, path)
    expect_null(attr(read_sav(path), "spss_metadata")$notes)
})

test_that("SAS formats, informats and widths share the canonical vendor list", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(answer = c(1.25, 2.5), text = c("one", "two"))
    attr(data$answer, "format.sas") <- "F10.2"
    attr(data$answer, "sas_metadata") <- list(informat = "F12.3")
    attr(data$text, "sas_metadata") <- list(informat = "$CHAR20")
    attr(data$text, "width") <- 20L
    writers <- list(sas7bdat = write_sas, xpt = write_xpt)
    readers <- list(sas7bdat = read_sas, xpt = read_xpt)

    for (extension in names(writers)) {
        path <- file.path(directory, paste0("data.", extension))
        writers[[extension]](data, path)
        result <- readers[[extension]](path)
        expect_identical(attr(result$answer, "sas_metadata"), list(format = "F10.2", informat = "F12.3", width = 8L))
        expect_identical(attr(result$text, "sas_metadata"), list(informat = "$CHAR20", width = 20L))
        expect_identical(attr(result$text, "sas_metadata")$width, 20L)

        attr(result$answer, "sas_metadata")$format <- "F16.4"
        writers[[extension]](result, path)
        expect_identical(attr(readers[[extension]](path)$answer, "sas_metadata")$format, "F16.4")
    }

    attr(data$answer, "sas_metadata") <- list(informat = "F12.3", width = 4L)
    write_xpt(data, path)
    expect_identical(attr(read_xpt(path)$answer, "sas_metadata")$width, 4L)
})

test_that("Stata storage choices preserve values, missing tags and temporal units", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(
        byte = c(1, 9, NA, 2), int = c(1, 9, NA, 2), long = c(1, 9, NA, 2),
        float = c(1, 9, NA, 2), double = c(1, 9, NA, 2)
    )
    names(data) <- paste0("v_", names(data))

    for (name in names(data)) {
        data[[name]] <- declared::declared(data[[name]], na_values = 9, labels = c(Missing = 9))
        attr(data[[name]], "stata_metadata") <- list(type = sub("^v_", "", name))
    }

    data$integer_float <- c(1L, 2L, NA, 3L)
    attr(data$integer_float, "stata_metadata") <- list(type = "float")
    data$logical_float <- c(TRUE, FALSE, NA, TRUE)
    attr(data$logical_float, "stata_metadata") <- list(type = "float")
    data$day <- as.Date("2000-01-01") + 0:3
    attr(data$day, "stata_metadata") <- list(type = "long")
    data$timestamp <- as.POSIXct("2000-01-01", tz = "UTC") + 0:3
    attr(data$timestamp, "stata_metadata") <- list(type = "double")

    for (version in c(8L, 13L, 15L)) {
        path <- file.path(directory, paste0(version, ".dta"))
        write_dta(data, path, version = version)
        result <- read_dta(path, num_threads = 1L)

        for (name in names(data)) {
            expect_identical(attr(result[[name]], "stata_metadata")["type"], attr(data[[name]], "stata_metadata"))
            expected <- as.vector(data[[name]])

            if (is.logical(expected)) {
                expected <- as.numeric(expected)
            }

            expect_equal(as.vector(result[[name]]), expected)
        }

        for (name in paste0("v_", c("byte", "int", "long", "float", "double"))) {
            expect_identical(names(attr(result[[name]], "na_index")), "-91")
        }

        expect_no_warning(write_dta(result, path))
        expect_identical(attr(read_dta(path), "stata_metadata")$version, version)
    }

    data <- data.frame(text = rep(c("short", "", "longer"), length.out = 20001L))
    attr(data$text, "stata_metadata") <- list(type = "strL")
    write_dta(data, path, version = 15L)
    serial <- read_dta(path, num_threads = 1L)
    expect_identical(attr(serial$text, "stata_metadata")$type, "strL")
    expect_identical(as.vector(serial$text), as.vector(data$text))

    if (readstat_parallel_supported()) {
        expect_identical(read_dta(path, num_threads = 2L), serial)
    }
})

test_that("unsafe storage and malformed native metadata leave destination files intact", {
    path <- tempfile()
    on.exit(unlink(path), add = TRUE)
    writeBin(charToRaw("keep this file"), path)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    cases <- list(
        list(value = 101, type = "byte"), list(value = -128, type = "byte"),
        list(value = 32741, type = "int"), list(value = 2147483621, type = "long"),
        list(value = 1.5, type = "long"), list(value = 1 + 1e-10, type = "byte"),
        list(value = 0.1, type = "float"),
        list(value = "text", type = "byte"), list(value = 1, type = "strL"),
        list(value = 1, type = "invalid"), list(value = as.Date("2000-01-01"), type = "byte")
    )

    for (case in cases) {
        data <- data.frame(answer = case$value)
        attr(data$answer, "stata_metadata") <- list(type = case$type)
        expect_error(write_dta(data, path), "stata_metadata")
        expect_identical(readBin(path, "raw", n = file.info(path)$size), bytes)
    }

    data <- data.frame(answer = "text")
    attr(data$answer, "stata_metadata") <- list(type = "strL")
    expect_error(write_dta(data, path, version = 12L), "requires Stata 13")
    expect_identical(readBin(path, "raw", n = file.info(path)$size), bytes)

    data <- data.frame(answer = 1)
    for (metadata in list(list(measure = "invalid"), list(alignment = c("left", "right")))) {
        attr(data$answer, "spss_metadata") <- metadata
        expect_error(write_sav(data, path), "spss_metadata")
        expect_identical(readBin(path, "raw", n = file.info(path)$size), bytes)
    }

    attr(data$answer, "spss_metadata") <- NULL
    for (metadata in list(list(created = Sys.Date()), list(created = as.POSIXct(NA)),
        list(created = structure(1e30, class = c("POSIXct", "POSIXt"))),
        list(created = as.POSIXct("1969-06-01", tz = "UTC")),
        list(notes = strrep("é", 41)))) {
        attr(data, "spss_metadata") <- metadata
        expect_error(write_sav(data, path), "spss_metadata|SPSS notes")
        expect_identical(readBin(path, "raw", n = file.info(path)$size), bytes)
    }

    attr(data, "sas_metadata") <- list(created = as.POSIXct("1959-06-01", tz = "UTC"))
    expect_error(write_xpt(data, path), "1960 through 2059")
    expect_identical(readBin(path, "raw", n = file.info(path)$size), bytes)
})

test_that("SPSS multiple-response sets are copied from independent dictionary records", {
    path <- tempfile(fileext = ".sav")
    rds <- tempfile(fileext = ".rds")
    output <- tempfile(fileext = ".sav")
    on.exit(unlink(c(path, rds, output)), add = TRUE)
    previous <- options(DDIwR.readstat_parallel_sav = FALSE)
    on.exit(options(previous), add = TRUE)
    data <- data.frame(long_variable_name = rep(1, 20001L), other = rep(2, 20001L),
        text = rep("yes", 20001L), othertext = rep("no", 20001L))
    attr(data$othertext, "width") <- 3L
    write_sav(data, path)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    termination <- writeBin(c(999L, 0L), raw(), size = 4L, endian = .Platform$endian)
    offset <- grepRaw(termination, bytes, fixed = TRUE, all = TRUE)
    expect_length(offset, 1L)
    records <- list(
        `7` = "$categories=C 5 été long_variable_name other\n$picked=D1 1 6 Picked long_variable_name other\n$words=D3 yes 5 Words text othertext\n",
        `19` = "$labels=E 11 1 1 0  long_variable_name other\n"
    )
    encoded <- lapply(names(records), function(subtype) {
        payload <- charToRaw(records[[subtype]])
        return(c(writeBin(as.integer(c(7, subtype, 1, length(payload))), raw(),
            size = 4L, endian = .Platform$endian), payload))
    })
    writeBin(c(bytes[seq_len(offset - 1L)], unlist(encoded), bytes[offset:length(bytes)]), path)
    serial <- read_sav(path)
    sets <- attr(serial, "spss_metadata")$mr_sets

    expect_named(sets, c("$categories", "$picked", "$words", "$labels"))
    expect_identical(sets$`$categories`$variables, c("long_variable_name", "other"))
    expect_identical(sets$`$categories`$label, "été")
    expect_identical(sets$`$categories`$is_dichotomy, FALSE)
    expect_null(sets$`$categories`$counted_value)
    expect_identical(sets$`$picked`$counted_value, 1L)
    expect_identical(sets$`$words`$counted_string, "yes")
    expect_identical(sets$`$labels`$type, "E")
    expect_identical(sets$`$labels`$label_from_var_label, TRUE)
    expect_identical(attr(convert(path, n_max = 0), "spss_metadata")$mr_sets, sets)
    convert(serial, rds)
    expect_identical(attr(readRDS(rds), "spss_metadata")$mr_sets, sets)

    if (readstat_parallel_supported()) {
        expect_identical(read_sav_parallel_prototype(path, num_threads = 2L), serial)
    }

    expect_no_warning(write_sav(serial, output))
    expect_identical(attr(read_sav(output), "spss_metadata")$mr_sets, sets)
    expect_identical(attr(serial, "spss_metadata")$mr_sets, sets)
})
