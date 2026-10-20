test_that("independent SAV dataset labels survive export and RDS conversion", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    previous <- options(DDIwR.readstat_parallel_sav = FALSE)
    on.exit(options(previous), add = TRUE)
    path <- file.path(directory, "source.sav")
    copy <- file.path(directory, "copy.sav")
    rds <- file.path(directory, "copy.rds")
    label <- "Enquête été — dataset"
    data <- data.frame(answer = declared::declared(c(1, 9, 2),
        labels = c(Yes = 1, Missing = 9), na_values = 9, label = "Answer"))
    write_sav(data, path)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    encoded <- charToRaw(enc2utf8(label))

    # The SAV header's 64-byte file-label field starts at byte offset 109.
    bytes[110:173] <- c(encoded, charToRaw(strrep(" ", 64L - length(encoded))))
    writeBin(bytes, path)
    imported <- convert(path)
    expect_identical(attr(imported, "label"), label)
    before <- imported

    for (compression in c("none", "byte", "zsav")) {
        write_sav(imported, copy, compress = compression)
        result <- read_sav(copy, user_na = TRUE)
        expect_identical(attr(result, "label"), label)
        expect_identical(attr(result$answer, "label"), "Answer")
        expect_identical(as.vector(result$answer), as.vector(imported$answer))
        written <- readBin(copy, "raw", n = file.info(copy)$size)
        expect_identical(written[110:173], bytes[110:173])

        for (window in list(list(n_max = 0L), list(n_max = 1L, skip = 1L))) {
            result <- do.call(convert, c(list(from = copy), window))
            expect_identical(attr(result, "label"), label)
        }
    }

    convert(path, rds)
    expect_identical(attr(readRDS(rds), "label"), label)
    convert(rds, copy)
    expect_identical(attr(convert(copy), "label"), label)
    empty <- convert(path, n_max = 0L)
    convert(empty, copy)
    expect_identical(attr(convert(copy), "label"), label)
    expect_identical(nrow(convert(copy)), 0L)
    expect_identical(imported, before)
})

test_that("SPSS label overrides and clearing use explicit argument presence", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    source <- file.path(directory, "source.rds")
    copy <- file.path(directory, "copy.sav")
    data <- data.frame(answer = declared::declared(c(1, 9, 2),
        labels = c(Yes = 1, Missing = 9), na_values = 9, label = "Answer"))
    attr(data, "label") <- "Original dataset"
    attr(data, "spss.attributes") <- list(label = "Custom dataset attribute")
    before <- data
    saveRDS(data, source)

    for (input in list(data, source)) {
        convert(input, copy)
        expect_identical(attr(convert(copy), "label"), "Original dataset")
        convert(input, copy, label = "Replacement été")
        result <- convert(copy)
        expect_identical(attr(result, "label"), "Replacement été")
        expect_identical(attr(result$answer, "label"), "Answer")
        expect_identical(attr(result, "spss.attributes"), attr(data, "spss.attributes"))

        for (clear in list(NULL, "")) {
            convert(input, copy, label = clear)
            expect_null(attr(convert(copy), "label"))
            write_sav(data, copy, label = clear)
            expect_null(attr(read_sav(copy), "label"))
        }
    }

    attr(data, "label") <- NA_character_
    expect_no_warning(convert(data, copy, label = "Valid override"))
    expect_identical(attr(convert(copy), "label"), "Valid override")
    expect_no_warning(convert(data, copy, label = NULL))
    expect_null(attr(convert(copy), "label"))
    attr(data, "label") <- attr(before, "label")
    expect_identical(data, before)
    expect_identical(readRDS(source), before)
})

test_that("SAV parallel import preserves exported dataset labels", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    previous <- options(DDIwR.readstat_parallel_sav = FALSE)
    on.exit(options(previous), add = TRUE)
    data <- data.frame(answer = rep(1:3, length.out = 20001L))
    attr(data, "label") <- "Parallel import — été"
    write_sav(data, path)
    serial <- read_sav(path)
    expect_identical(attr(serial, "label"), attr(data, "label"))

    if (readstat_parallel_supported()) {
        parallel <- read_sav_parallel_prototype(path, num_threads = 2L)
        expect_identical(parallel, serial)
    }
})

test_that("native SAS7BDAT export preserves dataset labels and empty datasets", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "source.sas7bdat")
    copy <- file.path(directory, "copy.sas7bdat")
    rds <- file.path(directory, "copy.rds")
    data <- data.frame(answer = structure(c(1, 2, 3), label = "Answer"))
    attr(data, "label") <- "Enquête SAS — été"
    before <- data
    write_sas(data, path)
    result <- convert(path)
    expect_identical(attr(result, "label"), attr(data, "label"))
    expect_identical(attr(result$answer, "label"), "Answer")
    expect_identical(as.vector(result$answer), as.vector(data$answer))
    convert(path, rds)
    expect_identical(attr(readRDS(rds), "label"), attr(data, "label"))
    write_sas(readRDS(rds), copy)
    expect_identical(attr(read_sas(copy), "label"), attr(data, "label"))

    for (window in list(list(n_max = 0L), list(n_max = 1L, skip = 1L))) {
        result <- do.call(convert, c(list(from = path), window))
        expect_identical(attr(result, "label"), attr(data, "label"))
        write_sas(result, copy)
        expect_identical(attr(read_sas(copy), "label"), attr(data, "label"))
        expect_identical(nrow(read_sas(copy)), nrow(result))
    }

    for (clear in list(NULL, "")) {
        attr(data, "label") <- clear
        write_sas(data, copy)
        expect_null(attr(read_sas(copy), "label"))
    }

    attr(data, "label") <- attr(before, "label")
    expect_identical(data, before)
})

test_that("dataset label byte limits are exact and invalid labels preserve output", {
    path <- tempfile()
    on.exit(unlink(path), add = TRUE)
    original <- charToRaw("existing destination")
    data <- data.frame(answer = declared::declared(1:3, label = "Answer"))
    writers <- list(sav = write_sav, sas7bdat = write_sas)
    readers <- list(sav = read_sav, sas7bdat = read_sas)
    limits <- c(sav = 64L, sas7bdat = 256L)

    for (format in names(writers)) {
        limit <- limits[[format]]

        for (label in c(strrep("a", limit), strrep("é", limit / 2L))) {
            attr(data, "label") <- label
            writers[[format]](data, path)
            expect_identical(attr(readers[[format]](path), "label"), label)
        }

        invalid <- list(NA_character_, character(), c("one", "two"), 1, TRUE,
            list("label"), strrep("a", limit + 1L), strrep("é", limit / 2L + 1L))

        for (label in invalid) {
            writeBin(original, path)
            attr(data, "label") <- label
            expect_error(writers[[format]](data, path), "Dataset.*label")
            expect_identical(readBin(path, "raw", n = file.info(path)$size), original)
        }
    }

    attr(data, "label") <- "Original"
    destination <- paste0(path, ".sav")
    on.exit(unlink(destination), add = TRUE)

    for (label in list(NA_character_, c("one", "two"), 1, strrep("é", 33L))) {
        writeBin(original, destination)
        expect_error(convert(data, destination, label = label), "Dataset.*label")
        expect_identical(readBin(destination, "raw", n = file.info(destination)$size), original)
    }
})
