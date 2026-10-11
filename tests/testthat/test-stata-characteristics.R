# Independent construction of Stata type-1 expansion fields / <ch> records.
stata_characteristic_record <- function(scope, name, value, format = 118L, encoding = "UTF-8", endian = .Platform$endian) {
    if (format < 110L) {
        width <- 9L
        length_width <- 2L
    }
    else if (format < 118L) {
        width <- 33L
        length_width <- 4L
    }
    else {
        width <- 129L
        length_width <- 4L
    }
    field <- function(text) {
        bytes <- charToRaw(iconv(text, from = "UTF-8", to = encoding))
        stopifnot(length(bytes) < width)

        return(c(bytes, raw(width - length(bytes))))
    }
    value <- c(charToRaw(iconv(value, from = "UTF-8", to = encoding)), as.raw(0L))
    payload <- c(field(scope), field(name), value)
    size <- writeBin(as.integer(length(payload)), raw(), size = length_width, endian = endian)

    if (format >= 117L) {
        return(c(charToRaw("<ch>"), size, payload, charToRaw("</ch>")))
    }

    return(c(as.raw(1L), size, payload))
}

stata_characteristic_fixture <- function(records, format = 118L) {
    path <- tempfile(fileext = ".dta")
    data <- data.frame(Answer = c(1, 9), answer = c(2, 8))
    .Call("declared_write_dta_", data, path, as.integer(format), NULL, 2045L, NULL, PACKAGE = "DDIwR")
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    inserted <- unlist(records, use.names = FALSE)

    if (format >= 117L) {
        offset <- grepRaw(charToRaw("</characteristics>"), bytes, fixed = TRUE)
        stopifnot(length(offset) == 1L)
        map_start <- grepRaw(charToRaw("<map>"), bytes, fixed = TRUE) + 5L
        little <- length(grepRaw(charToRaw("<byteorder>LSF</byteorder>"), bytes, fixed = TRUE)) == 1L

        for (index in 9:13) {
            positions <- map_start + index * 8L + 0:7
            digits <- as.integer(bytes[positions])

            if (!little) {
                digits <- rev(digits)
            }

            value <- sum(digits * 256^(0:7)) + length(inserted)
            replacement <- as.raw(floor(value / 256^(0:7)) %% 256)
            if (!little) {
                replacement <- rev(replacement)
            }

            bytes[positions] <- replacement
        }
    }
    else {
        name_width <- 33L
        label_width <- 81L
        format_width <- 49L
        terminator_width <- 5L

        if (format < 110L) {
            name_width <- 9L
            terminator_width <- 3L
        }

        if (format < 108L) {
            label_width <- 32L
        }

        if (format < 114L) {
            format_width <- 12L
        }

        offset <- 10L + label_width + 18L +
            2L * (1L + name_width + format_width + name_width + label_width) + 6L + 1L
        stopifnot(all(bytes[offset + seq_len(terminator_width) - 1L] == as.raw(0L)))
    }

    writeBin(c(bytes[seq_len(offset - 1L)], inserted, bytes[offset:length(bytes)]), path)

    return(path)
}

test_that("independent Stata records preserve both scopes and case-sensitive names", {
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (format in c(105L, 108L, 113L, 115L, 117L, 118L, 119L)) {
        encoding <- "UTF-8"

        if (format < 118L) {
            encoding <- "windows-1252"
        }
        record <- function(scope, name, value) {
            return(stata_characteristic_record(scope, name, value, format, encoding))
        }
        path <- stata_characteristic_fixture(list(
            record("_dta", "Source", "café\nO'Brien "),
            record("_dta", "source", "lowercase"),
            record("Answer", "Foo", "bar"),
            record("answer", "Foo", "different variable"),
            record("answer", "Empty", "")
        ), format)
        paths <- c(paths, path)
        result <- read_dta(path, num_threads = 1L)

        expect_identical(attr(result, "stata.characteristics"), list(Source = "café\nO'Brien ", source = "lowercase"))
        expect_identical(attr(result$Answer, "stata.characteristics"), list(Foo = "bar"))
        expect_identical(attr(result$answer, "stata.characteristics"), list(Foo = "different variable", Empty = ""))
        expect_equal(as.vector(result$Answer), c(1, 9))
        expect_equal(as.vector(result$answer), c(2, 8))

        skipped <- .Call(
            "declared_df_parse_dta_file", source_file(path), "", 0L, 1L, 1L,
            PACKAGE = "DDIwR"
        )
        expect_identical(names(skipped), "answer")
        expect_identical(attr(skipped$answer, "stata.characteristics"), attr(result$answer, "stata.characteristics"))
        expect_identical(attr(skipped, "stata.characteristics"), attr(result, "stata.characteristics"))
        expect_equal(as.vector(skipped$answer), 8)
    }
})

test_that("Stata characteristics round trip with labels, missing codes and dates", {
    data <- data.frame(
        answer = declared::declared(c(1, 9), labels = c(Yes = 1, Missing = 9), na_values = 9, label = "Answer"),
        text = c("first", "second"),
        when = as.Date(c("2026-01-01", "2026-01-02"))
    )
    attr(data, "stata.characteristics") <- list(Source = "café", source = "Other", Multiline = "one\ntwo ")
    attr(data$answer, "stata.characteristics") <- list(label = "custom label", Foo = "bar")
    attr(data$text, "stata.characteristics") <- list(Empty = "", Quote = "O'Brien / <ch></characteristics> ")
    attr(data$when, "stata.characteristics") <- list(Calendar = "Gregorian")
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (version in c(8L, 10L, 12L, 13L, 14L, 15L)) {
        encoding <- "UTF-8"

        if (version < 14L) {
            encoding <- "windows-1252"
        }

        path <- tempfile(fileext = ".dta")
        paths <- c(paths, path)
        convert(data, to = path, version = version)
        result <- convert(path, encoding = encoding)

        expect_identical(attr(result, "stata.characteristics"), attr(data, "stata.characteristics"))

        for (name in names(data)) {
            expect_identical(attr(result[[name]], "stata.characteristics"), attr(data[[name]], "stata.characteristics"))
        }

        expect_identical(attr(result$answer, "label"), "Answer")
        expect_identical(names(attr(result$answer, "labels")), names(attr(data$answer, "labels")))
        # Date values already have a separate epoch-conversion issue. Check
        # that adding characteristics does not change the existing result.
        baseline <- data
        attr(baseline, "stata.characteristics") <- NULL
        baseline[] <- lapply(baseline, function(column) {
            attr(column, "stata.characteristics") <- NULL

            return(column)
        })
        baseline_path <- tempfile(fileext = ".dta")
        paths <- c(paths, baseline_path)
        convert(baseline, to = baseline_path, version = version)
        baseline_result <- convert(baseline_path, encoding = encoding)
        expect_identical(as.vector(result$when), as.vector(baseline_result$when))
        expect_identical(as.vector(result$text), as.vector(data$text))

        repeated_path <- tempfile(fileext = ".dta")
        paths <- c(paths, repeated_path)
        convert(path, to = repeated_path, version = version, encoding = encoding)
        repeated <- convert(repeated_path, encoding = encoding)
        expect_identical(attr(repeated, "stata.characteristics"), attr(data, "stata.characteristics"))
        expect_identical(attr(repeated$answer, "stata.characteristics"), attr(data$answer, "stata.characteristics"))
    }
})

test_that("Stata writer emits complete records and correct map offsets", {
    data <- data.frame(answer = 1:2)
    attr(data, "stata.characteristics") <- list(Source = "Archive")
    attr(data$answer, "stata.characteristics") <- list(Foo = "bar ")
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (version in c(13L, 14L, 15L)) {
        path <- tempfile(fileext = ".dta")
        paths <- c(paths, path)
        write_dta(data, path, version = version)
        bytes <- readBin(path, "raw", n = file.info(path)$size)
        format <- stata_file_format(version)

        expect_length(grepRaw(stata_characteristic_record("_dta", "Source", "Archive", format), bytes, fixed = TRUE, all = TRUE), 1L)
        expect_length(grepRaw(stata_characteristic_record("answer", "Foo", "bar ", format), bytes, fixed = TRUE, all = TRUE), 1L)

        map_start <- grepRaw(charToRaw("<map>"), bytes, fixed = TRUE) + 5L
        positions <- map_start + 9L * 8L + 0:7
        digits <- as.integer(bytes[positions])

        if (.Platform$endian == "big") {
            digits <- rev(digits)
        }

        expect_equal(sum(digits * 256^(0:7)), grepRaw(charToRaw("<data>"), bytes, fixed = TRUE) - 1L)
    }
})

test_that("Stata parallel import retains notes, characteristics and strL data", {
    skip_if_not(readstat_parallel_supported())
    data <- data.frame(
        answer = rep(c(1, 2), length.out = 20001L),
        text = rep(c("longer string", "second string"), length.out = 20001L)
    )
    attr(data, "stata.characteristics") <- list(Source = "Archive", note0 = "3", note1 = "dataset note ", note3 = "last note")
    attr(data$answer, "stata.characteristics") <- list(Foo = "bar", foo = "baz", note0 = "1", note1 = "variable note")
    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)
    write_dta(data, path, version = 15L, strl_threshold = 4L)

    serial <- read_dta(path, num_threads = 1L)
    parallel <- read_dta(path, num_threads = 2L)

    expect_identical(parallel, serial)
    expect_identical(attr(parallel, "stata.characteristics"), attr(data, "stata.characteristics"))
    expect_identical(attr(parallel$answer, "stata.characteristics"), attr(data$answer, "stata.characteristics"))
    expect_identical(attr(parallel, "notes"), c("dataset note", "last note"))
    expect_identical(as.vector(parallel$text), as.vector(data$text))

    attr(serial, "notes") <- "edited convenience view"
    write_dta(serial, path, version = 15L, strl_threshold = 4L)
    repeated <- read_dta(path, num_threads = 1L)
    expect_identical(attr(repeated, "stata.characteristics"), attr(data, "stata.characteristics"))
    expect_identical(attr(repeated, "notes"), attr(parallel, "notes"))
})

test_that("Stata characteristics survive empty imports, Unicode names and recoding", {
    data <- data.frame(answer = 1:2)
    attr(data, "stata.characteristics") <- list(Source = "Archive")
    attr(data$answer, "stata.characteristics") <- list(Étiquette = "été", Long = strrep("é", 1000L))
    paths <- character()
    on.exit(unlink(paths), add = TRUE)
    path <- tempfile(fileext = ".dta")
    paths <- c(paths, path)
    write_dta(data, path)
    result <- read_dta(path, n_max = 0L, num_threads = 1L)

    expect_equal(nrow(result), 0L)
    expect_identical(attr(result, "stata.characteristics"), attr(data, "stata.characteristics"))
    expect_identical(attr(result$answer, "stata.characteristics"), attr(data$answer, "stata.characteristics"))

    empty <- data[0, , drop = FALSE]
    attr(empty$answer, "stata.characteristics") <- attr(data$answer, "stata.characteristics")
    path <- tempfile(fileext = ".dta")
    paths <- c(paths, path)
    write_dta(empty, path)
    result <- read_dta(path, num_threads = 1L)
    expect_equal(nrow(result), 0L)
    expect_identical(attr(result$answer, "stata.characteristics"), attr(data$answer, "stata.characteristics"))

    categorical <- data.frame(answer = declared::declared(c("yes", "no"), labels = c(Yes = "yes", No = "no")))
    attr(categorical$answer, "stata.characteristics") <- list(Foo = "bar")
    path <- tempfile(fileext = ".dta")
    paths <- c(paths, path)
    convert(categorical, path, chartonum = TRUE)
    result <- convert(path)
    expect_identical(attr(result$answer, "stata.characteristics"), list(Foo = "bar"))
})

test_that("Stata characteristic validation precedes opening the destination", {
    data <- data.frame(answer = 1)
    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)
    writeLines("keep existing content", path)
    invalid <- list(
        c(Foo = "bar"), list("bar"), list(Foo = NA_character_), list(Foo = 1),
        list(Foo = c("a", "b")), list(Foo = character()), list(`bad name` = "value"),
        setNames(list("a", "b"), c("Foo", "Foo")),
        setNames(list("a"), strrep("a", 33L)), list(Foo = strrep("a", 67784L))
    )

    for (characteristics in invalid) {
        attr(data, "stata.characteristics") <- characteristics
        expect_error(write_dta(data, path), "Stata|stata.characteristics|characteristic")
        expect_identical(readLines(path), "keep existing content")
    }

    attr(data, "stata.characteristics") <- NULL
    attr(data$answer, "stata.characteristics") <- list(Foo = "漢字")
    expect_error(write_dta(data, path, version = 13L), "Variable 'answer'.*Windows-1252")
    expect_identical(readLines(path), "keep existing content")
})

test_that("malformed Stata characteristic records fail explicitly", {
    valid <- stata_characteristic_record("_dta", "Source", "Archive")
    missing_scope_terminator <- valid
    missing_scope_terminator[9L:137L] <- as.raw(65L)
    short_record <- valid
    short_record[5L:8L] <- writeBin(2L, raw(), size = 4L, endian = .Platform$endian)
    invalid <- list(
        stata_characteristic_record("unknown", "Source", "Archive"),
        stata_characteristic_record("_dta", "", "Archive"),
        c(valid[seq_len(length(valid) - 6L)], as.raw(65L), tail(valid, 5L)),
        missing_scope_terminator, short_record
    )
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (record in invalid) {
        path <- stata_characteristic_fixture(list(record))
        paths <- c(paths, path)
        expect_error(read_dta(path, num_threads = 1L), "parse|Parse|Unable")
    }

    path <- stata_characteristic_fixture(list(valid, valid))
    paths <- c(paths, path)
    expect_error(read_dta(path, num_threads = 1L), "aborted|parse|Parse")
})

test_that("legacy characteristic lengths cannot overflow short expansion fields", {
    data <- data.frame(answer = 1)
    attr(data, "stata.characteristics") <- list(Source = strrep("a", 32749L))
    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)

    expect_error(.Call(
        "declared_write_dta_", data, path, 105L, NULL, 2045L, NULL,
        PACKAGE = "DDIwR"
    ), "note.*long|Note.*long")
})

test_that("Stata characteristic size limits include the final zero byte", {
    data <- data.frame(answer = 1)
    attr(data, "stata.characteristics") <- list(Source = strrep("a", 67783L))
    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)
    write_dta(data, path)

    result <- read_dta(path, num_threads = 1L)
    expect_identical(attr(result, "stata.characteristics"), attr(data, "stata.characteristics"))
})

test_that("dataset notes can be written without raw note characteristics", {
    data <- data.frame(answer = 1:2)
    attr(data, "stata.characteristics") <- list(Source = "Archive")
    attr(data, "notes") <- c("first café", "second note ")
    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)
    write_dta(data, path, version = 12L)
    result <- read_dta(path, num_threads = 1L)

    expect_identical(attr(result, "stata.characteristics"), list(note0 = "2", note1 = "first café", note2 = "second note ", Source = "Archive"))
    expect_identical(attr(result, "notes"), c("first café", "second note"))
})
