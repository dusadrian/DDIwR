# Construct dictionary records independently of the custom-attribute writer.
# Grammar: GNU PSPP, Data File and Variable Attributes Records (subtypes 17/18).
spss_attribute_record <- function(subtype, text, encoding = "UTF-8") {
    payload <- charToRaw(iconv(text, from = "UTF-8", to = encoding))
    header <- writeBin(
        as.integer(c(7L, subtype, 1L, length(payload))),
        raw(), size = 4L, endian = .Platform$endian
    )

    return(c(header, payload))
}

spss_attribute_fixture <- function(records) {
    path <- tempfile(fileext = ".sav")
    write_sav(data.frame(long_variable_name = c(1, 9), text = c("yes", "no")), path)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    termination <- writeBin(c(999L, 0L), raw(), size = 4L, endian = .Platform$endian)
    offset <- grepRaw(termination, bytes, fixed = TRUE, all = TRUE)
    stopifnot(length(offset) == 1L)
    bytes <- c(bytes[seq_len(offset - 1L)], unlist(records), bytes[offset:length(bytes)])
    writeBin(bytes, path)

    return(path)
}

test_that("independent SPSS records import dataset and variable attribute arrays", {
    path <- spss_attribute_fixture(list(
        spss_attribute_record(17L, "source('Archive été'\n)foo('bar'\n)"),
        spss_attribute_record(18L, "LONG_VARIABLE_NAME:fred('23'\n'34'\n)bert('123'\n)"),
        spss_attribute_record(18L, "text:note('O'Brien / (yes): '\n''\n)@Role('Input'\n)")
    ))
    on.exit(unlink(path), add = TRUE)

    result <- read_sav(path, n_max = 1L)

    expect_identical(attr(result, "spss.attributes"), list(source = "Archive été", foo = "bar"))
    expect_identical(attr(result$long_variable_name, "spss.attributes"), list(fred = c("23", "34"), bert = "123"))
    expect_identical(attr(result$text, "spss.attributes"), list(note = c("O'Brien / (yes): ", ""), `@Role` = "Input"))
    expect_equal(as.vector(result$long_variable_name), 1)

    converted <- convert(path, n_max = 1L)
    expect_identical(attr(converted, "spss.attributes"), attr(result, "spss.attributes"))
    expect_identical(attr(converted$text, "spss.attributes"), attr(result$text, "spss.attributes"))

    skipped <- .Call(
        "declared_df_parse_sav_file", source_file(path), "", TRUE,
        0L, -1L, 0L, PACKAGE = "DDIwR"
    )
    expect_identical(names(skipped), "text")
    expect_identical(attr(skipped$text, "spss.attributes"), attr(result$text, "spss.attributes"))
    expect_identical(attr(skipped, "spss.attributes"), attr(result, "spss.attributes"))
})

test_that("SPSS attributes use the file encoding", {
    path <- spss_attribute_fixture(list(
        spss_attribute_record(17L, "source('café'\n)", encoding = "windows-1252"),
        spss_attribute_record(18L, "text:note('déjà vu'\n)", encoding = "windows-1252")
    ))
    on.exit(unlink(path), add = TRUE)

    result <- read_sav(path, encoding = "windows-1252", n_max = 1L)

    expect_identical(attr(result, "spss.attributes"), list(source = "café"))
    expect_identical(attr(result$text, "spss.attributes"), list(note = "déjà vu"))
})

test_that("SPSS attributes round trip through all compression modes and convert", {
    data <- data.frame(
        long_variable_name = declared::declared(
            c(1, 9), labels = c(Yes = 1, Missing = 9),
            na_values = 9, label = "Answer"
        ),
        text = c("yes", "no"),
        when = as.Date(c("2026-01-01", "2026-01-02"))
    )
    attr(data, "spss.attributes") <- list(source = c("été", "O'Brien"), label = "custom dataset label")
    attr(data$long_variable_name, "spss.attributes") <- list(foo = "bar", array = c("", "a/b:()' "))
    attr(data$text, "spss.attributes") <- list(label = "custom variable label", `$@Role` = "Input")
    attr(data$when, "spss.attributes") <- list(calendar = "Gregorian")
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (compression in c("byte", "none", "zsav")) {
        path <- tempfile(fileext = if (compression == "zsav") ".zsav" else ".sav")
        paths <- c(paths, path)
        write_sav(data, path, compress = compression)
        result <- read_sav(path, user_na = TRUE, n_max = 2L)

        expect_identical(attr(result, "spss.attributes"), attr(data, "spss.attributes"))

        for (name in names(data)) {
            expect_identical(attr(result[[name]], "spss.attributes"), attr(data[[name]], "spss.attributes"))
        }

        expect_identical(attr(result$long_variable_name, "label"), "Answer")
        expect_equal(attr(result$long_variable_name, "labels"), attr(data$long_variable_name, "labels"))
        expect_equal(attr(result$long_variable_name, "na_values"), 9)
        expect_equal(as.vector(result$text), as.vector(data$text))
        expect_equal(as.Date(result$when), data$when, ignore_attr = TRUE)
    }

    path <- tempfile(fileext = ".sav")
    paths <- c(paths, path)
    convert(data, to = path)
    result <- convert(path)
    expect_identical(attr(result, "spss.attributes"), attr(data, "spss.attributes"))

    for (name in names(data)) {
        expect_identical(attr(result[[name]], "spss.attributes"), attr(data[[name]], "spss.attributes"))
    }

    second <- tempfile(fileext = ".sav")
    paths <- c(paths, second)
    convert(path, to = second)
    repeated <- convert(second)
    expect_identical(attr(repeated, "spss.attributes"), attr(data, "spss.attributes"))
    expect_identical(attr(repeated$long_variable_name, "spss.attributes"), attr(data$long_variable_name, "spss.attributes"))
})

test_that("SPSS writer emits the documented dictionary syntax", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(dummy = c(1, 2))
    attr(data, "spss.attributes") <- list(source = "O'Brien")
    attr(data$dummy, "spss.attributes") <- list(fred = c("23", "34"), bert = "123")

    write_sav(data, path, compress = "none")
    bytes <- readBin(path, "raw", n = file.info(path)$size)

    expect_length(grepRaw(spss_attribute_record(17L, "source('O'Brien'\n)"), bytes, fixed = TRUE, all = TRUE), 1L)
    expect_length(grepRaw(spss_attribute_record(18L, "dummy:fred('23'\n'34'\n)bert('123'\n)"), bytes, fixed = TRUE, all = TRUE), 1L)
})

test_that("SPSS attributes survive empty data, row windows and long UTF-8 strings", {
    data <- data.frame(réponse = c(strrep("é", 300L), ""))
    attr(data, "spss.attributes") <- list(source = "Archive")
    attr(data$réponse, "spss.attributes") <- list(unité = c("café", ""))
    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    path <- tempfile(fileext = ".sav")
    paths <- c(paths, path)
    write_sav(data, path)

    for (window in list(list(n_max = 0L), list(n_max = 1L, skip = 1L), list(n_max = 2L))) {
        result <- do.call(read_sav, c(list(file = path), window))
        expect_identical(attr(result, "spss.attributes"), attr(data, "spss.attributes"))
        expect_identical(attr(result$réponse, "spss.attributes"), attr(data$réponse, "spss.attributes"))
    }

    expect_identical(as.vector(read_sav(path, n_max = 2L)$réponse), as.vector(data$réponse))

    empty <- data[0, , drop = FALSE]
    attr(empty$réponse, "spss.attributes") <- attr(data$réponse, "spss.attributes")
    path <- tempfile(fileext = ".sav")
    paths <- c(paths, path)
    write_sav(empty, path)
    result <- read_sav(path, n_max = 0L)
    expect_equal(nrow(result), 0L)
    expect_identical(attr(result, "spss.attributes"), attr(data, "spss.attributes"))
    expect_identical(attr(result$réponse, "spss.attributes"), attr(data$réponse, "spss.attributes"))
})

test_that("parallel SPSS import preserves complete custom attributes", {
    skip_if_not(readstat_parallel_supported())
    data <- data.frame(answer = rep(c(1, 2), length.out = 20001L))
    attr(data, "spss.attributes") <- list(source = c("Archive", "édition"))
    attr(data$answer, "spss.attributes") <- list(foo = c("bar", "O'Brien "))
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    write_sav(data, path)

    serial <- read_sav(path, n_max = nrow(data))
    parallel <- read_sav_parallel_prototype(path, num_threads = 2L)

    expect_identical(parallel, serial)
    expect_identical(attr(parallel, "spss.attributes"), attr(data, "spss.attributes"))
    expect_identical(attr(parallel$answer, "spss.attributes"), attr(data$answer, "spss.attributes"))
    expect_equal(as.vector(parallel$answer), as.vector(data$answer))
})

test_that("SPSS attribute validation precedes opening the destination", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    writeLines("keep existing content", path)
    data <- data.frame(answer = 1)
    invalid <- list(
        c(foo = "bar"), list("bar"), list(foo = NA_character_),
        list(foo = character()), list(foo = 1), list(foo = "two\nlines"),
        list(`bad(name` = "value"), setNames(list("a", "b"), c("foo", "FOO")),
        setNames(list("a"), paste(rep("a", 65L), collapse = ""))
    )

    for (attributes in invalid) {
        attr(data, "spss.attributes") <- attributes
        expect_error(write_sav(data, path), "SPSS|spss.attributes|custom attribute")
        expect_identical(readLines(path), "keep existing content")
    }

    attr(data, "spss.attributes") <- NULL
    attr(data$answer, "spss.attributes") <- list(foo = "a\nb")
    expect_error(write_sav(data, path), "Variable 'answer'.*line feeds")
    expect_identical(readLines(path), "keep existing content")
})

test_that("malformed SPSS attribute records fail without dropping metadata", {
    for (payload in c("foo('bar'\n", "foo('bar\n)", "foo()", "foo('a'\n)/", "foo('a'\n)FOO('b'\n)")) {
        path <- spss_attribute_fixture(list(spss_attribute_record(17L, payload)))
        expect_error(read_sav(path, n_max = 1L), "parse|Parse|Unable")
        unlink(path)
    }

    path <- spss_attribute_fixture(list(spss_attribute_record(18L, "unknown:foo('bar'\n)")))
    on.exit(unlink(path), add = TRUE)
    expect_error(read_sav(path, n_max = 1L), "parse|Parse|Unable")
})

test_that("declared metadata helpers retain SPSS custom attributes", {
    column <- declared::declared(c(1, 9), labels = c(Yes = 1, Missing = 9), na_values = 9)
    attr(column, "spss.attributes") <- list(foo = c("bar", "baz"))
    data <- data.frame(answer = column)
    attr(data, "spss.attributes") <- list(source = "Archive")

    rebuilt <- as_declared(data, trusted = FALSE)
    expect_identical(attr(rebuilt$answer, "spss.attributes"), attr(column, "spss.attributes"))

    recoded <- recodeMissings(data, to = "SPSS", dictionary = data.frame(old = 9, new = -91))
    expect_identical(attr(recoded$answer, "spss.attributes"), attr(column, "spss.attributes"))
    expect_identical(attr(recoded, "spss.attributes"), attr(data, "spss.attributes"))
})
