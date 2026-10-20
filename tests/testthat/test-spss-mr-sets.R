# Independent dictionary bytes from the GNU PSPP Multiple Response Sets grammar.
spss_mr_record <- function(subtype, text) {
    payload <- charToRaw(enc2utf8(text))
    header <- writeBin(as.integer(c(7, subtype, 1, length(payload))), raw(),
        size = 4L, endian = .Platform$endian)

    return(c(header, payload))
}

spss_mr_definition <- function(type, variables, label = "", counted_string = NULL,
    counted_value = NULL, label_from_var_label = FALSE) {
    return(list(type = type, label = label, is_dichotomy = type != "C",
        counted_value = counted_value, counted_string = counted_string,
        variables = variables, label_from_var_label = label_from_var_label))
}

test_that("SPSS writers emit subtype 7 and 19 records with UTF-8 byte lengths", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(a = 1:2, b = 2:1, s1 = c("Yes é", "No"), s2 = c("No", "Yes é"))
    sets <- list(
        `$categories` = spss_mr_definition("C", c("a", "b"), "été"),
        `$negative` = spss_mr_definition("D", c("a", "b"), counted_value = -2),
        `$string` = spss_mr_definition("D", c("s1", "s2"), "O'Brien", "Yes é"),
        `$counted` = spss_mr_definition("E", c("a", "b"), "Counted", "1"),
        `$derived` = spss_mr_definition("E", c("s1", "s2"), counted_string = "Yes é",
            label_from_var_label = TRUE)
    )
    attr(data, "spss_metadata") <- list(mr_sets = sets)
    before <- data
    write_sav(data, path, compress = "none")
    bytes <- readBin(path, "raw", n = file.info(path)$size)

    expect_length(grepRaw(spss_mr_record(7L, paste0(
        "$categories=C 5 été a b\n",
        "$negative=D2 -2 0  a b\n",
        "$string=D6 Yes é 7 O'Brien s1 s2\n"
    )), bytes, fixed = TRUE, all = TRUE), 1L)
    expect_length(grepRaw(spss_mr_record(19L, paste0(
        "$counted=E 1 1 1 7 Counted a b\n",
        "$derived=E 11 6 Yes é 0  s1 s2\n"
    )), bytes, fixed = TRUE, all = TRUE), 1L)

    expected <- sets
    expected$`$negative`$counted_value <- -2L
    expected$`$negative`$counted_string <- "-2"
    expected$`$string`$counted_value <- -1L
    expected$`$counted`$counted_value <- 1L
    expected$`$derived`$counted_value <- -1L
    expect_identical(attr(read_sav(path), "spss_metadata")$mr_sets, expected)
    expect_identical(data, before)
})

test_that("SPSS MR sets survive compression, long names, windows and parallel readers", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    previous <- options(DDIwR.readstat_parallel_sav = FALSE)
    on.exit(options(previous), add = TRUE)
    data <- data.frame(
        long_variable_one = rep(c(1, 9, 2), length.out = 20001L),
        long_variable_two = rep(c(2, 1, 9), length.out = 20001L),
        V1 = rep(1, 20001L),
        réponse_longue = rep(c("yes", "no", ""), length.out = 20001L),
        réponse_autre = rep(c("no", "yes", ""), length.out = 20001L)
    )
    for (name in names(data)[1:2]) {
        data[[name]] <- declared::declared(data[[name]], label = "Choices",
            labels = c(Yes = 1, No = 2, Missing = 9), na_values = 9)
    }

    sets <- list(
        `$categories` = spss_mr_definition("C", names(data)[1:3], "Choices"),
        `$picked` = spss_mr_definition("D", names(data)[1:2], counted_string = "1", counted_value = 1L),
        `$words` = spss_mr_definition("D", names(data)[4:5], "Words", "yes", -1L),
        `$derived` = spss_mr_definition("E", names(data)[1:2], counted_string = "1",
            counted_value = 1L, label_from_var_label = TRUE)
    )
    attr(data, "spss_metadata") <- list(mr_sets = sets)
    attr(data, "spss.attributes") <- list(source = "Archive")
    attr(data, "notes") <- "Keep notes too"

    for (compression in c("byte", "none", "zsav")) {
        path <- file.path(directory, paste0(compression, ".sav"))
        convert(data, path, compress = compression)
        serial <- read_sav(path)
        expect_identical(attr(serial, "spss_metadata")$mr_sets, sets)
        expect_identical(attr(serial, "spss.attributes"), attr(data, "spss.attributes"))
        expect_identical(attr(serial, "spss_metadata")$notes, attr(data, "notes"))

        for (name in names(data)) {
            expect_equal(as.vector(serial[[name]]), as.vector(data[[name]]))
        }

        for (window in list(list(n_max = 0L), list(n_max = 3L, skip = 4L))) {
            result <- do.call(read_sav, c(list(file = path), window))
            expect_identical(attr(result, "spss_metadata")$mr_sets, sets)
        }

        if (compression == "byte" && readstat_parallel_supported()) {
            expect_identical(read_sav_parallel_prototype(path, num_threads = 2L), serial)
        }

        rds <- file.path(directory, "copy.rds")
        copy <- file.path(directory, "copy.sav")
        convert(path, rds)
        expect_identical(attr(readRDS(rds), "spss_metadata")$mr_sets, sets)
        convert(rds, copy, compress = compression)
        expect_identical(attr(read_sav(copy), "spss_metadata")$mr_sets, sets)

        empty <- read_sav(path, n_max = 0L)
        expect_no_warning(write_sav(empty, copy, compress = compression))
        expect_identical(attr(read_sav(copy), "spss_metadata")$mr_sets, sets)
        expect_identical(nrow(read_sav(copy)), 0L)
    }
})

test_that("MR counted strings are authoritative and empty lists clear definitions", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(a = c(1, 2), b = c(2, 1), s1 = c("1", ""), s2 = c("", "1"))
    attr(data$s1, "width") <- 8L
    attr(data$s2, "width") <- 8L
    attr(data, "spss_metadata") <- list(mr_sets = list(
        `$extended` = list(type = "E", variables = c("a", "b"), counted_string = "1"),
        `$numeric` = list(type = "D", variables = c("a", "b"), counted_string = "2", counted_value = 1),
        `$digits` = list(type = "D", variables = c("s1", "s2"), counted_string = "1 "),
        `$blank` = list(type = "D", variables = c("s1", "s2"), counted_string = ""),
        `$large` = list(type = "D", variables = c("a", "b"), counted_string = "2147483648"),
        `$token` = list(type = "D", variables = c("s1", "s2"), counted_string = "1x")
    ))
    write_sav(data, path)
    result <- read_sav(path)
    sets <- attr(result, "spss_metadata")$mr_sets

    expect_named(sets, c("$numeric", "$digits", "$blank", "$large", "$token", "$extended"))
    expect_identical(sets$`$numeric`$counted_value, 2L)
    expect_identical(sets$`$digits`$counted_string, "1")
    expect_identical(sets$`$blank`$counted_string, "")
    expect_identical(sets$`$extended`$label_from_var_label, FALSE)
    expect_identical(sets$`$large`$counted_string, "2147483648")
    expect_identical(sets$`$large`$counted_value, -1L)
    expect_identical(sets$`$token`$counted_value, -1L)

    write_sav(result, path)
    expect_identical(attr(read_sav(path), "spss_metadata")$mr_sets, sets)

    for (cleared in list(NULL, list())) {
        attr(result, "spss_metadata")$mr_sets <- cleared
        write_sav(result, path)
        expect_null(attr(read_sav(path), "spss_metadata")$mr_sets)
    }
})

test_that("invalid MR definitions fail before modifying the destination", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(a = 1:2, b = 2:1, s1 = c("y", "n"), s2 = c("n", "y"))
    base <- list(type = "D", variables = c("a", "b"), counted_value = 1)
    entries <- list(
        "not a list", list(type = "D"), modifyList(base, list(type = "X")),
        modifyList(base, list(variables = "a")),
        modifyList(base, list(variables = c("a", "absent"))),
        modifyList(base, list(variables = c("a", "a"))),
        modifyList(base, list(variables = c("a", "s1"))),
        modifyList(base, list(is_dichotomy = FALSE)),
        modifyList(base, list(label_from_var_label = TRUE)),
        modifyList(base, list(type = "E", label_from_var_label = TRUE, label = "Conflict")),
        modifyList(base, list(label = "break\nline")),
        modifyList(base, list(label = strrep("é", 129))),
        modifyList(base, list(counted_string = "1\n")),
        modifyList(base, list(counted_string = "1.5")),
        modifyList(base, list(counted_value = NA_real_)),
        modifyList(base, list(counted_value = 1.5)),
        modifyList(base, list(type = "C")),
        modifyList(base, list(future = TRUE)),
        list(type = "D", variables = c("s1", "s2"), counted_value = 1),
        list(type = "D", variables = c("s1", "s2"), counted_string = "Too long")
    )
    invalid <- lapply(entries, function(entry) {
        return(list(`$set` = entry))
    })
    invalid <- c(invalid, list(list(base), list(set = base), list(`$` = base),
        list(`$bad name` = base), setNames(list(base), paste0("$", strrep("a", 64))),
        list(`$set` = base, `$SET` = base)))
    original <- charToRaw("existing destination")

    for (sets in invalid) {
        writeBin(original, path)
        attr(data, "spss_metadata") <- list(mr_sets = sets)
        expect_error(write_sav(data, path), "mr_sets|metadata|multiple.response")
        expect_identical(readBin(path, "raw", n = file.info(path)$size), original)
    }
})
