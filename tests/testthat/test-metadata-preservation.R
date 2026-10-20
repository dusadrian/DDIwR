test_that("SPSS metadata survives default import and RDS conversion", {
    paths <- c(tempfile(fileext = ".sav"), tempfile(fileext = ".rds"),
        tempfile(fileext = ".sav"))
    on.exit(unlink(paths), add = TRUE)
    data <- data.frame(
        answer = declared::declared(
            c(1, 9, 2), labels = c(Yes = 1, Missing = 9),
            na_values = 9, label = "Answer"
        ),
        text = c("one", "two", "three")
    )
    attr(data$answer, "format.spss") <- "F12.3"
    attr(data$answer, "display_width") <- 21L
    attr(data$text, "label") <- "Text"
    attr(data$text, "width") <- 24L
    attr(data$text, "display_width") <- 30L
    attr(data, "spss.attributes") <- list(Source = "Archive")
    attr(data$text, "spss.attributes") <- list(Review = c("first", "second"))
    write_sav(data, paths[1])

    expected <- convert(paths[1], declared = FALSE, recode = FALSE)
    imported <- convert(paths[1])
    convert(paths[1], paths[2])
    convert(imported, paths[3])

    expect_identical(imported, expected)
    expect_identical(readRDS(paths[2]), expected)
    expect_identical(convert(paths[3], declared = FALSE, recode = FALSE), expected)
    expect_identical(attr(imported$text, "spss_metadata")$width, 24L)
    expect_identical(attr(imported$answer, "spss_metadata")$display_width, 21L)
    expect_identical(attr(imported$answer, "na_index"), c("9" = 2L))
})

test_that("SAS and Stata formats survive default import and missing recoding", {
    paths <- c(tempfile(fileext = ".dta"), tempfile(fileext = ".sas7bdat"),
        tempfile(fileext = ".xpt"))
    on.exit(unlink(paths), add = TRUE)
    data <- data.frame(
        answer = declared::declared(
            c(1, 9, 2), labels = c(Yes = 1, Missing = 9),
            na_values = 9, label = "Answer"
        ),
        text = c("one", "two", "three")
    )
    attr(data$answer, "format.stata") <- "%12.3f"
    attr(data$answer, "format.sas") <- "F12.3"
    attr(data$text, "label") <- "Text"
    attr(data$text, "width") <- 24L
    attr(data, "label") <- "Study"
    attr(data, "notes") <- "Original note"
    attr(data, "stata.characteristics") <- list(Source = "Archive")
    attr(data$text, "stata.characteristics") <- list(Review = "Complete")
    write_dta(data, paths[1])
    write_sas(data, paths[2])
    write_xpt(data, paths[3])

    for (path in paths) {
        raw <- convert(path, declared = FALSE, recode = FALSE)
        imported <- convert(path)
        formats <- c("stata_metadata", "sas_metadata")

        for (name in names(raw)) {
            for (attribute in formats) {
                expect_identical(
                    attr(imported[[name]], attribute), attr(raw[[name]], attribute),
                    info = paste(basename(path), name, attribute)
                )
            }
        }

        expect_identical(as.vector(imported$answer), c(1, NA_real_, 2))
        expect_identical(as.vector(imported$text), as.vector(raw$text))

        for (attribute in c("label", "spss_metadata", "stata_metadata", "sas_metadata", "stata.characteristics")) {
            expect_identical(attr(imported, attribute), attr(raw, attribute))
        }
    }
})

test_that("empty imports and row windows retain character storage metadata", {
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(text = c("one", "two", "three"))
    attr(data$text, "label") <- "Text"
    attr(data$text, "width") <- 24L
    attr(data$text, "display_width") <- 30L
    write_sav(data, path)

    for (rows in c(0L, 1L)) {
        expected <- convert(path, n_max = rows, skip = 1L, declared = FALSE)
        imported <- convert(path, n_max = rows, skip = 1L)

        expect_identical(imported, expected)
        expect_type(imported$text, "character")
        expect_identical(attr(imported$text, "spss_metadata")$width, 24L)
        expect_identical(attr(imported$text, "spss_metadata")$display_width, 30L)
        expect_identical(nrow(imported), rows)
    }

    missing <- convert(path, declared = FALSE)
    missing$text[] <- NA_character_
    expect_identical(convert(missing), missing)
})

test_that("declared conversion preserves read metadata without restoring stale missing codes", {
    column <- structure(
        c(1, 9, 2), label = "Answer", labels = c(Yes = 1, Missing = 9),
        na_values = 9, format.spss = "F12.3", format.stata = "%12.3f",
        format.sas = "F12.3", width = 8L, display_width = 21L,
        spss.attributes = list(Source = c("one", "two")),
        stata.characteristics = list(Source = "Archive")
    )
    data <- data.frame(answer = column)
    attr(data, "label") <- "Study"
    attr(data, "notes") <- c("One", "Two")
    attr(data, "spss.attributes") <- list(Source = "Archive")
    attr(data, "stata.characteristics") <- list(Source = "Archive")
    trusted <- as_declared(data, trusted = TRUE)

    expect_identical(as_declared(data), trusted)
    expect_identical(convert(data), trusted)
    expect_identical(convert(trusted), trusted)

    for (to in c("SPSS", "Stata", "SAS")) {
        code <- -91

        if (!identical(to, "SPSS")) {
            code <- "a"
        }

        recoded <- recodeMissings(
            trusted, to = to, dictionary = data.frame(old = 9, new = code)
        )

        for (attribute in c("format.spss", "format.stata", "format.sas", "width",
            "display_width", "spss.attributes", "stata.characteristics")) {
            expect_identical(attr(recoded$answer, attribute), attr(column, attribute))
        }

        for (attribute in c("label", "notes", "spss.attributes", "stata.characteristics")) {
            expect_identical(attr(recoded, attribute), attr(data, attribute))
        }

        expect_identical(as.vector(recoded$answer), c(1, NA_real_, 2))
        expect_identical(names(attr(recoded$answer, "na_index")), as.character(code))
        expect_identical(unname(attr(recoded$answer, "na_index")), 2L)
        expect_equal(attr(recoded$answer, "na_values"), code)
        expect_equal(unname(attr(recoded$answer, "labels"))[2], code)
    }
})

test_that("temporal classes and units survive conversion and missing recoding", {
    data <- data.frame(
        day = structure(c(20454, NA_real_, 20455), class = "Date"),
        moment = structure(c(1767225600.125, NA_real_, 1767225601.5),
            class = c("POSIXct", "POSIXt"), tzone = "UTC"),
        time = structure(c(10.25, NA_real_, 20.5),
            class = c("hms", "difftime"), units = "secs")
    )

    for (name in names(data)) {
        attr(data[[name]], "label") <- name
        attr(data[[name]], "na_values") <- 9
        attr(data[[name]], "na_index") <- c("9" = 2L)
    }

    without_missings <- data

    for (name in names(without_missings)) {
        attr(without_missings[[name]], "na_index") <- NULL
        attr(without_missings[[name]], "na_values") <- NULL
    }

    converted <- convert(without_missings)

    for (name in names(without_missings)) {
        expect_identical(setdiff(class(converted[[name]]), "declared"),
            class(without_missings[[name]]))
        expect_identical(as.numeric(converted[[name]]), as.numeric(without_missings[[name]]))
        expect_identical(attr(converted[[name]], "tzone"), attr(without_missings[[name]], "tzone"))
        expect_identical(attr(converted[[name]], "units"), attr(without_missings[[name]], "units"))
    }

    for (to in c("SPSS", "Stata", "SAS")) {
        code <- -91

        if (!identical(to, "SPSS")) {
            code <- "a"
        }

        imported <- convert(data)
        recoded <- recodeMissings(
            imported, to = to, dictionary = data.frame(old = 9, new = code)
        )

        for (name in names(data)) {
            expect_identical(names(attr(recoded[[name]], "na_index")), as.character(code))
            expect_equal(attr(recoded[[name]], "na_values"), code)

            for (result in list(imported[[name]], recoded[[name]])) {
                expect_identical(setdiff(class(result), "declared"), class(data[[name]]))
                expect_identical(attr(result, "tzone"), attr(data[[name]], "tzone"))
                expect_identical(attr(result, "units"), attr(data[[name]], "units"))
                expect_identical(as.numeric(result), as.numeric(data[[name]]))
            }
        }
    }
})

test_that("character recoding retains annotations but clears obsolete storage metadata", {
    x <- declared::declared(
        c("yes", "no", "missing"),
        labels = c(Yes = "yes", No = "no", Missing = "missing"),
        na_values = "missing", label = "Answer"
    )
    attr(x, "format.spss") <- "A20"
    attr(x, "format.stata") <- "%20s"
    attr(x, "format.sas") <- "$20"
    attr(x, "width") <- 20L
    attr(x, "display_width") <- 25L
    attr(x, "spss.attributes") <- list(Source = c("one", "two"))
    attr(x, "stata.characteristics") <- list(Source = "Archive")
    recoded <- recodeCharcat(x)

    expect_true(is.numeric(recoded))
    expect_identical(attr(recoded, "label"), "Answer")
    expect_identical(names(attr(recoded, "labels")), c("Missing", "No", "Yes"))
    expect_identical(is.na(recoded), c(FALSE, FALSE, TRUE))

    for (attribute in c("format.spss", "format.stata", "format.sas", "width")) {
        expect_null(attr(recoded, attribute))
    }

    for (attribute in c("display_width", "spss.attributes", "stata.characteristics")) {
        expect_identical(attr(recoded, attribute), attr(x, attribute))
    }

    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)
    convert(data.frame(answer = x), path, chartonum = TRUE)
    imported <- convert(path, recode = FALSE)

    expect_identical(attr(imported$answer, "stata.characteristics"),
        attr(x, "stata.characteristics"))
    expect_setequal(names(attr(imported$answer, "labels")), names(attr(recoded, "labels")))
    expect_false(grepl("s$", attr(imported$answer, "stata_metadata")$format))
})
