test_that("SPSS export sizes strings for restored missing codes and UTF-8", {
    exported <- data.frame(
        answer = declared::declared(
            rep(c("a", "-91", "été"), length.out = 20001L),
            labels = c(Yes = "a", Missing = "-91", Summer = "été"),
            na_values = "-91",
            label = "Réponse"
        )
    )
    attr(exported$answer, "width") <- 1L
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)

    write_sav(exported, path)

    previous <- options(DDIwR.readstat_parallel_sav = FALSE)
    on.exit(options(previous), add = TRUE)
    serial <- read_sav(path, user_na = TRUE)
    parallel <- read_sav_parallel_prototype(path, user_na = TRUE, num_threads = 2L)

    expect_identical(parallel, serial)
    expect_equal(
        declared::undeclare(serial$answer, drop = TRUE),
        rep(c("a", "-91", "été"), length.out = 20001L)
    )
    expect_identical(attr(serial$answer, "label"), "Réponse")
    expect_identical(attr(serial$answer, "na_values"), "-91")
    expect_equal(attr(serial$answer, "labels"), attr(exported$answer, "labels"))
})

test_that("Stata parallel import preserves significant blanks and strL references", {
    exported <- data.frame(
        fixed = rep(c("a     ", "b  ", ""), length.out = 20001L),
        longtext = rep(c(" third ", "fourth", ""), length.out = 20001L)
    )
    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)

    write_dta(exported, path, version = 15L, strl_threshold = 6L)
    serial <- read_dta(path, num_threads = 1L)
    parallel <- read_dta(path, num_threads = 2L)

    expect_identical(parallel, serial)
    expect_identical(as.vector(serial$fixed), exported$fixed)
    expect_identical(as.vector(serial$longtext), exported$longtext)
})

test_that("ReadStat reads independent upstream Stata regression fixtures", {
    strl <- read_dta(test_path("fixtures/readstat/strl_119.dta"), num_threads = 1L)
    blanks <- read_dta(test_path("fixtures/readstat/trailing_blanks_118.dta"), num_threads = 1L)
    binary <- read_dta(test_path("fixtures/readstat/binary_strl_118.dta"), num_threads = 1L)

    expect_identical(as.vector(strl$s), c("third", "fourth"))
    expect_identical(as.vector(blanks$s), c("a     ", "b  ", ""))
    expect_identical(as.vector(binary$s), c("bin", "hello"))
})

test_that("Stata missing-code limits apply per variable", {
    first_codes <- -91:-110
    second_codes <- -111:-130
    exported <- data.frame(
        first = declared::declared(
            c(1, first_codes),
            labels = setNames(c(1L, first_codes), c("Yes", paste0("Missing ", first_codes))),
            na_values = first_codes
        ),
        second = declared::declared(
            c(1, second_codes),
            labels = setNames(c(1L, second_codes), c("Yes", paste0("Missing ", second_codes))),
            na_values = second_codes
        )
    )
    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)

    write_dta(exported, path)
    imported <- read_dta(path, num_threads = 1L)

    expect_equal(declared::undeclare(imported$first, drop = TRUE), c(1L, first_codes))
    expect_equal(declared::undeclare(imported$second, drop = TRUE), c(1L, first_codes))
    expect_equal(attr(imported$first, "labels"), attr(exported$first, "labels"))
    expect_equal(
        attr(imported$second, "labels"),
        setNames(c(1L, first_codes), names(attr(exported$second, "labels")))
    )
})

test_that("SAS and XPORT retain values and trailing missing rows", {
    exported <- data.frame(answer = c(1, 2, rep(NA_real_, 12L)))
    sas_path <- tempfile(fileext = ".sas7bdat")
    xpt_path <- tempfile(fileext = ".xpt")
    on.exit(unlink(c(sas_path, xpt_path)), add = TRUE)

    write_sas(exported, sas_path)
    write_xpt(exported, xpt_path, version = 8L)

    expect_identical(as.vector(read_sas(sas_path)$answer), exported$answer)
    expect_identical(as.vector(read_xpt(xpt_path)$answer), exported$answer)
})
