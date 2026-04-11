# The following lines produce the R dataset "dfm" needed for all tests



dfm <- makedfm()

tmp <- tempdir()
convert(dfm, to = file.path(tmp, "dfm.sav"))
dfmspss <- convert(file.path(tmp, "dfm.sav"))

dfmspss[] <- lapply(dfmspss, function(x) {
  attr(x, "format.spss") <- NULL
  return(x)
})

test_that("convert() works from R to SPSS and return", {
  expect_equal(dfm$Area, dfmspss$Area)
  expect_equal(dfm$Gender, dfmspss$Gender)
  expect_equal(dfm$Opinion, dfmspss$Opinion)
  expect_equal(dfm$Age, dfmspss$Age)
  expect_equal(dfm$Children, dfmspss$Children)
})


convert(dfm, to = file.path(tmp, "dfm.dta"), chartonum = TRUE)
dfmstata <- convert(file.path(tmp, "dfm.dta"))
stataformat <- lapply(dfmstata, function(x) attr(x, "format.stata"))
dfmstata[] <- lapply(dfmstata, function(x) {
  attr(x, "format.stata") <- NULL
  return(x)
})

test_that("convert() works from R to Stata and return", {
  expect_equal(dfm$Area, dfmstata$Area)
  expect_equal(dfm$Gender, dfmstata$Gender)
  expect_equal(dfm$Opinion, dfmstata$Opinion)
  expect_equal(dfm$Age, dfmstata$Age)
  expect_equal(dfm$Children, dfmstata$Children)
})

test_that("Stata does not have an na_range concept", {
  expect_null(missing_range(dfmstata$narange))
  expect_null(missing_range(dfmstata$minusinf))
  expect_null(missing_range(dfmstata$plusinf))
})

test_that("The character variable was converted to numeric for Stata", {
  expect_true(is.numeric(dfmstata$charvar))
})

test_that("The labels of character variable are preserved in Stata", {
  expect_equal(names(labels(dfm$charvar)), names(labels(dfmstata$charvar)))
})


convert(dfm, to = file.path(tmp, "dfm.xml"))
dfmddi <- convert(file.path(tmp, "dfm.xml"))

convert(dfm, to = file.path(tmp, "dfm.rds"))
dfmrds <- readRDS(file.path(tmp, "dfm.rds"))

dfmddi[] <- lapply(dfmddi, function(x) {
  attr(x, "format.spss") <- NULL
  attr(x, "ID") <- NULL
  return(x)
})


test_that("convert() works from R to DDI and return", {
  expect_equal(dfm, dfmddi)
})

test_that("convert() works from R to RDS and return", {
  expect_equal(dfm, dfmrds)
})

test_that("Stata tagged missings are normalized to numeric declared missings on import", {
  read_dta_internal <- get("read_dta", envir = asNamespace("DDIwR"))
  tagged_file <- tempfile(fileext = ".dta")
  on.exit(unlink(tagged_file), add = TRUE)

  tagged_df <- data.frame(
    tagged = haven::labelled(
      c(1, haven::tagged_na("a"), haven::tagged_na("b"), 2),
      c(Yes = 1, No = 2, DK = haven::tagged_na("a"), NA2 = haven::tagged_na("b"))
    )
  )

  haven::write_dta(tagged_df, tagged_file)
  imported <- read_dta_internal(tagged_file)

  expect_equal(attr(imported$tagged, "na_values", exact = TRUE), c(-91, -92))
  expect_equal(unname(attr(imported$tagged, "na_index", exact = TRUE)), c(2L, 3L))
  expect_equal(names(attr(imported$tagged, "na_index", exact = TRUE)), c("-91", "-92"))
  expect_equal(unname(attr(imported$tagged, "labels", exact = TRUE)), c(1, 2, -91, -92))
})

test_that("Stata export dictionary is applied natively", {
  exported <- data.frame(
    A = declared(
      c(1, -91, -92),
      labels = c(Yes = 1, DK = -91, NR = -92),
      na_values = c(-91, -92)
    ),
    B = declared(
      c(1, -92, -93),
      labels = c(Yes = 1, DK = -92, REF = -93),
      na_values = c(-92, -93)
    )
  )
  dictionary <- recodeMissings(exported, to = "Stata", return_dictionary = TRUE)
  dta_file <- tempfile(fileext = ".dta")
  on.exit(unlink(dta_file), add = TRUE)

  write_dta(exported, dta_file, dictionary = dictionary)
  imported <- read_dta(dta_file)

  expect_equal(names(attr(imported$A, "na_index", exact = TRUE)), c("-91", "-92"))
  expect_equal(names(attr(imported$B, "na_index", exact = TRUE)), c("-92", "-93"))
  expect_equal(unname(attr(imported$A, "labels", exact = TRUE)), c(1, -91, -92))
  expect_equal(unname(attr(imported$B, "labels", exact = TRUE)), c(1, -92, -93))
})

test_that("convert() can auto-build a harmonized dictionary for Stata export", {
  exported <- data.frame(
    A = declared(
      c(1, -91, -92),
      labels = c(Yes = 1, DK = -91, NR = -92),
      na_values = c(-91, -92)
    ),
    B = declared(
      c(1, -92, -93),
      labels = c(Yes = 1, DK = -92, REF = -93),
      na_values = c(-92, -93)
    )
  )
  dta_file <- tempfile(fileext = ".dta")
  on.exit(unlink(dta_file), add = TRUE)

  convert(exported, to = dta_file, harmonize = TRUE)
  imported <- read_dta(dta_file)

  expect_equal(names(attr(imported$A, "na_index", exact = TRUE)), c("-91", "-92"))
  expect_equal(names(attr(imported$B, "na_index", exact = TRUE)), c("-92", "-93"))
  expect_equal(unname(attr(imported$A, "labels", exact = TRUE)), c(1, -91, -92))
  expect_equal(unname(attr(imported$B, "labels", exact = TRUE)), c(1, -92, -93))
})

test_that("read_dta() can read in batches with n_max and skip", {
  dta_file <- tempfile(fileext = ".dta")
  on.exit(unlink(dta_file), add = TRUE)

  exported <- data.frame(
    id = 1:10,
    score = seq(10, 100, by = 10)
  )

  haven::write_dta(exported, dta_file)

  imported <- read_dta(dta_file, n_max = 3, skip = 4)

  expect_equal(as.numeric(imported$id), 5:7)
  expect_equal(as.numeric(imported$score), c(50, 60, 70))
})

test_that("read_dta() preserves metadata when n_max is zero", {
  dta_file <- tempfile(fileext = ".dta")
  on.exit(unlink(dta_file), add = TRUE)

  exported <- data.frame(
    labelled = haven::labelled(c(1, 2, 1), c(Yes = 1, No = 2))
  )

  haven::write_dta(exported, dta_file)

  imported <- read_dta(dta_file, n_max = 0)

  expect_equal(nrow(imported), 0)
  expect_equal(names(imported), "labelled")
  expect_equal(unname(attr(imported$labelled, "labels", exact = TRUE)), c(1, 2))
  expect_equal(names(attr(imported$labelled, "labels", exact = TRUE)), c("Yes", "No"))
})

test_that("convert() forwards n_max and skip to foreign import readers", {
  dta_file <- tempfile(fileext = ".dta")
  on.exit(unlink(dta_file), add = TRUE)

  exported <- data.frame(
    id = 1:10,
    score = seq(10, 100, by = 10)
  )

  haven::write_dta(exported, dta_file)

  imported <- convert(dta_file, n_max = 3, skip = 4, recode = FALSE)

  expect_equal(as.numeric(imported$id), 5:7)
  expect_equal(as.numeric(imported$score), c(50, 60, 70))
})

test_that("convert() falls back to per-variable recoding when harmonized Stata export overflows", {
  manymissings <- seq(-950, -91)
  exported <- data.frame(
    A = declared(
      c(1:5, manymissings),
      labels = c(Good = 1, Bad = 5, setNames(manymissings, paste0("NR", manymissings))),
      na_range = c(-950, -91)
    )
  )
  dta_file <- tempfile(fileext = ".dta")
  on.exit(unlink(dta_file), add = TRUE)

  expect_message(
    convert(exported, to = dta_file, harmonize = TRUE),
    "Too many overall missing values for harmonized STATA recoding"
  )
})
