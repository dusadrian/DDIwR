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
