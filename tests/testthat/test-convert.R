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
stdyDscr <- attr(dfmddi, "stdyDscr")
attr(dfmddi, "stdyDscr") <- NULL

dfmddi[] <- lapply(dfmddi, function(x) {
  attr(x, "format.spss") <- NULL
  attr(x, "ID") <- NULL
  return(x)
})


test_that("convert() works from R to DDI and return", {
  expect_equal(dfm, dfmddi)
})
