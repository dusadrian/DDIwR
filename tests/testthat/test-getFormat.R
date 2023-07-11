
# introduce a multibyte space character to test the code
dfm$charvar[2] <- paste0(dfm$charvar[2], "\u00a0")

test_that("getFormat() works for SPSS", {
  expect_equal(
    sapply(dfm, getFormat),
    c(
      Area = "F1.0",
      Gender = "F1.0",
      Opinion = "F1.0",
      Age = "F2.0",
      Children = "F1.0",
      narange = "F1.0",
      minusinf = "F1.0",
      plusinf = "F1.0",
      charvar = "A3",
      fweight = "F17.3"
    )
  )
})

test_that("getFormat() works for Stata", {
  expect_equal(
    sapply(dfm, getFormat, type = "Stata"),
    c(
      Area = "%1.0g",
      Gender = "%1.0g",
      Opinion = "%1.0g",
      Age = "%2.0g",
      Children = "%1.0g",
      narange = "%1.0g",
      minusinf = "%1.0g",
      plusinf = "%1.0g",
      charvar = "%3s",
      fweight = "%17.3g"
    )
  )
})


dfm2 <- cbind(
  dfm,
  x = NA,
  y = declared(
    sample(letters[1:5], nrow(dfm), replace = TRUE),
    labels = c(Good = "a", Bad = "3")
  )
)

sapply(dfm2, getFormat)
