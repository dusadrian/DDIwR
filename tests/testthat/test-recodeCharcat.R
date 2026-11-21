dfm <- makedfm()
cv1 <- dfm$charvar
declared::missing_values(cv1) <- NULL
cv2 <- declared::undeclare(dfm$charvar)
class(cv2) <- "character"

test_that("recodeCharcat() works", {
  expect_equal(recodeCharcat(1:5), 1:5)
  expect_equal(recodeCharcat(letters[1:5]), letters[1:5])
  expect_true(is.numeric(recodeCharcat(dfm$charvar)))
  expect_true(is.numeric(recodeCharcat(cv1)))
  expect_true(is.numeric(recodeCharcat(cv2)))
})

test_that("alternate numeric spellings retain distinct labels and missing codes", {
    source <- declared(c("01", "1", "X", "09", "9"),
        labels = c(Leading = "01", Plain = "1", Other = "X", Missing = "09", Nine = "9"),
        na_values = "09")
    recoded <- recodeCharcat(source)
    codes <- attr(recoded, "labels")

    expect_equal(unname(codes[c("Plain", "Nine")]), c(1, 9))
    expect_length(unique(codes), 5L)
    expect_equal(attr(recoded, "na_values"), unname(codes["Missing"]))
    expect_equal(undeclare(recoded, drop = TRUE),
        unname(codes[c("Leading", "Plain", "Other", "Missing", "Nine")]))

    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)
    convert(data.frame(code = source), path, chartonum = TRUE)
    imported <- convert(path, recode = FALSE)$code
    expect_setequal(names(attr(imported, "labels")), names(codes))
    expect_length(unique(undeclare(imported, drop = TRUE)), 5L)
})
