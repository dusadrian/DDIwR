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
