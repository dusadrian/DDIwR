test_that("getEnter() works", {
  expect_equal(getEnter("Win"), "\r\n")
  expect_equal(getEnter("Mac"), "\n")
  expect_equal(getEnter("Linux"), "\n")
  expect_error(getEnter("Unknown"))
})
