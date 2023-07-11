current_os <- Sys.info()[["sysname"]]

test_that("getEnter() works", {
  expect_equal(getEnter("Win"), ifelse(current_os == "Windows", "\n", "\r\n"))
  expect_equal(getEnter("Mac"), ifelse(current_os == "Darwin", "\n", "\r"))
  expect_equal(getEnter("Linux"), "\n")
  expect_error(getEnter("Unknown"))
})
