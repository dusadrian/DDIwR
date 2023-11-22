
data(dfm)
cmdfm <- collectMetadata(dfm)

test_that("collectMetadata() works", {
  expect_true(is.list(cmdfm))

  expect_equal(length(cmdfm) - 1, ncol(dfm))

  expect_true(is.list(collectMetadata(
    cbind(
      dfm,
      x = factor(sample(letters[1:5], nrow(dfm), replace = TRUE)),
      y = labelled(
        sample(c(1:5, tagged_na("a")), nrow(dfm), replace = TRUE),
        labels = c(Good = 1, Bad = 5, DK = tagged_na("a"))
      )
    )
  )))

  expect_error(collectMetadata(data.frame(x = 1:5)))

  expect_error(collectMetadata(list(1:5)))
})
