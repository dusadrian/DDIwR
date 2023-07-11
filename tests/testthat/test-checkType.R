test_that("checkType() works", {
  expect_equal(checkType(letters), "char")
  
  expect_equal(
    checkType(
      c(1:5, -91),
      labels = c(Good = 1, Bad = 5, NR = -91),
      na_values = -91
    ),
    "cat"
  )

  expect_equal(
    checkType(
      c(1:7, -91),
      labels = c(Good = 1, Bad = 5, NR = -91),
      na_values = -91
    ),
    "catnum"
  )

  expect_equal(
    checkType(
      c(1:7, -91),
      labels = c(NR = -91),
      na_values = -91
    ),
    "numcat"
  )

  expect_equal(
    checkType(
      c(1:100, -91),
      labels = c(NR = -91),
      na_values = -91
    ),
    "num"
  )

  expect_equal(
    checkType(
      c(letters, -91),
      labels = c(NR = -91),
      na_values = -91
    ),
    "char"
  )
  
  expect_equal(
    checkType(
      c(1:5, -91),
      labels = c(VeryGood = 1, Good = 2, Middle = 3, Bad = 4, VeryBad = 5, NR = -91),
      na_values = -91
    ),
    "cat"
  )
  
  expect_equal(
    checkType(
      c(letters[1:5], -91),
      labels = c(VeryGood = "a", Good = "b", Middle = "c", Bad = "d", VeryBad = "e", NR = -91),
      na_values = -91
    ),
    "catchar"
  )

  expect_equal(checkType(1:10), "numcat")

  expect_equal(checkType(1:100), "num")

})
