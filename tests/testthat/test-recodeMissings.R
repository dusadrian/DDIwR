x <- data.frame(
  A = declared(
    c(1:5, -92),
    labels = c(Good = 1, Bad = 5, NR = -92),
    na_values = -92
  ),
  C = declared(
    c(1, -91, 3:5, -92),
    labels = c(DK = -91, NR = -92),
    na_values = c(-91, -92)
  ),
  D = declared(
    c(1, -91, 3:5, -92),
    labels = c(DK = -91, NR = -92),
    na_range = c(-99, -91)
  ),
  E = declared(
     c(letters[1:5], -92),
    labels = c(Good = "a", Bad = "e", NR = -92),
    na_values = -92
  ),
  F = declared( # range with a value to the right side not in the dictionary
    c(1, -95, 3:5, -94),
    labels = c(DK = -91, NR = -92),
    na_range = c(-95, -93)
  ),
  G = declared( # range with both values not in the dictionary
    c(1, -91, 3:5, -92),
    labels = c(DK = -91, NR = -92),
    na_range = c(-99, -90)
  )
)

dfm <- makedfm()

x2 <- x
# erase information about declared missing values
declared::missing_values(x2$A) <- NULL
declared::missing_values(x2$C) <- NULL
declared::missing_range(x2$D) <- NULL
declared::missing_range(x2$E) <- NULL
declared::missing_range(x2$F) <- NULL
declared::missing_range(x2$G) <- NULL

xspss <- recodeMissings(x, to = "SPSS")
xstata <- recodeMissings(x, to = "Stata")

xsas <- recodeMissings(x2, to = "SAS") # actually the same thing as "Stata"
attr(xsas, "dictionary") <- NULL

xchar <- data.frame(
  A = declared(
    letters[1:5],
    labels = c(Good = "a", Bad = "e")
  )
)

xexport <- data.frame(
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

manymissings <- seq(-950, -91)
xmany <- data.frame(
  A = declared(
    c(1:5, manymissings),
    labels = c(Good = 1, Bad = 5, setNames(manymissings, paste0("NR", manymissings))),
    na_range = c(-950, -91)
  )
)

test_that("recodeMissings() works", {
  expect_error(recodeMissings(1:5))
  expect_error(recodeMissings(data.frame(A = 1:5)))
  expect_error(recodeMissings(subset(xmany, select = "A"), to = "Stata"))
  expect_error(recodeMissings(xmany, to = "Stata"))
  expect_equal(recodeMissings(xchar, to = "SAS"), xchar)
  expect_equal(recodeMissings(xchar, to = "SPSS"), xchar)
  expect_equal(xsas, x2)
})

test_that("variables get declared NAs if they are mixed SPSS / Stata types", {
  expect_false(admisc::anyTagged(xspss))
})

# test_that("variables get tagged NAs if they are mixed SPSS / Stata types", {
#   # except variable E which is character
#   expect_true(all(sapply(xstata[, -5], admisc::anyTagged)))
# })

# test_that("variables get tagged NAs when they are all SPSS-type", {
#   onlyspss <- recodeMissings(x[, -c(2, 5)], to = "Stata")
#   expect_true(all(sapply(onlyspss, admisc::anyTagged)))
# })

test_that("a dictionary is produced from the missing codes in the data", {
  dictionary <- recodeMissings(x, to = "Stata", return_dictionary = TRUE)
  expect_true(is.data.frame(dictionary))
})

test_that("foreign export recoding is per variable by default", {
  export_fun <- get("prepare_foreign_export_missings", envir = asNamespace("DDIwR"))
  xvar <- export_fun(xexport, to = "Stata")

  expect_equal(names(attr(xvar$A, "na_index", exact = TRUE)), c("a", "b"))
  expect_equal(names(attr(xvar$B, "na_index", exact = TRUE)), c("a", "b"))
  expect_equal(unname(attr(xvar$A, "labels", exact = TRUE)), c("1", "a", "b"))
  expect_equal(unname(attr(xvar$B, "labels", exact = TRUE)), c("1", "a", "b"))
})

test_that("foreign export recoding is harmonized when a dictionary is supplied", {
  export_fun <- get("prepare_foreign_export_missings", envir = asNamespace("DDIwR"))
  dictionary <- recodeMissings(xexport, to = "Stata", return_dictionary = TRUE)
  xharm <- export_fun(xexport, to = "Stata", dictionary = dictionary)

  expect_equal(names(attr(xharm$A, "na_index", exact = TRUE)), c("a", "b"))
  expect_equal(names(attr(xharm$B, "na_index", exact = TRUE)), c("b", "c"))
  expect_equal(unname(attr(xharm$A, "labels", exact = TRUE)), c("1", "a", "b"))
  expect_equal(unname(attr(xharm$B, "labels", exact = TRUE)), c("1", "b", "c"))
})

test_that("missing values in the data, not present in a dictionary, trigget error", {
  dictionary <- data.frame(label = "DK", code = 999, new = -91)
  expect_error(recodeMissings(x, dictionary = dictionary))
})

test_that("range of values with infinite bounds are recalculated", {
  # see test-convert.R for the structure of the object dfm
  rdfm <- recodeMissings(dfm)
  expect_equal(declared::missing_range(dfm$plusinf), c(91, Inf))
  expect_equal(declared::missing_range(rdfm$plusinf), c(-Inf, -92))
  rdfm <- recodeMissings(dfm, start = 999)
  expect_equal(declared::missing_range(rdfm$plusinf), c(1000, Inf))
})

test_that("recoding works for declared data", {
  hxspss <- recodeMissings(x, to_declared = FALSE)
  expect_equal(dim(xspss), dim(hxspss))
})

test_that("missing ranges are reconstructed if not present in the dictionary", {
  expect_equal(declared::missing_range(xspss$F), c(-94, -92))
  expect_equal(
    diff(declared::missing_range(x$F)),
    diff(declared::missing_range(xspss$F))
  )
  expect_equal(declared::missing_range(xspss$G), c(-94, -91))
})
