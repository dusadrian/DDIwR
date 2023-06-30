# The following lines produce the R dataset needed for all tests,
# available in sysdata.rda

# n <- 123
# set.seed(n)
# dfm <- data.frame(
#   Area = declared(
#     sample(1:2, n, replace = TRUE, prob = c(0.45, 0.55)),
#     label = "Area of residence",
#     labels = c("Rural" = 1, "Urban" = 2)
#   ),
#   Gender = declared(
#     sample(1:2, n, replace = TRUE, prob = c(0.55, 0.45)),
#     label = "Respondent's gender",
#     labels = c("Males" = 1, "Females" = 2)
#   ),
#   Opinion = declared(
#     sample(c(1:5, NA, -91), n, replace = TRUE),
#     label = "Opinion about eating sugar",
#     labels = c(
#       "Very bad" = 1, "Bad" = 2, "Neither" = 3,
#       "Good" = 4, "Very good" = 5, "Don't know" = -91
#     ),
#     na_values = -91
#   ),
#   Age = sample(18:90, n, replace = TRUE),
#   Children = sample(0:5, n, replace = TRUE),
#   narange = declared(
#       sample(c(1:5, -91), n, replace = TRUE),
#       labels = c(Good = 1, Bad = 5, DK = -91),
#       na_range = c(-95, -91)
#   ),
#   minusinf = declared(
#     sample(c(1:5, -91), n, replace = TRUE),
#     labels = c(Good = 1, Bad = 5, DK = -91),
#     na_range = c(-Inf, -91)
#   ),
#   plusinf = declared(
#     sample(c(1:5, 91), n, replace = TRUE),
#     labels = c(Good = 1, Bad = 5, DK = 91),
#     na_range = c(91, Inf)
#   ),
#   charvar = declared(
#     sample(c(letters[1:5], -91), n, replace = TRUE),
#     labels = c(Good = "a", Bad = "e", DK = -91),
#     na_values = -91
#   )
# )

# op <- proportions(with(dfm, table(Gender, Area)))

# # Theoretical / population proportions:
# # 53% Rural, and 50% Females
# weights <- rep(c(0.53, 0.47), each = 2) * rep(0.5, 4) / op

# dfm$fweight <- weights[
#   match(10 * dfm$Area + dfm$Gender, c(11, 12, 21, 22))
# ]



tmp <- tempdir()
convert(dfm, to = file.path(tmp, "dfm.sav"))
dfmspss <- convert(file.path(tmp, "dfm.sav"))

dfmspss[] <- lapply(dfmspss, function(x) {
  attr(x, "format.spss") <- NULL
  return(x)
})

# test_that("convert() works from R to SPSS and return", {
#   expect_equal(dfm, dfmspss)
# })


convert(dfm, to = file.path(tmp, "dfm.dta"), chartonum = TRUE)
dfmstata <- convert(file.path(tmp, "dfm.dta"))
stataformat <- lapply(dfmstata, function(x) attr(x, "format.stata"))
dfmstata[] <- lapply(dfmstata, function(x) {
  attr(x, "format.stata") <- NULL
  return(x)
})

test_that("convert() works from R to Stata and return", {
  # expect_equal(dfm$Area, dfmstata$Area)
  # expect_equal(dfm$Gender, dfmstata$Gender)
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
  return(x)
})


test_that("convert() works from R to DDI and return", {
  expect_equal(dfm, dfmddi)
})
