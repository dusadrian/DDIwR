x <- data.frame(
    A = declared(
        c(1:5, -92),
        labels = c(Good = 1, Bad = 5, NR = -92),
        na_values = -92
    ),
    B = declared(
        c(1:5, -92),
        labels = c(Good = 1, Bad = 5, NR = -92),
        na_range = c(-99, -91)
    ),
    C = declared(
        c(1, -91, 3:5, -92),
        labels = c(DK = -91, NR = -92),
        na_values = c(-91, -92)
    ),
    D = declared(
        c(18, 52, 23, 47, 38, -91),
        label = "Respondent's age",
        labels = c(NR = -91),
        na_values = -91
    ),
    E = letters[1:6]
)

xdfm <- getMetadata(x)

tmp <- tempdir()
convert(x, to = file.path(tmp, "x.sav"))
xsav <- getMetadata(file.path(tmp, "x.sav"), save = TRUE, indent = 2)

convert(x, to = file.path(tmp, "x.xml"))
xxml <- getMetadata(file.path(tmp, "x.xml"))
rxml <- readLines(file.path(tmp, "x.xml"))

convert(x, to = file.path(tmp, "x.dta"))
xdta <- getMetadata(file.path(tmp, "x.dta"))

convert(x, to = file.path(tmp, "x.rds"))
xrds <- getMetadata(file.path(tmp, "x.rds"))


test_that("getMetadata() works", {
  expect_true(is.list(xdfm$dataDscr))
  expect_true(is.list(xsav$dataDscr))
  expect_true(is.list(xxml$dataDscr))
  expect_true(is.list(xdta$dataDscr))
  expect_true(is.list(xrds$dataDscr))

  # no metadata
  expect_error(getMetadata(data.frame(A = 1:5)))

  # still no metadata, but NULL instead of error
  expect_equal(getMetadata(data.frame(A = 1:5), error_null = TRUE), NULL)

  expect_true(
    is.data.frame(getMetadata(x, embed = TRUE)$fileDscr$datafile)
  )

  # file paths should be characters of length 1
  expect_error(getMetadata(c("file1", "file2")))
  expect_error(getMetadata(1))
})

test_that("an error is triggered if the XML file is not valid", {
    writeLines(rxml[-7], file.path(tmp, "x.xml"))
    expect_error(getMetadata(file.path(tmp, "x.xml")))
    expect_error(getMetadata(tmp))
})

test_that("an error is triggered if the XML file does not contain a namespace", {
    rxml[6] <- ">"
    writeLines(rxml, file.path(tmp, "x.xml"))
    expect_error(getMetadata(file.path(tmp, "x.xml")))
})

test_that("an error is triggered if the XML file does not contain any metadata", {
    rxml[6] <- "xmlns=\"ddi:codebook:2_5\">"
    writeLines(rxml[-c(55:128)], file.path(tmp, "x.xml"))
    expect_error(getMetadata(file.path(tmp, "x.xml")))
})

test_that("range of values changes function of metadata information", {
    rxml[75] <- "<range UNITS=\"INT\" min=\"-99\"/>"
    writeLines(rxml, file.path(tmp, "x.xml"))
    xdfm75 <- getMetadata(file.path(tmp, "x.xml"))
    expect_equal(declared::missing_range(xdfm75$fileDscr$datafile$B)[2], Inf)
    
    rxml[75] <- "<range UNITS=\"INT\" max=\"-91\"/>"
    writeLines(rxml, file.path(tmp, "x.xml"))
    xdfm75 <- getMetadata(file.path(tmp, "x.xml"))
    expect_equal(declared::missing_range(xdfm75$fileDscr$datafile$B)[1], -Inf)

    rxml[75] <- "<range UNITS=\"INT\" min=\"-99\" max=\"-91\"/>"
})

test_that("measurement is read, if existing", {
    xdfm$dataDscr$A$measurement <- "nominal"
    xdfm$dataDscr$B$measurement <- "ordinal"
    xdfm$dataDscr$C$measurement <- "unobserved"
    xdfm$dataDscr$D$measurement <- "ratio"
    xdfm$dataDscr$E$txt <- "This is just a normal character variable"
    exportDDI(xdfm, file = file.path(tmp, "x.xml"))
    xxml <- getMetadata(file.path(tmp, "x.xml"))
    expect_true(is.list(xxml))
})

test_that("an error is triggered if the embedded CSV file is not valid", {
    rxml[44] <- "\"A\"	\"B\"	\"C\"	\"D\"	\"E\""
    writeLines(rxml, file.path(tmp, "x.xml"))
    expect_error(getMetadata(file.path(tmp, "x.xml")))
})
