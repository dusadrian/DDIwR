
tmp <- tempdir()

codebook <- getMetadata(dfm)

codebook$fileDscr$datafile <- dfm
codebook$fileDscr$fileName <- "test"
codebook$dataDscr$Area$txt <- "test"

exportDDI(
  codebook,
  file = file.path(tmp, "dfm.xml")
)

dfm2 <- convert(file.path(tmp, "dfm.xml"))
measurement(dfm2$narange) <- "ordinal"
convert(dfm2, to = file.path(tmp, "dfm2.xml"))


exportDDI(
  codebook,
  file = file.path(tmp, "dfm.xml"),
  xmlns = "test",
  xmlang = "ro",
  embed = FALSE,
  monolang = TRUE,
  IDNo = 1234
)

test_that("exportDDI() errors on invalid parameters", {
  expect_error(exportDDI(codebook, IDNo = 1:2))
  
  codebook$fileDscr$datafile <- list(1:5)
  expect_error(exportDDI(
    codebook,
    file = file.path(tmp, "dfm.xml")
  ))

  codebook$fileDscr$datafile <- data.frame(x = 1:5)
  expect_error(exportDDI(
    codebook,
    file = file.path(tmp, "dfm.xml")
  ))
})
