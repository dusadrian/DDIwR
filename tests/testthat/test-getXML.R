test_that("getXML() reads XML files with very large text nodes", {
  huge_text <- paste(rep("x", 11000000), collapse = "")
  huge_xml <- file.path(tempdir(), "huge.xml")

  cat(
    paste0(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
      "<root><node>",
      huge_text,
      "</node></root>"
    ),
    file = huge_xml
  )

  xml <- getXML(huge_xml)

  expect_s3_class(xml, "xml_document")
  expect_equal(xml2::xml_text(xml2::xml_find_first(xml, "/root/node")), huge_text)
})

test_that("codebook import uses the enumerated directory and file encoding", {
    directory <- tempfile()
    child <- file.path(directory, "child")
    dir.create(child, recursive = TRUE)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    parent_data <- data.frame(wrong = declared(1:3, label = "Parent"))
    child_data <- data.frame(right = declared(1:3, label = "Child"))
    saveRDS(parent_data, file.path(directory, "survey.rds"))
    saveRDS(child_data, file.path(child, "survey.rds"))

    expect_identical(attr(getCodebook(child)$dataDscr$var, "name"), "right")
    for (extension in c("sav", "dta")) {
        path <- file.path(child, paste0("survey.", extension))
        convert(child_data, path)
        expect_identical(attr(getCodebook(path, encoding = NULL)$dataDscr$var, "name"), "right")
    }
    sas <- file.path(child, "scores.sas7bdat")
    convert(child_data, sas, catalog = NULL)
    writeLines("right", file.path(child, "scores.csv"))
    books <- getCodebook(child, print_processing = FALSE)
    expect_length(books, 4L)
    expect_identical(attr(books$scores$dataDscr$var, "name"), "right")

})

test_that("a mismatched companion CSV leaves the codebook available to MetadataPublisher", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    xml <- file.path(directory, "survey.xml")
    data <- data.frame(score = declared(1:3, label = "Score"))
    convert(data, xml, embed = FALSE)
    writeLines(c("other", "2"), file.path(directory, "survey.csv"))
    attach(list(dataset = data), name = "MetadataPublisher")
    on.exit(detach("MetadataPublisher", character.only = TRUE), add = TRUE)

    book <- getCodebook(xml, ignore = "dataDscr", dataset = TRUE)
    expect_true(book$.extra$dataset_missing)
    expect_false(exists("dataset", envir = as.environment("MetadataPublisher"), inherits = FALSE))
    expect_identical(attr(book$dataDscr$var, "name"), "score")
})

test_that("extractData reads standard list codebooks with and without data notes", {
    data <- data.frame(x = 1:3)
    book <- makeElement("codeBook", children = list(makeElement("fileDscr")))
    expect_null(extractData(book))
    addChildren(makeDataNotes(data), to = book$fileDscr)
    expect_identical(extractData(book), data)
})

test_that("writeRlist round trips quoted metadata inside a caller output sink", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    records <- list("survey item" = list(label = 'Use "other"', labels = c("Not sure" = "N"),
        na_values = c("A", "B")))
    depth <- sink.number()
    sink(file.path(directory, "log"))
    on.exit({
        while (sink.number() > depth) {
            sink()
        }
    }, add = TRUE)

    writeRlist(records, dirpath = directory, filename = "metadata")
    expect_identical(sink.number(), depth + 1L)
    result <- new.env()
    sys.source(file.path(directory, "metadata.R"), envir = result)
    expect_identical(result$codeBook$variables$var, records)
})
