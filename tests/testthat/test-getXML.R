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
