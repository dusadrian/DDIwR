test_that("XML text serialization preserves UTF-8 bytes and document content", {
    document <- xml2::read_xml(paste0(
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<root xmlns="urn:test" xmlns:p="urn:other" xml:lang="ro">',
        '<!--comment--><?instruction keep?>',
        '<p:value key="&quot;&amp;">é漢 &amp; text</p:value>',
        '<empty/><mixed> before <b>bold</b> after </mixed>',
        '<literal><![CDATA[<keep> & text]]></literal></root>'
    ))
    path <- tempfile(fileext = ".xml")
    on.exit(unlink(path), add = TRUE)
    xml2::write_xml(document, path)
    expected <- readBin(path, "raw", n = file.info(path)$size)

    output <- .xmlDocumentText(document)

    expect_identical(charToRaw(output), expected)
    expect_identical(Encoding(output), "UTF-8")
    expect_identical(xml2::as_list(xml2::read_xml(output)), xml2::as_list(document))
})

test_that("XML serialization closes its connection when writing fails", {
    document <- xml2::read_xml("<root/>")
    connections <- showConnections(all = TRUE)
    testthat::local_mocked_bindings(write_xml = function(...) {
        stop("serialization failed")
    }, .package = "xml2")

    expect_error(.xmlDocumentText(document), "serialization failed")
    expect_identical(showConnections(all = TRUE), connections)
})

test_that("XML node hashes retain their existing serialized representation", {
    document <- xml2::read_xml(paste0(
        '<root xmlns="urn:test"><x>é漢 &amp; text</x><empty/>',
        '<mixed> before <b>bold</b> after </mixed></root>'
    ))
    nodes <- xml2::xml_children(document)
    # Captured from xml2::xml_serialize() before the workaround. Serialize here
    # because the raw serialization header includes the running R version.
    serialized_text <- paste0('<?xml version="1.0" encoding="UTF-8"?>\n', c(
        '<x xmlns="urn:test">é漢 &amp; text</x>\n',
        '<empty xmlns="urn:test"/>\n',
        '<mixed xmlns="urn:test"> before <b>bold</b> after </mixed>\n'
    ))
    expected <- unname(vapply(serialized_text, function(text) {
        value <- structure(text, class = "xml_serialized_node")

        return(digest::digest(serialize(value, NULL)))
    }, character(1)))

    expect_identical(getHashes(nodes), expected)
    expect_identical(getHashes(list(value = nodes[[1]])), c(value = expected[1]))
    expect_identical(getHashes(nodes[FALSE]), list())
})
