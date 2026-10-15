test_that("XML conversion expands home paths and preserves embedded data", {
    directory <- tempfile("ddiwr-xml-", tmpdir = path.expand("~"))
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)

    data <- data.frame(
        A = declared(c(19, 45, 92), label = "QA1"),
        B = declared(c(1, 2, 1), labels = c(Desktop = 1, Mobile = 2),
            label = "status")
    )

    for (indent in c(2, 4)) {
        target <- file.path("~", basename(directory), paste0("data-", indent, ".xml"))
        expect_silent(convert(data, to = target, indent = indent))
        expect_true(file.exists(path.expand(target)))

        document <- xml2::read_xml(path.expand(target))
        ns <- c(d = "ddi:codebook:2_6")
        expect_identical(
            xml2::xml_name(xml2::xml_children(document)),
            c("docDscr", "stdyDscr", "fileDscr", "dataDscr")
        )
        expect_identical(xml2::xml_find_chr(document, "string(/d:codeBook/@xml:lang)", ns), "en")
        expect_identical(
            xml2::xml_text(xml2::xml_find_all(document, "/d:codeBook/d:dataDscr/d:var/d:labl", ns)),
            c("QA1", "status")
        )
        expect_identical(
            xml2::xml_text(xml2::xml_find_all(document, "//d:catgry/d:labl", ns)),
            c("Desktop", "Mobile")
        )

        restored <- convert(target)
        restored[] <- lapply(restored, function(x) {
            attr(x, "ID") <- NULL

            return(x)
        })
        expect_equal(restored, data)

        lines <- readLines(path.expand(target), warn = FALSE)
        expect_identical(lines[grepl("<dataDscr>", lines, fixed = TRUE)],
            paste0(strrep(" ", indent), "<dataDscr>"))
        expect_identical(lines[grepl("</dataDscr>", lines, fixed = TRUE)],
            paste0(strrep(" ", indent), "</dataDscr>"))
        expect_identical(tail(lines, 1), "</codeBook>")
    }
})

test_that("XML conversion uses supplied study metadata and writes companion data", {
    directory <- tempfile("ddiwr-xml-", tmpdir = path.expand("~"))
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    target <- file.path("~", basename(directory), "data.xml")
    data <- data.frame(A = c(19L, 45L, 92L))

    convert(data, to = target, embed = FALSE, titl = "Device survey",
        IDNo = "SURVEY1", agency = "Archive", abstract = "Survey abstract",
        xmlang = "ro")

    document <- xml2::read_xml(path.expand(target))
    ns <- c(d = "ddi:codebook:2_6")
    expect_identical(
        xml2::xml_text(xml2::xml_find_first(document, "/d:codeBook/d:stdyDscr/d:citation/d:titlStmt/d:titl", ns)),
        "Device survey"
    )
    expect_identical(xml2::xml_find_chr(document, "string(/d:codeBook/@xml:lang)", ns), "ro")
    expect_length(xml2::xml_find_all(document, "//d:notes[@ID='rawdata']", ns), 0L)
    expect_equal(read.csv(file.path(directory, "data.csv")), data)
})

test_that("direct codebook export supports home paths and connections", {
    directory <- tempfile("ddiwr-xml-", tmpdir = path.expand("~"))
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    target <- file.path("~", basename(directory), "book.xml")
    book <- makeElement("codeBook", children = list(
        makeElement("stdyDscr", fill = TRUE)
    ))

    expect_silent(exportCodebook(book, target))
    expect_true(file.exists(path.expand(target)))

    connection <- rawConnection(raw(), "w")
    on.exit(close(connection), add = TRUE)
    exportCodebook(book, connection)
    expect_s3_class(xml2::read_xml(rawConnectionValue(connection)), "xml_document")
})
