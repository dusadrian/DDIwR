test_that("exportCodebook output validates across its writing paths", {
    directory <- tempfile("ddiwr-export-xsd-")
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(
        answer = declared(c(1, 2, 1, NA), label = "Device & access",
            labels = c(Desktop = 1, Mobile = 2)),
        weight = c(1, 2, 3, 1)
    )
    session <- metadataSweepCapture(data, "v1")
    on.exit(metadataSweepClose(session), add = TRUE)
    variable <- makeElement("var", attributes = c(name = "answer"),
        children = list(makeElement("labl", content = "Device & access",
            attributes = c(xmlang = "ro"))))
    book <- makeElement("codeBook", children = list(
        makeElement("stdyDscr", fill = TRUE, titl = "Survey <2026>"),
        makeElement("dataDscr", children = list(variable))
    ))
    streamed <- makeElement("codeBook", children = list(
        makeElement("stdyDscr", fill = TRUE), makeElement("fileDscr")
    ))

    for (mode in c("list", "variables", "session", "weighted")) {
        for (custom in c(FALSE, TRUE)) {
            for (connection in c(FALSE, TRUE)) {
                target <- file.path(directory,
                    paste(mode, custom, connection, "xml", sep = "."))
                arguments <- list(codeBook = book, to = target, monolang = FALSE)

                if (custom) {
                    arguments$indent <- 4
                    arguments$OS <- "windows"
                }

                if (mode == "variables") {
                    arguments$codeBook <- streamed
                    arguments$dataDscr_directly_in_XML <- TRUE
                    arguments$data <- data
                    arguments$variables <- collectRMetadata(data)
                    arguments$embed <- TRUE
                }
                else if (is.element(mode, c("session", "weighted"))) {
                    arguments$session <- session
                    arguments$revision <- "v1"
                    arguments$embed <- TRUE

                    if (mode == "weighted") {
                        arguments$wt <- "weight"
                    }
                }

                if (connection) {
                    arguments$to <- rawConnection(raw(), "w")
                }

                tryCatch({
                    do.call(exportCodebook, arguments)
                    output <- target

                    if (connection) {
                        output <- rawConnectionValue(arguments$to)
                    }

                    document <- expect_ddi_26_schema(output, basename(target))
                    expect_identical(xml2::xml_text(xml2::xml_find_first(document,
                        "//d:var/d:labl", c(d = "ddi:codebook:2_6"))),
                        "Device & access")

                    if (mode != "list") {
                        expect_equal(extractData(document), data, ignore_attr = TRUE)
                    }

                    if (custom) {
                        bytes <- output

                        if (!connection) {
                            bytes <- readBin(target, "raw", n = file.info(target)$size)
                        }

                        expect_true(grepl("\r\n", rawToChar(bytes), fixed = TRUE))
                    }
                }, finally = {
                    if (connection) {
                        close(arguments$to)
                    }
                })
            }
        }
    }
})

test_that("metadata defaults and the deprecated export alias produce valid DDI", {
    target <- tempfile(fileext = ".xml")
    on.exit(unlink(target), add = TRUE)
    book <- makeElement("codeBook")

    exportCodebook(book, file = target, titl = "Survey", IDNo = "S1")
    expect_ddi_26_schema(target)

    expect_message(exportDDI(book, to = target, titl = "Survey"), "deprecated")
    expect_ddi_26_schema(target)
})

test_that("session exports with companion CSV conform to DDI 2.6", {
    target <- tempfile(fileext = ".xml")
    csv <- sub("\\.xml$", ".csv", target)
    on.exit(unlink(c(target, csv)), add = TRUE)
    data <- data.frame(answer = c(1L, 2L, 1L))
    session <- metadataSweepCapture(data, "v1")
    on.exit(metadataSweepClose(session), add = TRUE)
    book <- makeElement("codeBook", children = list(
        makeElement("stdyDscr", fill = TRUE)
    ))

    exportCodebook(book, to = target, session = session, revision = "v1", csv = TRUE)
    expect_ddi_26_schema(target)
    expect_equal(read.csv(csv), data)
})

test_that("exportCodebook rejects incomplete objects before writing", {
    target <- tempfile(fileext = ".xml")
    on.exit(unlink(target), add = TRUE)
    expect_error(exportCodebook(makeElement("codeBook"), to = target),
        "stdyDscr")
    expect_false(file.exists(target))
})

test_that("updateCodebook preserves schema validity when replacing and inserting sections", {
    target <- tempfile(fileext = ".xml")
    on.exit(unlink(target), add = TRUE)
    data <- data.frame(answer = declared(c(1, 2, 1), label = "Device"))

    for (format in c("formatted", "compact", "prefixed")) {
        convert(data, to = target)

        if (format == "compact") {
            # Remove inter-element whitespace, preserving text inside notes.
            contents <- paste(readLines(target), collapse = "\n")
            writeLines(gsub(">\\s+<", "><", contents, perl = TRUE), target)
        }
        else if (format == "prefixed") {
            contents <- paste(readLines(target), collapse = "\n")
            contents <- gsub("<(/?)([[:alpha:]][[:alnum:]]*)", "<\\1ddi:\\2", contents)
            contents <- sub("<ddi:codeBook",
                '<ddi:codeBook xmlns:ddi="ddi:codebook:2_6"', contents, fixed = TRUE)
            writeLines(contents, target)
        }

        before <- expect_ddi_26_schema(target)
        ns <- c(d = "ddi:codebook:2_6")
        original_notes <- xml2::xml_text(xml2::xml_find_all(before,
            "/d:codeBook/d:fileDscr/d:notes", ns))
        original_attributes <- xml2::xml_attrs(xml2::xml_root(before))

        replacements <- makeElement("codeBook", children = list(
            makeElement("docDscr", fill = TRUE, titl = "New document"),
            makeElement("stdyDscr", fill = TRUE, titl = "New study"),
            makeElement("otherMat", attributes = c(level = "study"))
        ))
        updateCodebook(target, with = replacements)
        document <- expect_ddi_26_schema(target)
        expect_identical(xml2::xml_text(xml2::xml_find_all(document,
            "/d:codeBook/*/d:citation/d:titlStmt/d:titl", ns)),
            c("New document", "New study"))
        expect_length(xml2::xml_find_all(document, "/d:codeBook/d:otherMat", ns), 1L)
        expect_identical(xml2::xml_attrs(xml2::xml_root(document)), original_attributes)
        expect_identical(xml2::xml_text(xml2::xml_find_all(document,
            "/d:codeBook/d:fileDscr/d:notes", ns)), original_notes)
        expect_equal(extractData(document), data, ignore_attr = TRUE)
    }
})

test_that("updateCodebook replaces repeated empty sections as a group", {
    target <- tempfile(fileext = ".xml")
    on.exit(unlink(target), add = TRUE)
    book <- makeElement("codeBook", children = list(
        makeElement("stdyDscr", fill = TRUE),
        makeElement("otherMat", attributes = c(level = "study", ID = "old1")),
        makeElement("otherMat", attributes = c(level = "study", ID = "old2"))
    ))
    exportCodebook(book, target)
    expect_ddi_26_schema(target)
    replacements <- makeElement("codeBook", children = list(
        makeElement("otherMat", attributes = c(level = "study", ID = "new1")),
        makeElement("otherMat", attributes = c(level = "study", ID = "new2"))
    ))

    updateCodebook(target, with = replacements, indent = 4, enter = "\r\n")
    document <- expect_ddi_26_schema(target)
    expect_identical(xml2::xml_attr(xml2::xml_find_all(document,
        "/d:codeBook/d:otherMat", c(d = "ddi:codebook:2_6")), "ID"),
        c("new1", "new2"))
})

test_that("updateCodebook rejects invalid roots before writing", {
    target <- tempfile(fileext = ".xml")
    on.exit(unlink(target), add = TRUE)
    writeLines("<root/>", target)
    original <- readBin(target, "raw", n = file.info(target)$size)

    expect_error(updateCodebook(target, with = list()), "standard codeBook")
    expect_error(updateCodebook(target, with = makeElement("codeBook")), "root element")
    expect_identical(readBin(target, "raw", n = file.info(target)$size), original)
})
