test_that("the pinned DDI 2.6 schema bundle is complete and local", {
    directory <- system.file("schemas", "ddi-codebook-2.6",
        package = "DDIwR", mustWork = TRUE)
    manifest <- read.table(file.path(directory, "SHA256SUMS"),
        col.names = c("sha256", "path"), colClasses = "character")
    upstream <- read.table(file.path(directory, "UPSTREAM-SHA256SUMS"),
        col.names = c("sha256", "path"), colClasses = "character")
    files <- sort(list.files(directory, pattern = "\\.(xsd|ent)$",
        recursive = TRUE))

    expect_identical(files, sort(manifest$path))
    expect_identical(manifest$path, upstream$path)
    expect_length(files, 29L)

    for (i in seq_len(nrow(manifest))) {
        path <- file.path(directory, manifest$path[i])
        expect_true(file.exists(path), info = manifest$path[i])
        expect_identical(digest::digest(file = path, algo = "sha256"),
            manifest$sha256[i], info = manifest$path[i])

        if (is.element(manifest$path[i], c("dc.xsd", "dcterms.xsd"))) {
            contents <- readBin(path, "raw", n = file.info(path)$size)
            original <- sub('schemaLocation="xml.xsd"',
                'schemaLocation="http://www.w3.org/2001/03/xml.xsd"',
                rawToChar(contents), fixed = TRUE)
            expect_identical(digest::digest(charToRaw(original),
                algo = "sha256", serialize = FALSE), upstream$sha256[i])
        }
        else {
            expect_identical(manifest$sha256[i], upstream$sha256[i])
        }

        if (grepl("\\.xsd$", path)) {
            schema <- xml2::read_xml(path, options = "NONET")
            locations <- xml2::xml_attr(xml2::xml_find_all(schema,
                "/xs:schema/xs:import | /xs:schema/xs:include | /xs:schema/xs:redefine",
                c(xs = "http://www.w3.org/2001/XMLSchema")), "schemaLocation")
            locations <- locations[!is.na(locations)]

            expect_false(any(grepl("^([A-Za-z][A-Za-z0-9+.-]*:|/)", locations)),
                info = manifest$path[i])
            expect_true(all(file.exists(file.path(dirname(path), locations))),
                info = manifest$path[i])
        }
    }
})

test_that("actual convert output conforms to the pinned DDI 2.6 XSD", {
    directory <- tempfile("ddiwr-xsd-")
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    schema <- xml2::read_xml(system.file("schemas", "ddi-codebook-2.6", "codebook.xsd",
        package = "DDIwR", mustWork = TRUE),
        options = "NONET")
    data <- data.frame(
        A = declared(c(19, 45, 92), label = "QA1"),
        B = declared(c(1, 2, 1), labels = c(Desktop = 1, Mobile = 2),
            label = "status")
    )

    for (embed in c(TRUE, FALSE)) {
        for (indent in c(2, 4)) {
            target <- file.path(directory, paste0("data-", embed, "-", indent, ".xml"))
            convert(data, to = target, embed = embed, indent = indent)

            document <- xml2::read_xml(target, options = "NONET")
            valid <- xml2::xml_validate(document, schema)
            diagnostics <- attr(valid, "errors")

            expect_true(isTRUE(valid), info = paste(basename(target),
                paste(diagnostics, collapse = "\n")))
            expect_length(diagnostics, 0L)
        }
    }
})

test_that("DDI 2.6 XSD rejects the original missing study description", {
    target <- tempfile(fileext = ".xml")
    on.exit(unlink(target), add = TRUE)
    schema <- xml2::read_xml(system.file("schemas", "ddi-codebook-2.6", "codebook.xsd",
        package = "DDIwR", mustWork = TRUE),
        options = "NONET")
    data <- data.frame(A = declared(c(19, 45, 92), label = "QA1"))
    convert(data, to = target)
    document <- xml2::read_xml(target, options = "NONET")
    study <- xml2::xml_find_all(document, "/d:codeBook/d:stdyDscr",
        c(d = "ddi:codebook:2_6"))
    expect_length(study, 1L)

    # Reproduce the original export defect without changing the other content.
    xml2::xml_remove(study)
    xml2::write_xml(document, target)
    invalid <- xml2::read_xml(target, options = "NONET")
    valid <- xml2::xml_validate(invalid, schema)

    expect_false(isTRUE(valid))
    expect_match(paste(attr(valid, "errors"), collapse = "\n"),
        "fileDscr.*not expected.*stdyDscr")
})
