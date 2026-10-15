test_that("invalid exports preserve destinations across all public writing paths", {
    directory <- tempfile("ddiwr-validation-")
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(answer = c(1L, 2L, 1L))
    session <- metadataSweepCapture(data, 1L)
    on.exit(metadataSweepClose(session), add = TRUE)
    original <- charToRaw("Existing contents\n")

    for (mode in c("list", "variables", "session", "weighted", "convert", "alias")) {
        for (custom in c(FALSE, TRUE)) {
            target <- file.path(directory, paste0(mode, "-", custom, ".xml"))
            csv <- sub("\\.xml$", ".csv", target)
            writeBin(original, target)
            writeBin(original, csv)
            writer <- exportCodebook
            arguments <- list(codeBook = makeElement("codeBook"), to = target)

            if (custom) {
                arguments$indent <- 4
                arguments$OS <- "windows"
            }

            if (mode == "variables") {
                arguments$dataDscr_directly_in_XML <- TRUE
                arguments$data <- data
                arguments$variables <- collectRMetadata(data)
                arguments$embed <- FALSE
            }
            else if (is.element(mode, c("session", "weighted"))) {
                arguments$session <- session
                arguments$revision <- 1L
                arguments$csv <- TRUE

                if (mode == "weighted") {
                    arguments$wt <- "answer"
                }
            }
            else if (mode == "convert") {
                writer <- convert
                arguments$codeBook <- NULL
                arguments$from <- data
                arguments$embed <- FALSE
                arguments$varIDs <- "1invalid"
            }
            else if (mode == "alias") {
                writer <- exportDDI
            }

            expect_error(suppressMessages(do.call(writer, arguments)),
                "DDI Codebook 2.6 XML validation failed", info = mode)
            expect_identical(readBin(target, "raw", n = file.info(target)$size), original)
            expect_identical(readBin(csv, "raw", n = file.info(csv)$size), original)
        }
    }
})

test_that("validation happens before writing to a connection or console", {
    data <- data.frame(answer = 1:3)
    original <- charToRaw("Existing contents\n")

    for (streamed in c(FALSE, TRUE)) {
        for (custom in c(FALSE, TRUE)) {
            connection <- rawConnection(raw(), "w")
            writeBin(original, connection)
            arguments <- list(codeBook = makeElement("codeBook"), to = connection)

            if (streamed) {
                arguments$dataDscr_directly_in_XML <- TRUE
                arguments$data <- data
                arguments$variables <- collectRMetadata(data)
                arguments$embed <- FALSE
                arguments$csv <- FALSE
            }

            if (custom) {
                arguments$indent <- 4
            }

            tryCatch({
                expect_error(do.call(exportCodebook, arguments), "XML validation failed")
                expect_identical(rawConnectionValue(connection), original)
                arguments$to <- ""
                output <- capture.output(expect_error(
                    do.call(exportCodebook, arguments), "XML validation failed"
                ))
                expect_length(output, 0L)
            }, finally = {
                close(connection)
            })
        }
    }
})

test_that("invalid updates leave the original XML unchanged", {
    target <- tempfile(fileext = ".xml")
    on.exit(unlink(target), add = TRUE)
    convert(data.frame(answer = 1:3), to = target)
    original <- readBin(target, "raw", n = file.info(target)$size)
    replacement <- makeElement("codeBook", children = list(makeElement("stdyDscr")))

    expect_error(updateCodebook(target, with = replacement),
        "citation")
    expect_identical(readBin(target, "raw", n = file.info(target)$size), original)
})

test_that("updates cannot silently emit another DDI namespace", {
    target <- tempfile(fileext = ".xml")
    on.exit(unlink(target), add = TRUE)
    convert(data.frame(answer = 1:3), to = target)
    contents <- readLines(target)
    writeLines(gsub("ddi:codebook:2_6", "ddi:codebook:2_5", contents, fixed = TRUE), target)
    original <- readBin(target, "raw", n = file.info(target)$size)
    replacement <- makeElement("codeBook", children = list(
        makeElement("stdyDscr", fill = TRUE, titl = "Updated study")
    ))

    expect_error(updateCodebook(target, with = replacement), "XML validation failed")
    expect_identical(readBin(target, "raw", n = file.info(target)$size), original)
})

test_that("unavailable schema dependencies fail closed", {
    directory <- tempfile("ddiwr-broken-schema-")
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    source <- system.file("schemas", "ddi-codebook-2.6", package = "DDIwR", mustWork = TRUE)
    expect_true(all(file.copy(list.files(source, full.names = TRUE), directory, recursive = TRUE)))
    unlink(file.path(directory, "xml.xsd"))
    schema_path <- file.path(directory, "codebook.xsd")
    validator <- .validateCodebookXML
    target <- file.path(directory, "output.xml")
    original <- charToRaw("Existing contents\n")
    writeBin(original, target)
    book <- makeElement("codeBook", children = list(makeElement("stdyDscr", fill = TRUE)))

    testthat::local_mocked_bindings(.validateCodebookXML = function(output) {
        validator(output, schema_path = schema_path)
    }, .package = "DDIwR")

    expect_error(exportCodebook(book, to = target), "XML validation failed")
    expect_identical(readBin(target, "raw", n = file.info(target)$size), original)
    expect_error(validator("<codeBook/>", schema_path = ""), "schema is unavailable")
})
