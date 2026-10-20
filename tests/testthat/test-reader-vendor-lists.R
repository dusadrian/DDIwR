test_that("native readers emit vendor lists without duplicate legacy attributes", {
    path <- tempfile()
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(number = c(1, 2), text = c("one", "two"))
    attr(data, "notes") <- c("First", "Second")
    attr(data$number, "label") <- "Number"
    attr(data$number, "labels") <- c(First = 1)
    attr(data$text, "width") <- 18L
    writers <- list(sav = write_sav, dta = write_dta, sas7bdat = write_sas, xpt = write_xpt)
    readers <- list(sav = read_sav, dta = read_dta, sas7bdat = read_sas, xpt = read_xpt)
    vendors <- c(sav = "spss_metadata", dta = "stata_metadata",
        sas7bdat = "sas_metadata", xpt = "sas_metadata")
    formats <- c(sav = "F12.3", dta = "%15.4f", sas7bdat = "F16.5", xpt = "F16.5")
    legacy <- c(sav = "format.spss", dta = "format.stata",
        sas7bdat = "format.sas", xpt = "format.sas")

    for (target in names(writers)) {
        unlink(path)
        path <- tempfile(fileext = paste0(".", target))
        attribute <- vendors[[target]]
        attr(data$number, legacy[[target]]) <- formats[[target]]
        writers[[target]](data, path)
        imported <- readers[[target]](path)
        empty <- readers[[target]](path, n_max = 0)
        inspected <- inspectFile(path)

        for (result in list(imported, empty)) {
            expect_null(attr(result, "notes", exact = TRUE))
            expect_identical(attr(result$number, attribute)$format, formats[[target]])
            expect_identical(attr(result$text, attribute)$width, 18L)
            expect_identical(attr(result$number, "label"), "Number")

            if (is.element(target, c("sav", "dta"))) {
                expect_identical(attr(result$number, "labels"), c(First = 1))
            }

            for (column in result) {
                for (name in c(unname(legacy), "width", "display_width")) {
                    expect_null(attr(column, name, exact = TRUE))
                }
            }
        }

        expect_identical(inspected$columns$metadata[[2]][[attribute]]$width, 18L)

        if (is.element(target, c("sav", "dta"))) {
            expect_identical(attr(imported, attribute)$notes, attr(data, "notes"))
        }

        writers[[target]](imported, path)
        expect_identical(attr(readers[[target]](path)$text, attribute)$width, 18L)

        # Conflicting legacy edits cannot shadow imported list fields.
        attr(imported$number, legacy[[target]]) <- "invalid legacy format"
        writers[[target]](imported, path)
        expect_identical(attr(readers[[target]](path)$number, attribute)$format, formats[[target]])

        attr(imported$number, attribute)["format"] <- list(NULL)
        expect_no_warning(writers[[target]](imported, path))
        default <- attr(readers[[target]](path)$number, attribute)$format
        expect_false(identical(default, formats[[target]]))
    }
})

test_that("format consumers share list precedence, legacy fallback and NULL defaults", {
    data <- data.frame(number = c(1.25, 2.5))
    attr(data$number, "format.spss") <- "F12.3"
    attr(data$number, "format.stata") <- "%15.4f"
    defaults <- c(getFormat(data$number |> clearFormats(), "SPSS"),
        getFormat(data$number |> clearFormats(), "Stata"))
    cases <- list(
        list(metadata = list(), expected = c("F12.3", "%15.4f")),
        list(metadata = list(format = "F18.5"), expected = c("F18.5", "%18.5f")),
        list(metadata = list(format = NULL), expected = defaults)
    )

    for (case in cases) {
        attr(data$number, "spss_metadata") <- case$metadata
        attr(data$number, "stata_metadata") <- case$metadata

        if (!is.null(case$metadata$format)) {
            attr(data$number, "stata_metadata")$format <- "%18.5f"
        }

        expect_identical(getFormat(data$number, "SPSS"), case$expected[1])
        expect_identical(getFormat(data$number, "Stata"), case$expected[2])
        expect_identical(collectRMetadata(data)$number$varFormat, case$expected)
        expect_identical(collectRMetadataC(data)$number$varFormat, case$expected)
    }

    data$day <- as.Date("2000-01-01") + 0:1
    attr(data$day, "spss_metadata") <- list(format = "ADATE10")
    expect_identical(collectRMetadataC(data)$day$varFormat, c("ADATE10", "%td"))
})

test_that("setupfile and XML import consume canonical formats", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(
        number = declared::declared(c(1.25, 2.5), label = "Number",
            labels = c(First = 1.25)),
        other = c(3, 4)
    )
    attr(data$number, "format.spss") <- "F9.1"
    attr(data$number, "spss_metadata") <- list(format = "F12.3")
    attr(data$number, "stata_metadata") <- list(format = "%15.4f")
    csv <- file.path(directory, "data.csv")
    write.csv(data, csv, row.names = FALSE)

    for (input in list(data, getCodebook(data))) {
        for (extension in c("sps", "do")) {
            path <- file.path(directory, paste0("data.", extension))
            setupfile(input, file = path, csv = csv)
            commands <- paste(readLines(path), collapse = "\n")
            expected <- switch(extension, sps = "F12.3", do = "%15.4f")
            if (is.data.frame(input)) {
                expect_match(commands, expected, fixed = TRUE)
            }
            expect_false(grepl("F9.1", commands, fixed = TRUE))
        }
    }

    xml <- file.path(directory, "data.xml")
    convert(clearFormats(data), xml, embed = FALSE)
    document <- xml2::read_xml(xml)
    node <- xml2::xml_find_first(document, "//*[local-name()='varFormat']")
    xml2::xml_set_text(node, "F12.3")
    xml2::write_xml(document, xml)
    imported <- convert(xml)
    expect_identical(attr(imported$number, "spss_metadata")$format, "F12.3")
    expect_null(attr(imported$number, "format.spss", exact = TRUE))
})

test_that("raw metadata snapshots capture lists without resolving legacy attributes", {
    data <- data.frame(number = 1:2)
    attr(data$number, "format.spss") <- "F9.1"
    metadata <- list(format = NULL, width = 8L, future = list(value = "keep"))
    attr(data$number, "spss_metadata") <- metadata
    snapshot <- metadataSnapshotRaw(data)
    on.exit(metadataSnapshotClose(snapshot), add = TRUE)
    captured <- metadataSnapshotRead(snapshot)$number
    expect_identical(captured$spss_metadata, metadata)
    expect_identical(captured$format.spss, "F9.1")
    expect_null(captured$stata_metadata)
})

test_that("Stata list notes retain sparse and empty raw characteristics until edited", {
    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(number = 1:2)
    raw_notes <- list(note0 = "3", note1 = "First ", note2 = "   ",
        note3 = "Last", note2147483648 = "Uninterpreted", Source = "Archive")
    attr(data, "stata.characteristics") <- raw_notes
    write_dta(data, path)
    imported <- read_dta(path, num_threads = 1L)
    expect_identical(attr(imported, "stata_metadata")$notes, c("First", "Last"))
    expect_null(attr(imported, "notes", exact = TRUE))
    write_dta(imported, path)
    repeated <- read_dta(path, num_threads = 1L)
    expect_identical(attr(repeated, "stata.characteristics"), raw_notes)

    attr(imported, "stata_metadata")$notes <- "Changed"
    write_dta(imported, path)
    repeated <- read_dta(path, num_threads = 1L)
    expect_identical(attr(repeated, "stata_metadata")$notes, "Changed")
    expect_identical(attr(repeated, "stata.characteristics")$Source, "Archive")

    attr(imported, "stata_metadata")["notes"] <- list(NULL)
    write_dta(imported, path)
    repeated <- read_dta(path, num_threads = 1L)
    expect_null(attr(repeated, "stata_metadata")$notes)
    expect_identical(attr(repeated, "stata.characteristics"), list(Source = "Archive"))
})

test_that("Stata width clearing removes list and legacy values together", {
    path <- tempfile(fileext = ".dta")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(text = c("one", "two"))
    attr(data$text, "width") <- 18L

    for (case in list(list(), list(width = 24L), list(width = NULL))) {
        attr(data$text, "stata_metadata") <- case
        write_dta(data, path)
        width <- attr(read_dta(path)$text, "stata_metadata")$width
        expected <- 18L

        if (is.element("width", names(case))) {
            expected <- 3L

            if (!is.null(case$width)) {
                expected <- case$width
            }
        }

        expect_identical(width, expected)
    }

    attr(data$text, "stata_metadata") <- list(width = 24L, type = "strL", future = NULL)
    cleared <- clearWidths(data)
    expect_null(attr(cleared$text, "width", exact = TRUE))
    expect_identical(attr(cleared$text, "stata_metadata"), list(type = "strL", future = NULL))
})

test_that("dialog metadata uses canonical widths with legacy fallback", {
    column <- structure(c("one", "two"), width = 50L)
    expect_identical(.metadataDialogVariable("text", column)$width, 50L)

    for (attribute in c("spss_metadata", "stata_metadata", "sas_metadata")) {
        attr(column, attribute) <- list(width = 20L)
        expect_identical(.metadataDialogVariable("text", column)$width, 20L)
        attr(column, attribute) <- list(width = NULL)
        expect_identical(.metadataDialogVariable("text", column)$width, 3L)
        attr(column, attribute) <- NULL
    }
})

test_that("POR fixture formats use the same canonical reader representation", {
    path <- test_path("fixtures", "readstat", "timestamp_epoch.por")
    imported <- read_por(path)
    expect_true(is.character(attr(imported[[1]], "spss_metadata")$format))
    expect_null(attr(imported[[1]], "format.spss", exact = TRUE))
    expect_null(attr(imported[[1]], "width", exact = TRUE))
    inspected <- inspectFile(path)
    expect_identical(inspected$columns$metadata[[1]]$spss_metadata,
        attr(imported[[1]], "spss_metadata"))
})

test_that("Stata setup does not restore string formats after numeric recoding", {
    path <- tempfile(fileext = ".do")
    on.exit(unlink(path), add = TRUE)
    data <- data.frame(
        choice = declared::declared(c("a", "b"), label = "Choice",
            labels = c(First = "a", Second = "b")),
        number = declared::declared(c(1.25, 2.5), label = "Number")
    )
    attr(data$choice, "stata_metadata") <- list(format = "%24s")
    attr(data$number, "stata_metadata") <- list(format = "%15.4f")
    setupfile(data, file = path)
    commands <- paste(readLines(path), collapse = "\n")
    expect_match(commands, "rename TEMPVAR CHOICE", fixed = TRUE)
    expect_match(commands, "format NUMBER %15.4f", fixed = TRUE)
    expect_false(grepl("format CHOICE", commands, fixed = TRUE))
})
