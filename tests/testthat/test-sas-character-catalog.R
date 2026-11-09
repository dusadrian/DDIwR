test_that("paired SAS catalogs retain mixed character and numeric label definitions", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    keys <- c("01", "1", " A", "é", "漢字", "a\tb", "a\nb", "")
    labels <- setNames(c(keys, "unused", "tail "),
        c(paste0("Label ", seq_along(keys)), "Unused é", "Trailing key"))
    names(labels)[1:3] <- c("Label 1  ", "", " Label 3")
    data <- data.frame(text = c(keys, "unknown", NA_character_), number = seq_len(10L))
    attr(data$text, "labels") <- labels
    attr(data$text, "sas_metadata") <- list(format = "$customfmt24", informat = "$CHAR40", width = 40L)
    attr(data$number, "labels") <- c(First = 1, Last = 10, System = NA_real_)
    before <- data
    convert(data, path, catalog = catalog)
    result <- read_sas(path, catalog_file = catalog)
    expected <- sub(" +$", "", labels)
    records <- sas_export_catalog_records(catalog)

    expect_identical(data, before)
    expect_identical(attr(result$text, "labels"), expected)
    expect_identical(attr(result$text, "sas_metadata"),
        list(format = "$CUSTOMFMT24", informat = "$CHAR40", width = 40L))
    expect_identical(attr(result$number, "labels"), c(First = 1, Last = 10, System = NA_real_))
    expect_identical(records$`$CUSTOMFMT`$keys, as.list(unname(expected)))
    expect_identical(records$`$CUSTOMFMT`$labels, names(labels))
    expect_identical(as.vector(result$text)[seq_along(keys)], keys)
    expect_null(attr(result$text, "sas_metadata")$label_tags)
    expect_identical(inspectFile(path, catalog_file = catalog)$columns$metadata[[1]]$labels, expected)

    for (limit in c(0L, 1L)) {
        expect_identical(attr(read_sas(path, catalog_file = catalog, n_max = limit)$text,
            "labels"), expected)
    }

    copy <- file.path(directory, "copy.sas7bdat")
    copy_catalog <- file.path(directory, "copy.sas7bcat")
    convert(path, copy, catalog = copy_catalog, encoding = NULL)
    expect_identical(attr(read_sas(copy, catalog_file = copy_catalog)$text, "labels"), expected)
})

test_that("independent character catalogs round trip across endian and string-offset layouts", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    source <- file.path(directory, "source.sas7bdat")
    fixture <- file.path(directory, "source.sas7bcat")
    path <- file.path(directory, "output.sas7bdat")
    catalog <- file.path(directory, "output.sas7bcat")
    labels <- c(Leading = " code", Unicode = "café", Blank = "", Long = strrep("z", 40),
        Unused = "unobserved")
    names(labels)[1:2] <- c("Leading label  ", "")
    data <- data.frame(text = c(" code", "café", ""))
    attr(data$text, "format.sas") <- "$LONG_CHARACTER_FORMAT20"
    write_sas(data, source)

    for (endian in c("little", "big")) {
        for (offset in c(0L, 1L)) {
            sas_label_catalog(fixture, endian = endian, formats = "$LONG_CHARACTER_FORMAT",
                character_labels = labels, string_offset = offset)
            imported <- read_sas(source, catalog_file = fixture)
            expect_identical(attr(imported$text, "labels"), labels)
            original <- readBin(fixture, "raw", n = file.info(fixture)$size)

            for (limit in c(-1L, 0L)) {
                convert(source, path, catalog = catalog,
                    encoding = NULL, n_max = limit)
                result <- read_sas(path, catalog_file = catalog)
                expect_identical(attr(result$text, "labels"), labels)
                expect_identical(sas_export_catalog_records(catalog)$`$LONG_CHARACTER_FORMAT`$keys,
                    as.list(unname(labels)))
                expect_identical(attr(result$text, "sas_metadata")$format,
                    "$LONG_CHARACTER_FORMAT20")
            }

            expect_identical(readBin(fixture, "raw", n = file.info(fixture)$size), original)
        }
    }

    sas_label_catalog(fixture, endian = "big", encoding = 0L, text_encoding = "windows-1252",
        formats = "$LONG_CHARACTER_FORMAT", character_labels = c(`Français é` = "café"))
    convert(source, path, catalog = catalog,
        encoding = NULL, catalog_encoding = "windows-1252")
    expect_identical(attr(read_sas(path, catalog_file = catalog)$text, "labels"),
        c(`Français é` = "café"))
})

test_that("character formats share definitions independently of numeric names", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    data <- data.frame(first = c("a", "b"), second = c("b", "a"), number = c(1, 2))
    attr(data$first, "labels") <- c(A = "a", B = "b")
    attr(data$second, "labels") <- c(B = "b", A = "a")
    attr(data$number, "labels") <- c(A = 1, B = 2)
    attr(data$first, "format.sas") <- "$SHARED12"
    attr(data$second, "format.sas") <- "$SHARED24"
    attr(data$number, "format.sas") <- "SHARED"
    write_sas(data, path, catalog_file = catalog)
    records <- sas_export_catalog_records(catalog)
    expect_setequal(names(records), c("$SHARED", "SHARED"))
    expect_identical(attr(read_sas(path, catalog_file = catalog)$second, "labels"), c(A = "a", B = "b"))

    original <- lapply(c(path, catalog), function(file) {
        return(readBin(file, "raw", n = file.info(file)$size))
    })
    attr(data$second, "labels") <- c(A = "a", Different = "b")
    expect_error(write_sas(data, path, catalog_file = catalog), "Conflicting")
    expect_identical(readBin(path, "raw", n = file.info(path)$size), original[[1]])
    expect_identical(readBin(catalog, "raw", n = file.info(catalog)$size), original[[2]])
})

test_that("generated character format names obey clearing and legacy precedence", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    data <- data.frame(text = c("01", "02"))
    attr(data$text, "labels") <- c(One = "01", Two = "02")
    attr(data$text, "format.sas") <- "$LEGACY12"

    for (case in list(list(), list(format = "$OVERRIDE20"), list(format = NULL))) {
        attr(data$text, "sas_metadata") <- case
        convert(data, path, catalog = catalog)
        expected <- "$LEGACY12"

        if (is.element("format", names(case))) {
            expected <- "$DDI1F"

            if (!is.null(case$format)) {
                expected <- case$format
            }
        }

        expect_identical(attr(read_sas(path, catalog_file = catalog)$text, "sas_metadata")$format,
            expected)
    }

    convert(clearFormats(data), path, catalog = catalog)
    expect_identical(attr(read_sas(path, catalog_file = catalog)$text, "sas_metadata")$format,
        "$DDI1F")
})

test_that("wide character keys and labels survive chained catalog pages", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    keys <- c(strrep("x", 16), strrep("y", 17), strrep("é", 100), strrep("z", 32767))
    labels <- setNames(keys, c("Sixteen", "Seventeen", "Unicode", strrep("L", 32767)))
    data <- data.frame(text = character())
    attr(data$text, "labels") <- labels
    attr(data$text, "sas_metadata") <- list(format = paste0("$", strrep("A", 31)))
    convert(data, path, catalog = catalog)
    result <- read_sas(path, catalog_file = catalog)
    expect_identical(attr(result$text, "labels"), labels)
    records <- sas_export_catalog_records(catalog)
    expect_identical(records[[1]]$keys, as.list(keys))
    expect_identical(records[[1]]$labels, names(labels))
    expect_identical(nrow(result), 0L)

    populated <- data.frame(text = keys)
    attributes(populated$text) <- attributes(data$text)
    convert(populated, path, catalog = catalog)
    result <- read_sas(path, catalog_file = catalog)
    expect_identical(as.vector(result$text), keys)
    expect_identical(attr(result$text, "labels"), labels)
})

test_that("invalid character catalog definitions preserve existing destinations", {
    path <- tempfile()
    catalog <- tempfile()
    on.exit(unlink(c(path, catalog)), add = TRUE)
    data <- data.frame(text = c("a", "b"))
    cases <- list(
        list(labels = c(A = NA_character_)),
        list(labels = c(A = "a", Again = "a")),
        list(labels = c(A = "a", Again = "a ")),
        list(labels = c(A = "", Again = "   ")),
        list(labels = c(A = strrep("x", 32768))),
        list(labels = setNames("a", NA_character_)),
        list(labels = c(A = "a"), format = "NUMERIC"),
        list(labels = c(A = "a"), format = "$CHAR20"),
        list(labels = c(A = "a"), format = "$"),
        list(labels = c(A = "a"), format = "$FMT12.2"),
        list(labels = c(A = "a"), format = paste0("$", strrep("A", 32))),
        list(labels = c(A = "a"), tags = ""),
        list(labels = c(A = 1))
    )
    bytes <- charToRaw("preserve original")

    for (case in cases) {
        modified <- data
        attr(modified$text, "labels") <- case$labels
        attr(modified$text, "sas_metadata") <- list(format = case$format, label_tags = case$tags)
        writeBin(bytes, path)
        writeBin(bytes, catalog)
        expect_error(write_sas(modified, path, catalog_file = catalog), "catalog|format|label")
        expect_identical(readBin(path, "raw", n = file.info(path)$size), bytes)
        expect_identical(readBin(catalog, "raw", n = file.info(catalog)$size), bytes)
    }
})
