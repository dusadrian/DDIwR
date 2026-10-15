test_that("unspecified SAS catalog encoding follows the data file (#836)", {
    data <- data.frame(
        number = c(0, NA_real_),
        day = structure(c(0, NA_real_), class = "Date"),
        moment = structure(c(0.125, NA_real_), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
        place = c("România", "Grüne")
    )
    formats <- c(number = "MISSNUM", day = "DATE", moment = "DATETIME")

    for (name in names(formats)) {
        attr(data[[name]], "format.sas") <- formats[[name]]
        attr(data[[name]], "na_index") <- c("_" = 2L)
        attr(data[[name]], "label") <- "Măsură"
    }

    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "source.sas7bdat")
    catalog <- file.path(directory, "source.sas7bcat")
    write_sas(data, path)

    for (endian in c("little", "big")) {
        sas_label_catalog(catalog, endian = endian, encoding = 0L)
        explicit <- read_sas(path, catalog_file = catalog, catalog_encoding = "UTF-8")
        imported <- read_sas(path, catalog_file = catalog)
        expect_identical(imported, explicit)

        for (name in names(formats)) {
            expect_identical(names(attr(imported[[name]], "labels")),
                paste(formats[[name]], "missing", c(".", "_", LETTERS), "é"))
        }

        expect_identical(as.character(imported$place), data$place)
        expect_identical(attr(imported$number, "label"), "Măsură")
        expect_identical(as.numeric(imported$day), as.numeric(data$day))
        expect_identical(as.numeric(imported$moment), as.numeric(data$moment))
        expect_identical(convert(path, encoding = NULL), explicit)

        empty <- read_sas(path, catalog_file = catalog, n_max = 0L)
        window <- read_sas(path, catalog_file = catalog, skip = 1L, n_max = 1L)
        expect_equal(nrow(empty), 0L)
        expect_identical(as.character(window$place), "Grüne")
        expect_identical(attr(window$number, "na_index"), c("_" = 1L))
        expect_identical(attr(empty$number, "labels"), attr(explicit$number, "labels"))

        # An explicitly supplied data encoding also supplies the catalog fallback.
        bytes <- readBin(path, "raw", n = file.info(path)$size)
        bytes[71L] <- as.raw(29L)
        writeBin(bytes, path)
        overridden <- read_sas(path, catalog_file = catalog,
            encoding = "UTF-8", catalog_encoding = NULL)
        expect_identical(overridden, explicit)
        bytes[71L] <- as.raw(20L)
        writeBin(bytes, path)
    }
})

test_that("SAS catalog encoding overrides remain separate from data encoding", {
    data <- data.frame(number = c(0, NA_real_), place = c("România", "Grüne"))
    attr(data$number, "format.sas") <- "MISSNUM"
    attr(data$number, "na_index") <- c("_" = 2L)
    attr(data$number, "label") <- "Măsură"
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "source.sas7bdat")
    catalog <- file.path(directory, "source.sas7bcat")
    write_sas(data, path)

    # A catalog with a real encoding must keep it, even if the data is UTF-8.
    sas_label_catalog(catalog, encoding = 29L, text_encoding = "latin1",
        label_suffix = "é label")
    imported <- read_sas(path, catalog_file = catalog)
    expect_identical(names(attr(imported$number, "labels")),
        paste("MISSNUM", "missing", c(".", "_", LETTERS), "é label"))
    expect_identical(as.character(imported$place), data$place)

    for (header_encoding in c(0L, 20L, 29L)) {
        sas_label_catalog(catalog, encoding = header_encoding, text_encoding = "latin1",
            label_suffix = "é label")
        explicit <- read_sas(path, catalog_file = catalog, catalog_encoding = "latin1")
        expect_identical(explicit, imported)
        expect_identical(attr(explicit$number, "label"), "Măsură")

        if (header_encoding != 29L) {
            automatic <- read_sas(path, catalog_file = catalog)
            expect_false(identical(attr(automatic$number, "labels"),
                attr(explicit$number, "labels")))
        }
    }

    expect_error(read_sas(path, catalog_file = catalog,
        catalog_encoding = "unknown-ddiwr-encoding"), "source.sas7bcat")
})
