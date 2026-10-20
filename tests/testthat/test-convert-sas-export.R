test_that("convert writes native SAS7BDAT with long names and vendor metadata", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "native.sas7bdat")
    created <- as.POSIXct("2001-02-03 04:05:06", tz = "UTC")
    data <- data.frame(long_numeric_name = c(1, 4097), text = c("été", "two"),
        day = as.Date("2000-01-01") + 0:1,
        moment = as.POSIXct("2000-01-01", tz = "UTC") + 0:1)
    attr(data, "label") <- "Native dataset"
    attr(data, "sas_metadata") <- list(created = created, modified = created + 86400,
        table_name = "ORIGINAL")
    attr(data$long_numeric_name, "format.sas") <- "F8.1"
    attr(data$long_numeric_name, "sas_metadata") <- list(format = "F12.3",
        informat = "F16.4", width = 3L)
    attr(data$text, "width") <- 13L
    attr(data$day, "sas_metadata") <- list(format = "DATE9")
    before <- data
    result <- withVisible(convert(data, path))
    imported <- convert(path, encoding = NULL, recode = FALSE, declared = FALSE)

    expect_false(result$visible)
    expect_identical(data, before)
    expect_identical(names(imported), names(data))
    expect_identical(as.numeric(imported$long_numeric_name), data$long_numeric_name |> as.vector())
    expect_identical(as.character(imported$text), as.character(data$text))
    expect_s3_class(imported$day, "Date")
    expect_identical(as.numeric(imported$day), as.numeric(data$day))
    expect_identical(as.numeric(imported$moment), as.numeric(data$moment))
    expect_identical(attr(imported, "label"), "Native dataset")
    expect_identical(attr(imported, "sas_metadata"), attr(data, "sas_metadata"))
    expect_identical(attr(imported$long_numeric_name, "sas_metadata"),
        list(format = "F12.3", informat = "F16.4", width = 3L))
    expect_identical(attr(imported$text, "sas_metadata")$width, 13L)
    expect_identical(list.files(directory), "native.sas7bdat")
})

test_that("public paired catalogs use the same missing dictionary as native data", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "labels.sas7bcat")
    data <- data.frame(answer = declared::declared(c(1, 9, 99, NA),
        label = "Answer", labels = c(Yes = 1, Refusal = 9, Unknown = 99),
        na_values = c(9, 99)), ordinary = c(9, 99, 9, 99))
    attr(data$answer, "sas_metadata") <- list(format = NULL, informat = "F12.2")
    attr(data$answer, "format.sas") <- "BEST12"
    attr(data$ordinary, "labels") <- c(Nine = 9, NinetyNine = 99)
    before <- data
    dictionary <- data.frame(old = c(9, 99), new = c("z", "b"))
    convert(data, path, catalog_to = catalog, dictionary = dictionary)
    result <- convert(path, catalog_file = catalog, encoding = NULL, recode = FALSE)

    expect_identical(data, before)
    expect_identical(as.numeric(result$answer), c(1, NA_real_, NA_real_, NA_real_))
    expect_identical(attr(result$answer, "na_index"), c("-116" = 2L, "-92" = 3L))
    expect_identical(attr(result$answer, "labels"), c(Yes = 1, Refusal = -116, Unknown = -92))
    expect_identical(as.numeric(result$ordinary), c(9, 99, 9, 99))
    expect_identical(attr(result$ordinary, "labels"), c(Nine = 9, NinetyNine = 99))
    expect_identical(attr(result$answer, "sas_metadata")$format, "DDI1F")
    expect_identical(attr(result$answer, "sas_metadata")$informat, "F12.2")
    expect_identical(attr(result$answer, "sas_metadata")$label_tags, c("", "Z", "B"))
    expect_setequal(list.files(directory), c("data.sas7bdat", "labels.sas7bcat"))

    convert(data, path, catalog_to = catalog)
    result <- read_sas(path, catalog_file = catalog)
    expect_setequal(as.numeric(attr(result$answer, "labels"))[-1], c(-91, -92))
    expect_identical(as.numeric(result$ordinary), c(9, 99, 9, 99))
})

test_that("file conversion keeps input and output catalogs and encodings separate", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    source <- file.path(directory, "source.sas7bdat")
    fixture <- file.path(directory, "input-labels.sas7bcat")
    path <- file.path(directory, "output.sas7bdat")
    catalog <- file.path(directory, "output.sas7bcat")
    data <- data.frame(number = c(0, NA_real_, NA_real_, NA_real_))
    attr(data$number, "na_index") <- c("_" = 3L, "Z" = 4L)
    attr(data$number, "format.sas") <- "SRCFMT"
    write_sas(data, source)
    sas_label_catalog(fixture, encoding = 0L, text_encoding = "windows-1252",
        formats = "SRCFMT", tags = c(".", "_", "Z"))
    original_data <- readBin(source, "raw", n = file.info(source)$size)
    original_catalog <- readBin(fixture, "raw", n = file.info(fixture)$size)
    expected <- read_sas(source, catalog_file = fixture, catalog_encoding = "windows-1252")

    for (limit in c(-1L, 0L, 1L)) {
        convert(source, path, catalog_file = fixture, catalog_to = catalog,
            encoding = NULL, catalog_encoding = "windows-1252", n_max = limit)
        result <- convert(path, encoding = NULL, recode = FALSE, declared = FALSE)
        expect_identical(attr(result$number, "labels"), attr(expected$number, "labels"))
        expect_identical(writeBin(unname(attr(result$number, "labels")), raw()),
            writeBin(unname(attr(expected$number, "labels")), raw()))
        expect_identical(attr(result$number, "sas_metadata")$label_tags, c(".", "_", "Z"))
        rows <- nrow(data)

        if (limit >= 0L) {
            rows <- limit
        }

        expect_identical(nrow(result), rows)
        expect_identical(readBin(source, "raw", n = file.info(source)$size), original_data)
        expect_identical(readBin(fixture, "raw", n = file.info(fixture)$size), original_catalog)
    }

    convert(source, path, catalog_file = fixture, catalog_to = catalog,
        encoding = NULL, catalog_encoding = "windows-1252",
        dictionary = data.frame(old = -116, new = "b"))
    remapped <- read_sas(path, catalog_file = catalog)
    expect_identical(attr(remapped$number, "na_index"), c("_" = 3L, "-92" = 4L))
    expect_identical(attr(remapped$number, "sas_metadata")$label_tags, c(".", "_", "B"))

    standalone <- file.path(directory, "standalone.sas7bdat")
    convert(source, standalone, catalog_file = fixture,
        encoding = NULL, catalog_encoding = "windows-1252")
    expect_false(file.exists(file.path(directory, "standalone.sas7bcat")))
    expect_null(attr(read_sas(standalone)$number, "labels"))
    expect_identical(readBin(fixture, "raw", n = file.info(fixture)$size), original_catalog)
})

test_that("automatic input catalogs do not become implicit output catalogs", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    source <- file.path(directory, "source.sas7bdat")
    fixture <- file.path(directory, "source.sas7bcat")
    path <- file.path(directory, "output.sas7bdat")
    catalog <- file.path(directory, "output.sas7bcat")
    data <- data.frame(number = c(-2.5, 1.25, NA_real_))
    attr(data$number, "labels") <- c(Negative = -2.5, Fraction = 1.25, System = NA_real_)
    convert(data, source, catalog_to = fixture)
    original <- readBin(fixture, "raw", n = file.info(fixture)$size)
    convert(source, path, encoding = NULL, catalog_to = catalog)
    result <- convert(path, encoding = NULL)
    expect_identical(attr(result$number, "labels"), attr(data$number, "labels"))
    expect_identical(as.numeric(result$number), as.numeric(data$number))
    expect_identical(readBin(fixture, "raw", n = file.info(fixture)$size), original)

    unlink(catalog)
    convert(source, path, encoding = NULL, catalog_to = NULL)
    expect_false(file.exists(catalog))
    expect_identical(readBin(fixture, "raw", n = file.info(fixture)$size), original)
})

test_that("invalid public catalog destinations fail without replacing files", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    bytes <- charToRaw("keep this file")
    writeBin(bytes, path)
    writeBin(bytes, catalog)
    data <- data.frame(number = 1:2)
    attr(data$number, "labels") <- c(First = 1)
    invalid <- list("", NA_character_, character(), c("a", "b"), 1, list("labels"))

    for (destination in invalid) {
        expect_error(convert(data, path, catalog_to = destination), "catalog_to")
        expect_identical(readBin(path, "raw", n = length(bytes)), bytes)
        expect_identical(readBin(catalog, "raw", n = length(bytes)), bytes)
    }

    for (destination in list(NULL, file.path(directory, "data.sav"),
        file.path(directory, "data.xpt"))) {
        expect_error(convert(data, destination, catalog_to = catalog), "only for SAS7BDAT output")
        expect_identical(readBin(catalog, "raw", n = length(bytes)), bytes)
    }

    expect_error(convert(data, path, catalog_to = path), "distinct files")
    expect_error(convert(data, path, catalog_to = directory), "distinct files")
    expect_error(convert(data, path, catalog_to = file.path(directory, "absent", "data.sas7bcat")),
        "existing directories")
    data$number <- c("a", "b")
    attr(data$number, "labels") <- c(First = 1)
    expect_error(convert(data, path, catalog_to = catalog), "keys must match")
    expect_identical(readBin(path, "raw", n = file.info(path)$size), bytes)
    expect_identical(readBin(catalog, "raw", n = file.info(catalog)$size), bytes)
})

test_that("SAS shorthand retains XPT output and its existing setup script", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    source <- file.path(directory, "survey.rds")
    data <- data.frame(answer = declared::declared(c(1, 2), label = "Answer",
        labels = c(Yes = 1, No = 2)))
    saveRDS(data, source)
    convert(source, "SAS")
    expect_true(file.exists(file.path(directory, "survey.xpt")))
    expect_true(file.exists(file.path(directory, "survey.sas")))
    expect_false(file.exists(file.path(directory, "survey.sas7bdat")))
    result <- convert(file.path(directory, "survey.xpt"), recode = FALSE)
    expect_identical(as.numeric(result$answer), c(1, 2))
})

test_that("native public export reserves canonical tags before harmonizing other codes", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    data <- data.frame(answer = declared::declared(c(1, -91, -116, 9, 99),
        labels = c(Yes = 1, OriginalA = -91, OriginalZ = -116, Refusal = 9, Unknown = 99),
        na_values = c(-91, -116, 9, 99)))
    before <- data

    for (harmonize in c(TRUE, FALSE)) {
        convert(data, path, catalog_to = catalog, harmonize = harmonize)
        result <- read_sas(path, catalog_file = catalog)
        other_codes <- c(-92, -93)

        if (!harmonize) {
            other_codes <- rev(other_codes)
        }

        expect_identical(attr(result$answer, "na_index"),
            setNames(2:5, as.character(c(-91, -116, other_codes))))
        expect_identical(attr(result$answer, "labels"),
            setNames(c(1, -91, -116, other_codes), names(attr(data$answer, "labels"))))
        expect_identical(data, before)
    }

    source <- file.path(directory, "source.dta")
    write_dta(data, source, dictionary = data.frame(old = c(-91, -116, 9, 99),
        new = c("a", "z", "b", "c")))
    convert(source, path, encoding = NULL, catalog_to = catalog)
    expect_identical(attr(read_sas(path, catalog_file = catalog)$answer, "na_index"),
        c("-91" = 2L, "-116" = 3L, "-92" = 4L, "-93" = 5L))
})

test_that("public paired export preserves every missing-label category", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    data <- data.frame(answer = rep(NA_real_, 28L))
    attr(data$answer, "na_index") <- structure(2:28,
        names = c("_", as.character(-91:-116)))
    attr(data$answer, "na_values") <- as.numeric(-91:-116)
    labels <- c(System = NA_real_, Underscore = sas_label_underscore(),
        setNames(as.numeric(-91:-116), LETTERS))
    attr(data$answer, "labels") <- labels
    convert(data, path, catalog_to = catalog)
    result <- read_sas(path, catalog_file = catalog)
    expect_identical(attr(result$answer, "na_index"), attr(data$answer, "na_index"))
    expect_identical(writeBin(unname(attr(result$answer, "labels")), raw()),
        writeBin(unname(labels), raw()))
    expect_identical(names(attr(result$answer, "labels")), names(labels))
    expect_identical(attr(result$answer, "sas_metadata")$label_tags, c(".", "_", LETTERS))
})

test_that("empty data frames retain numeric catalog definitions and factor codes", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "empty.sas7bdat")
    catalog <- file.path(directory, "empty.sas7bcat")
    data <- data.frame(answer = declared::declared(numeric(),
        label = "Answer", labels = c(Yes = 1, Missing = 99), na_values = 99),
        group = factor(character(), levels = c("one", "two")))
    attr(data, "label") <- "Empty dataset"
    convert(data, path, catalog_to = catalog)
    result <- convert(path, catalog_file = catalog, encoding = NULL, recode = FALSE)
    expect_identical(dim(result), c(0L, 2L))
    expect_identical(attr(result, "label"), "Empty dataset")
    expect_identical(attr(result$answer, "labels"), c(Yes = 1, Missing = -91))
    expect_identical(attr(result$group, "labels"), c(one = 1, two = 2))
    expect_identical(inspectFile(path, catalog_file = catalog)$row_count, 0)
})

test_that("SPSS and XPT inputs retain their missing values in native SAS output", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    data <- data.frame(answer = declared::declared(c(1, 9), label = "Answer",
        labels = c(Yes = 1, Missing = 9), na_values = 9))
    sav <- file.path(directory, "source.sav")
    xpt <- file.path(directory, "source.xpt")
    path <- file.path(directory, "output.sas7bdat")
    write_sav(data, sav)
    write_xpt(data, xpt, dictionary = data.frame(old = 9, new = "z"))
    convert(sav, path)
    expect_identical(attr(read_sas(path)$answer, "na_index"), c("-91" = 2L))
    expect_warning(convert(xpt, path), "sas_metadata\\$version")
    expect_identical(attr(read_sas(path)$answer, "na_index"), c("-116" = 2L))
})

test_that("requesting a catalog for unlabelled data produces a readable empty catalog", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    data <- data.frame(number = c(1, 2), text = c("one", "two"))
    convert(data, path, catalog_to = catalog)
    result <- convert(path, catalog_file = catalog, encoding = NULL, recode = FALSE)
    expect_identical(as.numeric(result$number), data$number)
    expect_identical(as.character(result$text), data$text)
    expect_null(attr(result$number, "labels"))
    expect_true(file.exists(catalog))
})
