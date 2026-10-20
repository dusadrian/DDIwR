test_that("numeric SAS catalogs retain ordinary keys and all missing-label categories", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    values <- c(-2.5, 0, 1.25, NA_real_, rep(NA_real_, 27L))
    labels <- c(-2.5, 0, 1.25, NA_real_, sas_label_underscore(), as.numeric(-91:-116))
    names(labels) <- c("Negative é", "Zero", "Fraction", "System", "Underscore", LETTERS)
    data <- data.frame(number = values, integer = seq_along(values), text = "unchanged")
    attr(data$number, "na_index") <- structure(5:31, names = c("_", as.character(-91:-116)))
    attr(data$number, "na_values") <- as.numeric(-91:-116)
    attr(data$number, "labels") <- labels
    attr(data$number, "sas_metadata") <- list(format = "customfmt12.2", informat = "F20.3", width = 8L)
    attr(data$integer, "labels") <- c(First = 1L, Last = 31L, Missing = NA_integer_)
    attr(data, "label") <- "Paired export"
    before <- data
    write_sas(data, path, catalog_file = catalog)
    imported <- read_sas(path, catalog_file = catalog)
    records <- sas_export_catalog_records(catalog)

    expect_identical(data, before)
    expect_identical(records$CUSTOMFMT$keys, c(list(-2.5, 0, 1.25, ".", "_"), as.list(LETTERS)))
    expect_identical(records$CUSTOMFMT$labels, names(labels))
    expect_identical(attr(imported$number, "sas_metadata")$format, "CUSTOMFMT12.2")
    expect_identical(attr(imported$number, "sas_metadata")$informat, "F20.3")
    expect_identical(as.numeric(imported$number), values)
    expect_identical(attr(imported$number, "labels"), labels)
    expect_identical(writeBin(unname(attr(imported$number, "labels")), raw()),
        writeBin(unname(labels), raw()))
    expect_identical(attr(imported$number, "na_index"), attr(data$number, "na_index"))
    expect_identical(attr(imported$integer, "labels"), c(First = 1, Last = 31, Missing = NA_real_))
    expect_identical(as.character(imported$text), rep("unchanged", length(values)))
    expect_identical(attr(convert(path)$number, "labels"), labels)

    for (result in list(read_sas(path, catalog_file = catalog, n_max = 0L),
        read_sas(path, catalog_file = catalog, skip = 1L, n_max = 2L))) {
        expect_identical(attr(result$number, "labels"), labels)
        expect_identical(attr(result$number, "sas_metadata")$label_tags,
            c("", "", "", ".", "_", LETTERS))
    }

    expect_identical(inspectFile(path, catalog_file = catalog)$columns$metadata[[1]]$labels, labels)
    expect_null(attr(read_sas(path)$number, "labels"))
})

test_that("independent catalogs retain unused and sparse tags through paired export", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    source <- file.path(directory, "source.sas7bdat")
    fixture <- file.path(directory, "fixture.sas7bcat")
    path <- file.path(directory, "repeated.sas7bdat")
    catalog <- file.path(directory, "repeated.sas7bcat")
    data <- data.frame(number = c(0, NA_real_, NA_real_, NA_real_))
    attr(data$number, "na_index") <- c("_" = 3L, "Z" = 4L)
    attr(data$number, "format.sas") <- "SRCFMT"
    write_sas(data, source)

    for (endian in c("little", "big")) {
        sas_label_catalog(fixture, endian = endian, formats = "SRCFMT", tags = c(".", "_", "Z"))
        for (limit in c(-1L, 0L, 1L)) {
            imported <- read_sas(source, catalog_file = fixture, n_max = limit)
            expected <- attr(imported$number, "labels")
            expect_identical(attr(imported$number, "sas_metadata")$label_tags, c(".", "_", "Z"))
            write_sas(imported, path, catalog_file = catalog)
            result <- read_sas(path, catalog_file = catalog)
            expect_identical(attr(result$number, "labels"), expected)
            expect_identical(writeBin(unname(attr(result$number, "labels")), raw()),
                writeBin(unname(expected), raw()))
            expect_identical(attr(result$number, "na_index"), attr(imported$number, "na_index"))
            expect_identical(sas_export_catalog_records(catalog)$SRCFMT$keys, list(".", "_", "Z"))
        }
    }
})

test_that("catalog label keys follow the data export dictionary", {
    path <- tempfile()
    catalog <- tempfile()
    on.exit(unlink(c(path, catalog)), add = TRUE)
    data <- data.frame(number = declared::declared(c(1, 9, 99, NA),
        labels = c(Yes = 1, Refusal = 9, Unknown = 99), na_values = c(9, 99)),
        ordinary = c(9, 99, 9, 99))
    attr(data$ordinary, "labels") <- c(Nine = 9, NinetyNine = 99)
    dictionary <- data.frame(old = c(9, 99), new = c("z", "b"))
    write_sas(data, path, dictionary = dictionary, catalog_file = catalog)
    result <- read_sas(path, catalog_file = catalog)
    expect_identical(as.numeric(result$number), c(1, NA_real_, NA_real_, NA_real_))
    expect_identical(attr(result$number, "na_index"), c("-116" = 2L, "-92" = 3L))
    expect_identical(attr(result$number, "labels"), c(Yes = 1, Refusal = -116, Unknown = -92))
    expect_identical(sas_export_catalog_records(catalog)$DDI1F$keys, list(1, "Z", "B"))
    expect_identical(sas_export_catalog_records(catalog)$DDI2F$keys, list(9, 99))
    expect_identical(as.numeric(result$ordinary), c(9, 99, 9, 99))

    expect_error(write_sas(data, path, dictionary = dictionary[1, , drop = FALSE],
        catalog_file = catalog), "not present in the export dictionary")
})

test_that("catalogs span chained data pages and overflow index pages without losing labels", {
    path <- tempfile()
    catalog <- tempfile()
    on.exit(unlink(c(path, catalog)), add = TRUE)
    data <- setNames(as.data.frame(rep(list(c(1, 2)), 80L)), paste0("v", seq_len(80L)))
    labels <- setNames(as.numeric(seq_len(1000L)), paste0("Label é ", seq_len(1000L)))
    for (index in seq_along(data)) {
        attr(data[[index]], "labels") <- c(Yes = 1, No = 2)
    }
    attr(data[[1]], "labels") <- labels
    attr(data[[1]], "format.sas") <- "FORMAT_NAME_LONGER_THAN_EIGHT"
    write_sas(data, path, catalog_file = catalog)
    result <- read_sas(path, catalog_file = catalog)
    records <- sas_export_catalog_records(catalog)
    expect_length(records, 80L)
    expect_identical(attr(result[[1]], "labels"), labels)
    expect_identical(records$FORMAT_NAME_LONGER_THAN_EIGHT$keys, as.list(unname(labels)))
    expect_identical(records$FORMAT_NAME_LONGER_THAN_EIGHT$labels, names(labels))
    for (index in seq.int(2L, length(data))) {
        expect_identical(attr(result[[index]], "labels"), c(Yes = 1, No = 2))
    }
})

test_that("paired exports reuse identical format definitions and preserve destinations on error", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    data <- data.frame(first = c(1, 2), second = c(2, 1))
    for (name in names(data)) {
        attr(data[[name]], "labels") <- c(Yes = 1, No = 2)
        attr(data[[name]], "format.sas") <- "SHARED"
    }
    attr(data$second, "labels") <- c(No = 2, Yes = 1)
    write_sas(data, path, catalog_file = catalog)
    expect_length(sas_export_catalog_records(catalog), 1L)
    expect_identical(attr(read_sas(path, catalog_file = catalog)$first, "labels"), c(Yes = 1, No = 2))
    write_sas(data, path, catalog_file = catalog)
    original_data <- readBin(path, "raw", n = file.info(path)$size)
    original_catalog <- readBin(catalog, "raw", n = file.info(catalog)$size)
    cases <- list(
        function(x) {
            attr(x$second, "labels") <- c(Yes = 1, Different = 2)

            return(x)
        },
        function(x) {
            attr(x$first, "labels") <- c(Yes = 1, Duplicate = 1)

            return(x)
        },
        function(x) {
            attr(x$first, "labels") <- c(Invalid = Inf)

            return(x)
        },
        function(x) {
            attr(x$first, "labels") <- setNames(1, NA_character_)

            return(x)
        },
        function(x) {
            attr(x$first, "format.sas") <- "BAD-NAME"

            return(x)
        },
        function(x) {
            attr(x$first, "format.sas") <- "BEST12"

            return(x)
        },
        function(x) {
            attr(x$first, "format.sas") <- "DATE"

            return(x)
        },
        function(x) {
            names(x)[2] <- "invalid name"

            return(x)
        },
        function(x) {
            x$second <- as.Date("2000-01-01") + 0:1
            attr(x$second, "labels") <- c(First = 0)
            attr(x$second, "sas_metadata") <- list(informat = "TIME20")

            return(x)
        },
        function(x) {
            x$second <- structure(c("a", "b"), labels = c(A = 1))

            return(x)
        },
        function(x) {
            attr(x$first, "sas_metadata") <- list(width = 3L)
            attr(x$first, "labels") <- c(Unsafe = 8193)

            return(x)
        }
    )
    for (modify in cases) {
        expect_error(write_sas(modify(data), path, catalog_file = catalog))
        expect_identical(readBin(path, "raw", n = file.info(path)$size), original_data)
        expect_identical(readBin(catalog, "raw", n = file.info(catalog)$size), original_catalog)
        expect_identical(sort(list.files(directory, all.files = TRUE, no.. = TRUE)),
            c("data.sas7bcat", "data.sas7bdat"))
    }

    expect_error(write_sas(data, path, catalog_file = path), "distinct")
    expect_error(write_sas(data, path, catalog_file = directory), "distinct")
    expect_error(write_sas(data, path, catalog_file = NA_character_), "file paths")
})

test_that("unused missing-label definitions do not reinterpret ordinary negative values", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    source <- file.path(directory, "source.sas7bdat")
    fixture <- file.path(directory, "fixture.sas7bcat")
    path <- file.path(directory, "out.sas7bdat")
    catalog <- file.path(directory, "out.sas7bcat")
    data <- data.frame(number = c(-116, 1))
    attr(data$number, "format.sas") <- "SRCFMT"
    write_sas(data, source)
    sas_label_catalog(fixture, formats = "SRCFMT", tags = "Z")
    imported <- read_sas(source, catalog_file = fixture)
    expect_null(attr(imported$number, "na_values"))
    expect_null(attr(imported$number, "na_index"))
    expect_identical(as.numeric(imported$number), c(-116, 1))
    write_sas(imported, path, catalog_file = catalog)
    expect_identical(as.numeric(read_sas(path, catalog_file = catalog)$number), c(-116, 1))
    expect_identical(sas_export_catalog_records(catalog)$SRCFMT$keys, list("Z"))
})

test_that("paired catalogs retain zero rows, cleared formats, factor codes and long label text", {
    path <- tempfile()
    catalog <- tempfile()
    on.exit(unlink(c(path, catalog)), add = TRUE)
    text <- paste(rep("é", 16383L), collapse = "")
    data <- data.frame(number = c(0, 1), group = factor(c("yes", "no")),
        ordinary = c("one", "two"), day = as.Date("2000-01-01") + 0:1)
    attr(data$number, "labels") <- setNames(c(0, 1), c("", text))
    attr(data$number, "format.sas") <- "BEST12"
    attr(data$number, "sas_metadata") <- list(format = NULL, width = 3L)
    attr(data$ordinary, "format.sas") <- "DDI1F"
    write_sas(data, path, catalog_file = catalog)
    records <- sas_export_catalog_records(catalog)
    expect_false(is.element("DDI1F", names(records)))
    result <- read_sas(path, catalog_file = catalog)
    expect_identical(attr(result$number, "labels"), attr(data$number, "labels"))
    expect_identical(as.numeric(result$group), as.numeric(data$group))
    expect_identical(attr(result$group, "labels"), c(no = 1, yes = 2))
    expect_identical(as.numeric(result$day), as.numeric(data$day))
    expect_identical(records[[1]]$labels, c("", text))

    empty <- data.frame(number = numeric())
    attr(empty$number, "labels") <- c(Yes = 1, No = 2)
    write_sas(empty, path, catalog_file = catalog)
    expect_identical(nrow(read_sas(path, catalog_file = catalog)), 0L)
    expect_identical(attr(read_sas(path, catalog_file = catalog)$number, "labels"), c(Yes = 1, No = 2))

    write_sas(data.frame(number = 1:2), path, catalog_file = catalog)
    expect_length(sas_export_catalog_records(catalog), 0L)
    expect_null(attr(read_sas(path, catalog_file = catalog)$number, "labels"))
})

test_that("pair publication restores both originals when the second rename fails", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    destinations <- file.path(directory, c("data.sas7bdat", "data.sas7bcat"))
    staged <- file.path(directory, c("staged", "absent"))
    writeBin(charToRaw("original data"), destinations[1])
    writeBin(charToRaw("original catalog"), destinations[2])
    writeBin(charToRaw("replacement data"), staged[1])

    suppressWarnings(expect_error(publish_sas_pair(staged, destinations), "Could not publish"))
    expect_identical(readBin(destinations[1], "raw", n = 100L), charToRaw("original data"))
    expect_identical(readBin(destinations[2], "raw", n = 100L), charToRaw("original catalog"))
    expect_identical(sort(list.files(directory, all.files = TRUE, no.. = TRUE)),
        c("data.sas7bcat", "data.sas7bdat"))
})

test_that("numeric catalogs retain large and subnormal keys without integer projection", {
    path <- tempfile()
    catalog <- tempfile()
    on.exit(unlink(c(path, catalog)), add = TRUE)
    values <- c(.Machine$double.xmax, -.Machine$double.xmax,
        .Machine$double.xmin, 2^-1074, -0)
    data <- data.frame(number = values)
    attr(data$number, "labels") <- setNames(values, paste0("Key", seq_along(values)))
    write_sas(data, path, catalog_file = catalog)
    result <- read_sas(path, catalog_file = catalog)
    expect_identical(as.numeric(result$number), values)
    expect_identical(attr(result$number, "labels"), attr(data$number, "labels"))
    expect_identical(sas_export_catalog_records(catalog)$DDI1F$keys, as.list(values))
})

test_that("stale label tag vectors and invalid format widths preserve existing output", {
    path <- tempfile()
    catalog <- tempfile()
    on.exit(unlink(c(path, catalog)), add = TRUE)
    writeBin(charToRaw("data"), path)
    writeBin(charToRaw("catalog"), catalog)
    data <- data.frame(number = c(1, 2))
    attr(data$number, "labels") <- c(Yes = 1, No = 2)

    for (tags in list("Z", c("", "Z"), c("", "invalid"))) {
        attr(data$number, "sas_metadata") <- list(label_tags = tags)
        expect_error(write_sas(data, path, catalog_file = catalog), "label_tags|tag")
        expect_identical(readBin(path, "raw", n = 100L), charToRaw("data"))
        expect_identical(readBin(catalog, "raw", n = 100L), charToRaw("catalog"))
    }

    attr(data$number, "sas_metadata") <- NULL
    for (format in c("CUSTOM65536", paste0("CUSTOM", strrep("9", 400L)))) {
        attr(data$number, "format.sas") <- format
        expect_error(write_sas(data, path, catalog_file = catalog), "width and decimals")
        expect_identical(readBin(path, "raw", n = 100L), charToRaw("data"))
        expect_identical(readBin(catalog, "raw", n = 100L), charToRaw("catalog"))
    }
})
