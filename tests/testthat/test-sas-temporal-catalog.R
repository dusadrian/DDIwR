test_that("paired SAS catalogs preserve temporal classes, units and every missing category", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    missing_labels <- c(System = NA_real_, Underscore = sas_label_underscore(),
        setNames(as.numeric(-91:-116), LETTERS))
    tags <- c(rep("", 4L), ".", "_", LETTERS)
    values <- c(-3654, -3653, 0, 20454, rep(NA_real_, 28L))
    data <- data.frame(
        day = structure(values, class = "Date"),
        moment = structure(c(-315619200.125, -315619200, 0, 1767225600.125,
            rep(NA_real_, 28L)), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
        clock = structure(c(-0.125, 0, 1.25, 90000.5, rep(NA_real_, 28L)),
            class = c("hms", "difftime"), units = "secs")
    )
    informats <- c(day = "DATE9", moment = "DATETIME25.6", clock = "TIME20.6")
    offsets <- c(day = 3653, moment = 315619200, clock = 0)

    for (name in names(data)) {
        keys <- c(setNames(as.numeric(data[[name]])[1:4], c("Before", "SAS epoch", "Zero", "Later é")),
            missing_labels)
        attr(data[[name]], "labels") <- keys
        attr(data[[name]], "na_index") <- structure(6:32,
            names = c("_", as.character(-91:-116)))
        attr(data[[name]], "na_values") <- as.numeric(-91:-116)
        attr(data[[name]], "sas_metadata") <- list(label_tags = tags)
    }

    before <- data
    convert(data, path, catalog_to = catalog)
    expect_identical(data, before)
    result <- read_sas(path, catalog_file = catalog)
    records <- sas_export_catalog_records(catalog)
    inspected <- inspectFile(path, catalog_file = catalog)
    expect_identical(inspected$row_count, 32)

    for (index in seq_along(data)) {
        name <- names(data)[index]
        expected <- attr(data[[name]], "labels")
        expect_identical(as.numeric(result[[name]]), as.numeric(data[[name]]))
        expect_identical(attr(result[[name]], "labels"), expected)
        expect_identical(writeBin(unname(attr(result[[name]], "labels")), raw()),
            writeBin(unname(expected), raw()))
        expect_identical(attr(result[[name]], "na_index"), attr(data[[name]], "na_index"))
        expect_identical(attr(result[[name]], "sas_metadata")$informat, informats[[name]])
        expect_identical(attr(result[[name]], "sas_metadata")$label_tags, tags)
        expect_identical(records[[index]]$keys,
            c(as.list(unname(expected[1:4]) + offsets[[name]]), list(".", "_"), as.list(LETTERS)))
        expect_identical(inspected$columns$metadata[[index]]$labels, expected)
        expect_identical(as.numeric(read_sas(path)[[name]]), as.numeric(data[[name]]))
    }

    expect_s3_class(result$day, "Date")
    expect_s3_class(result$moment, "POSIXct")
    expect_identical(attr(result$moment, "tzone"), "UTC")
    expect_s3_class(result$clock, "hms")
    expect_identical(attr(result$clock, "units"), "secs")

    for (window in list(read_sas(path, catalog_file = catalog, n_max = 0L),
        read_sas(path, catalog_file = catalog, skip = 4L, n_max = 3L))) {
        for (name in names(data)) {
            expect_identical(attr(window[[name]], "labels"), attr(result[[name]], "labels"))
            expect_identical(class(window[[name]]), class(result[[name]]))
        }
    }

    copy <- file.path(directory, "copy.sas7bdat")
    copy_catalog <- file.path(directory, "copy.sas7bcat")
    convert(path, copy, catalog_file = catalog, catalog_to = copy_catalog, encoding = NULL,
        recode = FALSE)
    repeated <- read_sas(copy, catalog_file = copy_catalog)

    for (name in names(data)) {
        expect_identical(as.numeric(repeated[[name]]), as.numeric(result[[name]]))
        expect_identical(attr(repeated[[name]], "labels"), attr(result[[name]], "labels"))
        expect_identical(attr(repeated[[name]], "na_index"), attr(result[[name]], "na_index"))
    }
})

test_that("independent temporal catalogs convert ordinary keys per column and retain sparse tags", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    source <- file.path(directory, "source.sas7bdat")
    fixture <- file.path(directory, "fixture.sas7bcat")
    path <- file.path(directory, "output.sas7bdat")
    catalog <- file.path(directory, "output.sas7bcat")
    # Raw SAS units and independent catalog bytes bypass both temporal export paths.
    raw_values <- c(-1, 0, 3653, 315619200.125, NA_real_, NA_real_, NA_real_)
    data <- data.frame(day = raw_values, moment = raw_values, clock = raw_values)
    informats <- c(day = "date9", moment = "datetime25.6", clock = "time20.6")
    offsets <- c(day = 3653, moment = 315619200, clock = 0)
    keys <- c(Before = -1, Epoch = 0, Date = 3653, Fraction = 315619200.125,
        System = NA_real_, Underscore = sas_label_underscore(), UnusedZ = -116)
    tags <- c(rep("", 4L), ".", "_", "Z")

    for (name in names(data)) {
        attr(data[[name]], "sas_metadata") <- list(format = "SHARED", informat = informats[[name]])
        attr(data[[name]], "na_index") <- c("_" = 6L, "Z" = 7L)
    }

    write_sas(data, source)

    for (endian in c("little", "big")) {
        sas_label_catalog(fixture, endian = endian, formats = "SHARED", tags = tags,
            numeric_labels = keys)

        for (limit in c(-1L, 0L, 3L)) {
            imported <- read_sas(source, catalog_file = fixture, n_max = limit)
            inspected <- inspectFile(source, catalog_file = fixture)

            for (index in seq_along(data)) {
                name <- names(data)[index]
                expected <- keys
                expected[1:4] <- expected[1:4] - offsets[[name]]
                expect_identical(attr(imported[[name]], "labels"), expected)
                expect_identical(inspected$columns$metadata[[index]]$labels, expected)
                expect_identical(attr(imported[[name]], "sas_metadata")$label_tags, tags)
                count <- if (limit < 0L) { nrow(data) } else { limit }
                expect_identical(as.numeric(imported[[name]]),
                    (raw_values - offsets[[name]])[seq_len(count)])
            }

            write_sas(imported, path, catalog_file = catalog)
            repeated <- read_sas(path, catalog_file = catalog)
            expect_identical(sas_export_catalog_records(catalog)$SHARED$keys,
                c(as.list(unname(keys[1:4])), list(".", "_", "Z")))

            for (name in names(data)) {
                expect_identical(attr(repeated[[name]], "labels"), attr(imported[[name]], "labels"))
                expect_identical(as.numeric(repeated[[name]]), as.numeric(imported[[name]]))
            }
        }
    }
})

test_that("integer temporal storage and difftime units use the same data and catalog units", {
    path <- tempfile()
    catalog <- tempfile()
    on.exit(unlink(c(path, catalog)), add = TRUE)
    data <- data.frame(
        day = structure(c(-3653L, 0L, NA_integer_), class = "Date"),
        moment = structure(c(-315619200L, 0L, NA_integer_),
            class = c("POSIXct", "POSIXt"), tzone = "UTC"),
        duration = structure(c(1.25, -0.5, NA_real_), class = "difftime", units = "hours")
    )
    attr(data$day, "labels") <- c(SAS = -3653L, R = 0L, Missing = NA_integer_)
    attr(data$moment, "labels") <- c(SAS = -315619200L, R = 0L, Missing = NA_integer_)
    attr(data$duration, "labels") <- c(Long = 1.25, Negative = -0.5, Missing = NA_real_)
    before <- data
    write_sas(data, path, catalog_file = catalog)
    result <- read_sas(path, catalog_file = catalog)
    records <- sas_export_catalog_records(catalog)

    expect_identical(data, before)
    expect_identical(records$DDI1F$keys, list(0, 3653, "."))
    expect_identical(records$DDI2F$keys, list(0, 315619200, "."))
    expect_identical(records$DDI3F$keys, list(4500, -1800, "."))
    expect_identical(as.numeric(result$day), c(-3653, 0, NA_real_))
    expect_identical(as.numeric(result$moment), c(-315619200, 0, NA_real_))
    expect_identical(as.numeric(result$duration), c(4500, -1800, NA_real_))
    expect_identical(attr(result$duration, "labels"), c(Long = 4500, Negative = -1800, Missing = NA_real_))
    expect_identical(attr(result$duration, "units"), "secs")
    expect_s3_class(result$duration, "hms")
})

test_that("temporal label codes and genuine negative dates remain distinct", {
    path <- tempfile()
    catalog <- tempfile()
    on.exit(unlink(c(path, catalog)), add = TRUE)
    day <- structure(c(-91, 0, NA_real_, NA_real_), class = "Date",
        labels = c(RealDate = -91, Epoch = 0, Refusal = -91, UnusedZ = -116),
        na_index = c("-91" = 3L, "_" = 4L), na_values = -91,
        sas_metadata = list(format = "CALF", label_tags = c("", "", "A", "Z")))
    data <- data.frame(day = day)
    write_sas(data, path, catalog_file = catalog)
    result <- read_sas(path, catalog_file = catalog)
    expect_identical(as.numeric(result$day), c(-91, 0, NA_real_, NA_real_))
    expect_identical(attr(result$day, "labels"), attr(day, "labels"))
    expect_identical(sas_export_catalog_records(catalog)$CALF$keys, list(3562, 3653, "A", "Z"))
    write_sas(result, path, catalog_file = catalog)
    expect_identical(attr(read_sas(path, catalog_file = catalog)$day, "labels"), attr(day, "labels"))

    dictionary <- data.frame(old = c(-91, -116), new = c("b", "y"))
    write_sas(result, path, catalog_file = catalog, dictionary = dictionary)
    changed <- read_sas(path, catalog_file = catalog)
    expect_identical(attr(changed$day, "labels"), c(RealDate = -91, Epoch = 0,
        Refusal = -92, UnusedZ = -115))
    expect_identical(attr(changed$day, "na_index"), c("-92" = 3L, "_" = 4L))
    expect_identical(sas_export_catalog_records(catalog)$CALF$keys, list(3562, 3653, "B", "Y"))
})

test_that("temporal informats preserve metadata precedence and reject unsafe paired exports", {
    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "data.sas7bdat")
    catalog <- file.path(directory, "data.sas7bcat")
    data <- data.frame(day = structure(c(0, 1), class = "Date"))
    attr(data$day, "labels") <- c(Epoch = 0, Tomorrow = 1)
    attr(data$day, "format.sas") <- "DATE9"
    attr(data$day, "sas_metadata") <- list(format = NULL, informat = "E8601DA10", width = 3L)
    write_sas(data, path, catalog_file = catalog)
    result <- read_sas(path, catalog_file = catalog)
    expect_s3_class(result$day, "Date")
    expect_identical(attr(result$day, "sas_metadata")$informat, "E8601DA10")
    expect_identical(attr(result$day, "sas_metadata")$format, "DDI1F")
    expect_identical(attr(result$day, "labels"), attr(data$day, "labels"))
    expect_identical(sas_export_catalog_records(catalog)$DDI1F$keys, list(3653, 3654))
    original_data <- readBin(path, "raw", n = file.info(path)$size)
    original_catalog <- readBin(catalog, "raw", n = file.info(catalog)$size)
    cases <- list(
        function(x) {
            attr(x$day, "sas_metadata") <- list(informat = NULL)

            return(x)
        },
        function(x) {
            attr(x$day, "sas_metadata") <- list(informat = "TIME20")
            x <- clearFormats(x)

            return(x)
        },
        function(x) {
            attr(x$day, "sas_metadata") <- list(format = NULL, informat = "F12.2")

            return(x)
        },
        function(x) {
            # R key 4540 is exact at width 3; its SAS key 8193 is not.
            attr(x$day, "labels") <- c(Unsafe = 4540)

            return(x)
        },
        function(x) {
            x$day <- structure(c(.Machine$double.xmax, 1), class = "difftime", units = "weeks",
                labels = c(One = 1))

            return(x)
        },
        function(x) {
            x$day <- structure(c(1, 2), class = "difftime", units = "invalid",
                labels = c(One = 1))

            return(x)
        },
        function(x) {
            x$day <- structure(c(0, 0.000000001), class = c("POSIXct", "POSIXt"),
                tzone = "UTC", labels = c(First = 0, Second = 0.000000001))

            return(x)
        }
    )

    for (modify in cases) {
        expect_error(write_sas(modify(data), path, catalog_file = catalog))
        expect_identical(readBin(path, "raw", n = file.info(path)$size), original_data)
        expect_identical(readBin(catalog, "raw", n = file.info(catalog)$size), original_catalog)
    }

    # Clear built-in display formats while retaining a compatible informat.
    attr(data$day, "sas_metadata") <- list(format = "DATE9", informat = "YYMMDD10", width = 8L)
    write_sas(clearFormats(data), path, catalog_file = catalog)
    expect_identical(attr(read_sas(path, catalog_file = catalog)$day, "sas_metadata")$informat, "YYMMDD10")

    # A recognized display format takes precedence over a different informat on import.
    raw <- data.frame(day = c(3653, 3654))
    attr(raw$day, "sas_metadata") <- list(format = "DATE9", informat = "TIME20")
    write_sas(raw, path)
    expect_identical(as.numeric(read_sas(path)$day), c(0, 1))
})

test_that("temporal payload tags and duration missing codes bypass unit conversion", {
    path <- tempfile()
    catalog <- tempfile()
    on.exit(unlink(c(path, catalog)), add = TRUE)
    bytes <- writeBin(NA_real_, raw(), size = 8L, endian = "little")
    bytes[5L] <- charToRaw("a")
    tagged <- readBin(bytes, "double", n = 1L, size = 8L, endian = "little")
    data <- data.frame(
        moment = structure(c(0, tagged, sas_label_underscore(), NA_real_),
            class = c("POSIXct", "POSIXt"), tzone = "America/New_York"),
        duration = structure(c(1.25, NA_real_, sas_label_underscore(), NA_real_),
            class = "difftime", units = "days")
    )
    attr(data$moment, "labels") <- c(Epoch = 0, Refusal = tagged,
        Underscore = sas_label_underscore(), System = NA_real_)
    attr(data$duration, "labels") <- c(Day = 1.25, Missing = -91,
        Underscore = sas_label_underscore(), System = NA_real_)
    attr(data$duration, "na_index") <- c("-91" = 2L)
    attr(data$duration, "na_values") <- -91
    write_sas(data, path, catalog_file = catalog)
    result <- read_sas(path, catalog_file = catalog)
    records <- sas_export_catalog_records(catalog)
    expect_identical(records$DDI1F$keys, list(315619200, "A", "_", "."))
    expect_identical(records$DDI2F$keys, list(108000, "A", "_", "."))
    expect_identical(attr(result$moment, "labels"), c(Epoch = 0, Refusal = -91,
        Underscore = sas_label_underscore(), System = NA_real_))
    expect_identical(attr(result$moment, "na_index"), c("-91" = 2L, "_" = 3L))
    expect_identical(as.numeric(result$moment), c(0, NA_real_, NA_real_, NA_real_))
    expect_identical(attr(result$moment, "tzone"), "UTC")
    expect_identical(attr(result$duration, "labels"), c(Day = 108000, Missing = -91,
        Underscore = sas_label_underscore(), System = NA_real_))
    expect_identical(attr(result$duration, "na_index"), c("-91" = 2L, "_" = 3L))
})


test_that("duration labels use missing-position codes even without na_values", {
    path <- tempfile()
    catalog <- tempfile()
    on.exit(unlink(c(path, catalog)), add = TRUE)
    data <- data.frame(duration = structure(c(1.25, NA_real_), class = "difftime",
        units = "hours", labels = c(Long = 1.25, Refusal = 99), na_index = c("99" = 2L)))
    write_sas(data, path, catalog_file = catalog)
    result <- read_sas(path, catalog_file = catalog)
    expect_identical(sas_export_catalog_records(catalog)$DDI1F$keys, list(4500, "A"))
    expect_identical(attr(result$duration, "labels"), c(Long = 4500, Refusal = -91))
    expect_identical(attr(result$duration, "na_index"), c("-91" = 2L))

    write_sas(data, path, catalog_file = catalog,
        dictionary = data.frame(old = 99, new = "z"))
    expect_identical(sas_export_catalog_records(catalog)$DDI1F$keys, list(4500, "Z"))
    expect_identical(attr(read_sas(path, catalog_file = catalog)$duration, "labels"),
        c(Long = 4500, Refusal = -116))
})
