test_that("SAS missing labels survive import, RDS and export using the original catalog", {
    values <- c(0, NA_real_, rep(NA_real_, 27L))
    data <- data.frame(
        number = values,
        day = structure(values, class = "Date"),
        moment = structure(values + 0.125, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )
    formats <- c(number = "MISSNUM", day = "DATE", moment = "DATETIME")
    expected_index <- structure(3:29, names = c("_", as.character(-91:-116)))

    for (name in names(data)) {
        attr(data[[name]], "na_index") <- expected_index
        attr(data[[name]], "format.sas") <- formats[[name]]
    }

    directory <- tempfile()
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    path <- file.path(directory, "source.sas7bdat")
    catalog <- file.path(directory, "source.sas7bcat")
    rds_path <- file.path(directory, "source.rds")
    repeated_path <- file.path(directory, "repeated.sas7bdat")
    write_sas(data, path)

    for (endian in c("little", "big")) {
        sas_label_catalog(catalog, endian = endian)
        imported <- read_sas(path, catalog_file = catalog)
        window <- read_sas(path, catalog_file = catalog, skip = 2L, n_max = 2L)
        empty <- read_sas(path, catalog_file = catalog, n_max = 0L)
        convert(path, to = rds_path)
        saved <- readRDS(rds_path)
        write_sas(imported, repeated_path)
        repeated <- read_sas(repeated_path, catalog_file = catalog)

        for (result in list(imported, saved, repeated)) {
            for (name in names(data)) {
                expected_labels <- c(NA_real_, sas_label_underscore(), as.numeric(-91:-116))
                names(expected_labels) <- paste(formats[[name]], "missing", c(".", "_", LETTERS), "é")
                labels <- attr(result[[name]], "labels")
                expect_identical(labels, expected_labels, info = endian)
                # R's identical() treats different NA payloads as equal.
                expect_identical(writeBin(unname(labels), raw(), size = 8L, endian = "little"),
                    writeBin(unname(expected_labels), raw(), size = 8L, endian = "little"))
                expect_identical(attr(result[[name]], "na_index"), expected_index)
                expect_identical(as.numeric(result[[name]]), as.numeric(data[[name]]))
            }

            expect_s3_class(result$day, "Date")
            expect_s3_class(result$moment, "POSIXct")
        }

        for (result in list(window, empty)) {
            for (name in names(data)) {
                expect_identical(attr(result[[name]], "labels"), attr(imported[[name]], "labels"))
                expect_identical(writeBin(unname(attr(result[[name]], "labels")), raw()),
                    writeBin(unname(attr(imported[[name]], "labels")), raw()))
            }
        }
    }
})

test_that("standalone SAS and XPT exports do not carry value-label definitions", {
    values <- c(0, NA_real_, rep(NA_real_, 27L))
    data <- data.frame(
        number = values,
        day = structure(values, class = "Date"),
        moment = structure(values, class = c("POSIXct", "POSIXt"), tzone = "UTC")
    )

    for (name in names(data)) {
        attr(data[[name]], "na_index") <- structure(3:29, names = c("_", as.character(-91:-116)))
        labels <- c(NA_real_, sas_label_underscore(), as.numeric(-91:-116))
        names(labels) <- paste("Missing", c(".", "_", LETTERS))
        attr(data[[name]], "labels") <- labels
    }

    paths <- character()
    on.exit(unlink(paths), add = TRUE)

    for (format in c("sas7bdat", "xpt-5", "xpt-8")) {
        path <- tempfile()
        paths <- c(paths, path)

        if (identical(format, "sas7bdat")) {
            write_sas(data, path)
            imported <- read_sas(path)
        }
        else {
            version <- as.integer(sub("xpt-", "", format, fixed = TRUE))
            write_xpt(data, path, version = version, name = "LABELS", adjust_tz = FALSE)
            imported <- read_xpt(path)
        }

        for (name in names(data)) {
            expect_null(attr(imported[[name]], "labels"))
        }
    }
})
