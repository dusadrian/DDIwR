sas_native_fixture_payloads <- function() {
    bytes <- readBin(test_path("fixtures", "readstat", "sas94-m9-reference.sas7bdat"),
        "raw", n = 262144L)
    # The SAS-created fixture stores its date first, followed by 40 text bytes.
    marker <- c(writeBin(3653, raw(), size = 8L, endian = "little"),
        charToRaw("01"), rep(as.raw(32L), 38L))
    positions <- grepRaw(marker, bytes, fixed = TRUE, all = TRUE)

    if (length(positions) != 1L) {
        stop("The SAS-native fixture must have one unambiguous epoch row.")
    }

    payloads <- lapply(0:4, function(index) {
        offset <- positions + index * 48L

        return(bytes[offset + 0:7])
    })
    names(payloads) <- c("epoch", ".", "_", "A", "Z")

    return(payloads)
}

sas_native_payload_order <- function(bytes, endian = .Platform$endian, width = 8L) {
    # SAS discards the least significant bytes when reducing numeric storage.
    bytes <- bytes[seq.int(9L - width, 8L)]

    if (identical(endian, "big")) {
        bytes <- rev(bytes)
    }

    return(bytes)
}

test_that("native SAS export matches missing payloads produced by SAS itself", {
    fixture <- test_path("fixtures", "readstat", "sas94-m9-reference.sas7bdat")
    catalog <- test_path("fixtures", "readstat", "sas94-m9-reference.sas7bcat")
    data <- read_sas(fixture, catalog_file = catalog)
    payloads <- sas_native_fixture_payloads()
    path <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(path), add = TRUE)
    expected_keys <- c("epoch", ".", "_", "A", "Z")
    strings <- as.character(plain_values(data$text))

    for (writer in list(write_sas, convert)) {
        writer(data, path)
        bytes <- readBin(path, "raw", n = file.info(path)$size)
        endian <- if (bytes[38L] == as.raw(1L)) { "little" } else { "big" }
        matches <- vapply(seq_along(strings), function(index) {
            text <- charToRaw(enc2utf8(strings[index]))
            row <- c(sas_native_payload_order(payloads[[expected_keys[index]]], endian),
                text, rep(as.raw(32L), 40L - length(text)))

            return(length(grepRaw(row, bytes, fixed = TRUE, all = TRUE)))
        }, integer(1))

        expect_identical(matches, rep(1L, 5L), info = "Each complete row must use SAS-native missing bytes")
        imported <- read_sas(path)
        expect_identical(as.numeric(imported$day), as.numeric(data$day))
        expect_identical(attr(imported$day, "na_index"), attr(data$day, "na_index"))
    }
})

test_that("native SAS missing bytes retain every tag at widths three through eight", {
    fixture <- sas_native_fixture_payloads()
    tags <- c(".", "_", LETTERS)
    payloads <- c(fixture[c(".", "_")], setNames(lapply(seq_along(LETTERS), function(index) {
        bytes <- fixture[["A"]]
        bytes[6L] <- as.raw(as.integer(bytes[6L]) - index + 1L)

        return(bytes)
    }), LETTERS))
    expect_identical(payloads[["Z"]], fixture[["Z"]])
    expected_index <- structure(2:28, names = c("_", as.character(-91:-116)))
    columns <- list(
        number = rep(NA_real_, 28L), integer = rep(NA_integer_, 28L),
        day = structure(rep(NA_real_, 28L), class = "Date"),
        integer_day = structure(rep(NA_integer_, 28L), class = "Date"),
        moment = structure(rep(NA_real_, 28L), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
        integer_moment = structure(rep(NA_integer_, 28L), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
        clock = structure(rep(NA_real_, 28L), class = c("hms", "difftime"), units = "secs")
    )
    markers <- sprintf("r%06d!", seq_along(tags))
    path <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(path), add = TRUE)

    for (width in 3:8) {
        for (name in names(columns)) {
            column <- columns[[name]]
            attr(column, "na_index") <- expected_index
            attr(column, "sas_metadata") <- list(width = width)
            data <- data.frame(marker = markers, number = column)
            write_sas(data, path)
            bytes <- readBin(path, "raw", n = file.info(path)$size)
            endian <- if (bytes[38L] == as.raw(1L)) { "little" } else { "big" }
            matches <- vapply(seq_along(tags), function(index) {
                row <- c(sas_native_payload_order(payloads[[tags[index]]], endian, width),
                    charToRaw(markers[index]))

                return(length(grepRaw(row, bytes, fixed = TRUE, all = TRUE)))
            }, integer(1))

            expect_identical(matches, rep(1L, 28L), info = paste(name, "width", width))
            imported <- read_sas(path)
            expect_identical(as.numeric(imported$number), rep(NA_real_, 28L))
            expect_identical(attr(imported$number, "na_index"), expected_index)
        }
    }
})

test_that("SAS import still accepts older ASCII missing payloads", {
    path <- tempfile(fileext = ".sas7bdat")
    on.exit(unlink(path), add = TRUE)
    tags <- c(".", "_", LETTERS)
    data <- data.frame(marker = sprintf("r%06d!", seq_along(tags)), number = rep(NA_real_, 28L))
    attr(data$number, "na_index") <- structure(2:28, names = c("_", as.character(-91:-116)))
    write_sas(data, path)
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    endian <- if (bytes[38L] == as.raw(1L)) { "little" } else { "big" }

    for (index in seq_along(tags)) {
        position <- grepRaw(charToRaw(data$marker[index]), bytes, fixed = TRUE, all = TRUE)
        expect_length(position, 1L)
        legacy <- as.raw(c(0L, 0L, 0L, 0L, 0L, bitwXor(utf8ToInt(tags[index]), 255L), 248L, 127L))

        if (identical(endian, "big")) {
            legacy <- rev(legacy)
        }

        bytes[position - 8:1] <- legacy
    }

    writeBin(bytes, path)
    imported <- read_sas(path)
    expect_identical(as.numeric(imported$number), rep(NA_real_, 28L))
    expect_identical(attr(imported$number, "na_index"), structure(2:28,
        names = c("_", as.character(-91:-116))))
})
