rule_fixture <- function(name) {
    return(test_path("fixtures", "readstat", name))
}

test_that("native SAS ranges retain boundaries, exclusions, OTHER and defaults", {
    source <- rule_fixture("sas94-m9-rules.sas7bdat")
    catalog <- rule_fixture("sas94-m9-rules.sas7bcat")
    data <- read_sas(source, catalog_file = catalog)
    numeric <- vendorMetadata(data$n, "SAS", "catalog_rules")
    character <- vendorMetadata(data$c, "SAS", "catalog_rules")
    expect_identical(numeric$start, c("LOW", "0", "0", "10", ".", ".A", ""))
    expect_identical(numeric$end, c("0", "0", "10", "HIGH", ".", ".A", ""))
    expect_identical(numeric$hlo, c("L", "", "", "H", "", "", "O"))
    expect_identical(numeric$sexcl, c("N", "N", "Y", "Y", "N", "N", "N"))
    expect_identical(numeric$eexcl, c("Y", rep("N", 6L)))
    expect_identical(numeric$default, rep(24L, 7L))
    expect_equal(numeric$fuzz, rep(1e-12, 7L))
    expect_identical(character$start, c("A", "D", ""))
    expect_identical(character$end, c("D", "F", ""))
    expect_identical(character$default, rep(18L, 3L))
    expect_identical(names(attr(data$n, "labels")), c("zero", "missing", "tagged"))
    expect_null(attr(data$c, "labels"))
    info <- inspectFile(source, catalog_file = catalog)
    expect_identical(info$columns$metadata[[match("n", info$columns$name)]]$sas_metadata$catalog_rules, numeric)
    path <- tempfile(fileext = ".sas7bdat")
    output_catalog <- tempfile(fileext = ".sas7bcat")
    on.exit(unlink(c(path, output_catalog)), add = TRUE)
    convert(data, path, catalog_to = output_catalog)
    output <- read_sas(path, catalog_file = output_catalog)
    expect_identical(vendorMetadata(output$n, "SAS", "catalog_rules"), numeric)
    expect_identical(vendorMetadata(output$c, "SAS", "catalog_rules"), character)
    expect_identical(plain_values(output$n), plain_values(data$n))
    expect_identical(plain_values(output$c), plain_values(data$c))
})

test_that("long character rules and default-only native formats round trip", {
    source <- tempfile(fileext = ".sas7bdat")
    path <- tempfile(fileext = ".sas7bdat")
    catalog <- tempfile(fileext = ".sas7bcat")
    on.exit(unlink(c(source, path, catalog)), add = TRUE)
    fixture <- rule_fixture("sas94-m9-long-rules.sas7bcat")

    for (format in c("$LONGRULE", "$LIMITS", "ONLYOTHER")) {
        data <- data.frame(value = if (startsWith(format, "$")) {
            c("A", "abcdefghijklmnopqrstuw", "漢字abcdefghijklmnopq")
        } else {
            c(1, NA_real_, 2)
        })
        vendorMetadata(data$value, "SAS", "format") <- format
        write_sas(data, source)
        imported <- read_sas(source, catalog_file = fixture)
        rules <- vendorMetadata(imported$value, "SAS", "catalog_rules")
        expect_s3_class(rules, "data.frame")

        if (format == "$LONGRULE") {
            expect_identical(rules$start, c("abcdefghijklmnopqrstuv", "漢字abcdefghijklmnopq", ""))
            expect_identical(rules$end, c("abcdefghijklmnopqrstuz", "漢字abcdefghijklmnopz", ""))
        }
        else if (format == "$LIMITS") {
            expect_identical(rules$hlo, c("L", "H", "O"))
        }
        else {
            expect_identical(rules$label, "anything")
            expect_null(attr(imported$value, "labels"))
        }

        convert(imported, path, catalog_to = catalog)
        output <- read_sas(path, catalog_file = catalog)
        expect_identical(vendorMetadata(output$value, "SAS", "catalog_rules"), rules)
        expect_identical(plain_values(output$value), plain_values(imported$value))
    }
})

test_that("rule edits and NULL clearing select the intended catalog representation", {
    data <- data.frame(value = c(1, 2, 3))
    attr(data$value, "labels") <- c(One = 1)
    rules <- data.frame(start = c("1", "2", ""), end = c("1", "3", ""),
        label = c("exact", "range", "other"), hlo = c("", "", "O"))
    vendorMetadata(data$value, "SAS", "catalog_rules") <- rules
    before <- data
    path <- tempfile(fileext = ".sas7bdat")
    catalog <- tempfile(fileext = ".sas7bcat")
    on.exit(unlink(c(path, catalog)), add = TRUE)
    convert(data, path, catalog_to = catalog)
    imported <- read_sas(path, catalog_file = catalog)
    expect_identical(vendorMetadata(imported$value, "SAS", "catalog_rules")$label, rules$label)
    expect_identical(attr(imported$value, "labels"), c(exact = 1))
    expect_identical(data, before)
    vendorMetadata(data$value, "SAS", "catalog_rules") <- NULL
    convert(data, path, catalog_to = catalog)
    expect_identical(attr(read_sas(path, catalog_file = catalog)$value, "labels"), c(One = 1))
})

test_that("invalid catalog rules fail before publishing either file", {
    data <- data.frame(value = c(1, 2))
    valid <- data.frame(start = c("1", "2"), end = c("1", "3"), label = c("one", "range"))
    invalid <- list(
        data.frame(start = "1", label = NA_character_),
        transform(valid, end = c("0", "3")),
        transform(valid, start = c("1", "1")),
        transform(valid, fuzz = c(0, 1)),
        transform(valid, default = c(12, 20)),
        transform(valid, hlo = c("O", "O")),
        transform(valid, sexcl = c("Y", "N")),
        transform(valid, start = c("NaN", "2"))
    )
    path <- tempfile(fileext = ".sas7bdat")
    catalog <- tempfile(fileext = ".sas7bcat")
    writeLines("data sentinel", path)
    writeLines("catalog sentinel", catalog)
    on.exit(unlink(c(path, catalog)), add = TRUE)

    for (rules in invalid) {
        vendorMetadata(data$value, "SAS", "catalog_rules") <- rules
        expect_error(convert(data, path, catalog_to = catalog))
        expect_identical(readLines(path), "data sentinel")
        expect_identical(readLines(catalog), "catalog sentinel")
    }
})

test_that("rule catalogs retain temporal meaning and share identical definitions", {
    path <- tempfile(fileext = ".sas7bdat")
    catalog <- tempfile(fileext = ".sas7bcat")
    on.exit(unlink(c(path, catalog)), add = TRUE)
    data <- data.frame(day = as.Date("2020-01-01") + 0:2)
    rules <- data.frame(start = c("21915", "21916", ""),
        end = c("21915", "21917", ""), label = c("First", "Later", "Other"),
        hlo = c("", "", "O"))
    vendorMetadata(data$day, "SAS", "catalog_rules") <- rules
    vendorMetadata(data$day, "SAS", "format") <- "DAYRULE"
    data$second <- data$day
    convert(data, path, catalog_to = catalog)
    imported <- read_sas(path, catalog_file = catalog)
    expect_s3_class(imported$day, "Date")
    expect_identical(as.vector(imported$day), as.vector(data$day))
    expect_identical(vendorMetadata(imported$day, "SAS", "informat"), "DATE9")
    expect_identical(vendorMetadata(imported$day, "SAS", "catalog_rules"),
        vendorMetadata(imported$second, "SAS", "catalog_rules"))
    vendorMetadata(data$second, "SAS", "catalog_rules") <- transform(rules,
        label = c("Changed", "Later", "Other"))
    previous <- readBin(path, "raw", file.info(path)$size)
    expect_error(convert(data, path, catalog_to = catalog), "Conflicting")
    expect_identical(readBin(path, "raw", file.info(path)$size), previous)
})

test_that("changing character storage clears incompatible SAS rule tables", {
    data <- data.frame(value = c("A", "B"))
    vendorMetadata(data$value, "SAS") <- list(format = "$RULE", width = 8L,
        catalog_rules = data.frame(start = "A", end = "B", label = "range"),
        unrelated = "retained")
    attr(data$value, "labels") <- c(A = "A", B = "B")
    result <- recodeCharcat(data$value)
    expect_null(vendorMetadata(result, "SAS", "catalog_rules"))
    expect_identical(vendorMetadata(result, "SAS", "unrelated"), "retained")
})

test_that("character LOW bounds sort first and use native padded sentinel bytes", {
    source <- tempfile(fileext = ".sas7bdat")
    path <- tempfile(fileext = ".sas7bdat")
    catalog <- tempfile(fileext = ".sas7bcat")
    on.exit(unlink(c(source, path, catalog)), add = TRUE)
    data <- data.frame(value = c("", "0", "A", "B"))
    vendorMetadata(data$value, "SAS", "format") <- "$LIMITS"
    write_sas(data, source)
    fixture <- rule_fixture("sas94-m9-long-rules.sas7bcat")
    imported <- read_sas(source, catalog_file = fixture)
    convert(imported, path, catalog_to = catalog)
    actual <- readBin(catalog, "raw", file.info(catalog)$size)
    native <- readBin(fixture, "raw", file.info(fixture)$size)
    marker <- charToRaw("$LIMITS ")
    actual_start <- grepRaw(marker, actual, fixed = TRUE)[1L] - 9L + 158L
    native_start <- grepRaw(marker, native, fixed = TRUE)[1L] - 9L + 158L
    expect_identical(actual[actual_start + 8L], as.raw(9L))
    expect_identical(actual[actual_start + 23:54], native[native_start + 23:54])
})
