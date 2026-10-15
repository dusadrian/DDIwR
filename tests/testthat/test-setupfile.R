test_that("setupfile accepts labelled data frames and getCodebook metadata", {
    directory <- tempfile("setupfile-")
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    survey <- data.frame(
        age = declared(c(28, 35, 42), label = "Age in years"),
        device = declared(c(1, 2, 1), label = "Device used",
            labels = c(Desktop = 1, Mobile = 2))
    )
    csv <- file.path(directory, "survey.csv")
    do.call(admisc::export, list(undeclare(survey, drop = TRUE), file = csv))
    codeBook <- getCodebook(survey)
    xml <- file.path(directory, "survey.xml")
    convert(survey, to = xml, embed = FALSE)

    for (input in list(survey, codeBook, xml)) {
        path <- file.path(directory, "survey.do")
        setupfile(input, file = path, csv = csv)
        commands <- paste(readLines(path), collapse = "\n")

        expect_match(commands, 'label variable AGE +"Age in years"')
        expect_match(commands, 'label variable DEVICE +"Device used"')
        expect_match(commands, '1 "Desktop" 2 "Mobile"', fixed = TRUE)
        expect_match(commands, "label values DEVICE", fixed = TRUE)
        expect_equal(read.csv(csv), undeclare(survey, drop = TRUE))
    }

    path <- file.path(directory, "survey.sps")
    setupfile(survey, file = path)
    commands <- paste(readLines(path), collapse = "\n")
    expect_match(commands, "VARIABLE LABELS", fixed = TRUE)
    expect_match(commands, "Age in years", fixed = TRUE)
    expect_match(commands, "Desktop", fixed = TRUE)

    path <- file.path(directory, "metadata-only.sps")
    setupfile(codeBook, file = path)
    commands <- paste(readLines(path), collapse = "\n")
    expect_match(commands, "VARIABLE LABELS", fixed = TRUE)
    expect_match(commands, "Age in years", fixed = TRUE)
    expect_match(commands, "Device used", fixed = TRUE)
    expect_match(commands, "Desktop", fixed = TRUE)
    expect_match(commands, "Mobile", fixed = TRUE)
})

test_that("metadata-only SPSS setup handles labelled character variables", {
    directory <- tempfile("setupfile-character-")
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)
    survey <- data.frame(country = declared(c("RO", "RO", NA_character_),
        label = "Country", labels = c(Romania = "RO")))
    codeBook <- getCodebook(survey)
    path <- file.path(directory, "survey.sps")

    setupfile(codeBook, file = path)
    commands <- paste(readLines(path), collapse = "\n")
    expect_match(commands, 'RECODE COUNTRY ("RO" = 1)', fixed = TRUE)
    expect_match(commands, "Romania", fixed = TRUE)
    expect_false(grepl("is completely empty", commands, fixed = TRUE))

    setupfile(codeBook, file = path, stringnum = FALSE)
    commands <- paste(readLines(path), collapse = "\n")
    expect_match(commands, "Romania", fixed = TRUE)
    expect_false(grepl("RECODE COUNTRY", commands, fixed = TRUE))
})

test_that("setupfile preserves caller output sinks on success and error", {
    directory <- tempfile("setupfile-sinks-")
    dir.create(directory)
    original_depth <- sink.number(type = "output")
    caller_output <- file(file.path(directory, "caller.txt"), "wt")
    on.exit({
        while (sink.number(type = "output") > original_depth) {
            sink(type = "output")
        }
        close(caller_output)
        unlink(directory, recursive = TRUE)
    }, add = TRUE)
    survey <- data.frame(
        age = declared(c(28, 35, 42), label = "Age in years"),
        device = declared(c(1, 2, 1), label = "Device used",
            labels = c(Desktop = 1, Mobile = 2))
    )
    codeBook <- getCodebook(survey)
    sink(caller_output)
    caller_depth <- sink.number(type = "output")

    for (extension in c("sps", "do", "sas")) {
        path <- file.path(directory, paste0("survey.", extension))
        setupfile(codeBook, file = path)
        expect_equal(sink.number(type = "output"), caller_depth)
        expect_true(file.exists(path))
        cat(paste0(extension, " complete\n"))
    }

    expect_error(setupfile(codeBook, file = file.path(
        directory, "survey.unknown"
    )))
    expect_equal(sink.number(type = "output"), caller_depth)
    cat("error recovered\n")
    flush(caller_output)
    expect_equal(readLines(file.path(directory, "caller.txt")), c(
        "sps complete", "do complete", "sas complete", "error recovered"
    ))
})
