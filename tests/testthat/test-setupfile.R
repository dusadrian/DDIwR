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

    for (extension in c("sps", "do", "sas", "R")) {
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
        "sps complete", "do complete", "sas complete", "R complete", "error recovered"
    ))
})

test_that("validation errors preserve nested caller sinks without extra output", {
    directory <- tempfile("setupfile-nested-sinks-")
    dir.create(directory)
    original_depth <- sink.number(type = "output")
    original_message_sink <- sink.number(type = "message")
    outer_output <- file(file.path(directory, "outer.txt"), "wt")
    inner_output <- file(file.path(directory, "inner.txt"), "wt")
    on.exit({
        while (sink.number(type = "output") > original_depth) {
            sink(type = "output")
        }
        close(inner_output)
        close(outer_output)
        unlink(directory, recursive = TRUE)
    }, add = TRUE)
    survey <- data.frame(choice = declared::declared(c(1, 2), label = "Choice",
        labels = c(Yes = 1, No = 2)))
    codeBook <- getCodebook(survey)
    sink(outer_output)
    cat("outer before\n")
    sink(inner_output)
    caller_depth <- sink.number(type = "output")
    path <- file.path(directory, "survey.sps")
    cases <- list(
        list(arguments = list(codeBook, file = file.path(directory, "survey.unknown")),
            message = "Unknown file type."),
        list(arguments = list(codeBook, file = path, type = "invalid"),
            message = "Incompatible type and file extension."),
        list(arguments = list(codeBook, file = file.path(directory, "survey"), type = "invalid"),
            message = "Unknown destination type."),
        list(arguments = list(1, file = path), message = "Unknown input for the argument <obj>"),
        list(arguments = list(data.frame(choice = 1:2), file = path),
            message = "The object does not contain variables and / or labels.")
    )

    for (index in seq_along(cases)) {
        cat("before ", index, "\n", sep = "")
        expect_error(do.call(setupfile, cases[[index]]$arguments),
            cases[[index]]$message, fixed = TRUE)
        expect_identical(sink.number(type = "output"), caller_depth)
        expect_identical(sink.number(type = "message"), original_message_sink)
        cat("after ", index, "\n", sep = "")
    }

    sink(type = "output")
    expect_identical(sink.number(type = "output"), caller_depth - 1L)
    cat("outer after\n")
    flush(inner_output)
    flush(outer_output)
    expect_identical(readLines(file.path(directory, "inner.txt")),
        unname(as.vector(rbind(paste("before", seq_along(cases)),
            paste("after", seq_along(cases))))))
    expect_identical(readLines(file.path(directory, "outer.txt")),
        c("outer before", "outer after"))
})

test_that("errors during export close only setupfile's own output sink", {
    directory <- tempfile("setupfile-write-error-")
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
    survey <- data.frame(choice = declared::declared(c(1, 9), label = "Choice",
        labels = c(Yes = 1, Missing = 9), na_values = 9))
    codeBook <- getCodebook(survey)
    sink(caller_output)
    caller_depth <- sink.number(type = "output")

    for (extension in c("do", "sas")) {
        path <- file.path(directory, paste0("survey.", extension))
        cat(extension, " before\n", sep = "")
        expect_error(setupfile(codeBook, file = path, dictionary = 1),
            "$ operator is invalid for atomic vectors", fixed = TRUE)
        expect_identical(sink.number(type = "output"), caller_depth)
        expect_true(file.exists(path))
        expect_gt(file.info(path)$size, 0)
        cat(extension, " after\n", sep = "")
    }

    flush(caller_output)
    expect_identical(readLines(file.path(directory, "caller.txt")),
        c("do before", "do after", "sas before", "sas after"))
})
