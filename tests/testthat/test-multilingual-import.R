multilingual_ddi <- function() {
    ddi <- tempfile(fileext = ".xml")

    writeLines(c(
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<codeBook version="2.6" xmlns="ddi:codebook:2_6">',
        '  <dataDscr>',
        '    <var ID="V1" name="answer" files="F1">',
        '      <labl xml:lang="ro">Răspuns la întrebare</labl>',
        '      <labl xml:lang="en">Survey answer</labl>',
        '      <catgry missing="N">',
        '        <catValu>1</catValu>',
        '        <labl xml:lang="ro">Da</labl>',
        '        <labl xml:lang="en">Yes</labl>',
        '      </catgry>',
        '      <catgry missing="Y">',
        '        <catValu>9</catValu>',
        '        <labl xml:lang="ro">Fără răspuns</labl>',
        '        <labl xml:lang="en">No answer</labl>',
        '      </catgry>',
        '      <varFormat type="numeric"/>',
        '    </var>',
        '  </dataDscr>',
        '</codeBook>'
    ), ddi)

    ddi
}


test_that("convert() imports the first DDI label language by default", {
    ddi <- multilingual_ddi()
    on.exit(unlink(ddi), add = TRUE)

    imported <- convert(ddi, csv = data.frame(answer = c(1, 9)))

    expect_equal(
        attr(imported$answer, "label", exact = TRUE),
        "Răspuns la întrebare"
    )
    expect_equal(
        attr(imported$answer, "labels", exact = TRUE),
        c(Da = 1, `Fără răspuns` = 9)
    )
    expect_equal(attr(imported$answer, "na_values", exact = TRUE), 9)
})


test_that("convert() imports only the requested DDI label language", {
    ddi <- multilingual_ddi()
    on.exit(unlink(ddi), add = TRUE)

    imported <- convert(
        ddi,
        csv = data.frame(answer = c(1, 9)),
        language = "en"
    )

    expect_equal(attr(imported$answer, "label", exact = TRUE), "Survey answer")
    expect_equal(
        attr(imported$answer, "labels", exact = TRUE),
        c(Yes = 1, `No answer` = 9)
    )
    expect_equal(length(attr(imported$answer, "labels", exact = TRUE)), 2)
})


test_that("convert() reports unavailable DDI label languages", {
    ddi <- multilingual_ddi()
    on.exit(unlink(ddi), add = TRUE)

    expect_error(
        convert(
            ddi,
            csv = data.frame(answer = 1),
            language = "fr"
        ),
        "Available\\s+languages: ro, en"
    )
})


test_that("convert() honors inherited DDI language declarations", {
    ddi <- tempfile(fileext = ".xml")
    on.exit(unlink(ddi), add = TRUE)

    writeLines(c(
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<codeBook version="2.6" xmlns="ddi:codebook:2_6" xml:lang="en">',
        '  <dataDscr>',
        '    <var ID="V1" name="answer">',
        '      <labl>Survey answer</labl>',
        '      <catgry><catValu>1</catValu><labl>Yes</labl></catgry>',
        '      <varFormat type="numeric"/>',
        '    </var>',
        '  </dataDscr>',
        '</codeBook>'
    ), ddi)

    imported <- convert(
        ddi,
        csv = data.frame(answer = 1),
        language = "en"
    )

    expect_equal(attr(imported$answer, "label", exact = TRUE), "Survey answer")
    expect_equal(attr(imported$answer, "labels", exact = TRUE), c(Yes = 1))
})
