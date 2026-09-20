test_that("metadata sweep imports prepare the requested consumer profile", {
    source <- data.frame(
        choice = declared::declared(
            c(1, 2, 1),
            labels = c(Yes = 1, No = 2),
            label = "Choice"
        ),
        score = c(2, 4, 6)
    )
    path <- tempfile(fileext = ".sav")
    on.exit(unlink(path), add = TRUE)
    write_sav(source, path)

    imported <- metadataSweepImport(
        path,
        revision = "import-1",
        prepare = "progressive",
        columns = 1L
    )
    on.exit(metadataSweepClose(imported$session), add = TRUE)

    expect_s3_class(imported, "ddiwr_metadata_import")
    expect_s3_class(imported$data, "data.frame")
    expect_identical(imported$projection$profile, "progressive")
    expect_identical(imported$projection$columns, 1L)

    dialog <- metadataSweepRead(
        imported$session,
        revision = "import-1",
        columns = 1L,
        profile = "dialogr"
    )

    expect_identical(dialog$profile, "dialogr")
    expect_length(dialog$projection$dialogr, 1L)
})

test_that("metadata sweep imports reject output conversions", {
    expect_error(
        metadataSweepImport(
            tempfile(fileext = ".sav"),
            revision = 1L,
            to = "R"
        ),
        "do not supply 'to'"
    )
})
