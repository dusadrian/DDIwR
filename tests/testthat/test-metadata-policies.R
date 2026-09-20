test_that("stored application fixture preserves distinct interpretation inputs", {
    fixture <- dget(test_path("fixtures", "metadata-policies.R"))
    snapshot <- metadataSnapshotRaw(fixture$data,
        c("measurement", "nature", "levels", "labels", "na_values"))
    on.exit(metadataSnapshotClose(snapshot))
    raw <- metadataSnapshotRead(snapshot)

    expect_identical(raw$explicit$measurement, "Unspecified, Ratio")
    expect_identical(raw$explicit$nature, "ordinal")
    expect_identical(raw$ordered$levels, levels(fixture$data$ordered))
    expect_null(raw$ordered$labels)
    expect_identical(raw$labelled$labels, attr(fixture$data$labelled, "labels"))
    expect_identical(raw$labelled$na_values, 99)

    # Frozen outputs came from the actual application functions, not replicas.
    expect_identical(fixture$dialog_measure$explicit, "ratio")
    expect_identical(fixture$publisher_measure[[5]]$measure, "")
    expect_identical(lapply(fixture$data, checkType), fixture$ddi_type)
    expect_identical(collectRMetadata(fixture$data, infer_type = FALSE,
        include_formats = FALSE), fixture$ddi_metadata)
})
