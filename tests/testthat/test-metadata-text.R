test_that("C text cleanup matches ordered R replacements and fallback", {
    text <- c(NA_character_, "", " \t\n hello \r\v\f", "&amp;lt; &amp;amp;",
        "&apos; &quot; \" `", "\\folder\\file", "<![CDATA[ spaced ]]>",
        "&lt;![CDATA[x]]&gt;", "]]<![CDATA[>", "étiquette", "\u00c2\u00b4", "\u2003text\u2003")
    names(text) <- seq_along(text)
    expect_identical(metadataNormalizeText(text), cleanup(text))
    expect_identical(metadataNormalizeText(NULL), cleanup(NULL))
    expect_identical(metadataNormalizeText(1:3), cleanup(1:3))
    expect_identical(metadataNormalizeText(character()), cleanup(character()))

    set.seed(231)
    tokens <- c("&amp;", "&lt;", "&gt;", "&quot;", "&apos;", "<![CDATA[",
        "]]>", " ", "\t", "\n", "`", "\\", "a", "&", ";", "<", ">")
    generated <- replicate(500, paste(sample(tokens, 20, replace = TRUE), collapse = ""))
    expect_identical(metadataNormalizeText(generated), cleanup(generated))
})

test_that("normalized projection preserves the immutable raw snapshot", {
    data <- data.frame(x = 1:3)
    attr(data$x, "label") <- "  A &amp; B  "
    attr(data$x, "measurement") <- " ratio "
    attr(data$x, "labels") <- c(" yes " = 1, " no &amp; more " = 2)
    snapshot <- metadataSnapshotRaw(data)
    on.exit(metadataSnapshotClose(snapshot))
    before <- metadataSnapshotRead(snapshot)
    normalized <- metadataSnapshotRead(snapshot, normalize = TRUE)
    oracle <- collectRMetadata(data, infer_type = FALSE, include_formats = FALSE)

    expect_identical(normalized$x[c("label", "measurement", "labels")],
        oracle$x[c("label", "measurement", "labels")])
    expect_identical(metadataSnapshotRead(snapshot), before)
    expect_identical(metadataSnapshotRead(snapshot, fields = "label", normalize = TRUE),
        list(x = list(label = "A & B")))
    expect_error(metadataSnapshotRead(snapshot, normalize = NA), "TRUE or FALSE")
})

test_that("parallel snapshot normalization preserves order, fallbacks and raw records", {
    data <- as.data.frame(setNames(rep(list(1:3), 40), paste0("v", 1:40)))
    for (i in seq_along(data)) {
        attr(data[[i]], "label") <- " &amp;amp; `test` "
        attr(data[[i]], "labels") <- c(" étiquette " = "&amp;amp;", " &quot; " = "é")
    }
    snapshot <- metadataSnapshotRaw(data)
    on.exit(metadataSnapshotClose(snapshot))
    raw <- metadataSnapshotRead(snapshot)
    serial <- metadataSnapshotRead(snapshot, normalize = TRUE)
    for (threads in 2:4) {
        expect_identical(metadataSnapshotRead(snapshot, normalize = TRUE, threads = threads), serial)
        expect_identical(metadataSnapshotRead(snapshot, c(40, 1, 40), "label",
            normalize = TRUE, threads = threads), serial[c(40, 1, 40)] |> lapply(function(x) x["label"]))
    }
    expect_identical(serial[[1]]$label, cleanup(raw[[1]]$label))
    expect_identical(serial[[1]]$labels, setNames(cleanup(raw[[1]]$labels), cleanup(names(raw[[1]]$labels))))
    expect_identical(metadataSnapshotRead(snapshot), raw)
    expect_length(metadataSnapshotRead(snapshot, integer(), normalize = TRUE, threads = 4), 0)
    expect_error(metadataSnapshotRead(snapshot, normalize = TRUE, threads = 0), "one to four")
    empty <- metadataSnapshotRaw(data, character())
    on.exit(metadataSnapshotClose(empty), add = TRUE)
    expect_identical(metadataSnapshotRead(empty, normalize = TRUE, threads = 4),
        metadataSnapshotRead(empty))
})
