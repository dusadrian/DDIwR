# Run with the patched xml2 library first in R_LIBS. Under Linux ASan, preload
# libasan and enable ASAN_OPTIONS=detect_leaks=1 to verify native cleanup.
local({
    if (requireNamespace("DDIwR", quietly = TRUE)) {
        loadNamespace("DDIwR")
    }

    path <- tempfile(fileext = ".xml")
    on.exit(unlink(path), add = TRUE)
    malformed <- c(
        "<root><child></root>",
        "<root><child/></root>trailing",
        "<root attribute='unterminated>"
    )
    readers <- list(
        file = function(bytes) {
            return(xml2::read_xml(path, options = c("HUGE", "NOBLANKS")))
        },
        raw = function(bytes) {
            return(xml2::read_xml(bytes, base_url = path))
        },
        connection = function(bytes) {
            connection <- file(path, "rb")
            on.exit(close(connection), add = TRUE)

            return(xml2::read_xml(connection, base_url = path))
        }
    )

    for (iteration in seq_len(20L)) {
        for (text in malformed) {
            bytes <- charToRaw(text)
            writeBin(bytes, path)

            for (reader in readers) {
                failure <- tryCatch(reader(bytes), error = identity)
                stopifnot(inherits(failure, "error"))
                stopifnot(nzchar(conditionMessage(failure)))
            }
        }
    }

    writeBin(charToRaw('<root><child>café &amp; text</child></root>'), path)

    for (reader in readers) {
        document <- reader(readBin(path, "raw", n = file.info(path)$size))
        stopifnot(identical(xml2::xml_text(document), "café & text"))
    }

    # A warning promoted to an error must also release the active parser.
    tryCatch(withCallingHandlers(
        xml2::read_xml('<root xmlns="relative"><child/></root>'),
        warning = function(warning) {
            stop(conditionMessage(warning), call. = FALSE)
        }
    ), error = function(error) {
        invisible(NULL)
    })
    stopifnot(identical(xml2::xml_name(xml2::read_html("<p>Text</p>")), "html"))
})

gc()
gc()
cat("Parser cleanup regression checks passed.\n")
