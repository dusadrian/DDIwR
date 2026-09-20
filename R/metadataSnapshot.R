# Internal baseline: normalize once, then retain only owned C metadata.
# Value scans and worker execution are deliberately separate implementation steps.
metadataSnapshot <- function(from) {
    if (!is.data.frame(from)) {
        stop("The input must be a data frame.")
    }

    records <- collectRMetadata(from, infer_type = FALSE, include_formats = FALSE)
    pointer <- .Call("metadata_snapshot_create", records, PACKAGE = "DDIwR")

    return(structure(list(pointer = pointer, columns = ncol(from)),
        class = "ddiwr_metadata_snapshot"))
}

metadataSnapshotRead <- function(snapshot, columns = seq_len(snapshot$columns), fields = NULL,
    normalize = FALSE, threads = 1L) {
    if (!inherits(snapshot, "ddiwr_metadata_snapshot")) {
        stop("Invalid metadata snapshot.")
    }

    if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize)) {
        stop("Normalize must be TRUE or FALSE.")
    }

    if (normalize && is.null(snapshot$fields)) {
        stop("Normalization requires a raw metadata snapshot.")
    }

    if (!is.numeric(threads) || length(threads) != 1L || is.na(threads) ||
        !is.element(threads, 1:4)) {
        stop("Threads must be an integer from one to four.")
    }

    if (!is.numeric(columns) || anyNA(columns) ||
        any(!is.finite(columns) | columns != trunc(columns) |
            columns < 1 | columns > snapshot$columns)) {
        stop("Columns must be valid integer positions.")
    }

    available <- c("classes", "label", "measurement", "labels", "na_values",
        "na_range", "xmlang", "ID")

    if (!is.null(snapshot$fields)) {
        available <- snapshot$fields
    }

    if (!is.null(fields) && (!is.character(fields) || anyNA(fields) ||
        any(!is.element(fields, available)) || anyDuplicated(fields))) {
        stop("Fields must be distinct supported metadata names.")
    }

    if (normalize) {
        result <- .Call("metadata_snapshot_normalized", snapshot$pointer,
            as.integer(columns), fields, as.integer(threads), PACKAGE = "DDIwR")
        records <- result[[1]]
        records <- lapply(records, function(record) {
            pending <- names(record)[attr(record, "ddiwr_cleanup_pending")]
            attr(record, "ddiwr_cleanup_pending") <- NULL

            for (field in intersect(c("label", "measurement"), pending)) {
                record[field] <- list(cleanup(record[[field]]))
            }

            if (is.element("labels", pending) && !is.null(record$labels)) {
                labels <- record$labels
                if (is.character(labels)) {
                    labels <- cleanup(labels)
                }
                names(labels) <- cleanup(names(labels))
                record$labels <- labels
            }

            return(record)
        })
    }
    else {
        records <- .Call("metadata_snapshot_read", snapshot$pointer,
            as.integer(columns), fields, PACKAGE = "DDIwR")
    }

    return(records)
}

# ASCII cleanup runs in the R-free kernel; locale-dependent text retains R rules.
metadataNormalizeText <- function(text) {
    if (!is.character(text)) {
        return(cleanup(text))
    }

    result <- .Call("metadata_clean_text", text, PACKAGE = "DDIwR")
    fallback <- result[[2]]
    output <- result[[1]]

    if (any(fallback)) {
        output[fallback] <- cleanup(text[fallback])
    }

    return(output)
}

# Capture stored attributes without cleaning, inference or factor-label synthesis.
# A NULL entry means that attribute was absent; an empty vector stays empty.
metadataSnapshotRaw <- function(from, fields = c("position", "storage", "length",
    "classes", "label", "labels", "levels", "measurement", "nature", "na_values",
    "na_range", "xmlang", "ID", "format.spss", "format.stata", "tzone")) {
    if (!is.data.frame(from)) {
        stop("The input must be a data frame.")
    }

    available <- c("position", "storage", "length", "classes", "label", "labels",
        "levels", "measurement", "nature", "na_values", "na_range", "xmlang",
        "ID", "format.spss", "format.stata", "tzone")

    if (!is.character(fields) || anyNA(fields) || anyDuplicated(fields) ||
        any(!is.element(fields, available))) {
        stop("Fields must be distinct supported raw metadata names.")
    }

    pointer <- .Call("metadata_snapshot_raw", from, fields, PACKAGE = "DDIwR")

    return(structure(list(pointer = pointer, columns = ncol(from), fields = fields),
        class = "ddiwr_metadata_snapshot"))
}

metadataSnapshotClose <- function(snapshot) {
    if (!inherits(snapshot, "ddiwr_metadata_snapshot")) {
        stop("Invalid metadata snapshot.")
    }

    .Call("metadata_snapshot_close", snapshot$pointer, PACKAGE = "DDIwR")

    return(invisible(NULL))
}
