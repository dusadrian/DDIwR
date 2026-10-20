sas_label_catalog <- function(path, endian = "little", encoding = 20L,
    text_encoding = "UTF-8", label_suffix = "é",
    formats = c("MISSNUM", "DATE", "DATETIME"), tags = c(".", "_", LETTERS),
    character_labels = NULL, string_offset = 0L, numeric_labels = NULL) {
    put_bytes <- function(bytes, offset, value) {
        bytes[offset + seq_along(value)] <- value

        return(bytes)
    }

    put_integer <- function(bytes, offset, value, size = 4L) {
        return(put_bytes(bytes, offset,
            writeBin(as.integer(value), raw(), size = size, endian = endian)))
    }

    # Construct a 32-bit catalog independently of ReadStat's catalog writer.
    header <- raw(1024L)
    magic <- c(rep(0L, 12L), 0xc2, 0xea, 0x81, 0x63, 0xb3, 0x14, 0x11,
        0xcf, 0xbd, 0x92, 0x08, 0x00, 0x09, 0xc7, 0x31, 0x8c, 0x18, 0x1f, 0x10, 0x11)
    header <- put_bytes(header, 0L, as.raw(magic))
    header[38L] <- as.raw(as.integer(identical(endian, "little")))
    header[71L] <- as.raw(encoding)
    header <- put_bytes(header, 84L, charToRaw("SAS FILE"))
    header <- put_bytes(header, 156L, charToRaw("CATALOG "))
    header <- put_integer(header, 196L, 1024L)
    header <- put_integer(header, 200L, 4096L)
    header <- put_integer(header, 204L, 6L)
    header <- put_bytes(header, 216L, charToRaw("9.0000M0"))
    pages <- raw(6L * 4096L)

    for (format_index in seq_along(formats)) {
        format <- formats[format_index]
        block <- raw(106L)
        block <- put_bytes(block, 8L, charToRaw(sprintf("%-8s", substr(format, 1L, 8L))))
        string_keys <- startsWith(format, "$")
        count <- length(tags)

        if (string_keys) {
            count <- length(character_labels)
            block <- put_integer(block, 104L, string_offset, size = 2L)
        }

        block <- put_integer(block, 38L, count)
        block <- put_integer(block, 42L, count)

        if (nchar(format, type = "bytes") > 8L) {
            block <- put_integer(block, 2L, 128L, size = 2L)
            block <- c(block, charToRaw(sprintf("%-32s", format)))
        }

        labels <- raw()

        for (tag_index in seq_len(count)) {
            entry_length <- 30L
            key <- raw()

            if (string_keys) {
                key <- charToRaw(iconv(character_labels[tag_index], from = "UTF-8",
                    to = text_encoding))
                entry_length <- 22L + string_offset + max(16L, length(key))
            }

            entry <- raw(entry_length)
            entry <- put_integer(entry, 2L, entry_length - 6L, size = 2L)
            entry <- put_integer(entry, 10L, tag_index - 1L)

            if (string_keys) {
                entry <- put_bytes(entry, 22L + string_offset,
                    rep(charToRaw(" "), max(16L, length(key))))
                entry <- put_bytes(entry, 22L + string_offset, key)
                text <- charToRaw(iconv(names(character_labels)[tag_index],
                    from = "UTF-8", to = text_encoding))
            }
            else {
                tag_number <- match(tags[tag_index], LETTERS) + 1L

                if (identical(tags[tag_index], ".")) {
                    tag_number <- 1L
                }
                else if (identical(tags[tag_index], "_")) {
                    tag_number <- 0L
                }

                # Catalog missing keys are big-endian: 00 00 tag FF FF FF FF FF.
                key <- raw(8L)

                if (nzchar(tags[tag_index])) {
                    key <- as.raw(c(0L, 0L, tag_number, rep(255L, 5L)))
                }

                if (!is.null(numeric_labels)) {
                    stopifnot(length(numeric_labels) == length(tags))

                    if (identical(tags[tag_index], "")) {
                        value <- numeric_labels[tag_index]
                        stopifnot(is.finite(value))

                        if (value >= 0) {
                            key <- writeBin(-unname(value), raw(), size = 8L, endian = "big")
                        }
                        else {
                            key <- as.raw(255L - as.integer(writeBin(unname(value), raw(),
                                size = 8L, endian = "big")))
                        }
                    }

                    text <- charToRaw(iconv(names(numeric_labels)[tag_index],
                        from = "UTF-8", to = text_encoding))
                }
                else {
                    text <- charToRaw(iconv(paste(format, "missing", tags[tag_index], label_suffix),
                        from = "UTF-8", to = text_encoding))
                }

                entry <- put_bytes(entry, 22L, key)
            }

            block <- c(block, entry)
            label <- raw(11L + length(text))
            label <- put_integer(label, 8L, length(text), size = 2L)
            label <- put_bytes(label, 10L, text)
            labels <- c(labels, label)
        }

        block <- c(block, labels)
        stopifnot(length(block) + 32L < 4096L)
        index_offset <- 4096L + 856L + (format_index - 1L) * 212L
        pages <- put_bytes(pages, index_offset, charToRaw("XLSR"))
        pages <- put_integer(pages, index_offset + 4L, format_index + 3L)
        pages <- put_integer(pages, index_offset + 8L, 16L, size = 2L)
        pages <- put_bytes(pages, index_offset + 50L, charToRaw("O"))
        block_offset <- (format_index + 2L) * 4096L + 16L
        pages <- put_integer(pages, block_offset + 6L, length(block), size = 2L)
        pages <- put_bytes(pages, block_offset + 16L, block)
    }

    writeBin(c(header, pages), path)

    return(invisible(NULL))
}

sas_label_underscore <- function() {
    bytes <- writeBin(NA_real_, raw(), size = 8L, endian = "little")
    bytes[5L] <- charToRaw("_")

    return(readBin(bytes, "double", n = 1L, size = 8L, endian = "little"))
}

sas_export_catalog_records <- function(path) {
    bytes <- readBin(path, "raw", n = file.info(path)$size)
    endian <- if (bytes[38L] == as.raw(1L)) { "little" } else { "big" }

    take <- function(offset, count) {
        stopifnot(offset >= 0, offset + count <= length(bytes))

        return(bytes[offset + seq_len(count)])
    }

    integer_at <- function(offset, size = 4L) {
        return(readBin(take(offset, size), "integer", n = 1L, size = size,
            signed = size != 2L, endian = endian))
    }

    # Decode the documented fixture layout independently of ReadStat callbacks.
    native <- bytes[33L] == as.raw(0x33)
    alignment <- if (bytes[36L] == as.raw(0x33)) { 4L } else { 0L }
    header <- integer_at(196L + alignment)
    page_size <- integer_at(200L + alignment)
    page_count <- integer_at(204L + alignment)
    index_size <- if (native) { 288L } else { 212L }
    link_size <- if (native) { 32L } else { 16L }
    stopifnot(length(bytes) == header + page_size * page_count)
    pointers <- list()

    for (page in seq.int(1L, page_count - 1L)) {
        offset <- if (page == 1L) { if (native) { 1008L } else { 856L } } else { link_size }
        position <- header + page * page_size + offset

        while (offset + index_size <= page_size &&
            identical(take(position, 4L), charToRaw("XLSR"))) {
            if (!native || take(position + 78L, 1L) == charToRaw("O")) {
                pointers[[length(pointers) + 1L]] <- c(
                    integer_at(position + if (native) { 8L } else { 4L }),
                    integer_at(position + if (native) { 16L } else { 8L }, 2L))
            }

            offset <- offset + index_size
            position <- position + index_size
        }
    }

    records <- list()
    for (pointer in pointers) {
        block <- raw()
        visited <- integer()
        page <- pointer[1L]
        offset <- pointer[2L]

        while (page > 0L) {
            stopifnot(!is.element(page, visited), page <= page_count)
            visited <- c(visited, page)
            position <- header + (page - 1L) * page_size + offset
            block <- c(block, take(position + link_size,
                integer_at(position + if (native) { 10L } else { 6L }, 2L)))
            page <- integer_at(position)
            offset <- integer_at(position + if (native) { 8L } else { 4L }, 2L)
        }

        block_integer <- function(offset, size = 4L) {
            return(readBin(block[offset + seq_len(size)], "integer", n = 1L,
                size = size, signed = size != 2L, endian = endian))
        }

        name <- trimws(rawToChar(block[9:16]), which = "right")
        flags <- block_integer(2L, 2L)
        padding <- if (bitwAnd(flags, 8L) != 0L) { 20L } else { 0L }
        start <- if (native) { 138L + padding } else { 106L + padding }
        long_flag <- if (native) { 32L } else { 128L }

        if (bitwAnd(flags, long_flag) != 0L) {
            name <- trimws(rawToChar(block[start + seq_len(32L)]), which = "right")
            start <- start + 32L
        }

        count <- block_integer(if (native) { 54L } else { 42L })
        keys <- vector("list", count)
        for (index in seq_len(count)) {
            position <- block_integer(start + if (native) { 14L } else { 10L }) + 1L
            key <- block[start + 23:30]

            if (startsWith(name, "$")) {
                string_offset <- block_integer(if (native) { 128L } else { 104L }, 2L)
                entry_size <- 6L + block_integer(start + 2L, 2L)
                value <- rawToChar(block[start + 22L + string_offset +
                    seq_len(entry_size - 22L - string_offset)])
                Encoding(value) <- "UTF-8"
                keys[[position]] <- sub(" +$", "", value)
            }
            else if (all(key[1:2] == as.raw(0L)) && all(key[4:8] == as.raw(255L))) {
                tag <- as.integer(key[3L])
                keys[[position]] <- c("_", ".", LETTERS)[tag + 1L]
            }
            else if (all(key == as.raw(0L))) {
                keys[[position]] <- -0
            }
            else if (as.integer(key[1L]) >= 128L) {
                keys[[position]] <- -readBin(key, "double", n = 1L, endian = "big")
            }
            else {
                keys[[position]] <- readBin(as.raw(255L - as.integer(key)),
                    "double", n = 1L, endian = "big")
            }

            start <- start + 6L + block_integer(start + 2L, 2L)
        }

        labels <- character(count)
        for (index in seq_len(count)) {
            size <- block_integer(start + 8L, 2L)
            labels[index] <- rawToChar(block[start + 10L + seq_len(size)])
            Encoding(labels[index]) <- "UTF-8"
            start <- start + 11L + size
        }

        stopifnot(start == length(block))
        records[[name]] <- list(keys = keys, labels = labels)
    }

    return(records)
}
