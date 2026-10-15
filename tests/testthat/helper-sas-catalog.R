sas_label_catalog <- function(path, endian = "little", encoding = 20L,
    text_encoding = "UTF-8", label_suffix = "é") {
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
    tags <- c(".", "_", LETTERS)
    formats <- c("MISSNUM", "DATE", "DATETIME")

    for (format_index in seq_along(formats)) {
        format <- formats[format_index]
        block <- raw(106L)
        block <- put_bytes(block, 8L, charToRaw(sprintf("%-8s", format)))
        block <- put_integer(block, 38L, length(tags))
        block <- put_integer(block, 42L, length(tags))
        labels <- raw()

        for (tag_index in seq_along(tags)) {
            entry <- raw(30L)
            entry <- put_integer(entry, 2L, 24L, size = 2L)
            entry <- put_integer(entry, 10L, tag_index - 1L)
            tag_number <- tag_index - 1L

            if (identical(tags[tag_index], ".")) {
                tag_number <- 1L
            }
            else if (identical(tags[tag_index], "_")) {
                tag_number <- 0L
            }

            # Catalog missing keys are big-endian: 00 00 tag FF FF FF FF FF.
            entry <- put_bytes(entry, 22L, as.raw(c(0L, 0L, tag_number, rep(255L, 5L))))
            block <- c(block, entry)
            text <- charToRaw(iconv(paste(format, "missing", tags[tag_index], label_suffix),
                from = "UTF-8", to = text_encoding))
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
