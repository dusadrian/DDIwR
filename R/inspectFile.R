#' Inspect statistical file metadata without importing observations
#'
#' @description
#' Reads column definitions and dataset metadata from SPSS, Stata and SAS files
#' without decoding observations or allocating vectors for data rows.
#'
#' @details
#' Supported extensions are `.sav`, `.zsav`, `.por`, `.dta`, `.sas7bdat` and
#' `.xpt`. The file's original row count is returned when its metadata provides
#' one, including zero for a known empty dataset. Unknown counts are `NA_real_`;
#' observations are never scanned to calculate them. POR and XPT 5 do not store
#' row counts. XPT 8 counts are returned when the observation header supplies
#' one; blank or all-zero reserved fields mean unknown. SAS7BDAT counts exclude
#' rows marked as deleted in the metadata.
#'
#' Column definitions retain source order, ReadStat storage `type` and byte
#' `width`. Types are `"string"`, `"int8"`, `"int16"`, `"int32"`, `"float"`,
#' `"double"` or `"string_ref"`. A `metadata` list column holds each column's R
#' attributes, including labels, missing definitions, formats, temporal classes,
#' vendor metadata and custom attributes. Data-derived attributes such as
#' missing-value row positions are unavailable. Dataset metadata uses the same
#' attribute names as `convert()`, including `label`, `spss_metadata`,
#' `stata_metadata`, `sas_metadata`, `spss.attributes` and `stata.characteristics`.
#'
#' Metadata may occur after observations, so the reader can seek beyond data
#' sections to obtain definitions such as Stata value labels. SAS metadata
#' pages may also contain observation bytes; those pages are read as blocks,
#' but observations are not decoded or decompressed. Inspection does not
#' validate observation content. Supply `catalog_file` to include SAS value
#' labels from a separate format catalog. No catalog is selected automatically.
#'
#' @param file One path to a supported statistical file.
#' @param encoding One character encoding override, or NULL to use file metadata
#' and the reader's fallback.
#' @param catalog_file Optional path to a SAS format catalog, for `.sas7bdat`.
#' @param catalog_encoding Optional catalog encoding override; defaults to
#' `encoding`. Data and catalog overrides follow the same precedence as native
#' SAS import.
#'
#' @return A list with numeric `row_count`, integer `column_count`, a `columns`
#' data frame with `name`, `type`, `width` and `metadata`, and a named list of
#' dataset `metadata`.
#'
#' @examples
#' \dontrun{
#' info <- inspectFile("survey.sav")
#' info$row_count
#' info$columns
#' info$metadata$label
#' }
#'
#' @export
inspectFile <- function(file, encoding = NULL, catalog_file = NULL,
    catalog_encoding = encoding) {
    validate_path <- function(path, argument) {
        if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
            stop(argument, " must be one non-missing file path.", call. = FALSE)
        }

        path <- path.expand(path)

        if (!file.exists(path) || dir.exists(path)) {
            stop(argument, " is not an existing file: ", path, call. = FALSE)
        }

        return(normalizePath(path, mustWork = TRUE))
    }

    file <- validate_path(file, "file")
    extension <- tolower(tools::file_ext(file))
    formats <- c(sav = 0L, zsav = 0L, por = 1L, dta = 2L, sas7bdat = 3L, xpt = 5L)

    if (!is.element(extension, names(formats))) {
        stop("Unsupported statistical file extension: ", extension, call. = FALSE)
    }

    for (argument in c("encoding", "catalog_encoding")) {
        value <- get(argument)

        if (!is.null(value) && (!is.character(value) || length(value) != 1L || is.na(value))) {
            stop(argument, " must be NULL or one non-missing character string.", call. = FALSE)
        }
    }

    catalog <- list()

    if (!is.null(catalog_file)) {
        if (extension != "sas7bdat") {
            stop("catalog_file is supported only for SAS7BDAT files.", call. = FALSE)
        }

        catalog <- list(validate_path(catalog_file, "catalog_file"))
    }

    source <- .Call("ddiwr_inspect_file", list(file), unname(formats[[extension]]),
        encoding, catalog, catalog_encoding, PACKAGE = "DDIwR")
    definitions <- as_declared(source$data, trusted = TRUE)
    columns <- data.frame(name = names(definitions), type = source$types,
        width = source$widths, stringsAsFactors = FALSE)
    columns$metadata <- I(unname(lapply(definitions, attributes)))
    metadata <- attributes(definitions)
    metadata <- metadata[setdiff(names(metadata), c("names", "row.names", "class"))]

    return(list(row_count = source$row_count, column_count = ncol(definitions),
        columns = columns, metadata = metadata))
}
