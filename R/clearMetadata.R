#' Clear statistical variable formats and widths
#'
#' @description
#' Removes format or width metadata from a vector, or from every column of a
#' data frame, across the supported statistical vendors.
#'
#' @details
#' `clearFormats()` removes `format.spss`, `format.stata` and `format.sas`,
#' together with the `format` field in `spss_metadata`, `stata_metadata` and
#' `sas_metadata`.
#'
#' `clearWidths()` removes the existing `width` and `display_width` attributes,
#' the `width` and `display_width` fields in `spss_metadata`, and the `width`
#' fields in `stata_metadata` and `sas_metadata`. The existing `width`
#' attribute is shared by the writers, including Stata string storage. Stata's `type` field is retained.
#'
#' Both helpers remove matching list fields even when their value is `NULL`.
#' Clearing the corresponding existing attributes at the same time prevents
#' writer fallback from restoring a cleared format or width. Subsequent
#' exports use the writer's defaults, including formats inferred from temporal
#' classes and widths sufficient for string values.
#'
#' Other metadata is retained, including labels, missing-value definitions,
#' temporal classes and units, SAS informats, SPSS measurement and alignment,
#' and custom attributes. A vendor list is removed when clearing its last
#' field; otherwise its remaining fields, including unrelated `NULL` fields,
#' are preserved. Existing empty lists are left unchanged.
#'
#' For data frames, only column attributes are changed; dataset attributes
#' are retained. List columns are treated as individual vectors without
#' traversing their contents. Malformed vendor lists examined by the helper
#' cause an error. Inputs are not modified; assign the returned object to keep
#' the changes.
#'
#' @param x An R vector or a data frame.
#'
#' @return An object with the same values, class and structure as `x`, with
#' the selected column metadata removed.
#'
#' @examples
#' x <- structure(c(1, 2), format.spss = "F12.3", width = 8L)
#' attr(x, "spss_metadata") <- list(format = "F16.4", measure = "scale")
#' clearFormats(x)
#' clearWidths(x)
#'
#' @seealso [convert()], [inspectFile()]
#'
#' @export
clearFormats <- function(x) {
    return(clear_vendor_fields(x,
        fields = list(
            spss_metadata = "format", stata_metadata = "format",
            sas_metadata = "format"
        ),
        legacy = c("format.spss", "format.stata", "format.sas")
    ))
}

#' @rdname clearFormats
#' @export
clearWidths <- function(x) {
    return(clear_vendor_fields(x,
        fields = list(
            spss_metadata = c("width", "display_width"),
            stata_metadata = "width", sas_metadata = "width"
        ),
        legacy = c("width", "display_width")
    ))
}

clear_vendor_fields <- function(x, fields, legacy) {
    if (is.null(x)) {
        return(x)
    }

    if (!is.atomic(x) && !is.list(x)) {
        stop("x must be an R vector or a data frame.", call. = FALSE)
    }

    clear_column <- function(column, scope) {
        if (!is.atomic(column) && !is.list(column)) {
            stop(scope, " must be an R vector.", call. = FALSE)
        }

        for (attribute in names(fields)) {
            metadata <- vendor_metadata(column, attribute, scope)
            removed <- intersect(names(metadata), fields[[attribute]])

            if (length(removed) == 0L) {
                next
            }

            metadata[removed] <- NULL

            if (length(metadata) == 0L) {
                metadata <- NULL
            }

            attr(column, attribute) <- metadata
        }

        for (attribute in legacy) {
            if (!is.null(attr(column, attribute, exact = TRUE))) {
                attr(column, attribute) <- NULL
            }
        }

        return(column)
    }

    if (is.data.frame(x)) {
        columns <- unclass(x)

        for (index in seq_along(x)) {
            columns[[index]] <- clear_column(x[[index]],
                paste0("Variable '", names(x)[index], "'"))
        }

        attributes(columns) <- attributes(x)
        x <- columns
    }
    else {
        x <- clear_column(x, "Variable")
    }

    return(x)
}
