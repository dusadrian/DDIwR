#' Read and edit vendor-specific metadata
#'
#' @description
#' Accesses statistical metadata without editing attributes directly. Replacement
#' returns a modified object and preserves unrelated metadata and values.
#'
#' @details
#' `vendor` is `"SPSS"`, `"Stata"` or `"SAS"` (case insensitive). With no
#' `field`, the getter returns the stored vendor list. With a field, it returns
#' that field, falling back to legacy format/width/display-width/notes attributes
#' only when the field is absent. A stored NULL suppresses that fallback.
#'
#' Replacing one field with NULL stores an explicit clearing marker; it does not
#' delete the field. Replacing the whole list with NULL removes that container,
#' allowing legacy fallback again. Use [clearFormats()] or [clearWidths()] to
#' remove the corresponding fields and legacy attributes together. Replacement
#' validates list structure; field values are validated by the destination writer.
#' Dataset and column metadata use the same interface. The function does not
#' traverse data-frame columns: select a column explicitly to edit its metadata.
#'
#' @param x An R vector or data frame.
#' @param vendor Statistical vendor.
#' @param field One field name, or NULL for the whole list.
#' @param value Replacement field value or named metadata list.
#' @return The metadata list or field; replacement returns the modified object.
#' @examples
#' x <- c(1, 2)
#' vendorMetadata(x, "SAS", "format") <- "BEST12."
#' vendorMetadata(x, "SAS", "informat") <- "BEST32."
#' vendorMetadata(x, "SAS", "format")
#' vendorMetadata(x, "SAS", "format") <- NULL
#' @export
vendorMetadata <- function(x, vendor, field = NULL) {
    attribute <- vendor_metadata_arguments(x, vendor, field)
    metadata <- vendor_metadata(x, attribute)

    if (is.null(field)) {
        return(metadata)
    }

    legacy <- switch(field,
        format = switch(attribute,
            spss_metadata = "format.spss", stata_metadata = "format.stata",
            sas_metadata = "format.sas"),
        width = "width",
        display_width = {
            if (attribute == "spss_metadata") {
                "display_width"
            }
        },
        notes = {
            if (is.element(attribute, c("spss_metadata", "stata_metadata"))) {
                "notes"
            }
        }
    )

    return(effective_vendor_metadata(x, attribute, field, legacy))
}

#' @rdname vendorMetadata
#' @export
`vendorMetadata<-` <- function(x, vendor, field = NULL, value) {
    attribute <- vendor_metadata_arguments(x, vendor, field)
    if (is.null(field)) {
        candidate <- x
        attr(candidate, attribute) <- value
        vendor_metadata(candidate, attribute)
        metadata <- value
    }
    else {
        metadata <- vendor_metadata(x, attribute)
        metadata[field] <- list(value)
    }

    attr(x, attribute) <- metadata

    return(x)
}

vendor_metadata_arguments <- function(x, vendor, field) {
    if (is.null(x) || (!is.atomic(x) && !is.list(x))) {
        stop("x must be an R vector or a data frame.", call. = FALSE)
    }

    if (!is.character(vendor) || length(vendor) != 1L || is.na(vendor) ||
        !is.element(toupper(vendor), c("SPSS", "STATA", "SAS"))) {
        stop("vendor must be 'SPSS', 'Stata' or 'SAS'.", call. = FALSE)
    }

    if (!is.null(field) && (!is.character(field) || length(field) != 1L ||
        is.na(field) || !nzchar(field))) {
        stop("field must be one nonempty name or NULL.", call. = FALSE)
    }

    return(switch(toupper(vendor), SPSS = "spss_metadata",
        STATA = "stata_metadata", SAS = "sas_metadata"))
}
