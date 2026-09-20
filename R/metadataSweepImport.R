#' Import data with a prepared metadata sweep session
#'
#' @description
#' Import a dataset and immediately prepare metadata for a declared consumer
#' profile. The returned session remains reusable for every profile; `prepare`
#' controls only which projection is warm when the import returns.
#'
#' @param from Path to a dataset supported by [convert()].
#' @param revision One non-missing atomic revision token for the imported data.
#' @param prepare One of `"progressive"`, `"dialogr"`, or `"codebook"`.
#' @param columns Integer positions or unambiguous variable names to prepare.
#' `NULL` prepares every column. Progressive consumers should provide their
#' initial visible range.
#' @param fields Optional field groups passed to [metadataSweepRead()].
#' @param xml_options Named arguments passed to the variable XML generator.
#' @param ... Import arguments passed to [convert()]. The import destination is
#' always R; `to` must not be supplied.
#'
#' @return An object with `data`, `session`, and the prepared `projection`.
#' Close `session` with [metadataSweepClose()] when it is no longer needed.
#'
#' @examples
#' \dontrun{
#' imported <- metadataSweepImport(
#'     "survey.sav",
#'     revision = 1L,
#'     prepare = "progressive",
#'     columns = 1:20
#' )
#' metadataSweepClose(imported$session)
#' }
#'
#' @export
metadataSweepImport <- function(from, revision, prepare = "progressive",
    columns = NULL, fields = NULL, xml_options = list(), ...) {
    .metadataSessionValidateRevision(revision)

    profiles <- c("progressive", "dialogr", "codebook")
    if (!is.character(prepare) || length(prepare) != 1L || is.na(prepare) ||
        !is.element(prepare, profiles)) {
        stop("Prepare must be one supported metadata profile.")
    }

    dots <- list(...)
    if (is.element("to", names(dots))) {
        stop("Metadata sweep imports always import to R; do not supply 'to'.")
    }

    data <- do.call(convert, c(list(from = from, to = NULL), dots))
    if (!is.data.frame(data)) {
        stop("The imported object is not a data frame.")
    }

    session <- metadataSweepCapture(data, revision)
    complete <- FALSE

    on.exit({
        if (!complete) {
            metadataSweepClose(session)
        }
    })

    projection <- metadataSweepRead(
        session = session,
        revision = revision,
        columns = columns,
        fields = fields,
        xml_options = xml_options,
        profile = prepare
    )

    result <- structure(
        list(
            data = data,
            session = session,
            projection = projection
        ),
        class = "ddiwr_metadata_import"
    )
    complete <- TRUE

    return(result)
}
