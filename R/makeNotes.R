#' @name makeNotes
#'
#' @title
#' Create a `notes` element for the dataset.
#'
#' @description
#' Create the `notes` element to embed a serialized, gzip-ed version of the data
#' in the `fileDscr` section of the `codeBook`.

#' @return A standard `notes` DDI element.
#'
#' @author Adrian Dusa
#'
#' @param data An R dataframe.
#'
#' @export
`makeNotes` <- function(data) {
    enter <- getEnter(OS = Sys.info()[['sysname']])

    notes <- makeElement("notes")
    addContent(
        paste0(
            enter,
            repeatSpace(3, indent = 2),
            base64enc::base64encode(
                memCompress(serialize(data, NULL), type = "gzip"),
                linewidth = 500,
                newline = paste(enter, repeatSpace(3, indent = 2), sep = "")
            ),
            enter,
            repeatSpace(2, indent = 2)
        ),
        to = notes
    )

    addAttributes(
        c(
            ID = "rawdata",
            level = "file",
            subject = "R dataset, serialized gzip"
        ),
        to = notes
    )

    return(notes)
}
