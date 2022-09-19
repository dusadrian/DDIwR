#' @description Get the carriage return code from the current Operating System
#' @return Character scalar
#' @noRd
`getEnter` <- function(OS) {

    current_os <- Sys.info()[["sysname"]]
    target_os <- toupper(OS)

    if (target_os == "WINDOWS" | target_os == "WIN") {
        enter <- ifelse(current_os == "Windows", "\n", "\r\n")
    }
    else if (target_os == "LINUX") {
        enter <- "\n"
    }
    else if (
        target_os == "DARWIN" |
        target_os == "MACOS" |
        target_os == "APPLE" |
        target_os == "MAC"
    ) {
        enter <- ifelse(current_os == "Darwin", "\n", "\r")
    }
    else {
        admisc::stopError("The specified OS is not supported.")
    }

    return(enter)
}
