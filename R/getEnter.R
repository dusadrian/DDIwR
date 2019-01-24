getEnter <- function(OS) {

    currentOS <- Sys.info()[['sysname']]
    targetOS <- toupper(OS)
    
    if (targetOS == "WINDOWS" | targetOS == "WIN") {
        enter <- ifelse(currentOS == "Windows", "\n", "\r\n")
    }
    else if (targetOS == "LINUX") {
        enter <- "\n"
    }
    else if (targetOS == "DARWIN" | targetOS == "MACOS" | targetOS == "APPLE" | targetOS == "MAC") {
        enter <- ifelse(currentOS == "Darwin", "\n", "\r")
    }
    else {
        cat("\n")
        stop("The specified OS is not supported.\n\n", call. = FALSE)
    }
    
    return(enter)
}
