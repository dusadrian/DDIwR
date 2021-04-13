.onAttach <- function(...) {
    core <- c("haven", "labelled", "admisc")
    
    # code borrowed from package tidyverse
    
    # Attach the package from the same package library it was
    # loaded from before. https://github.com/tidyverse/tidyverse/issues/171
    same_library <- function(pkg) {
        loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
        do.call(
            "library",
            list(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
        )
    }

    core_unloaded <- function() {
        search <- paste0("package:", core)
        core[!search %in% search()]
    }
    
    to_load <- core_unloaded()
    if (length(to_load) == 0) {
        return(invisible())
    }

    packageStartupMessage(
        paste("Also attaching packages:", paste(to_load, collapse = ", "))
    )

    suppressPackageStartupMessages(
        lapply(to_load, same_library)
    )

    return(invisible())

}
