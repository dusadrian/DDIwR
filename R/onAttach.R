.onAttach <- function(...) {

    core <- c("admisc", "declared", "haven")

    # code borrowed from package tidyverse

    # Attach the package from the same package library it was
    # loaded from before. https://github.com/tidyverse/tidyverse/issues/171
    same_library <- function(pkg) {
        if (is.element(pkg, setdiff(loadedNamespaces(), .packages()))) {
            loc <- dirname(getNamespaceInfo(pkg, "path"))
            do.call(
                "library",
                list(
                    pkg,
                    lib.loc = loc,
                    character.only = TRUE,
                    warn.conflicts = FALSE
                )
            )
        }
    }

    core_unloaded <- function() {
        search <- paste0("package:", core)
        return(core[!is.element(search, search())])
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
