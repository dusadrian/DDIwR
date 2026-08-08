args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)

script_path <- if (length(file_arg) > 0) {
    normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE)
} else {
    normalizePath(file.path("tools", "build-cran-source.R"), mustWork = TRUE)
}

package_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
output_dir <- package_root

user_args <- commandArgs(trailingOnly = TRUE)
if (length(user_args) > 0) {
    output_dir <- normalizePath(user_args[[1]], mustWork = FALSE)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

description_path <- file.path(package_root, "DESCRIPTION")
description <- read.dcf(description_path)
package_name <- description[[1, "Package"]]
package_version <- description[[1, "Version"]]

staging_parent <- tempfile("DDIwR-cran-source-")
staging_root <- file.path(staging_parent, package_name)
dir.create(staging_root, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(staging_parent, recursive = TRUE, force = TRUE), add = TRUE)

entries <- list.files(package_root, all.files = TRUE, no.. = TRUE, full.names = TRUE)
entries <- entries[!basename(entries) %in% c(".git", ".Rproj.user")]
entries <- entries[!grepl("[.]tar[.]gz$", basename(entries))]
entries <- entries[!grepl("[.]Rcheck$", basename(entries))]

copied <- file.copy(entries, staging_root, recursive = TRUE, copy.date = TRUE)
if (!all(copied)) {
    stop(
        "Failed to copy these paths into the CRAN staging directory: ",
        paste(basename(entries[!copied]), collapse = ", ")
    )
}

buildignore_path <- file.path(staging_root, ".Rbuildignore")
buildignore <- readLines(buildignore_path, warn = FALSE)
if (!"^tests$" %in% buildignore) {
    writeLines(c(buildignore, "^tests$"), buildignore_path)
}

staged_description_path <- file.path(staging_root, "DESCRIPTION")
staged_description <- read.dcf(staged_description_path)

if ("Suggests" %in% colnames(staged_description)) {
    suggests <- trimws(strsplit(staged_description[[1, "Suggests"]], ",", fixed = TRUE)[[1]])
    suggests <- suggests[
        !grepl("^(spelling|testthat)(\\s*\\(|$)", suggests)
    ]

    if (length(suggests) == 0) {
        staged_description <- staged_description[
            , colnames(staged_description) != "Suggests", drop = FALSE
        ]
    } else {
        staged_description[[1, "Suggests"]] <- paste(suggests, collapse = ", ")
    }
}

staged_description <- staged_description[
    , colnames(staged_description) != "Config/testthat/edition", drop = FALSE
]

write.dcf(staged_description, staged_description_path)

old_working_directory <- setwd(staging_parent)
on.exit(setwd(old_working_directory), add = TRUE)

build_status <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "build", shQuote(staging_root)),
    stdout = "",
    stderr = ""
)

if (!identical(build_status, 0L)) {
    stop("R CMD build failed with exit status ", build_status)
}

tarball <- file.path(
    staging_parent,
    paste0(package_name, "_", package_version, ".tar.gz")
)
target <- file.path(output_dir, basename(tarball))

if (!file.copy(tarball, target, overwrite = TRUE)) {
    stop("Failed to copy the CRAN source package to ", target)
}

message("Built CRAN source package: ", normalizePath(target, mustWork = TRUE))
