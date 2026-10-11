# Verify, capture, and update the embedded ReadStat sources using R and Git.

vendor_read_bytes <- function(path) {
    return(readBin(path, what = "raw", n = file.info(path)$size))
}

vendor_write_bytes <- function(path, bytes) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeBin(bytes, path)

    return(invisible(NULL))
}

vendor_git <- function(directory, args, allowed_status = 0L) {
    output <- tempfile("readstat-git-output-")
    errors <- tempfile("readstat-git-errors-")
    on.exit(unlink(c(output, errors)), add = TRUE)

    status <- system2(
        "git",
        args = shQuote(c("-C", directory, args)),
        stdout = output,
        stderr = errors
    )

    if (!is.element(status, allowed_status)) {
        stop(
            "Git failed (", status, "): ",
            rawToChar(vendor_read_bytes(errors)),
            call. = FALSE
        )
    }

    return(list(status = status, output = vendor_read_bytes(output)))
}

vendor_git_text <- function(directory, args) {
    result <- vendor_git(directory, args)

    return(trimws(rawToChar(result$output)))
}

vendor_read_state <- function(directory) {
    records <- read.dcf(file.path(directory, "upstream.dcf"))
    metadata_fields <- c(
        "Repository", "Revision", "Describe", "CommitDate",
        "BootstrapRevision", "BootstrapNote"
    )
    file_fields <- c("File", "Source", "UpstreamSHA256", "VendoredSHA256")

    if (nrow(records) < 2L || !all(is.element(
        c(metadata_fields, file_fields), colnames(records)
    ))) {
        stop("Invalid ReadStat manifest.", call. = FALSE)
    }

    metadata <- records[1L, metadata_fields]
    files <- records[-1L, file_fields, drop = FALSE]

    if (anyNA(metadata) || anyNA(files) || anyDuplicated(files[, "File"])) {
        stop("Incomplete or duplicate ReadStat manifest entries.", call. = FALSE)
    }

    rownames(files) <- files[, "File"]

    return(list(metadata = metadata, files = files))
}

vendor_write_state <- function(directory, state, patch) {
    fields <- c(names(state$metadata), colnames(state$files))
    records <- matrix(
        NA_character_, nrow = nrow(state$files) + 1L, ncol = length(fields),
        dimnames = list(NULL, fields)
    )
    records[1L, names(state$metadata)] <- state$metadata
    records[-1L, colnames(state$files)] <- state$files

    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    write.dcf(records, file = file.path(directory, "upstream.dcf"), width = 1000L)
    vendor_write_bytes(file.path(directory, "ddiwr.patch"), patch)

    return(invisible(NULL))
}

vendor_source_paths <- function(upstream, revision) {
    output <- vendor_git_text(upstream, c("ls-tree", "-r", "--name-only", revision))
    paths <- strsplit(output, "\n", fixed = TRUE)[[1L]]
    selected <- grepl(
        "^src/([^/]+|(?:sas|spss|stata|txt)/[^/]+)[.](c|h|rl)$",
        paths, perl = TRUE
    )
    paths <- paths[selected]
    names(paths) <- sub("^src/", "", paths)
    paths <- c(LICENSE = "LICENSE", NEWS = "NEWS", paths)

    return(paths[order(names(paths))])
}

vendor_upstream_files <- function(upstream, revision, paths) {
    files <- lapply(paths, function(path) {
        result <- vendor_git(upstream, c("show", paste0(revision, ":", path)))

        return(result$output)
    })

    return(files)
}

vendor_write_tree <- function(directory, files) {
    for (name in names(files)) {
        vendor_write_bytes(file.path(directory, name), files[[name]])
    }

    return(invisible(NULL))
}

vendor_hashes <- function(files) {
    return(vapply(files, digest::digest, character(1L), algo = "sha256", serialize = FALSE))
}

vendor_make_state <- function(upstream, revision, paths, base, local, previous) {
    metadata <- previous$metadata
    metadata["Revision"] <- revision
    metadata["Describe"] <- vendor_git_text(upstream, c("describe", "--tags", "--always", revision))
    metadata["CommitDate"] <- vendor_git_text(upstream, c("show", "-s", "--format=%cI", revision))
    files <- cbind(
        File = names(paths),
        Source = unname(paths),
        UpstreamSHA256 = unname(vendor_hashes(base)),
        VendoredSHA256 = unname(vendor_hashes(local))
    )
    rownames(files) <- names(paths)

    return(list(metadata = metadata, files = files))
}

vendor_make_patch <- function(base, local) {
    directory <- tempfile("readstat-diff-")
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)

    vendor_write_tree(file.path(directory, "a"), base)
    vendor_write_tree(file.path(directory, "b"), local)
    result <- vendor_git(directory, c(
        "diff", "--no-index", "--no-ext-diff", "--no-textconv",
        "--src-prefix=", "--dst-prefix=", "--", "a", "b"
    ), allowed_status = c(0L, 1L))

    return(result$output)
}

vendor_read_current <- function(directory, state, check_hashes = TRUE) {
    expected <- rownames(state$files)
    actual <- list.files(directory, recursive = TRUE, all.files = TRUE)
    actual <- actual[
        grepl("[.](c|h|rl)$", actual) | is.element(actual, c("LICENSE", "NEWS"))
    ]

    if (!setequal(actual, expected)) {
        stop(
            "Vendored file inventory changed; review it before updating. Added: ",
            paste(setdiff(actual, expected), collapse = ", "),
            "; removed: ", paste(setdiff(expected, actual), collapse = ", "),
            call. = FALSE
        )
    }

    local <- setNames(lapply(expected, function(name) {
        return(vendor_read_bytes(file.path(directory, name)))
    }), expected)

    if (check_hashes) {
        changed <- expected[
            vendor_hashes(local) != state$files[, "VendoredSHA256"]
        ]

        if (length(changed) > 0L) {
            stop(
                "Uncaptured local edits: ", paste(changed, collapse = ", "),
                ". Review them, then run refresh before updating.", call. = FALSE
            )
        }
    }

    return(local)
}

vendor_verify_patch <- function(base, local, patch) {
    directory <- tempfile("readstat-verify-")
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)

    vendor_write_tree(directory, base)

    if (file.info(patch)$size > 0L) {
        vendor_git(directory, c("apply", "--no-index", "--check", patch))
        vendor_git(directory, c("apply", "--no-index", patch))
    }

    mismatched <- names(local)[!vapply(names(local), function(name) {
        return(identical(vendor_read_bytes(file.path(directory, name)), local[[name]]))
    }, logical(1L))]

    if (length(mismatched) > 0L) {
        stop("Patch does not reproduce: ", paste(mismatched, collapse = ", "), call. = FALSE)
    }

    return(invisible(NULL))
}

vendor_merge_files <- function(base, local, updated) {
    directory <- tempfile("readstat-merge-")
    dir.create(directory)
    on.exit(unlink(directory, recursive = TRUE), add = TRUE)

    merged <- list()
    conflicts <- character()

    for (name in sort(union(names(base), names(updated)))) {
        if (!is.element(name, names(base))) {
            merged[[name]] <- updated[[name]]
        }
        else if (!is.element(name, names(updated))) {
            if (!identical(local[[name]], base[[name]])) {
                conflicts <- c(conflicts, paste0(name, " (upstream deleted a locally modified file)"))
            }
        }
        else if (identical(local[[name]], base[[name]])) {
            merged[[name]] <- updated[[name]]
        }
        else {
            vendor_write_bytes(file.path(directory, "local"), local[[name]])
            vendor_write_bytes(file.path(directory, "base"), base[[name]])
            vendor_write_bytes(file.path(directory, "upstream"), updated[[name]])
            result <- vendor_git(
                directory, c("merge-file", "-p", "local", "base", "upstream"),
                allowed_status = 0:127
            )

            if (result$status != 0L) {
                conflicts <- c(conflicts, name)
            }
            else {
                merged[[name]] <- result$output
            }
        }
    }

    if (length(conflicts) > 0L) {
        stop(
            "Three-way merge needs manual review; no sources were changed: ",
            paste(conflicts, collapse = ", "), call. = FALSE
        )
    }

    return(merged)
}

vendor_script_path <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)

    if (length(file_arg) != 1L) {
        stop("Run this tool with Rscript tools/readstat/vendor.R.", call. = FALSE)
    }

    path <- sub("^--file=", "", file_arg[[1L]])

    # Rscript can encode spaces in its --file argument using this marker.
    if (!file.exists(path)) {
        path <- gsub("~+~", " ", path, fixed = TRUE)
    }

    return(normalizePath(path, mustWork = TRUE))
}

vendor_parse_args <- function(args) {
    usage <- paste(
        "Usage: Rscript tools/readstat/vendor.R <verify|refresh|update>",
        "--upstream <clone> [--revision <commit> (--output <new-directory> | --apply)]"
    )

    if (length(args) == 0L || !is.element(args[[1L]], c("verify", "refresh", "update"))) {
        stop(usage, call. = FALSE)
    }

    options <- list(command = args[[1L]], apply = FALSE)
    index <- 2L

    while (index <= length(args)) {
        option <- args[[index]]

        if (identical(option, "--apply") && !options$apply) {
            options$apply <- TRUE
            index <- index + 1L
        }
        else if (
            is.element(option, c("--upstream", "--revision", "--output")) &&
            index < length(args)
        ) {
            name <- sub("^--", "", option)

            if (!is.null(options[[name]])) {
                stop("Repeated option: ", option, call. = FALSE)
            }

            options[[name]] <- args[[index + 1L]]
            index <- index + 2L
        }
        else {
            stop(usage, call. = FALSE)
        }
    }

    if (is.null(options$upstream)) {
        stop("--upstream must name a local ReadStat clone.", call. = FALSE)
    }

    options$upstream <- normalizePath(options$upstream, mustWork = TRUE)

    if (identical(options$command, "update")) {
        if (is.null(options$revision) || identical(!is.null(options$output), options$apply)) {
            stop("update requires --revision and exactly one of --output or --apply.", call. = FALSE)
        }

        if (!is.null(options$output)) {
            if (file.exists(options$output)) {
                stop("--output must name a directory that does not exist.", call. = FALSE)
            }

            options$output <- normalizePath(options$output, mustWork = FALSE)
        }
    }
    else if (!is.null(options$revision) || !is.null(options$output) || options$apply) {
        stop("--revision, --output and --apply are only valid for update.", call. = FALSE)
    }

    return(options)
}

vendor_main <- function(args = commandArgs(trailingOnly = TRUE)) {
    options <- vendor_parse_args(args)
    root <- normalizePath(file.path(dirname(vendor_script_path()), "../.."), mustWork = TRUE)
    directory <- file.path(root, "src/readstat")
    state_directory <- file.path(root, "tools/readstat")
    patch_path <- file.path(state_directory, "ddiwr.patch")
    state <- vendor_read_state(state_directory)
    paths <- setNames(state$files[, "Source"], rownames(state$files))
    base <- vendor_upstream_files(options$upstream, state$metadata[["Revision"]], paths)

    if (any(vendor_hashes(base) != state$files[, "UpstreamSHA256"])) {
        stop("Upstream fingerprint mismatch.", call. = FALSE)
    }

    local <- vendor_read_current(directory, state, check_hashes = options$command != "refresh")

    if (identical(options$command, "refresh")) {
        refreshed <- vendor_make_state(
            options$upstream, state$metadata[["Revision"]], paths, base, local, state
        )
        patch <- vendor_make_patch(base, local)
        staged_patch <- tempfile("readstat-patch-")
        on.exit(unlink(staged_patch), add = TRUE)
        vendor_write_bytes(staged_patch, patch)
        vendor_verify_patch(base, local, staged_patch)
        vendor_write_state(state_directory, refreshed, patch)
        cat("Captured and verified local edits against ", state$metadata[["Revision"]], "\n", sep = "")

        return(invisible(NULL))
    }

    vendor_verify_patch(base, local, patch_path)

    if (identical(options$command, "verify")) {
        cat("Verified ", length(local), " files: pinned upstream + DDIwR patch = embedded sources\n", sep = "")

        return(invisible(NULL))
    }

    revision <- vendor_git_text(options$upstream, c(
        "rev-parse", "--verify", paste0(options$revision, "^{commit}")
    ))
    updated_paths <- vendor_source_paths(options$upstream, revision)
    updated <- vendor_upstream_files(options$upstream, revision, updated_paths)
    merged <- vendor_merge_files(base, local, updated)
    refreshed <- vendor_make_state(options$upstream, revision, updated_paths, updated, merged, state)
    patch <- vendor_make_patch(updated, merged)

    # Validate the new recipe before either writing a preview or changing sources.
    staged_patch <- tempfile("readstat-patch-")
    on.exit(unlink(staged_patch), add = TRUE)
    vendor_write_bytes(staged_patch, patch)
    vendor_verify_patch(updated, merged, staged_patch)

    if (!is.null(options$output)) {
        vendor_write_tree(file.path(options$output, "src/readstat"), merged)
        vendor_write_state(file.path(options$output, "tools/readstat"), refreshed, patch)
    }
    else {
        removed <- setdiff(names(local), names(merged))
        unlink(file.path(directory, removed))
        vendor_write_tree(directory, merged)
        vendor_write_state(state_directory, refreshed, patch)
    }

    changed <- sum(!vapply(names(merged), function(name) {
        return(identical(updated[[name]], merged[[name]]))
    }, logical(1L)))
    cat("Updated upstream pin: ", revision, "\n", sep = "")
    cat("Files: ", length(merged), "; local changes: ", changed, "\n", sep = "")
    cat("Review the diff and run focused conversion/parallel-reader tests before accepting the update.\n")

    return(invisible(NULL))
}

if (sys.nframe() == 0L) {
    tryCatch(vendor_main(), error = function(error) {
        message(conditionMessage(error))
        quit(status = 1L)
    })
}
