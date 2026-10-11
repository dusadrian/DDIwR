# Focused maintainer tests; run with Rscript tools/readstat/test-vendor.R.

script_args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", grep("^--file=", script_args, value = TRUE)[[1L]])

if (!file.exists(script_path)) {
    script_path <- gsub("~+~", " ", script_path, fixed = TRUE)
}

script_path <- normalizePath(script_path, mustWork = TRUE)
source(file.path(dirname(script_path), "vendor.R"))
state_directory <- dirname(script_path)
package_root <- normalizePath(file.path(state_directory, "../.."), mustWork = TRUE)
upstream <- Sys.getenv("READSTAT_UPSTREAM", "")

new_fixture <- function() {
    root <- tempfile("ReadStat R workflow ")
    state_path <- file.path(root, "tools/readstat")
    dir.create(state_path, recursive = TRUE)

    for (name in c("vendor.R", "upstream.dcf", "ddiwr.patch")) {
        if (!file.copy(file.path(state_directory, name), state_path)) {
            stop("Failed to prepare the updater test fixture.")
        }
    }

    state <- vendor_read_state(state_path)
    original <- vendor_read_current(file.path(package_root, "src/readstat"), state)
    vendor_write_tree(file.path(root, "src/readstat"), original)

    return(list(root = root, state_path = state_path, state = state, original = original))
}

run_vendor <- function(fixture, command, args = character()) {
    output <- tempfile("readstat-test-output-")
    errors <- tempfile("readstat-test-errors-")
    on.exit(unlink(c(output, errors)), add = TRUE)

    status <- system2(
        file.path(R.home("bin"), "Rscript"),
        args = shQuote(c(
            file.path(fixture$state_path, "vendor.R"),
            command, "--upstream", upstream, args
        )),
        stdout = output,
        stderr = errors
    )

    return(list(
        status = status,
        output = rawToChar(vendor_read_bytes(output)),
        errors = rawToChar(vendor_read_bytes(errors))
    ))
}

testthat::test_that("three-way merging preserves local and upstream changes", {
    base <- list(file.c = charToRaw("one\ntwo\nthree\nfour\nfive\n"))
    local <- list(file.c = charToRaw("local\ntwo\nthree\nfour\nfive\n"))
    updated <- list(file.c = charToRaw("one\ntwo\nthree\nfour\nupstream\n"))

    testthat::expect_identical(
        vendor_merge_files(base, local, updated),
        list(file.c = charToRaw("local\ntwo\nthree\nfour\nupstream\n"))
    )
})

testthat::test_that("conflicting updates are rejected", {
    base <- list(file.c = charToRaw("original\n"))
    local <- list(file.c = charToRaw("local\n"))
    updated <- list(file.c = charToRaw("upstream\n"))

    testthat::expect_error(
        vendor_merge_files(base, local, updated),
        "no sources were changed", fixed = TRUE
    )
    testthat::expect_identical(local$file.c, charToRaw("local\n"))
})

testthat::test_that("upstream cannot delete a locally modified file", {
    testthat::expect_error(
        vendor_merge_files(
            list(file.c = charToRaw("base\n")),
            list(file.c = charToRaw("local\n")),
            list()
        ),
        "deleted a locally modified file", fixed = TRUE
    )
})

testthat::test_that("patch replay reproduces the embedded sources", {
    testthat::skip_if(!nzchar(upstream), "Set READSTAT_UPSTREAM to a local upstream clone")
    fixture <- new_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)

    result <- run_vendor(fixture, "verify")

    testthat::expect_identical(result$status, 0L, info = result$errors)
    testthat::expect_match(result$output, "pinned upstream + DDIwR patch", fixed = TRUE)
})

testthat::test_that("updates refuse uncaptured edits and refresh preserves them", {
    testthat::skip_if(!nzchar(upstream), "Set READSTAT_UPSTREAM to a local upstream clone")
    fixture <- new_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    path <- file.path(fixture$root, "src/readstat/readstat_iconv.h")
    edited <- c(vendor_read_bytes(path), charToRaw("\n/* Uncaptured local edit. */\n"))
    vendor_write_bytes(path, edited)
    patch_before <- vendor_read_bytes(file.path(fixture$state_path, "ddiwr.patch"))

    rejected <- run_vendor(fixture, "update", c(
        "--revision", fixture$state$metadata[["Revision"]], "--apply"
    ))

    testthat::expect_identical(rejected$status, 1L)
    testthat::expect_match(rejected$errors, "Uncaptured local edits", fixed = TRUE)
    testthat::expect_identical(vendor_read_bytes(path), edited)
    testthat::expect_identical(
        vendor_read_bytes(file.path(fixture$state_path, "ddiwr.patch")), patch_before
    )

    refreshed <- run_vendor(fixture, "refresh")
    verified <- run_vendor(fixture, "verify")

    testthat::expect_identical(refreshed$status, 0L, info = refreshed$errors)
    testthat::expect_identical(verified$status, 0L, info = verified$errors)
    testthat::expect_identical(vendor_read_bytes(path), edited)
})

testthat::test_that("previews leave the checkout unchanged and applying the same pin is safe", {
    testthat::skip_if(!nzchar(upstream), "Set READSTAT_UPSTREAM to a local upstream clone")
    fixture <- new_fixture()
    on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
    output <- file.path(fixture$root, "update preview")
    patch_before <- vendor_read_bytes(file.path(fixture$state_path, "ddiwr.patch"))
    state_before <- vendor_read_bytes(file.path(fixture$state_path, "upstream.dcf"))

    preview <- run_vendor(fixture, "update", c(
        "--revision", fixture$state$metadata[["Revision"]], "--output", output
    ))
    testthat::expect_identical(preview$status, 0L, info = preview$errors)
    testthat::expect_identical(
        vendor_read_current(file.path(fixture$root, "src/readstat"), fixture$state),
        fixture$original
    )
    testthat::expect_identical(
        vendor_read_bytes(file.path(fixture$state_path, "ddiwr.patch")), patch_before
    )
    testthat::expect_identical(
        vendor_read_bytes(file.path(fixture$state_path, "upstream.dcf")), state_before
    )

    preview_state <- vendor_read_state(file.path(output, "tools/readstat"))
    preview_sources <- vendor_read_current(file.path(output, "src/readstat"), preview_state)
    testthat::expect_identical(preview_sources[names(fixture$original)], fixture$original)
    testthat::expect_identical(
        vendor_read_bytes(file.path(output, "tools/readstat/ddiwr.patch")), patch_before
    )

    applied <- run_vendor(fixture, "update", c(
        "--revision", fixture$state$metadata[["Revision"]], "--apply"
    ))
    testthat::expect_identical(applied$status, 0L, info = applied$errors)
    updated_state <- vendor_read_state(fixture$state_path)
    updated_sources <- vendor_read_current(file.path(fixture$root, "src/readstat"), updated_state)
    testthat::expect_identical(updated_sources[names(fixture$original)], fixture$original)
})
