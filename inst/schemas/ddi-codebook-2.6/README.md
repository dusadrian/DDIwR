# Bundled offline DDI Codebook 2.6 schema

Source: https://github.com/ddialliance/ddi-c_2/tree/94006d85b995c4013a07cd4e50a66df51aead33e/schemas

Release: DDI Codebook 2.6, publication edition 2026-04-15.
Pinned commit: `94006d85b995c4013a07cd4e50a66df51aead33e`.

Installed as `schemas/ddi-codebook-2.6` in the package.

This directory contains all 29 `.xsd` and `.ent` files from that commit's
`schemas/` directory, preserving its directory layout and copyright/license
notices. This includes the XHTML schema modules and their three entity files,
the Dublin Core schemas, and the XML namespace schema.

The only local changes are the `schemaLocation` attributes on the XML namespace
imports in `dc.xsd` and `dcterms.xsd`: `http://www.w3.org/2001/03/xml.xsd` becomes
`xml.xsd`, using the same bundled schema that `codebook.xsd` already imports.
This removes external resolution and duplicate-location diagnostics without
changing schema declarations or constraints. URLs in documentation, namespace
identifiers, comments, and public entity identifiers remain unchanged.

`UPSTREAM-SHA256SUMS` records hashes of the unmodified upstream files.
`SHA256SUMS` records hashes of the offline copies. Upstream file contents were
also checked against their Git blob IDs before copying. The regression suite
checks both manifests, the exact two local substitutions, and that every active
schema dependency resolves to a bundled file. No test downloads schemas or
skips validation when a fixture is missing.

`test-convert-ddi-schema.R` validates actual files written by `convert()` and
checks that removing the required study description produces the original XSD
failure. These checks are independent of DDIwR's internal content model and its
embedded-data round trip.

## Reproduce the regression proof

From the repository root, the fixed implementation passes:

```sh
Rscript -e 'testthat::test_local(".", filter = "convert-ddi-schema")'
```

The following runs the same tests with the pre-fix `convert()` from commit
`48e325207c927d172300e2cbfd0cfe820974a19a`, in a separate R process. It does not
change the checkout or installed package. Before runtime validation was added,
all four positive output-validation cases rejected `fileDscr` because the
required `stdyDscr` was absent. With the mandatory runtime guard now enabled,
this same pre-fix converter is rejected by `exportCodebook()` before it can
write the invalid document, so the command still exits with a test failure.

```r
pkgload::load_all(".", quiet = TRUE)
source_file <- tempfile(fileext = ".R")
status <- system2("git", c("show",
    "48e325207c927d172300e2cbfd0cfe820974a19a:R/convert.R"), stdout = source_file)
stopifnot(status == 0L)
original <- new.env(parent = globalenv())
sys.source(source_file, envir = original)
unlink(source_file)
environment(original$convert) <- asNamespace("DDIwR")
testthat::test_file("tests/testthat/test-convert-ddi-schema.R",
    env = original, reporter = "summary", stop_on_failure = TRUE)
```

The fixed tests also passed under macOS `sandbox-exec` with `(deny network*)`.

## Other XML writers

`test-export-codebook-schema.R` validates the list, generated-variable, cached
session, and weighted-session paths in `exportCodebook()`, using files and binary
connections with default and custom formatting. It also covers metadata defaults,
the deprecated `exportDDI()` alias, and `updateCodebook()` section replacement in
formatted, compact, and prefixed XML, including repeated empty sections. Tests
check embedded-data preservation alongside XSD validity. Existing translation
and session-export tests also validate their final documents through the shared
`expect_ddi_26_schema()` test helper.

Runtime validation is mandatory in `convert()`, `exportCodebook()` (including
its deprecated alias), and `updateCodebook()`. The finished XML is validated
before destination files, companion CSVs, or connections are written. Invalid
or incomplete documents, missing schemas, and schema-loading diagnostics all
cause an error. Other DDI namespaces are rejected on update rather than silently
rewritten to 2.6. There is no option to bypass validation and no schema download.

The same canonical bundle supports runtime validation and the conformance tests;
the manifest tests check its pinned contents. `test-xml-validation.R` checks
failure-before-write behavior and preservation of existing XML/CSV bytes, output
connections and update inputs, including when a bundled dependency is missing.
