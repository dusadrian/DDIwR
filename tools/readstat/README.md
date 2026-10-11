# Maintaining embedded ReadStat

DDIwR embeds the library sources from WizardMac/ReadStat, with a small local
patch. `upstream.dcf` pins the full upstream commit and SHA-256 fingerprints
for every upstream and embedded file. `ddiwr.patch` records every local source
change. Applying that patch to the pinned sources must reproduce `src/readstat`
byte for byte.

The October 11, 2026 update uses upstream's default branch, **dev**, at
`b05b645661779ce1863d9fa35a84a7542027085a` (`v1.1.9-111-gb05b645`). This is a
development snapshot. The latest tagged release and upstream master remain
1.1.9 (`104ba03a8da116eb8c094abc18bc2530b733eda9`, February 20, 2023).

The original import had no recorded upstream revision. Its baseline was
reconstructed against `495a46bb2cc66019c1a3122f318fe7dc810f0aa8`: 87 of 92
files were identical, with differences only in the five files containing our
iconv adaptation and SPSS/Stata parallel-reader preparation hooks. This is a
reproducible baseline, not proof of the original import's exact commit.

## Local changes

- `readstat_iconv.h`: retain the Windows/Solaris iconv signature adaptation.
- `spss/readstat_sav.h` and `spss/readstat_sav_read.c`: retain SAV stream probing
  and prepared decoder contexts. Initialize encoding conversion after pass 1,
  matching the new upstream parser, and check variable allocation failures.
- `stata/readstat_dta.h` and `stata/readstat_dta_read.c`: retain prepared DTA
  contexts. Provide a local strL lookup adapter so parallel workers share the
  upstream reference decoder/comparator, including format 119 and empty strLs.
- `readstat.h`, `readstat_writer.c`, and `spss/readstat_sav*`: preserve SPSS
  dataset and variable custom attributes in SAV/ZSAV dictionary subtypes 17
  and 18. Metadata callbacks receive a parser-owned `spss_attributes` list;
  consumers that retain it use `readstat_attributes_copy()` and
  `readstat_attributes_free()`. Writers register ordered string arrays with
  `readstat_writer_add_spss_attribute()` before the first row, passing NULL
  for dataset scope or a writer-owned variable for variable scope. Values
  preserve literal quotes, empty strings and trailing spaces. Line feeds
  cannot be represented by this grammar and are rejected. Duplicate names
  within a scope and malformed records fail explicitly.

The custom attribute record grammar follows the [GNU PSPP developer
documentation](https://www.gnu.org/software/pspp/pspp-dev/html_node/Data-File-and-Variable-Attributes-Records.html).
DDIwR exposes the attributes as a named list of character vectors in
`attr(data, "spss.attributes")` and `attr(data$variable, "spss.attributes")`.
The adapters preserve these through serial/parallel reading, export,
missing-value recoding, and declared-vector reconstruction. This explicit
container avoids collisions with R attributes such as `names`, `class` and
`label`; unrelated R attributes are not automatically exported to SPSS.

The package adapters outside `src/readstat` remain ordinary DDIwR code. They
are not overwritten by the updater. The October update also adapts string
width calculation to include restored missing codes and makes parallel Stata
string conversion preserve significant trailing blanks. Stata strL values are
registered before the first data row, as required by the new upstream writer,
and a failed registration now raises an error instead of producing empty data.

New upstream validation rejects invalid missing tags. The former test that
exported hundreds of distinct missing codes from a single Stata variable
accepted an unrepresentable result. DDIwR now rejects more than 26 numeric
missing categories per variable with an explicit error. Multiple variables
can each use their own 26 tags; this limit is separate from dictionary
harmonization across a dataset.

## Workflow

Use R and Git. SHA-256 calculation uses `digest`, which DDIwR already imports;
the manifest uses R's native DCF format. Keep the upstream clone outside the R package:

```sh
git clone https://github.com/WizardMac/ReadStat.git /tmp/ReadStat
Rscript tools/readstat/vendor.R verify --upstream /tmp/ReadStat
```

After intentionally editing an embedded source, inspect the diff and capture
the change before updating. This regenerates the patch and fingerprints
against the existing pin; it does not update upstream:

```sh
Rscript tools/readstat/vendor.R refresh --upstream /tmp/ReadStat
Rscript tools/readstat/vendor.R verify --upstream /tmp/ReadStat
```

Deliberate additions or removals of local source files require a separate
inventory review. The tool currently refuses such inventory drift rather than
silently dropping a file.

For an update, fetch upstream and choose an explicit commit or tag. Do not
assume master contains the newest development changes. First create a preview
in a directory that does not exist:

```sh
git -C /tmp/ReadStat fetch origin --tags
Rscript tools/readstat/vendor.R update --upstream /tmp/ReadStat \
    --revision <commit-or-tag> --output /tmp/readstat-preview
```

The tool verifies the current inventory, fingerprints, and patch replay, then
performs a three-way merge of old upstream, our local sources, and new
upstream. It refuses uncaptured edits, conflicting hunks, or upstream deletion
of a locally modified file before changing package sources. The preview
contains the merged sources, new pin, and rebased patch.

Review the preview against `src/readstat`, including upstream deletions and
additions. New or removed C files may require changes to `src/Makevars.in`
and `src/Makevars.win`. Then apply the same revision:

```sh
Rscript tools/readstat/vendor.R update --upstream /tmp/ReadStat \
    --revision <same-commit> --apply
Rscript tools/readstat/vendor.R verify --upstream /tmp/ReadStat
READSTAT_UPSTREAM=/tmp/ReadStat Rscript tools/readstat/test-vendor.R
Rscript -e 'testthat::test_local(".", filter = "^(convert|readstat-update|metadata-import|spss-attributes)$")'
```

A clean text merge does not prove compatibility: preparation hooks call
upstream internals, and the parallel adapter has its own row processing.
Review upstream changes affecting these paths and compare full serial/parallel
results. Capture any necessary local adaptations with `refresh`, then verify
again. Do not weaken upstream validation merely to retain acceptance of an
invalid output file.

The SPSS attribute regressions include independently constructed dictionary
records and byte-level checks against the documented grammar, legacy encoding
conversion, all supported SAV compression modes, row windows, empty data,
Unicode long strings, and complete serial/parallel comparisons. They do not
establish interoperability with an installed IBM SPSS application.

This tooling is tracked for development and excluded from R source builds.
No Git commit or network fetch is performed by the updater itself.
