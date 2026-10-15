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
- `readstat.h`, `readstat_parser.c`, `readstat_writer.c`, and
  `stata/readstat_dta_read.c` / `stata/readstat_dta_write.c`: preserve dataset
  and variable characteristics in legacy type-1 expansion fields and modern
  `<ch>` records. The new characteristic callback runs after variable
  callbacks, with NULL for dataset scope and a parser-owned variable otherwise.
  The callback strings are temporary; `readstat_characteristic_add()` copies
  them to an owned list, released with `readstat_attributes_free()`. Names
  are case sensitive and each characteristic has one string value.
  Writers register characteristics with `readstat_writer_add_characteristic()`
  before the first row. Modern file-map sizing includes all characteristics.
  Name widths, value lengths, string terminators and variable scopes are
  checked; malformed records fail explicitly. The R adapter encodes legacy
  characteristic values as Windows-1252 before registering them, matching the
  legacy reader's default encoding. Direct C callers use the target file's
  encoding, as with the other legacy writer metadata.

The custom attribute record grammar follows the [GNU PSPP developer
documentation](https://www.gnu.org/software/pspp/pspp-dev/html_node/Data-File-and-Variable-Attributes-Records.html).
DDIwR exposes the attributes as a named list of character vectors in
`attr(data, "spss.attributes")` and `attr(data$variable, "spss.attributes")`.
The adapters preserve these through serial/parallel reading, export,
missing-value recoding, and declared-vector reconstruction. This explicit
container avoids collisions with R attributes such as `names`, `class` and
`label`; unrelated R attributes are not automatically exported to SPSS.

DDIwR exposes Stata characteristics as named lists of single strings in
`attr(data, "stata.characteristics")` and
`attr(data$variable, "stata.characteristics")`. The representations remain
separate: SPSS attributes are not automatically translated into Stata
characteristics or vice versa. Stata note characteristics are included in
full, preserving `note0`, gaps in numbering, variable notes and literal
contents. Dataset `notes` remains a convenience view. When raw dataset note
characteristics exist, they take precedence during export; otherwise the
adapter writes dataset `notes` through ReadStat's note API. The low-level
writer rejects simultaneous registration of raw dataset note characteristics
and generated notes to prevent duplicate records.

See Stata's [characteristics manual](https://www.stata.com/manuals/pchar.pdf)
and [DTA format documentation](https://www.stata.com/help.cgi?dta).

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
Rscript -e 'testthat::test_local(".", filter = "^(convert|recodeMissings|readstat-update|metadata-import|spss-attributes|stata-characteristics|stata-datetime|spss-sas-datetime|sas-underscore|sas-missing-labels|sas-catalog-encoding)$")'
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

The Stata regressions include independently constructed characteristic records
for formats 105, 108, 113, 115, 117, 118 and 119; byte-level record and file-map
checks; legacy encoding; case-sensitive variable and characteristic names;
raw notes; empty and windowed imports; categorical recoding; and complete
serial/parallel comparisons with strL values. They do not establish
interoperability with an installed Stata application. Date-column checks
assert preservation of the original dates. The datetime regressions additionally
cover epoch and millisecond conversion, system and extended missing values,
and storage boundaries in the DDIwR writer adapter. Storage selection uses
the converted Stata values, including for integer-backed R dates. These
adapter changes are outside the vendored ReadStat tree and its patch.

The SPSS/SAS datetime regressions cover integer-backed temporal columns,
int32 boundaries, epoch and fractional-second conversion, system and
user-missing values, and all-missing columns. They exercise every SAV
compression mode, native SAS7BDAT, and XPT versions 5 and 8. Explicit numeric
formats check stored date/time units without applying the reader's epoch
conversion; a numeric SPSS fixture also checks serial/parallel missing-value
handling independently of the temporal writer. These tests do not establish
interoperability with an installed SPSS or SAS application.

The SAS underscore regressions additionally cover `._` in ordinary numeric
and temporal columns, including integer-backed columns. They inject the
on-disk tag bytes independently of the tag writer, check row windows and
output bytes for system missing, `._` and all A-Z tags, and repeat imports
and exports with automatic and explicit dictionaries. SAS7BDAT and XPT imports
through `convert()`, RDS storage, and subsequent XPT exports also retain the tag.
DDIwR stores `._` as an `NA` with an `"_"` name in
`na_index`, separately from the A-Z numeric codes -91 through -116. These
adapter changes require no modifications to the vendored ReadStat patch,
because ReadStat already supports `._`.

The SAS missing-label tests build little- and big-endian catalogs independently
of ReadStat's catalog writer, with labels for system missing, `._` and every
A-Z tag. They check numeric, date and timestamp columns, row windows, empty
imports, UTF-8 text, RDS storage, and exports read with the original catalog.
The `._` label key retains its tagged NA payload; byte comparisons are needed
because R's `identical()` treats different NA payloads as equal. These tests
also document that standalone SAS7BDAT and XPT exports do not contain value-label
definitions. Reusing a catalog requires unchanged formats and code mappings;
it does not demonstrate creation or updating of a catalog. Native catalog export
remains unsupported. The reader fix is outside the vendored ReadStat patch.

The October 11 audit also used the original attachments from haven issues
[#716](https://github.com/tidyverse/haven/issues/716) and
[#836](https://github.com/tidyverse/haven/issues/836). Both #716 data/catalog
pairs already recover the correct six `OUTCOME` and twenty `PREGORDR` labels:
164 of 171 columns are labelled in `labelled` (10,215 rows), and 246 of 248
in `no-labels` (9,553 rows). The upstream catalog fixes included in haven's
merged [PR #832](https://github.com/tidyverse/haven/pull/832) are already present
in the pinned ReadStat snapshot.

The ESS catalog from #836 has UTF-8 text but declares encoding zero. Automatic
native import previously failed; explicitly choosing Latin-1 produced garbled
accents. The DDIwR reader now uses the effective data-file encoding when a
catalog declares zero and no catalog override was supplied, matching the
approach proposed in haven's [PR #840](https://github.com/tidyverse/haven/pull/840)
(still open at the time of this audit). A metadata-only parse obtains the
data-file encoding; an explicit data encoding takes precedence. A nonzero
catalog encoding and an explicit catalog override retain precedence for that
catalog. The reader resets the encoding before parsing the data file, so a
catalog override cannot change data decoding. Catalog errors name the catalog
path. These changes belong to the DDIwR adapter and require no vendor patch.
After the fix, all 1,668 ESS rows import successfully, with labels on 558 of
572 columns and correct accents including `SPÖ`, `Grüne`, and `DéFI`.
Automatic native import and public `convert(encoding = NULL)` match their
explicit UTF-8 catalog counterparts across the full dataset.

The catalog-encoding regressions cover numeric/date/timestamp labels, little-
and big-endian catalogs, unspecified and declared encodings, independent
overrides, UTF-8 data values and variable labels, row windows, empty imports,
and public `convert()` discovery with automatic encoding. The original issue
attachments are external audit inputs, not bundled package fixtures.

This tooling is tracked for development and excluded from R source builds.
No Git commit or network fetch is performed by the updater itself.
