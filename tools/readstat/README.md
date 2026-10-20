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
- `readstat.h`, `readstat_writer.c`, and `spss/readstat_sav_write.c`: register
  owned multiple-response definitions with `readstat_writer_add_mr_set()`
  before the first row. SAV/ZSAV exports write categories and variable-labelled
  dichotomies in subtype 7, and counted-value-labelled dichotomies in subtype
  19. Member references use the writer's actual short-name mapping, including
  collision resolution. Validation checks names, homogeneous member types,
  counted values, string widths and label-source flags before the R adapter
  opens the destination. `spss/readstat_sav_parse_mr_name.c` also parses signed
  integer counted tokens without overflowing its integer convenience field;
  the original token remains available in `counted_string`.
- `readstat.h`, `readstat_writer.c`, and the SAS/XPT adapters: register a
  separate modification timestamp with
  `readstat_writer_set_file_modified_timestamp()`. SAS7BDAT writes each
  timestamp with its own UTC offset, including across daylight-saving changes.
  XPT versions 5 and 8 write separate creation/modification fields in both the
  library and member headers. The XPT reader exposes member timestamps and
  uses library timestamps only for blank member fields. The override has an
  explicit presence flag so epoch zero remains a valid value; an unset
  override retains the original creation-time default.
- `sas/readstat_sas7bdat_write.c`: accept numeric storage widths of 3 through
  8 bytes, with 0 selecting the eight-byte default. Row slots, physical column
  offsets and column definitions use the requested width. Numeric insertion
  retains the most significant IEEE double bytes in native file byte order,
  including the SAS missing-tag byte. Invalid numeric widths are rejected by
  variable validation. Reduced widths truncate precision after temporal unit
  conversion; they do not convert to IEEE single precision. The R adapter
  accepts `sas_metadata$width` and the legacy `width` attribute with the usual
  list precedence and NULL clearing, before opening the output file.
- `readstat.h`, `readstat_metadata.c`, and the SPSS/Stata/SAS readers: expose
  `readstat_has_creation_time()` and `readstat_has_modified_time()` separately
  from the numeric time values. SAV and Stata mark creation only after a
  successful parse; POR has a mandatory creation date. These formats have no
  separate modification timestamp. SAS7BDAT and catalogs track each finite
  header timestamp and its offset independently. XPT tracks presence through
  each member/library fallback without substituting creation for modification.
  The R adapters omit absent fields, using the same builder for serial and
  parallel readers. `readstat_writer_set_file_timestamp()` also records explicit
  presence so Stata exports an explicitly supplied epoch zero instead of a
  blank stamp.
- SAV, Stata, SAS7BDAT and XPT readers support DDIwR's `inspectFile()` adapter
  with no value callback and zero-length column prototypes. SAV retains the
  original header count and reads the 64-bit subtype-16 count without applying
  row-window limits. SAS7BDAT retains 64-bit live counts, skips observation-only
  page payloads and avoids row buffers, decompression, moved-row reads and
  deleted-row processing. Metadata pages can contain observation bytes but no
  observations are decoded. Stata skips strL contents while seeking to value
  labels after the data. XPT submits metadata after the observation header so
  inspection can report its count, including an explicit space-padded zero;
  blank and all-zero reserved counts remain unknown. POR and XPT 5 counts stay
  unknown. `inspectFile()` never scans observations to calculate a count.

The XPT timestamp locations follow the SAS transport layouts for
[version 5](https://support.sas.com/techsup/technote/ts140.pdf) and
[version 8](https://support.sas.com/techsup/technote/ts140_2.pdf).
Focused timestamp tests include independently edited XPT member/library
headers, SAS7BDAT header doubles and UTC offsets, RDS conversion, empty data,
row windows, explicit zero, defaults and validation before opening output.
SPSS and Stata retain their single header timestamp; their `modified` metadata
is not fabricated on import. A user-supplied `modified` field is preserved in R
but does not replace creation time on export. Presence regressions cover blank
and partially populated XPT headers, blank/NUL SAV stamps under all compression
modes, legacy/modern Stata optional stamps, non-finite SAS header timestamps,
and real epoch-zero values. A saved POR fixture checks its mandatory creation
timestamp. Windowed/empty reads, RDS storage and serial/parallel imports retain
the same presence information. Export without a creation timestamp keeps the
writer's current-time default.

Inspection regressions remove complete SAV/POR observation payloads, corrupt
Stata strL contents while retaining later value labels, remove SAS data-page
payloads and corrupt compressed SAS rows. Metadata inspection still succeeds
where ordinary imports fail. They also cover counts above R's integer range
without row allocation, zero/unknown counts, all SAV compression modes, XPT
long labels, storage definitions, custom metadata and separate SAS catalogs.

Multiple-response records follow the [GNU PSPP record grammar](https://www.gnu.org/software/pspp/pspp-dev/html_node/Multiple-Response-Sets-Records.html).
DDIwR exposes them as dataset `spss_metadata$mr_sets`. Focused tests check exact
record bytes, all three set types, Unicode, numeric and string counted values,
long-name collisions, all SAV compression modes, zero rows, row windows,
RDS conversions and serial/parallel parity. They do not establish compatibility
with an installed IBM SPSS application.

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

The SAV and native SAS7BDAT adapters pass the data frame's `label` to ReadStat's
existing file-label API. SPSS `convert(..., label = ...)` overrides that label;
explicit NULL or an empty string clears it. R validation rejects non-scalar,
missing or oversized labels before opening the destination: 64 UTF-8 bytes for
SAV and 256 for SAS7BDAT. Dataset labels remain separate from variable labels
and custom attributes named `label`. Focused tests cover independent SAV header
bytes, compression modes, serial/parallel import, RDS conversions, row windows,
empty datasets, Unicode limits and unchanged destination bytes on errors. This
adapter change requires no additional vendored ReadStat patch.

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
definitions. Reusing a catalog requires unchanged formats and code mappings.
`convert(..., catalog_to = ...)` and the internal
`write_sas(..., catalog_file = ...)` path stage and publish SAS7BDAT/catalog
pairs, reusing the data writer's missing-code maps for numeric definitions.
Character definitions use `$` format names and named character label vectors.
Keys use UTF-8, with trailing ASCII padding spaces removed before duplicate
checks. Blank keys, unused definitions, shared formats and keys up to 32767
bytes are supported; missing R string keys are rejected. Numeric and character
formats with the same base name remain distinct. The catalog writer uses an
unsigned two-byte entry length for long string keys and avoids copying a NULL
key buffer for the blank value. The catalog reader retains spaces in character
label text using its explicit byte length; only key padding is trimmed.
Import retains missing-label identity in `sas_metadata$label_tags`, separately
from observed missing positions, including empty and windowed imports. SAS
canonical -91 through -116 codes retain their letter tags, including sparse
subsets. SAS7BDAT catalog lookup uses the format name without display width.

`sas/readstat_sas7bcat_write.c` encodes system, underscore and A-Z missing keys
and writes chained data pages and overflow index pages instead of silently
dropping definitions that exceed one page. Independent little/big-endian
fixtures exercise missing-label re-export; a separate R byte decoder checks
generated numeric and character keys, text, long names, page chains and index
overflow. Character fixtures cover both byte orders, both string offsets,
blank/leading-space/long keys and separate catalog encodings.
Paired-export tests cover dictionaries, reduced-width precision checks,
conflicting shared definitions and preservation/rollback of existing files.
The export supports exact numeric, character and temporal labels and explicit
`sas_metadata$catalog_rules` tables for ranges, endpoint exclusions, LOW/HIGH,
OTHER, default widths and numeric fuzz. Imported rules remain separate from
point labels. Rule endpoints for temporal columns use native SAS units.
Multilabel, PICTURE and executable/nested format definitions are unsupported.
For labelled dates/timestamps/times,
the R adapter supplies an absent DATE9/DATETIME25.6/TIME20.6 informat while
the display format references the catalog. Compatible existing informats are
retained; cleared, unrecognized or conflicting informats fail before publication.
The reader uses a temporal informat only when the display format does not
already identify a temporal type. Ordinary label keys use the same epoch/unit
conversion as rows; missing tags and canonical code surrogates are never shifted.
Difftime values and ordinary keys are normalized from their explicit units to
seconds. Catalog precision and shared-definition checks use exported SAS keys.
The temporal catalog suite uses independently assembled little/big-endian
catalogs with raw SAS data units, shared definitions interpreted separately
for date/datetime/time columns, all missing categories, empty/windowed imports,
UTC timestamps, duration units, dictionary remapping and reduced-width rejection.
Default informats follow SAS's
[date and datetime informat conventions](https://support.sas.com/documentation/cdl/en/etsug/67525/HTML/default/etsug_intervals_sect009.htm).
Actual SAS 9.4M9/Linux/UTF-8 validation exposed an ASCII missing-tag encoding
defect in native data export. The corrected writer uses SAS's negative NaN
prefix and complemented ordinal codes for system, underscore and A–Z missings,
including numeric widths 3–8. Byte regressions use the unchanged SAS-created
fixture; import of older ASCII-tagged files remains supported. A second actual
SAS run passed all ten targeted checks, including SAS-native fixture re-export,
all missing categories across numeric/temporal types and storage widths,
dictionary remapping and empty data. Catalog export now uses the native
64-bit directory, linked leaves and branch offsets, XLSR indexes, allocation
sizes and format records required by SAS. New regressions compare directory
and key records with unchanged SAS-created fixtures. In a fresh SAS session,
all six native-fixture comparisons passed, including a catalog with 80 formats
and 1,158 definitions, long character keys and rendered labels. The complete
mixed/empty/three-byte paired-export run also finished with zero failures and
SYSCC=0. These checks cover SAS 9.4M9/Linux x86-64/UTF-8; other SAS hosts have
not been exercised. An additional actual SAS run passed all eight rule-table
and rendered-value comparisons, including long Unicode ranges and character
LOW/HIGH sentinels. It exposed and verified a fix for LOW-key ordering.
See the [actual SAS runs and reproduction](interop/README.md).

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
