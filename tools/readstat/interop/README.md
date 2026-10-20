# Actual SAS interoperability run

Run on 2026-10-20 UTC (2026-10-19 local) in SAS OnDemand for Academics,
using SAS **9.04.01M9P06052025**, Linux x86-64, UTF-8. Input files were
synthetic exports from the installed DDIwR 0.22.1 built from this checkout.
No research data was uploaded. This is an executed SAS run, not an inference
from ReadStat round trips.

## Current result: native data and catalog export pass the tested SAS checks

After correcting the catalog writer, the native-fixture run
`20261020_022343` passed all six comparisons in a fresh SAS session, with
`DDIWR_CATALOG_RESULT failures=0 SYSCC=0`. SAS reopened the exported catalogs
with PROC CATALOG, extracted their definitions with PROC FORMAT/CNTLOUT and
applied their formats to the paired data.

| Native-fixture comparison | Definitions | Rows and rendered labels |
| --- | --- | --- |
| Original numeric/date and character reference | PASS: 12 definitions | PASS: all five rows |
| Long character keys, blank/leading-space/Unicode keys and a long format name | PASS: four definitions | PASS: all four rows |
| Multi-page catalog with 80 formats, including 1,000 labels in one format | PASS: all 1,158 definitions | PASS: both rows across all 80 columns |

Definition comparisons return `SYSINFO=0`. Row comparisons return 8 solely
because the exported format names are deliberately different from the native
controls; the reports show zero unequal values. Fresh names prevent earlier
interactive definitions from masking lookup failures.

The complete mixed/empty/narrow paired-export run `20261020_022723` also
finished with `DDIWR_SAS_INTEROP_RESULT failures=0 SYSCC=0`. It verifies raw
numeric/temporal/character values, all system/underscore/A–Z missing-label
definitions, rendered labels, empty datasets retaining definitions and
three-byte numeric/date storage. The validation script preserves display
formats while calling VVALUE; a FORMAT statement applies at compilation time,
even when it follows an assignment in the source.

The corrected writer emits the native 64-bit catalog header, linked directory
leaves and branch keys, XLSR index entries and overflow pages, bitmap and
chained format records. Record headers include the aligned allocation sizes
used by SAS, and long character keys use the native six-byte padding and blank
key representation. Regression tests compare these structures with unchanged
SAS-created fixtures, in addition to ordinary round trips. Catalog import
continues to support the older 32-bit representation.

To reproduce after installing the checkout:

```sh
Rscript tools/readstat/sas-catalog-interop.R /tmp/ddiwr-new-catalog-run
Rscript tools/readstat/sas-interop.R /tmp/ddiwr-new-paired-run
```

For each bundle, upload all files listed in its manifest to SAS Studio's
**Files (Home)** and run the generated program. `native-catalog/` contains the
successful SAS logs, text reports, exact executed programs, uploaded-file
manifests, fixture-creation programs and source fingerprints. Server account
identifiers in saved logs are replaced with `SAS_USER`; results are unchanged.
These runs verify SAS 9.4M9 on Linux x86-64 in UTF-8. Other SAS hosts/releases
have not been exercised.

## Range, default and OTHER rules

The additional run `20261020034443` passed all eight comparisons with
`failures=0 SYSCC=0` in the same actual SAS runtime. Four cases compare native
and re-exported definitions and rendered values: mixed numeric/character
ranges, long Unicode character ranges, character LOW/HIGH and an OTHER-only
format. Together they cover 17 definitions and 24 data rows. Every comparison
reports zero unequal values. Definition codes 16 for the last two cases reflect
character metadata lengths; row codes 8 reflect deliberately distinct format
names. These are metadata differences, not value inequalities.

The first run exposed incorrect ordering of character LOW sentinels. The
corrected writer sorts LOW first and writes native zero/FF sentinels. The final
run used fresh catalog names so an earlier loaded definition could not conceal
a lookup defect. `native-rules/` saves the exact executed program, sanitized
SAS log, environment/summary CSVs, manifests and source fingerprints. Its program
includes a byte-preserving transfer of the corrected LOW/HIGH catalog; the
other files come from bundle `20261020034235`. The uploaded manifest records
that earlier bundle, and the separate fixed-catalog manifest identifies the
replacement used by the final program.

To repeat after installing this checkout:

```sh
Rscript tools/readstat/sas-rules-interop.R /tmp/ddiwr-new-rules-run
```

Upload the manifest files and run the generated program. An optional second
argument selects the directory visible to SAS on another host. The supported
R rule-table contract is documented in `convert()`; multilabel, PICTURE and
executable/nested format definitions are not supported.

## Additional applications and hosts

`spss-interop.R` and `stata-interop.R` create synthetic files, native validation
programs and SHA-256 manifests. Generating these bundles is not an application
pass. The SPSS bundle preserves existing named datasets; the Stata program uses
a temporary frame when supported and preserve/restore on older releases.

The available SPSS macOS VM trial opened an uncompressed SAV through
File > Open > Data and displayed its numeric and Unicode values, formats,
widths, labels, measurement levels and alignment correctly. It locks syntax,
Save As and Define Multiple Response Sets; Save As produced an Add-On Required
dialog. Native save-back and MR/custom-attribute validation remain unverified.
The user's existing named dataset and SPSS application were left open.
See `spss-menu-checklist.md` for the reproducible menu procedure and its limits.

No Stata installation or additional SAS release/host is currently available.
Those actual application runs remain pending. Do not substitute ReadStat
round trips for these checks or infer cross-host SAS coverage from OnDemand.

## Earlier targeted native missing-value run

A second run after correcting the native data writer passed all ten targeted
checks, with `SYSINFO=0` for every comparison and
`DDIWR_NATIVE_MISSING_RESULT failures=0 SYSCC=0`. It used the same SAS runtime
and an installed package rebuilt from the corrected checkout.

| Targeted check | Actual SAS result |
| --- | --- |
| SAS-native fixture re-export through `write_sas()` | PASS: finite, system, underscore, A and Z values match the unchanged SAS-created data |
| SAS-native fixture re-export through `convert()` | PASS: same independent reference |
| Numeric storage widths 3, 4, 5, 6, 7 and 8 | PASS: all 31 rows match independently created SAS reference tables; PROC CONTENTS confirms widths |
| Reversed letter dictionary | PASS: all missing identities match the requested mapping |
| Empty dataset | PASS: original row count is zero |

The width and dictionary cases cover double/integer numeric values, dates,
timestamps, time durations and logical system missings. They include `NA`,
`NaN`, `._` and every letter `.A` through `.Z`, with finite controls. SAS itself
creates the expected values and applies each storage width; PROC COMPARE uses
an absolute criterion of zero. Catalogs were excluded from this targeted run;
their subsequent validation is recorded above.

To repeat this targeted validation, install the checkout, then run:

```sh
Rscript tools/readstat/sas-missing-interop.R /tmp/ddiwr-new-missing-run
```

Upload the eleven generated `.sas7bdat` files and the `missing_*.sas` program
to SAS Studio's **Files (Home)**, then run the program. It saves its log and
reference tables under `$HOME/ddiwr_missing_<run_id>`.

Saved evidence under `native-missing/` includes the actual SAS log, text
results, exact executed program, uploaded-file manifest and source digests.
The run ID is `20261020_015222`. Account identifiers in the saved log are
replaced with `SAS_USER`; results are unchanged. Local regression tests also
compare complete exported rows against payloads extracted from the unmodified
SAS-native fixture and retain import coverage for older ASCII-tagged files.

## Initial run: two export defects

| Check | Actual SAS result |
| --- | --- |
| Finite numeric/date/datetime/time values | PASS: all 16 values equal, including SAS/R epoch zero and fractions |
| Character data | PASS: all 32 strings equal, including leading spaces, blank values and Unicode |
| System/extended missing identities | FAIL: 112 differences across 112 missing values in four numeric columns |
| Generated value-label catalog | FAIL: both PROC CATALOG and PROC FORMAT reject the file |
| SAS-native catalog control | PASS: SAS creates and reopens a catalog and extracts 12 numeric/character definitions |
| Empty dataset | PASS: original row count is zero |
| Three-byte numeric/date storage | PASS: values equal and PROC CONTENTS reports length 3 |

The catalog error is `File INCOMING.MIXED_20261020_012855.CATALOG is not a SAS data set.`
PROC FORMAT returns SYSERR=1012. It reproduces independently of FMTSEARCH.
A catalog created by SAS succeeds through the same LIBRARY/CNTLOUT path.
The catalog layout was subsequently corrected as described above.

For data values, PROC COMPARE returns SYSINFO=4100: 4096 reports value
inequality and 4 reports different informats. The separate narrow-data
comparison returns 20 (informat and storage-length differences), with no
unequal values. Metadata differences are distinguished from data differences.

A byte comparison also establishes a concrete missing-value encoding gap.
The SAS-native system missing in the saved reference is little-endian
`00 00 00 00 00 FE FF FF`; our initial system missing was
`00 00 00 00 00 D1 F8 7F`. SAS-native `._`, `.A` and `.Z` use
`00 00 00 00 00 FF FF FF`, `00 00 00 00 00 FD FF FF` and
`00 00 00 00 00 E4 FF FF`, respectively. Our reader accepts both schemes,
which is why the existing self round trips did not expose this export defect.
The corrected writer now emits the native ordinal payloads at widths 3–8.

## Initial run reproduction and evidence

Run from the repository root:

```sh
Rscript tools/readstat/sas-interop.R /tmp/ddiwr-new-sas-run
```

The output directory must be new. Upload the six generated `.sas7bdat` and
`.sas7bcat` files and the generated `validate_*.sas` program to SAS Studio's
**Files (Home)**. Open and run the program. It checks raw values before catalog
operations so a rejected catalog cannot conceal missing-value failures.
It also captures the SAS version/platform/encoding, tests catalog definitions
and applied labels, and writes a log and summary under
`$HOME/ddiwr_validation_<run_id>`. A result with nonzero failures or SYSCC is
not a successful interoperability check. R bundle generation alone is not a pass.

- `diagnose.sas` is the exact isolated diagnostic executed in a clean SAS
  process after the first run. It creates the native reference, independently
  compares data values, checks widths/counts and reproduces catalog rejection.
- `summary.sas` computes the compact results from the actual SAS comparison
  tables. It was also executed in SAS.
- `sas94-m9-diagnostic.log` and `sas94-m9-summary.log` preserve the SAS-generated
  diagnostic and compact report. Server account identifiers are replaced with
  `SAS_USER`; numeric results and errors are unchanged.
- `sas94-m9-runner.log` records a fresh bundle generated by `sas-interop.R`
  and executed in SAS. It completed with `failures=5 SYSCC=1012`: a raw-value
  mismatch, two rejected mixed/empty catalogs, a rejected narrow catalog and
  unequal rendered narrow labels. These are five failed checks arising from
  the two export defects, not a successful run.
- `manifest.csv` records the sizes and MD5 digests of the first uploaded bundle;
  `runner-manifest.csv` identifies the fresh reproducibility run.
- `source-fingerprints.csv` identifies the serializer sources in this checkout.
- `tests/testthat/fixtures/readstat/sas94-m9-reference.*` is the unmodified pair
  created by SAS in the diagnostic. The import regression test reads it directly.

The initial failures are retained as historical evidence. The successful
catalog and complete paired-export runs above supersede their export status.
