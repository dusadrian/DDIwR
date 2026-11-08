#' @name convert
#'
#' @title
#' Converts a dataset from one statistical software to another
#'
#' @description
#' This function converts (or transfers) between R, Stata, SPSS, SAS, Excel and
#' DDI XML files. It uses the DDI standard as an exchange platform and
#' facilitates a consistent conversion of missing values. Statistical files
#' are read and written through ReadStat directly, without the haven package.
#'
#' @details
#' When the argument **`to`** specifies a certain statistical package
#' (`"R"`, `"Stata"`, `"SPSS"`, `"SAS"`, `"XPT"`) or `"Excel"`, the name of the
#' destination file will be identical to the one in the argument **`from`**,
#' with an automatically added software specific extension. `to = "SAS"`
#' retains the XPT destination. Supply an explicit `.sas7bdat` path to select
#' native SAS7BDAT output.
#'
#' SPSS portable file (with the extension `".por"`) can only be read, but not
#' written.
#'
#' SPSS exports default to byte compression for `.sav` destinations and ZSAV
#' compression for `.zsav` destinations. Supply `compress = "none"`, `"byte"`
#' or `"zsav"` through `...` to override that default. `compress = FALSE`
#' selects no compression and `compress = TRUE` selects ZSAV compression.
#'
#' SAS transport exports (`.xpt` or `to = "SAS"`) accept data frames without
#' labels. A companion SAS setup script is generated only when the data has
#' variable or value labels.
#'
#' SPSS system files (`.sav` and `.zsav`) preserve custom dataset and variable
#' attributes. They are stored in the R attribute `spss.attributes` on the data
#' frame and on each column, as a named list of character vectors. For example,
#' `attr(data, "spss.attributes") <- list(source = "Archive")` and
#' `attr(data$answer, "spss.attributes") <- list(foo = "bar")` are written as
#' SPSS dataset and variable attributes, respectively. A character vector with
#' several elements represents an attribute array; a single element represents
#' a scalar. Custom names such as `label` remain separate from standard R
#' metadata. Names must be valid SPSS attribute names of at most 64 UTF-8 bytes,
#' unique ignoring case within their scope. Values must be non-missing strings
#' without line feeds, which the SPSS attribute format cannot represent.
#'
#' Stata dataset and variable characteristics are preserved in `.dta` files
#' through the R attribute `stata.characteristics` on the data frame and each
#' column. Use a named list of single, non-missing strings, for example
#' `attr(data, "stata.characteristics") <- list(Source = "Archive")` and
#' `attr(data$answer, "stata.characteristics") <- list(Foo = "bar")`.
#' Characteristic names are case sensitive and may contain at most 32
#' characters. Values may contain line feeds and trailing spaces, and may use
#' at most 67783 bytes in the target encoding (67784 including the terminating
#' zero byte). Stata 14 and later use UTF-8.
#' Earlier versions write characteristic values in Windows-1252 and require
#' ASCII characteristic names; unrepresentable values are rejected. When
#' importing these older files with `convert()`, specify
#' `encoding = "windows-1252"` (or their actual source encoding).
#' Dataset and variable note characteristics, including `note0`, are retained.
#' Raw dataset note characteristics take precedence over the legacy R `notes`
#' attribute when writing, preserving note numbering and contents.
#' If no raw note characteristics are present, dataset `notes` are written as
#' Stata note characteristics.
#'
#' Import to R and conversion to RDS retain the formats, storage widths,
#' display widths, temporal classes and units, and custom SPSS/Stata metadata
#' returned by the readers, including during declared conversion and missing
#' recoding. Dataset labels and notes are retained in R. Character-to-numeric
#' recoding removes obsolete string formats and storage widths. Preservation
#' in exported statistical files depends on the destination writer's support.
#'
#' SPSS exports and the native SAS7BDAT writer preserve the data frame's `label`
#' attribute as the dataset label. When exporting to SPSS, `label` in `...`
#' overrides that attribute: `convert(data, "data.sav", label = "Survey")`.
#' An explicit `label = NULL` or `label = ""` clears the exported dataset label;
#' omitting the argument retains it. Variable labels and custom attributes named
#' `label` are separate. Dataset labels must be single, non-missing character
#' strings of at most 64 UTF-8 bytes for SPSS or 256 bytes for native SAS7BDAT,
#' or NULL. Invalid labels cause an error before opening the destination file.
#' Trailing padding spaces are removed on import. Export does not modify the
#' input object's label.
#'
#' Optional `spss_metadata`, `stata_metadata` and `sas_metadata` attributes
#' group format-specific metadata in ordinary named R lists on data frames or
#' columns. These are DDIwR's metadata containers. Field names must be nonempty
#' and unique. Statistical file readers store formats, storage widths, display
#' widths and dataset notes only in these lists. Shared attributes such as
#' `label`, `labels`, `na_values` and `na_range` remain separate. Edit the list
#' fields after import; legacy attributes are accepted as writer fallbacks for
#' older R objects but are not emitted by readers.
#'
#' Writers use explicit arguments first, then fields in the destination vendor's
#' list, then existing attributes, then their usual defaults. A missing list
#' field falls back to the existing attribute; a present `NULL` field clears
#' it and uses the writer default. For example,
#' `attr(data$answer, "spss_metadata") <- list(format = "F12.3")` overrides
#' `format.spss`, while `list(format = NULL)` clears it. Other vendors' lists
#' are not interpreted. Input objects are not modified.
#' Use [vendorMetadata()] to read or replace list fields without editing attributes.
#' Use [clearFormats()] or [clearWidths()] to remove the corresponding column
#' metadata from both vendor lists and existing attributes, preventing legacy
#' fallback from restoring cleared fields.
#'
#' Supported column fields are `format` for all three vendors, `width` for
#' all three vendors, and `display_width`, `measure` and `alignment` for SPSS.
#' SPSS measurement levels are `"nominal"`, `"ordinal"` and `"scale"`;
#' alignments are `"left"`, `"right"` and `"center"`. SAS also supports
#' `informat`. Formats and informats are single, non-missing, nonempty strings;
#' widths are nonnegative whole numbers up to R's integer
#' maximum. String storage is enlarged when needed to fit the values. Numeric
#' storage width is fixed at eight bytes in SAV output. Numeric SAS7BDAT and
#' XPT widths must be 3 through 8 bytes, or 0 for the eight-byte default.
#' Native SAS7BDAT output discards the least significant bytes of the stored
#' double, which can change fractions and large integers. Date/time values
#' are shortened after conversion to SAS units. Use eight bytes to retain full
#' precision. Numeric SAS7BDAT widths in the existing `width` attribute follow
#' the same rules. Invalid widths cause an error before opening the destination.
#'
#' Stata column `type` selects `"byte"`, `"int"`, `"long"`, `"float"`,
#' `"double"` or `"strL"`. Numeric storage must represent all exported values
#' exactly, including converted date/time units; unsafe choices cause an error.
#' Clear `type` to restore automatic storage selection after editing values.
#' `"strL"` requires a character column and Stata 13 or later. Readers retain
#' numeric storage types and `strL` in `stata_metadata`. Fixed Stata string
#' `width` records the on-disk byte capacity, excluding the buffer terminator.
#'
#' Dataset `notes` are supported for SPSS and Stata as character vectors without
#' missing values. SPSS notes allow at most 80 UTF-8 bytes per entry, including
#' empty entries; trailing padding spaces are removed on import. Stata list
#' notes override both legacy `notes` and raw dataset note characteristics.
#' Unchanged imported notes retain their original raw numbering; other
#' characteristics remain intact. Dataset `version` uses Stata release numbers
#' 8 through 15, or 5 and 8 for SAS XPT. On import, shared Stata file formats
#' map to the earliest supported release that writes them (8, 10, 12, 13, 14
#' or 15). Without metadata, `convert()` defaults to Stata 14 or XPT 5;
#' `version = NULL` explicitly selects that default. SAS dataset `table_name`
#' is supported for SAS7BDAT and XPT, and supplies the default for the XPT
#' writer's `name` argument.
#'
#' Dataset `created` and `modified` timestamps are read as `POSIXct` values
#' displayed in UTC. Fields are included only when the format provides a
#' timestamp that the reader can parse. Blank or unavailable timestamps remain
#' absent through conversion, recoding and RDS storage; the Unix epoch (zero)
#' is a valid timestamp and is not treated as missing. SPSS and Stata imports
#' expose their single header timestamp as `created`, without a `modified` field.
#' `created` can be written for all three vendors, and
#' `sas_metadata$modified` can be written to SAS7BDAT and XPT versions 5 and 8.
#' Each writable timestamp must be one finite `POSIXct` value with whole-second
#' precision. An absent or NULL `modified` uses the creation timestamp; setting
#' only `modified` leaves creation at the writer's current-time default. The
#' current-time default also applies when exporting without `created`.
#' Local time is used for file headers without
#' timezone information; Stata timestamps have minute precision. SPSS creation
#' years must be 1970 through 2069 and both XPT timestamps must use years 1960
#' through 2059 because their headers use two-digit years. XPT imports use the
#' dataset member's timestamps, falling back to the corresponding library
#' timestamps when the member fields are blank. Creation and modification are
#' resolved independently; one does not substitute for the other. SAS7BDAT stores
#' separate UTC
#' offsets for the two timestamps, preserving instants across timezone changes.
#' SPSS and Stata headers have no separate modification timestamp:
#' `spss_metadata$modified` and `stata_metadata$modified` remain preserved in R
#' and RDS but are ignored on export, leaving the creation timestamp unchanged.
#'
#' SPSS multiple-response definitions are read and written through dataset
#' `spss_metadata$mr_sets`, a list named by set name. Each entry contains `type`,
#' `label`, `is_dichotomy`, `counted_value`, `counted_string`, `variables` and
#' `label_from_var_label`. Counted fields apply to dichotomies; `counted_string`
#' retains the source token, including string values. Variable references use
#' long variable names; exports translate them to the file's short names.
#' Definitions are retained in R and RDS and written in SAV and ZSAV files.
#'
#' To create a set, supply `type` (`"C"` for categories, `"D"` for dichotomies
#' labelled by variables, or `"E"` for dichotomies labelled by counted values)
#' and at least two distinct, existing `variables`, all numeric or all string.
#' Set names must begin with `$`, follow SPSS naming rules, fit in 64 UTF-8
#' bytes, and be unique ignoring case. For example, this defines a numeric
#' dichotomy set:
#' ```
#' attr(data, "spss_metadata") <- list(mr_sets = list(
#'     "$choices" = list(type = "D", variables = c("first", "second"), counted_value = 1)
#' ))
#' ```
#' Optional `label` defaults to `""` and is
#' limited to 256 UTF-8 bytes. `label_from_var_label = TRUE` is allowed only
#' for type `"E"` with an empty label. `is_dichotomy`, when supplied, must
#' agree with `type`.
#'
#' For dichotomies, `counted_string` takes precedence over `counted_value`.
#' Numeric sets require an integer token or a finite whole-number
#' `counted_value`; string sets require `counted_string`, which must fit the
#' narrowest exported member. Category sets cannot specify either counted
#' field. Trailing padding spaces are removed from counted strings; imported
#' labels also lose trailing padding spaces. Labels and counted strings cannot
#' contain line breaks. Unknown fields, invalid definitions and missing member
#' variables cause an error before opening the destination. NULL or empty
#' `mr_sets` writes no definitions. Files group types `"C"` and `"D"` before
#' `"E"`, preserving relative set order within each group.
#'
#' All list fields, including unsupported ones, are retained during conversion
#' to R or RDS and missing-value recoding. Character-to-numeric recoding removes
#' list fields describing string storage (`format`, `width`, SAS `informat`
#' and Stata `type`). Export warns about non-NULL destination-vendor fields the
#' writer cannot honor, apart from the read-only dataset `modified` field for
#' SPSS and Stata.
#' These lists are separate from custom `spss.attributes` and
#' `stata.characteristics`. Malformed lists and supported fields with invalid
#' types or shapes cause an error before the destination file is opened.
#' The native writers also enforce their format-specific restrictions.
#'
#' Stata export converts R `Date` columns to days since 1960-01-01 and
#' `POSIXct` columns to milliseconds since that date. Integer storage is used
#' only when the converted values fit its range and precision. System and
#' extended missing values are preserved separately from the date/time values.
#'
#' The argument **`to`** can also be specified as a path to a specific file,
#' in which case the software package is determined from its file extension.
#' Home-directory paths beginning with `~` are supported.
#' DDI exports include the required study description with generic defaults.
#' Supply `titl`, `IDNo`, `agency`, `distrbtr`, `URI` and `abstract` through
#' `...` to describe the actual study.
#' XML exports are validated against the bundled DDI Codebook 2.6 schema before
#' writing. Invalid XML causes an error and leaves destination files unchanged.
#' The following extentions are currently recognized: `.xml` for DDI,
#' `.rds` for R, `.dta` for Stata, `.sav` and `.zsav` for SPSS, `.xpt` for SAS, and
#' `.xlsx` for Excel.
#'
#' Additional parameters can be specified via the three dots argument
#' **`...`**, for the destination or source format. For example, **`version`**
#' selects the Stata release when writing a `.dta` file. Excel import options
#' are passed to \bold{\pkg{readxl}}.
#'
#' The formal arguments **`n_max`** and **`skip`** are used only when importing
#' foreign data files through the direct ReadStat readers, namely SPSS (`.sav`,
#' `.zsav`, `.por`), Stata (`.dta`) and SAS (`.sas7bdat`, `.xpt`). They can be
#' used to read only the first `n_max` rows or to import large files in batches
#' by skipping the first `skip` rows.
#' Use [inspectFile()] to inspect column definitions and metadata without
#' decoding observations, including the original row count when the file's
#' metadata provides one. Unlike inspection, `n_max = 0` uses a limited data read.
#'
#' SPSS user-defined missing codes are retained by default. Specify
#' `user_na = FALSE` in **`convert()`** to discard them during import.
#'
#' The same three dots argument is used to pass additional parameters to other
#' functions in this package, for instance **`exportCodebook()`** when writing
#' to a DDI file. One of its argument **`embed`** (activated by default) can be
#' used to control embedding the data in the XML file. Deactivating it will
#' create a CSV file in the same directory, using the same file name as the
#' XML file.
#'
#' When converting from DDI, if the dataset is not embedded in the XML file, the
#' CSV file is expected to be found in the same directory as the DDI Codebook,
#' and it should have the same file name as the XML file. The path to the CSV
#' file can be provided via the **`csv`** argument. Additional formal
#' parameters of the function \bold{\code{\link[utils]{read.csv}()}} can
#' be passed via the same three dots **`...`** argument. Alternatively, the
#' **`csv`** argument can also be an R data frame.
#'
#' A multilingual DDI Codebook is imported in one language at a time. The
#' DDI-specific **`language`** option is supplied via **`...`**, for example
#' `convert("codebook.xml", language = "en")`. When it is omitted, the
#' effective `xml:lang` of the first variable label is used. Supplying it selects
#' that translation throughout the variable and category labels. Language
#' declarations inherited from a parent DDI element are honored. An unqualified
#' label is used as a neutral fallback, but a label in a different language is
#' not substituted.
#'
#' When converting to DDI, if the argument **`embed`** is set to `FALSE`, users
#' have the option to save the data in a separate CSV file (the default) or not
#' to save the data at all, by setting **`csv`** to `FALSE`.
#'
#' The DDI .xml file generates unique IDs for all variables, if not already
#' present in the attributes. These IDs are useful for referencing purposes, in
#' newer versions of the DDI Codebook.
#'
#' The argument **`chartonum`** signals recoding character categorical
#' variables, and employs the function \bold{\code{\link{recodeCharcat}()}}.
#' This only makes sense when recoding to Stata, which does not allow allocating
#' labels for anything but integer variables.
#'
#' If the argument **`to`** is left to `NULL`, the data is (invisibly) returned
#' to the R enviroment. Conversion to R, either in the working space or as
#' a data file, will result (by default) in a data frame containing declared
#' labelled variables, as defined in package \bold{\pkg{declared}}.
#'
#' The current version reads DDI Codebook versions 1.2.2, 2.5 and 2.6, and
#' creates version 2.6, with future
#' versions to extend the functionality for DDI Lifecycle versions 3.x and link
#' to the future package \bold{DDI4R} for the UML model based version 4. It
#' extends the standard DDI Codebook by offering the possibility to embed a
#' serialized version of the R dataset into the XML file containing the
#' Codebook, within a `notes` child of the `fileDscr` component. This type of
#' generated codebook is unique to this package and automatically detected when
#' converting to another statistical software. This will likely be replaced with
#' a time insensitive text version.
#'
#' Native SAS import uses ReadStat directly. SAS value-label definitions are
#' read from a companion format catalog. Supply its path via **`catalog_file`**,
#' or place it alongside the `.sas7bdat` file with the same base name and the
#' extension `.sas7bcat` for automatic discovery.
#' When a catalog does not declare an encoding, it inherits the effective
#' data-file encoding. Use **`encoding`** and **`catalog_encoding`** to override
#' their encodings independently; a catalog override does not affect data-file
#' decoding.
#'
#' Catalog labels for `.A` through `.Z` use numeric keys -91 through -116 in
#' the R `labels` attribute. A label for `._` retains an underscore-tagged NA
#' key, distinct from the ordinary NA key for system missing. These keys also
#' apply to dates and timestamps and are preserved in RDS storage.
#'
#' Standalone SAS7BDAT and XPT output does not contain value-label definitions.
#' Native SAS7BDAT output supports an optional paired value-label catalog:
#' `convert(data, "data.sas7bdat", catalog_to = "data.sas7bcat")`.
#' `catalog_file` and `catalog_encoding` apply only to the input catalog;
#' `catalog_to` is the output catalog path and is supported only with a
#' `.sas7bdat` destination. Omitting `catalog_to`, or setting it to NULL,
#' writes only the data file and leaves existing catalog files unchanged.
#' An input catalog is never reused as an output
#' destination implicitly. For example, this reads one catalog and writes another:
#' ```
#' convert("input.sas7bdat", "output.sas7bdat",
#'     catalog_file = "input.sas7bcat", catalog_to = "output.sas7bcat")
#' ```
#' Native SAS7BDAT output preserves system missing, `._` and existing letter
#' missing tags using SAS's native ordinal encoding at every numeric storage
#' width. Readers also accept the older ASCII-tagged representation.
#' Automatically generated dictionaries assign other missing codes to unused
#' letters. An explicit `dictionary` can override that mapping. Native output does not
#' generate an additional SAS setup script.
#' Other destinations retain their existing behavior.
#' Numeric, integer and logical columns, including factor codes, can have
#' exact numeric value labels, system missing labels, `._` and `.A` through
#' `.Z` labels. Character columns accept named character vectors of exact
#' value-label keys, including an empty key for the blank character value.
#' Keys must be non-missing and at most 32767 UTF-8 bytes. Trailing ASCII spaces
#' are removed from character keys to match SAS padding semantics; leading
#' spaces and other characters remain significant. Character label text retains
#' its leading and trailing spaces, including empty labels. Keys that become duplicates
#' after removing padding cause an error. `NA_character_` is not a catalog key;
#' use `""` for a blank key. Unused definitions and empty datasets are supported.
#' Labelled `Date`, `POSIXct`, `hms` and `difftime` columns are supported.
#' Ordinary label keys use the column's R units, just like its data values:
#' days since 1970 for dates, seconds since 1970 for timestamps, and the
#' column's declared units for durations. Export converts both rows and keys
#' to SAS days/seconds since 1960 or time seconds. Durations are normalized
#' to seconds on import; timestamps retain their instant and are returned in UTC.
#' System and extended missing-label keys are categories, never shifted or scaled.
#' Numeric and character ranges, endpoint exclusions, LOW/HIGH and OTHER rules
#' use column `sas_metadata$catalog_rules`, a data frame with character fields
#' `start`, `end`, `label`, `hlo`, `sexcl` and `eexcl`, integer `default` and
#' numeric `fuzz`. `end` defaults to `start`, `hlo` to `""`, exclusions to `"N"`,
#' `default` to the longest label's byte width (at least one), and `fuzz` to zero.
#' `default` and `fuzz` must be constant within each format. HLO accepts `""`,
#' `"L"`, `"H"`, `"LH"` or `"O"`; exclusions accept `"N"` or `"Y"`. An OTHER
#' row has no endpoints; L/H mark unbounded ordinary numeric/character bounds.
#' Numeric endpoints are strings containing finite SAS numbers or missing tokens
#' `"."`, `"._"`, `".A"` through `".Z"`. Temporal endpoints use native SAS units
#' (days/seconds since 1960 or time seconds), as in PROC FORMAT/CNTLIN, rather
#' than R date/time units. Character endpoints retain leading spaces and drop
#' trailing ASCII padding. Endpoints and labels allow up to 32767 UTF-8 bytes,
#' subject to native record/page capacity. Overlapping or empty intervals,
#' multiple OTHER rows and unsupported rule fields are rejected before publication.
#' Rule tables take precedence over `labels`; a NULL table restores exact labels.
#' Imported ranges are retained as rules, never misrepresented as point labels;
#' exact entries remain available in `labels`. Native default widths and numeric
#' fuzz are retained. Rules are written only with a paired catalog. Multilabel,
#' PICTURE, nested formats and executable expressions are not supported.
#' Catalog output uses the native 64-bit layout, including SAS directory,
#' index and format records. SAS catalogs depend on host layout; compatibility
#' is checked with SAS 9.4M9 on Linux x86-64 in UTF-8.
#'
#' A custom label format cannot also identify a temporal type. Paired temporal
#' export therefore supplies an absent `sas_metadata$informat` as `DATE9`,
#' `DATETIME25.6` or `TIME20.6`. An existing compatible informat is retained;
#' an explicitly NULL, unrecognized or conflicting informat causes an error.
#' On SAS import, a recognized temporal display format takes precedence;
#' otherwise a recognized temporal informat determines the date/time class
#' and unit conversion, even without a catalog. Ordinary catalog keys follow
#' the same conversion; missing-code surrogates remain unchanged.
#'
#' A labelled column's SAS format reference must name a user-defined format.
#' Numeric names allow up to 32 ASCII letters, digits and underscores, starting
#' and ending with a letter or underscore. Character format names use a `$`
#' prefix followed by up to 31 such characters. The prefix must match the
#' column's type; numeric and character names are distinct. Display width is
#' preserved separately. Decimal places apply only to numeric formats.
#' Common built-in display formats are rejected; use [clearFormats()] before
#' export to request generated names. Missing or explicitly cleared references
#' get unique `DDI...F` names for numeric columns or `$DDI...F` for character
#' columns. Columns can share a format only when their exported keys and label
#' texts agree. Label texts must be non-missing and at most 32767 UTF-8 bytes each.
#' Duplicate keys, conflicting definitions and reduced numeric widths that
#' cannot retain a label key exactly cause an error.
#'
#' Catalog imports retain a `sas_metadata$label_tags` character vector aligned
#' with the value labels: `""` for ordinary keys, `"."` for system missing,
#' `"_"` or an uppercase letter for tagged missing. This distinguishes label
#' definitions even when no observations use them, without treating ordinary
#' negative values as missing. Paired exports use these tags and the same
#' missing-code dictionary as the data writer; mismatched tag-vector lengths
#' and stale definitions cause an error. `label_tags` applies only to numeric
#' label definitions and must be absent or NULL for character catalogs.
#' Standalone SAS7BDAT and XPT exports
#' retain their existing behavior and do not write these label definitions.
#'
#' Paired export writes both files to temporary siblings before replacing the
#' destinations. Publication retains backups and attempts to restore both
#' originals if either rename fails. The two renames are not a single atomic
#' operation. Input objects are not modified. SAS value-label catalogs can
#' also be supplied as setup commands; see \bold{\code{\link{setupfile}()}}.
#'
#' The argument **`recode`** controls how missing values are treated. If the
#' input file has SPSS like numeric codes, they will be recoded to extended
#' (a-z) missing types when converting to Stata or SAS. If the input has Stata
#' like extended codes, they will be recoded to SPSS like numeric codes.
#'
#' Missing values are harmonized across the entire dataset by default when
#' exporting to Stata or SAS. This automatically builds a dataset-level missing
#' value dictionary when possible. Use **`harmonize = FALSE`** via the three
#' dots argument to deactivate this behavior. The alias
#' **`harmonise = FALSE`** is also accepted.
#' If the dataset-level dictionary exceeds 26 codes, conversion falls back to
#' separate dictionaries for each variable. A variable with more than 26
#' distinct numeric missing codes cannot be represented this way and export fails.
#' SAS's additional `._` tag is preserved as an `NA` with the name `"_"` in
#' the column's `na_index` attribute, for numeric columns, dates and timestamps.
#' It does not consume a letter-tag dictionary entry; the A-Z tags retain their
#' existing numeric codes from -91 to -116.
#'
#' The character **`encoding`** is passed to the direct ReadStat readers that
#' accept an encoding override. Set it to `NULL` to use the file's encoding
#' information and the reader's fallback.
#'
#' Converting to SPSS works with numerical and character labelled vectors, with
#' or without labels. The native date/time adapters write SPSS dates and
#' timestamps as seconds since 1582-10-14, SAS dates as days since 1960-01-01,
#' and SAS timestamps as seconds since 1960-01-01. Integer-backed R temporal
#' columns use double storage when exporting to SPSS or SAS to avoid overflow.
#' SPSS user-missing codes and ranges remain separate from date/time values;
#' they are retained when importing with `user_na = TRUE` and discarded with
#' `user_na = FALSE`.
#'
#' @return An invisible R data frame, when the argument **`to`** is NULL.
#'
#' @examples
#' \dontrun{
#' # Assuming an SPSS file called test.sav is located in the working directory
#' # The following command imports the file into the R environment:
#' test <- convert("test.sav")
#'
#' # The following command will extract the metadata in a DDI Codebook and
#' # produce a test.xml file in the same directory
#' convert("test.sav", to = "DDI")
#'
#' # The data may be saved separately from the DDI file, using:
#' convert("test.sav", to = "DDI", embed = FALSE)
#'
#' # Format-specific options are supplied through the three dots. For example,
#' # import the English labels from a multilingual DDI Codebook using:
#' test <- convert("test.xml", language = "en")
#'
#' # To produce a Stata file:
#' convert("test.sav", to = "Stata")
#'
#' # To produce an R file:
#' convert("test.sav", to = "R")
#'
#' # To produce an Excel file:
#' convert("test.sav", to = "Excel")
#' }
#'
#' @references
#' DDI - Data Documentation Initiative, see the
#' \href{https://ddialliance.org/}{DDI Alliance} website.
#'
#' @seealso
#' \code{\link{setupfile}},
#' \code{\link{getCodebook}},
#' \code{\link[declared]{declared}}
#'
#' @author Adrian Dusa
#'
#' @param from A path to a file, or a data.frame object
#' @param to Character, the name of a software package or a path to a specific
#' file
#' @param declared Logical, return the resulting dataset as a declared object
#' @param chartonum Logical, recode character categorical variables to numerical
#' categorical variables
#' @param recode Logical, recode missing values
#' @param encoding The character encoding used to read a file
#' @param csv Complex argument, see the Details section
#' @param n_max Integer, maximum number of rows to import from SPSS, Stata or
#' SAS files
#' @param skip Integer, number of rows to skip when importing from SPSS, Stata
#' or SAS files
#' @param ... Additional options specific to the source or destination format,
#' including `language` when importing translated DDI labels, `catalog_file`
#' and `catalog_encoding` for SAS catalog input, and `catalog_to` for paired
#' SAS value-label catalog output; see the Details section
#'
#' @export

`convert` <- function(
    from, to = NULL, declared = TRUE, chartonum = FALSE, recode = TRUE,
    encoding = "UTF-8", csv = NULL, n_max = -1L, skip = 0L, ...
) {

    if (missing(from)) {
        admisc::stopError("Argument 'from' is missing.")
    }

    funargs <- sapply(
        lapply(match.call(), deparse)[-1],
        function(x) gsub("'|\"|[[:space:]]", "", x)
    )

    # if (missing(to)) {
    #     admisc::stopError(sprintf("Argument %s is missing.", dQuote("to").)
    # }

    dots <- list(...)
    language <- dots[["language", exact = TRUE]]
    dots[["language"]] <- NULL
    catalog_to <- dots[["catalog_to", exact = TRUE]]
    dots[["catalog_to"]] <- NULL

    embed <- !isFALSE(dots$embed)

    file_extension <- dots$file_extension
    file_name <- dots$file_name
    file_id <- dots$file_id
    dots$file_extension <- NULL
    dots$file_name <- NULL
    dots$file_id <- NULL

    codeBook <- NULL
    dictionary <- dots$dictionary
    harmonize <- TRUE
    if ("harmonize" %in% names(dots)) {
        harmonize <- !isFALSE(dots$harmonize)
    }
    if ("harmonise" %in% names(dots)) {
        harmonize <- !isFALSE(dots$harmonise)
    }
    dots$harmonize <- NULL
    dots$harmonise <- NULL

    maybe_build_dictionary <- function(dataset, to, preserve_sas_tags = FALSE) {
        if (!recode || !is.null(dictionary) || !harmonize) {
            return(dictionary)
        }

        if (can_build_dictionary(dataset, to = to)) {
            result <- buildDictionary(dataset = dataset, to = to)

            if (preserve_sas_tags) {
                # Keep canonical SAS tags when assigning dataset-wide missing codes.
                tags <- match(result$old, -91:-116)
                canonical <- !is.na(tags)
                result$new[canonical] <- letters[tags[canonical]]
                available <- setdiff(letters, result$new[canonical])
                result$new[!canonical] <- available[seq_len(sum(!canonical))]
            }

            return(result)
        }

        message(
            "Too many overall missing values for harmonized ", toupper(to),
            " recoding; using per-variable recoding instead."
        )

        return(NULL)
    }

    Robject <- FALSE
    if (is.character(from)) {
        tp_from <- treatPath(from, type = "*", single = TRUE)
    } else if (is.element("data.frame", class(from))) {
        Robject <- TRUE
        # filename <- as.character(substitute(from))
        # filename <- admisc::getName(funargs["from"], object = TRUE)
        filename <- all.vars(substitute(from))[1]

        # it can have a length > 1, when something like this might be used:
        # foo(subset(dd[, c("A", "B")], subset = A == 1)) # "dd" "A"
        # or
        # foo(dd[, i]) # "dd" "i"
        # in any case, the first is (or should be) always the object's name

        tp_from <- list(
            completePath = normalizePath("~"),
            filenames = filename,
            fileext = "RDS"
        )

        dots$file_extension <- NULL

    } else {
        admisc::stopError("Unsuitable input.")
    }

    tp_to <- to

    if (!is.null(to)) {

        if (identical(toupper(to), "EXCEL")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "xlsx", sep = ".")
            )
        }
        else if (identical(toupper(to), "DDI") | identical(toupper(to), "XML")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "xml", sep = ".")
            )
        }
        else if (identical(toupper(to), "SPSS")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "sav", sep = ".")
            )
        }
        else if (identical(toupper(to), "STATA")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "dta", sep = ".")
            )
        }
        else if (identical(toupper(to), "R")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "rds", sep = ".")
            )
        }
        else if (identical(toupper(to), "SAS")) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "xpt", sep = ".")
            )
        }
        else if (!grepl("[.]", to)) {
            admisc::stopError(
                "Unknown destination software."
            )
        }

        to <- path.expand(to)
        tp_to <- treatPath(to, single = TRUE, check = FALSE)

        if (tp_to$fileext == "XLS") {
            tp_to$fileext <- "XLSX"
        }

        if (is.na(tp_to$fileext)) {
            admisc::stopError(
                "Cannot determine the destination software without a file extension."
            )
        }
        else if (
            !is.element(
                tp_to$fileext,
                c("RDS", "SAV", "ZSAV", "DTA", "XML", "XPT", "SAS7BDAT", "XLSX")
            )
        ) {
            admisc::stopError("Unknown destination software.")
        }
    }

    if (!is.null(catalog_to)) {
        if (!is.character(catalog_to) || length(catalog_to) != 1L ||
            is.na(catalog_to) || !nzchar(catalog_to)) {
            stop("catalog_to must be one non-missing, nonempty file path.", call. = FALSE)
        }

        if (is.null(to) || !identical(tp_to$fileext, "SAS7BDAT")) {
            stop("catalog_to is supported only for SAS7BDAT output.", call. = FALSE)
        }
    }

    if (tp_from$fileext == "XML") {
        xml <- getXML(from, encoding = encoding)
        data <- extractData(xml)

        dns <- getDNS(xml) # default name space
        xpath <- sprintf("/%scodeBook/%sdataDscr/%svar", dns, dns, dns)
        xmlvars <- xml2::xml_find_all(xml, xpath)

        if (length(xmlvars) == 0) {
            admisc::stopError(
                "This DDI Codebook file does not contain any variable level metadata."
            )
        }

        selected_language <- resolveDDILanguage(xmlvars, language = language)

        header <- TRUE

        # if not present in the codeBook, maybe it is on a separate .csv file
        # in the same directory as the DDI .xml Codebook
        if (is.null(data)) {
            csvisdata <- FALSE
            if (is.null(csv)) {
                files <- getFiles(tp_from$completePath, "*")

                csvexists <- FALSE
                csvfiles <- files$fileext == "CSV"

                if (any(csvfiles)) {
                    csvexists <- is.element(
                        toupper(tp_from$filenames),
                        toupper(files$filenames[csvfiles])
                    )
                    csvfile <- files$files[csvfiles][
                        match(
                            toupper(tp_from$filenames),
                            toupper(files$filenames[csvfiles])
                        )
                    ]
                }

                if (!csvexists) {
                    admisc::stopError("Datafile not found.")
                }

                csv <- file.path(tp_from$completePath, csvfile)
            }
            else {
                if (inherits(csv, "tibble")) {
                    csv <- as.data.frame(csv)
                }

                if (csvisdata <- inherits(csv, "data.frame")) {
                    data <- csv
                } else {
                    # test if the csv is a path
                    treatPath(csv)
                }
            }

            if (!csvisdata) {
                callist <- list(file = csv)
                for (f in names(formals(utils::read.csv))) {
                    if (is.element(f, names(dots))) {
                        callist[[f]] <- dots[[f]]
                    }
                }

                header <- ifelse(isFALSE(callist$header), FALSE, TRUE)
                data <- do.call("read.csv", callist)
            }

            variables <- lapply(
                xmlvars,
                XMLtoRmetadata,
                dns = dns,
                language = selected_language
            )

            xpath <- sprintf("/%scodeBook/%sdataDscr/%svar/@name", dns, dns, dns)
            names(variables) <- admisc::trimstr(
                xml2::xml_text(xml2::xml_find_all(xml, xpath))
            )

            if (!is.element(ncol(data), c(length(variables), length(variables) + 1L))) {
                admisc::stopError(
                    sprintf(
                        paste0(
                            "The .csv file does not match the DDI Codebook: ",
                            "%s data columns for %s documented variables."
                        ),
                        ncol(data),
                        length(variables)
                    )
                )
            }

            if (ncol(data) == length(variables)) {
                if (header) {
                    if (!identical(names(data), names(variables))) {
                        admisc::stopError(
                            sprintf(
                                "The .csv file does not match the DDI Codebook%s.",
                                ifelse(
                                    identical(tolower(names(data)), tolower(names(variables))),
                                    ", the variable names have differences in upper / lower case",
                                    ""
                                )
                            )
                        )
                    }
                }
                else {
                    names(data) <- names(variables)
                }
            }

            if (ncol(data) == length(variables) + 1) {
                if (header) {
                    if (!identical(names(data)[-1], names(variables))) {
                        admisc::stopError("The .csv file does not match the DDI Codebook")
                    }
                }
                else {
                    names(data) <- c("row_names_csv_file", names(variables))
                }

                rownames(data) <- data[, 1]
                data <- subset(
                    data,
                    select = seq(2, ncol(data))
                )
                # data <- data[, -1, drop = FALSE]
            }

            data <- makeLabelled(data, variables)
        }
        else {
            hashes <- attr(data, "hashes")
            attr(data, "hashes") <- NULL

            if (!is.null(hashes) || !is.null(selected_language)) {
                metadata_info <- getXMLMetadataInfo(
                    xmlvars,
                    dns = dns,
                    language = selected_language
                )
                checkhashes <- metadata_info$hashes

                if (is.null(hashes)) {
                    different <- seq_along(metadata_info$metadata)
                }
                else if (!identical(hashes, checkhashes)) {
                    different <- which(hashes != checkhashes)
                }
                else {
                    different <- integer(0)
                }

                if (length(different) > 0) {
                    for (i in different) {
                        metadata <- metadata_info$metadata[[i]]
                        for (att in c("label", "labels", "na_values", "na_range")) {
                            attr(data[[i]], att) <- getElement(metadata, att)
                        }
                    }
                }
            }
        }
    }
    else if (tp_from$fileext == "XLS" || tp_from$fileext == "XLSX") {
        data <- import_excel(from, dots)
    }
    else if (tp_from$fileext == "SAV" || tp_from$fileext == "ZSAV") {
        fargs <- names(formals(read_sav))
        arglist <- dots[is.element(names(dots), fargs)]
        arglist$file <- from
        arglist$user_na <- !isFALSE(dots$user_na)
        arglist$encoding <- encoding
        arglist$n_max <- n_max
        arglist$skip <- skip
        tc <- admisc::tryCatchWEM(
            data <- do.call(read_sav, arglist)
        )
        if (!is.null(tc$error) && grepl("Unable to convert string", tc$error)) {
            admisc::stopError(
                "This file contains non standard strings, check the encoding argument."
            )
        }
    }
    else if (tp_from$fileext == "POR") {
        fargs <- names(formals(read_por))
        arglist <- dots[is.element(names(dots), fargs)]
        arglist$file <- from
        arglist$user_na <- !isFALSE(dots$user_na)
        arglist$n_max <- n_max
        arglist$skip <- skip
        data <- do.call(read_por, arglist)
    }
    else if (tp_from$fileext == "DTA") {
        fargs <- names(formals(read_dta))
        arglist <- dots[is.element(names(dots), fargs)]
        arglist$file <- from
        arglist$encoding <- encoding
        arglist$n_max <- n_max
        arglist$skip <- skip
        data <- do.call(read_dta, arglist)

        # return(list(data = data, to = to, dictionary = dictionary, chartonum = chartonum, to_declared = FALSE, error_null = FALSE))
        if (recode && !identical(tp_to$fileext, "SAS7BDAT")) {
            data <- recodeMissings(
                dataset = data,
                to = "SPSS",
                dictionary = dictionary,
                chartonum = chartonum,
                to_declared = declared,
                error_null = FALSE
            )
        }
    }
    else if (tp_from$fileext == "SAS7BDAT") {
        fargs <- names(formals(read_sas))
        arglist <- dots[is.element(names(dots), fargs)]
        arglist$data_file <- from
        arglist$encoding <- encoding
        arglist$n_max <- n_max
        arglist$skip <- skip

        if (is.null(arglist$catalog_file)) {
            cats <- treatPath(tp_from$completePath, type = "sas7bcat")
            if (is.list(cats) && any(is.element(cats$filenames, tp_from$filenames))) {
                arglist$catalog_file <- file.path(
                    tp_from$completePath,
                    paste(tp_from$filenames, "sas7bcat", sep = ".")
                )
            }
        }

        data <- do.call(read_sas, arglist)
        if (recode && !identical(tp_to$fileext, "SAS7BDAT")) {
            data <- recodeMissings(
                dataset = data,
                to = "SPSS",
                dictionary = dictionary,
                chartonum = chartonum,
                to_declared = declared,
                error_null = FALSE
            )
        }

    }
    else if (tp_from$fileext == "XPT") {
        fargs <- names(formals(read_xpt))
        arglist <- dots[is.element(names(dots), fargs)]
        arglist$file <- from
        arglist$n_max <- n_max
        arglist$skip <- skip
        # arglist$encoding <- encoding
        data <- do.call(read_xpt, arglist)

        if (recode && !identical(tp_to$fileext, "SAS7BDAT")) {
            data <- recodeMissings(
                dataset = data,
                to = "SPSS",
                dictionary = dictionary,
                chartonum = chartonum,
                to_declared = FALSE,
                error_null = FALSE
            )
        }

    }
    else if (tp_from$fileext == "RDS" & !Robject) {
        data <- readRDS(from)
    }
    else {
        data <- from
    }

    subset <- dots$subset
    if (!is.null(subset)) {
        data <- eval(parse(text = paste("data <- subset(data, ", subset, ")")))
    }

    if (is.null(to)) {
        if (declared) {
            data <- as_declared_dataset(data)
        }

        return(invisible(data))
    }

    filetypes <- c("SPSS", "SPSS", "SPSS", "Stata", "SAS", "XPT", "R", "DDI", "Excel", "Excel")
    fileexts <- c("SAV", "ZSAV", "POR", "DTA", "SAS7BDAT", "XPT", "RDS", "XML", "XLS", "XLSX")

    if (!is.null(dots$varIDs) && length(dots$varIDs) == ncol(data)) {
        for (i in seq(ncol(data))) {
            attr(data[[i]], "ID") <- dots$varIDs[i]
        }
    }

    variables <- NULL

    if (tp_to$fileext == "XML") {
        variables <- collectRMetadata(
            data,
            infer_type = FALSE,
            include_formats = FALSE
        )

        if (admisc::anyTagged(data)) {
            admisc::stopError("DDI does not support extended missing codes")
        }

        codeBook <- makeElement("codeBook")

        addChildren(
            makeElement("stdyDscr", fill = TRUE, ... = ...),
            to = codeBook
        )

        fileName <- makeElement(
            "fileName",
            content = ifelse(
                is.null(file_name),
                tp_from$filenames,
                file_name
            )
        )

        if (is.null(file_extension)) {
            file_extension <- tp_from$fileext
        }

        fileType <- makeElement(
            "fileType",
            content = filetypes[which(fileexts == file_extension)]
        )

        dimensns <- makeElement(
            "dimensns",
            children = list(
                makeElement("caseQnty", content = nrow(data)),
                makeElement("varQnty", content = ncol(data))
            )
        )

        fileTxt <- makeElement("fileTxt", children = list(fileName, fileType, dimensns))
        fileDscr <- makeElement("fileDscr", children = list(fileTxt))

        if (!is.null(file_id)) {
            if (!is.atomic(file_id) || !is.character(file_id) || length(file_id) != 1) {
                admisc::stopError("The argument 'fileid' must be a single character string.")
            }

            names(file_id) <- "ID"
            addAttributes(file_id, to = fileDscr)
        }

        data[] <- lapply(data, function(x) {
            if (is.factor(x)) {
                x <- as.numeric(x)
            }
            return(x)
        })

        addChildren(fileDscr, to = codeBook)

        exportCodebook(
            codeBook,
            file = to,
            data = data,
            embed = embed,
            dataDscr_directly_in_XML = TRUE,
            variables = variables,
            csv = csv,
            ... = ...
        )
    }
    else if (is.element(tp_to$fileext, c("SAV", "ZSAV"))) {
        data[] <- lapply(data, function(x) {
            x <- ensure_format(x, type = "SPSS")

            na_values <- attr(x, "na_values")
            na_range <- attr(x, "na_range")

            if (is.character(x)) {
                if (length(na_values) > 3) {
                    attr(x, "na_values") <- na_values[1:3]
                }
            }
            else {
                if (
                    length(na_values) > 3 &
                    length(na_range) == 0
                ) {
                    na_range <- range(na_values)
                    attr(x, "na_values") <- NULL
                    attr(x, "na_range") <- na_range
                }
            }
            return(x)
        })

        if (admisc::anyTagged(data)) {
            admisc::stopError("SPSS does not support extended missing codes")
        }

        arglist <- list(data = data, path = to)

        if (is.element("compress", names(dots))) {
            arglist["compress"] <- dots["compress"]
        }
        else if (identical(tp_to$fileext, "ZSAV")) {
            arglist$compress <- "zsav"
        }

        if (is.element("label", names(dots))) {
            arglist["label"] <- dots["label"]
        }

        do.call(write_sav, arglist)
    }
    else if (identical(tp_to$fileext, "DTA")) {
        colnms <- colnames(data)
        arglist <- list(data = data)

        rechars <- c()
        err <- paste(
            "Stata does not allow labels for non-integer variables",
            "(e.g. \"%s\")."
        )

        for (i in seq(ncol(data))) {
            x <- data[[colnms[i]]]
            labels <- attr(x, "labels", exact = TRUE)

            if (is.null(labels) && is.factor(x)) {
                xlevels <- levels(x)
                labels <- setNames(seq_along(xlevels), xlevels)
            }

            if (!is.null(labels)) {
                if (admisc::possibleNumeric(x)) {
                    if (!admisc::wholeNumeric(admisc::asNumeric(x))) {
                        admisc::stopError(sprintf(err, colnms[i]))
                    }
                }

                if (is.character(x)) {
                    rechars <- c(rechars, i)
                    # Stata does not allow labels for character variables
                    if (chartonum && !is.null(labels)) {
                        metadata <- list(
                            label = attr(x, "label", exact = TRUE),
                            labels = labels,
                            na_values = attr(x, "na_values", exact = TRUE)
                        )
                        x <- recodeCharcat(
                            x,
                            metadata = metadata
                        )
                    }
                    else {
                        label <- attr(x, "label", exact = TRUE)
                        x <- as.character(x)
                        attr(x, "label") <- label
                    }

                    data[[colnms[i]]] <- restore_variable_metadata(x, data[[colnms[i]]])
                }
            }
        }

        data[] <- lapply(data, function(x) {
            x <- ensure_format(x, type = "Stata")
            return(x)
        })

        dictionary <- maybe_build_dictionary(data, to = "Stata")

        arglist <- list(data = data)

        if (is.element("version", names(dots))) {
            arglist$version <- dots[["version", exact = TRUE]]

            if (is.null(arglist$version)) {
                arglist$version <- 14
            }
        }
        if (!is.null(dictionary)) {
            arglist$dictionary <- dictionary
        }


        # if (requireNamespace("readstata13", quietly = TRUE)) {

        #     attr(callist$data, "var.labels") <- lapply(data, function(x) {
        #         result <- attr(x, "label", exact = TRUE)
        #         result <- lapply(result, function(x){
        #             if (is.null(x)) "" else x
        #         })
        #         base::unlist(result, use.names = TRUE)
        #     })

        #     callist$file <- to
        #     callist$compress <- TRUE
        #     do.call(readstata13::save.dta13, callist)
        # }
        # else {
            arglist$path <- to

            do.call(write_dta, arglist)
            # return(invisible(arglist$data))
        # }
    }
    else if (identical(tp_to$fileext, "RDS")) {
        if (declared) {
            if (!all(vapply(data, declared::is.declared, logical(1)))) {
                data <- as_declared_dataset(data)
            }
        }
        saveRDS(data, to)
    }
    else if (identical(tp_to$fileext, "XLSX")) {
        variables <- collectRMetadata(data, infer_type = FALSE)
        var_labels <- sapply(variables, function(x) {
            lbl <- getElement(x, "label")
            if (is.null(lbl)) {
                lbl <- ""
            }
            return(lbl)
        })

        excel_data <- data
        excel_data[] <- lapply(excel_data, function(x) {
            if (declared::is.declared(x)) {
                return(declared::undeclare(x, drop = TRUE))
            }
            x
        })
        class(excel_data) <- "data.frame"

        excel <- list(
            data = excel_data,
            variables = data.frame(
                name = names(var_labels),
                label = var_labels,
                type = sapply(data, mode)
            ),
            values = data.frame(
                variable = character(0),
                value = character(0),
                label = character(0),
                missing = character(0)
            )
        )

        varFormat <- lapply(variables, function(x) {
            as.numeric(
                unlist(
                    strsplit(
                        substring(x$varFormat[1], 2),
                        split = "\\."
                    )
                )
            )
        })

        excel$variables$width <- sapply(varFormat, function(x) {
            ifelse (all(is.na(x)), NA, x[1])
        })

        excel$variables$decimals <- sapply(varFormat, function(x) {
            ifelse (
                all(is.na(x)) || length(x) == 1,
                NA,
                x[2]
            )
        })

        value_list <- list()
        for (v in names(variables)) {
            labels <- getElement(variables[[v]], "labels")
            if (!is.null(labels)) {
                temp <- data.frame(
                    variable = v,
                    value = unname(labels),
                    label = names(labels),
                    missing = NA
                )

                na_values <- getElement(variables[[v]], "na_values")

                if (!is.null(na_values)) {
                    temp$missing[is.element(labels, na_values)] <- "y"
                }

                value_list[[length(value_list) + 1]] <- temp
            }
        }
        if (length(value_list) > 0) {
            excel$values <- do.call(rbind, c(list(excel$values), value_list))
        }

        writexl::write_xlsx(excel, path = to)
    }
    else if (identical(tp_to$fileext, "SAS7BDAT")) {
        dictionary <- maybe_build_dictionary(data, to = "SAS", preserve_sas_tags = TRUE)
        arglist <- list(data = data, path = to, catalog_file = catalog_to)

        if (!is.null(dictionary)) {
            arglist$dictionary <- dictionary
        }

        do.call(write_sas, arglist)
    }
    else {
        lnms <- nchar(colnames(data))
        if (any(lnms > 8)) {
            admisc::stopError("SAS .xpt files do not allow more than 8 characters for column names.")
        }
        fargs <- names(formals(write_xpt))
        arglist <- dots[is.element(names(dots), fargs)]
        arglist$data <- data
        arglist$path <- to
        metadata <- vendor_metadata(data, "sas_metadata", "Dataset")
        if (is.null(arglist$version) && (
            is.element("version", names(arglist)) ||
            is.null(metadata[["version", exact = TRUE]])
        )) {
            # Preserve the public default when no vendor version is supplied.
            arglist$version <- 5
        }
        dictionary <- maybe_build_dictionary(arglist$data, to = "SAS")
        if (recode && !is.null(dictionary)) {
            arglist$dictionary <- dictionary
        }
        do.call(write_xpt, arglist)

        if (hasLabels(arglist$data)) {
            to <- file.path(
                tp_from$completePath,
                paste(tp_from$filenames, "sas", sep = ".")
            )

            setupfile(
                obj = getCodebook(arglist$data),
                file = to,
                type = "SAS",
                csv = arglist$data,
                recode = recode,
                catalog = TRUE,
                dictionary = dictionary
            )
        }
    }
}
