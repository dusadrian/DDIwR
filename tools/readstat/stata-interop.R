# Generate synthetic Stata exports and independent native assertions.
# Usage: Rscript tools/readstat/stata-interop.R NEW_DIRECTORY
args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1L || file.exists(args[1L]) ||
    !dir.create(args[1L], recursive = TRUE)) {
    stop("Supply one new output directory.")
}

suppressPackageStartupMessages(library(DDIwR))
output <- normalizePath(args[1L])
write_dta <- getFromNamespace("write_dta", "DDIwR")
data <- data.frame(id = 1:4)

for (type in c("byte", "int", "long", "float", "double")) {
    name <- paste0("v_", type)
    data[[name]] <- declared::declared(c(1, 9, NA, 2), na_values = 9,
        labels = c(One = 1, Missing = 9))
    attr(data[[name]], "stata_metadata") <- list(type = type)
}

data$day <- as.Date("2000-01-01") + 0:3
data$moment <- as.POSIXct("2000-01-01", tz = "UTC") + 0:3
data$text <- c("été", "漢字", "", paste(rep("long", 600L), collapse = ""))
attr(data$text, "stata_metadata") <- list(type = "strL")
attr(data, "label") <- "DDIwR native Stata validation"
attr(data, "stata_metadata") <- list(notes = c("First note", "Deuxième note"))
attr(data, "stata.characteristics") <- list(Source = "Archive é", source = "case sensitive")
attr(data$id, "stata.characteristics") <- list(Question = "Identifier")
attr(data$id, "label") <- "Identifier é"
write_dta(data, file.path(output, "modern.dta"), version = 15L)
legacy <- data[setdiff(names(data), "text")]
write_dta(legacy, file.path(output, "legacy.dta"), version = 8L)
program <- c(
    "* Run this do-file from the generated directory in Stata 15 or later.",
    "* A separate frame preserves the active dataset in Stata 16 and later.",
    "capture log close ddiwrinterop",
    "log using ddiwr-stata.log, text name(ddiwrinterop) replace",
    "display c(stata_version) c(os) c(machine_type)",
    "local oldframe",
    "if c(stata_version) >= 16 {",
    "    local oldframe `c(frame)'",
    "    tempname interopframe",
    "    frame create `interopframe'",
    "    frame change `interopframe'",
    "}",
    "else {", "    preserve", "}",
    "capture noisily {",
    "    foreach file in modern legacy {",
    "        use `file'.dta, clear",
    "        assert _N == 4",
    "        assert id == _n",
    "        foreach type in byte int long float double {",
    "            local actual : type v_`type'",
    "            assert \"`actual'\" == \"`type'\"",
    "            assert v_`type' == 1 in 1",
    "            assert v_`type' == .a in 2",
    "            assert v_`type' == . in 3",
    "            assert v_`type' == 2 in 4",
    "        }",
    "        assert day == td(01jan2000) + _n - 1",
    "        assert moment == tc(01jan2000 00:00:00) + (_n-1)*1000",
    "        local question : char id[Question]",
    "        assert \"`question'\" == \"Identifier\"",
    "        save roundtrip_`file'.dta, replace",
    "    }",
    "    use modern.dta, clear",
    "    local actual : type text",
    "    assert \"`actual'\" == \"strL\"",
    "    assert text == \"été\" in 1",
    "    assert text == \"漢字\" in 2",
    "    assert text == \"\" in 3",
    "    assert strlen(text) == 2400 in 4",
    "    local source : char _dta[Source]",
    "    assert \"`source'\" == \"Archive é\"",
    "    local lower : char _dta[source]",
    "    assert \"`lower'\" == \"case sensitive\"",
    "    local note : char _dta[note2]",
    "    assert \"`note'\" == \"Deuxième note\"",
    "}",
    "local result = _rc",
    "display \"DDIWR_STATA_RESULT return_code=`result'\"",
    "if c(stata_version) >= 16 {",
    "    frame change `oldframe'", "}",
    "else {", "    restore", "}",
    "log close ddiwrinterop",
    "exit `result'"
)
writeLines(enc2utf8(program), file.path(output, "validate.do"), useBytes = TRUE)
files <- list.files(output, full.names = TRUE)
write.csv(data.frame(file = basename(files), sha256 = vapply(files, function(path) {
    return(digest::digest(file = path, algo = "sha256"))
}, character(1))), file.path(output, "manifest.csv"), row.names = FALSE)
cat(output, "\n")
