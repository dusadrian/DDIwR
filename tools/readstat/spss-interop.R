# Create synthetic files and a native IBM SPSS validation program.
# Usage: Rscript tools/readstat/spss-interop.R NEW_DIRECTORY [SPSS_DIRECTORY]
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1L || length(args) > 2L || file.exists(args[1L]) ||
    !dir.create(args[1L], recursive = TRUE)) {
    stop("Supply a new output directory and optional directory as seen by SPSS.")
}

suppressPackageStartupMessages(library(DDIwR))
output <- normalizePath(args[1L])
root <- if (length(args) == 2L) { args[2L] } else { output }
write_sav <- getFromNamespace("write_sav", "DDIwR")
data <- data.frame(id = 1:3, first = c(1, 0, 1), second = c(0, 1, 1),
    text_one = c("Yes é", "No", "漢字"), text_two = c("No", "Yes é", "漢字"))
attr(data, "label") <- "DDIwR interoperability é"
attr(data, "spss.attributes") <- list(Source = c("Archive", "étude"))
attr(data$first, "spss.attributes") <- list(Question = "Choose é")
attr(data$first, "label") <- "First choice"
attr(data$second, "label") <- "Second choice"
attr(data$first, "labels") <- c(No = 0, Yes = 1)
attr(data$second, "labels") <- c(No = 0, Yes = 1)
attr(data$first, "spss_metadata") <- list(format = "F9.2", measure = "ordinal",
    alignment = "center", display_width = 17L)
attr(data$text_one, "spss_metadata") <- list(width = 20L, alignment = "left",
    display_width = 23L, measure = "nominal")
sets <- list(
    `$categories` = list(type = "C", label = "Categories é", variables = c("first", "second")),
    `$picked` = list(type = "D", label = "Picked é", variables = c("first", "second"), counted_value = 1),
    `$counted` = list(type = "E", label = "Counted é", variables = c("first", "second"), counted_value = 1),
    `$words` = list(type = "D", label = "Words é", variables = c("text_one", "text_two"), counted_string = "Yes é")
)
attr(data, "spss_metadata") <- list(notes = c("One", "Deux é"), mr_sets = sets)

for (compression in c("none", "byte", "zsav")) {
    extension <- if (compression == "zsav") { ".zsav" } else { ".sav" }
    write_sav(data, file.path(output, paste0(compression, extension)), compress = compression)
}

empty <- data[FALSE, , drop = FALSE]
attributes(empty)[setdiff(names(attributes(data)), c("names", "row.names", "class"))] <-
    attributes(data)[setdiff(names(attributes(data)), c("names", "row.names", "class"))]
for (name in names(data)) {
    attributes(empty[[name]]) <- attributes(data[[name]])
}
attr(empty$text_two, "spss_metadata") <- list(width = 20L)
write_sav(empty, file.path(output, "empty.sav"))
program <- c(
    "* Synthetic validation only. Existing named datasets remain open.",
    "DATASET DECLARE DDIWR_INTEROP.", "DATASET ACTIVATE DDIWR_INTEROP.",
    "BEGIN PROGRAM Python3.",
    "import os, json, traceback, spss",
    paste0("root = os.path.expanduser(", encodeString(root, quote = '"'), ")"),
    "report = {'version': spss.GetDefaultPlugInVersion(), 'cases': [], 'failures': 0}",
    "for filename, count in [('none.sav',3),('byte.sav',3),('zsav.zsav',3),('empty.sav',0)]:",
    "    case = {'file': filename}",
    "    try:",
    "        spss.Submit(\"GET FILE='%s'.\" % os.path.join(root,filename).replace(\"'\",\"''\"))",
    "        spss.Submit('DATASET NAME DDIWR_INTEROP.')",
    "        assert spss.GetCaseCount() == count, 'row count'",
    "        assert spss.GetVariableCount() == 5, 'column count'",
    "        assert spss.GetVariableFormat(1) == 'F9.2', 'format'",
    "        assert spss.GetVariableMeasurementLevel(1) == 'ordinal', 'measurement'",
    "        assert spss.GetVariableType(3) == 20, 'string storage width'",
    "        assert spss.GetVarAttributes(1,'Question') == ('Choose é',), 'variable attributes'",
    "        assert spss.GetDataFileAttributes('Source') == ('Archive','étude'), 'dataset attributes'",
    "        names = spss.GetMultiResponseSetNames()",
    "        assert set(n.lower() for n in names) == {'$categories','$picked','$counted','$words'}, 'MR set names'",
    "        case['mr_sets'] = {name: spss.GetMultiResponseSet(name) for name in names}",
    "        spss.StartDataStep()",
    "        try:",
    "            dataset = spss.Dataset()",
    "            assert dataset.varlist[1].alignment == 2, 'alignment'",
    "            assert dataset.varlist[1].columnWidth == 17, 'display width'",
    "            if count:",
    "                assert [row[0] for row in dataset.cases] == [1,2,3], 'IDs'",
    "                assert [row[1] for row in dataset.cases] == [1,0,1], 'numeric data'",
    "                assert [row[3].rstrip() for row in dataset.cases] == ['Yes é','No','漢字'], 'Unicode data'",
    "        finally:",
    "            spss.EndDataStep()",
    "        saved = os.path.expanduser('~/Desktop/ddiwr-'+filename+'.sav')",
    "        spss.Submit(\"SAVE OUTFILE='%s'.\" % saved.replace(\"'\",\"''\"))",
    "        case['roundtrip'] = saved",
    "        case['status'] = 'PASS'",
    "    except Exception:",
    "        case['status'] = 'FAIL'",
    "        case['error'] = traceback.format_exc()",
    "        report['failures'] += 1",
    "    report['cases'].append(case)",
    "destination = os.path.expanduser('~/Desktop/ddiwr-spss-results.json')",
    "with open(destination,'w',encoding='utf-8') as handle: json.dump(report,handle,ensure_ascii=False,indent=2)",
    "print('DDIWR_SPSS_RESULT',json.dumps(report,ensure_ascii=False))",
    "print('Saved evidence:',destination)",
    "relay = os.path.join(root,'relay.txt')",
    "if os.path.isfile(relay):",
    "    import urllib.request",
    "    with open(relay) as handle: address = handle.read().strip()",
    "    request = urllib.request.Request(address+'/results.json',data=json.dumps(report,ensure_ascii=False).encode('utf-8'),method='POST')",
    "    urllib.request.urlopen(request,timeout=10).close()",
    "    for case in report['cases']:",
    "        if 'roundtrip' in case:",
    "            with open(case['roundtrip'],'rb') as handle: payload = handle.read()",
    "            request = urllib.request.Request(address+'/roundtrip-'+case['file']+'.sav',data=payload,method='POST')",
    "            urllib.request.urlopen(request,timeout=10).close()",
    "END PROGRAM."
)
writeLines(enc2utf8(program), file.path(output, "validate.sps"), useBytes = TRUE)
files <- list.files(output, full.names = TRUE)
write.csv(data.frame(file = basename(files), sha256 = vapply(files, function(path) {
    return(digest::digest(file = path, algo = "sha256"))
}, character(1))), file.path(output, "manifest.csv"), row.names = FALSE)
cat(output, "\n")
