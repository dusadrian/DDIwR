# Generate independent SAS rule-catalog comparisons for any available SAS host.
# Usage: Rscript tools/readstat/sas-rules-interop.R NEW_DIRECTORY [SAS_DIRECTORY]
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1L || length(args) > 2L || file.exists(args[1L]) ||
    !dir.create(args[1L], recursive = TRUE)) {
    stop("Supply a new output directory and optional directory as seen by SAS.")
}

suppressPackageStartupMessages(library(DDIwR))
output <- normalizePath(args[1L])
fixtures <- file.path("tests", "testthat", "fixtures", "readstat")
read_sas <- getFromNamespace("read_sas", "DDIwR")
write_sas <- getFromNamespace("write_sas", "DDIwR")
run_id <- format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC")
root <- if (length(args) == 2L) { gsub("'", "''", args[2L], fixed = TRUE) } else { "%sysget(HOME)" }
program <- c(
    "/* Upload every manifest file to the input directory, then run. */",
    paste0("%let root=", root, ";"),
    "libname incoming \"&root\" access=readonly;",
    paste0("data _null_; path=dcreate('ddiwr_rules_", run_id, "',symget('root')); if missing(path) then abort cancel; run;"),
    paste0("libname evidence \"&root/ddiwr_rules_", run_id, "\";"),
    paste0("proc printto log=\"&root/ddiwr_rules_", run_id, "/sas.log\" new; run;"),
    "options source source2 nosyntaxcheck fmterr; %let syscc=0; %let failures=0;",
    "data evidence.summary; length check $60 code 8; stop; run;",
    "%macro compare(base,actual,variables,check);",
    "    proc compare base=&base compare=&actual criterion=0 method=absolute; var &variables; run;",
    "    %let code=&sysinfo;",
    "    data work.check; length check $60 code 8; check=\"&check\"; code=&code; run;",
    "    proc append base=evidence.summary data=work.check; run;",
    "    %if &code >= 64 %then %let failures=%eval(&failures+1);",
    "%mend;"
)
cases <- list(
    mixed = list(catalog = "sas94-m9-rules.sas7bcat", data = "sas94-m9-rules.sas7bdat"),
    long = list(catalog = "sas94-m9-long-rules.sas7bcat", format = "$LONGRULE",
        values = c("A", "abcdefghijklmnopqrstuv", "abcdefghijklmnopqrstuw", "abcdefghijklmnopqrstuz", "漢字abcdefghijklmnopq", "漢字abcdefghijklmnopz")),
    limits = list(catalog = "sas94-m9-long-rules.sas7bcat", format = "$LIMITS",
        values = c("", " ", "0", "A", "B", "漢字")),
    only = list(catalog = "sas94-m9-long-rules.sas7bcat", format = "ONLYOTHER", values = c(-1, 0, 1, NA_real_))
)

for (case in names(cases)) {
    definition <- cases[[case]]
    base <- paste0("base_", case, "_", run_id)
    actual <- paste0("out_", case, "_", run_id)
    input <- file.path(output, paste0(base, ".sas7bdat"))
    native_catalog <- file.path(output, paste0(base, ".sas7bcat"))
    stopifnot(file.copy(file.path(fixtures, definition$catalog), native_catalog))

    if (case == "mixed") {
        stopifnot(file.copy(file.path(fixtures, definition$data), input))
    }
    else {
        data <- data.frame(value = definition$values)
        vendorMetadata(data$value, "SAS", "format") <- definition$format
        write_sas(data, input)
    }

    data <- read_sas(input, catalog_file = native_catalog)
    originals <- vapply(data, function(column) {
        return(sub("^[$]", "", vendorMetadata(column, "SAS", "format")))
    }, character(1))
    formats <- paste0("R", run_id, "_", toupper(case), "_", seq_along(data), "F")

    for (index in seq_along(data)) {
        prefix <- if (is.character(data[[index]])) { "$" } else { "" }
        vendorMetadata(data[[index]], "SAS", "format") <- paste0(prefix, formats[index])
    }

    convert(data, file.path(output, paste0(actual, ".sas7bdat")),
        catalog = file.path(output, paste0(actual, ".sas7bcat")))
    program <- c(program,
        paste0("proc format library=incoming.", base, " cntlout=work.native; run;"),
        "data work.expected; set work.native; select(fmtname);"
    )

    for (index in seq_along(data)) {
        program <- c(program, paste0("when ('", originals[index], "') fmtname='", formats[index], "';"))
    }

    program <- c(program,
        "otherwise delete; end; run;",
        paste0("proc format library=incoming.", actual, " cntlout=work.actual; run;"),
        "proc sort data=work.expected; by fmtname type start; run;",
        "proc sort data=work.actual; by fmtname type start; run;",
        paste0("%compare(work.expected,work.actual,fmtname type start end label hlo sexcl eexcl default fuzz,", case, " definitions);"),
        paste0("options fmtsearch=(incoming.", base, ");"),
        paste0("data work.expected_rows; set incoming.", base, ";"),
        paste0("length ", paste0("rendered", seq_along(data), collapse = " "), " $32767;")
    )

    for (index in seq_along(data)) {
        program <- c(program, paste0("rendered", index, "=vvalue(", names(data)[index], ");"))
    }

    program <- c(program, "run;",
        paste0("options fmtsearch=(incoming.", actual, ");"),
        paste0("data work.actual_rows; set incoming.", actual, ";"),
        paste0("length ", paste0("rendered", seq_along(data), collapse = " "), " $32767;")
    )

    for (index in seq_along(data)) {
        program <- c(program, paste0("rendered", index, "=vvalue(", names(data)[index], ");"))
    }

    program <- c(program, "run;", paste0(
        "%compare(work.expected_rows,work.actual_rows,",
        paste(c(names(data), paste0("rendered", seq_along(data))), collapse = " "),
        ",", case, " rows and labels);"))
}

program <- c(program,
    "data evidence.environment; length sas_version platform encoding $80;",
    "    sas_version=\"&sysvlong4\"; platform=\"&sysscp &sysscpl\"; encoding=getoption('encoding');",
    "    failures=&failures; syscc=&syscc; run;",
    "proc print data=evidence.summary; run; proc print data=evidence.environment; run;",
    "proc export data=evidence.environment outfile=\"%sysfunc(pathname(evidence))/environment.csv\" dbms=csv replace; run;",
    "proc export data=evidence.summary outfile=\"%sysfunc(pathname(evidence))/summary.csv\" dbms=csv replace; run;",
    "%put DDIWR_RULES_RESULT failures=&failures SYSCC=&syscc;",
    "proc printto; run;"
)
writeLines(program, file.path(output, paste0("rules_", run_id, ".sas")))
files <- list.files(output, full.names = TRUE)
write.csv(data.frame(file = basename(files), sha256 = vapply(files, function(path) {
    return(digest::digest(file = path, algo = "sha256"))
}, character(1))), file.path(output, "manifest.csv"), row.names = FALSE)
cat(output, "\n")
