# Export SAS-native catalog fixtures and generate independent SAS comparisons.
# Usage from the repository root:
# Rscript tools/readstat/sas-catalog-interop.R [new-output-directory]
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 1L) {
    stop("Supply at most one new output directory.")
}

output <- if (length(args) == 1L) { args[[1L]] } else { tempfile("ddiwr-sas-catalog-") }
fixture_dir <- file.path("tests", "testthat", "fixtures", "readstat")

if (!file.exists(file.path(fixture_dir, "sas94-m9-reference.sas7bcat"))) {
    stop("Run from the DDIwR repository root; the SAS-native fixtures are required.")
}

if (file.exists(output) || !dir.create(output, recursive = TRUE)) {
    stop("The output directory must be new and creatable.")
}

output <- normalizePath(output)
suppressPackageStartupMessages(library(DDIwR))
write_sas <- getFromNamespace("write_sas", "DDIwR")
read_sas <- getFromNamespace("read_sas", "DDIwR")
run_id <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")
prefix <- paste0("Z", gsub("_", "", run_id, fixed = TRUE))
files <- character()
program <- c(
    "/* Upload all manifest files to Files (Home), then run this program. */",
    "%let root=%sysget(HOME);",
    paste0("%let run_id=", run_id, ";"),
    "data _null_; path=dcreate(\"ddiwr_catalog_&run_id\",\"&root\"); if missing(path) then abort cancel; run;",
    "proc printto log=\"&root/ddiwr_catalog_&run_id/sas.log\" new; run;",
    "options source source2 nosyntaxcheck fmterr; %let syscc=0; %let failures=0;",
    "libname incoming \"&root\" access=readonly;",
    "libname evidence \"&root/ddiwr_catalog_&run_id\";",
    "data evidence.summary; length check $60 status $8 sysinfo 8; stop; run;",
    "%macro record(check,code);",
    "    data work.check; length check $60 status $8 sysinfo 8;",
    "        check=\"&check\"; sysinfo=&code; status=ifc(sysinfo < 64,'PASS','FAIL'); run;",
    "    proc append base=evidence.summary data=work.check; run;",
    "    %if &code >= 64 %then %do; %let failures=%eval(&failures+1);",
    "        %put ERROR: DDIWR_CATALOG_FAIL &check SYSINFO=&code; %end;",
    "    %else %put DDIWR_CATALOG_PASS &check SYSINFO=&code;",
    "%mend;",
    "%macro compare(base,actual,variables,check);",
    "    proc compare base=&base compare=&actual criterion=0 method=absolute; var &variables; run;",
    "    %record(&check,&sysinfo);",
    "%mend;"
)
cases <- list(
    reference = list(fixture = "sas94-m9-reference.sas7bcat", data = NULL),
    longkeys = list(fixture = "sas94-m9-long-keys.sas7bcat",
        data = data.frame(text = c("", " A", "abcdefghijklmnopqrstuv", "é"))),
    many = list(fixture = "sas94-m9-many.sas7bcat",
        data = setNames(as.data.frame(rep(list(c(1, 2)), 80L)), paste0("v", seq_len(80L))))
)

for (case in names(cases)) {
    base <- paste0("base_", case, "_", run_id)
    member <- paste0("out_", case, "_", run_id)
    base_path <- file.path(output, paste0(base, ".sas7bdat"))
    native_catalog <- file.path(output, paste0(base, ".sas7bcat"))
    path <- file.path(output, paste0(member, ".sas7bdat"))
    catalog <- file.path(output, paste0(member, ".sas7bcat"))
    copied <- file.copy(file.path(fixture_dir, cases[[case]]$fixture), native_catalog)

    if (!copied) {
        stop("Could not copy a SAS-native catalog fixture.")
    }

    if (case == "reference") {
        copied <- file.copy(file.path(fixture_dir, "sas94-m9-reference.sas7bdat"), base_path)

        if (!copied) {
            stop("Could not copy the SAS-native data fixture.")
        }
    }
    else {
        data <- cases[[case]]$data

        for (index in seq_along(data)) {
            format <- if (case == "longkeys") { "$CHARACTER_LONG_NAME" } else { sprintf("FORM%03dF", index) }
            attr(data[[index]], "sas_metadata") <- list(format = format)
        }

        write_sas(data, base_path)
    }

    data <- read_sas(base_path, catalog_file = native_catalog)
    mapping <- character(length(data))
    original <- character(length(data))

    for (index in seq_along(data)) {
        metadata <- attr(data[[index]], "sas_metadata")
        original[index] <- sub("^\\$", "", metadata$format)
        mapping[index] <- paste0(prefix, "_", index, "F")

        if (case == "longkeys") {
            mapping[index] <- paste0(prefix, "_LONG_KEY_FORMAT")
        }

        metadata$format <- paste0(if (is.character(data[[index]])) { "$" } else { "" }, mapping[index])
        attr(data[[index]], "sas_metadata") <- metadata
    }

    convert(data, path, catalog = catalog)
    files <- c(files, base_path, native_catalog, path, catalog)
    program <- c(program,
        paste0("proc catalog catalog=incoming.", member, "; contents; run; quit;"),
        paste0("proc format library=incoming.", base, " cntlout=work.native; run;"),
        "data work.expected; set work.native; select(fmtname);"
    )

    for (index in seq_along(mapping)) {
        program <- c(program, paste0("when ('", original[index], "') fmtname='", mapping[index], "';"))
    }

    program <- c(program,
        "otherwise; end; run;",
        paste0("proc format library=incoming.", member, " cntlout=work.actual; run;"),
        "proc sort data=work.expected; by fmtname type start; run;",
        "proc sort data=work.actual; by fmtname type start; run;",
        paste0("%compare(work.expected,work.actual,fmtname type start end label,", case, " native definitions);"),
        paste0("options fmtsearch=(incoming.", base, " work);"),
        paste0("data work.expected_rows; set incoming.", base, ";"),
        paste0("length ", paste0("label", seq_along(data), collapse = " "), " $32767;")
    )

    for (index in seq_along(data)) {
        program <- c(program, paste0("label", index, "=vvalue(", names(data)[index], ");"))
    }

    program <- c(program, "run;",
        paste0("options fmtsearch=(incoming.", member, " work);"),
        paste0("data work.actual_rows; set incoming.", member, ";"),
        paste0("length ", paste0("label", seq_along(data), collapse = " "), " $32767;")
    )

    for (index in seq_along(data)) {
        program <- c(program, paste0("label", index, "=vvalue(", names(data)[index], ");"))
    }

    program <- c(program, "run;",
        paste0("%compare(work.expected_rows,work.actual_rows,",
            paste(c(names(data), paste0("label", seq_along(data))), collapse = " "), ",", case, " native rows and rendered labels);"))
}

program <- c(program,
    "%put DDIWR_CATALOG_SAS_VERSION=&sysvlong4;",
    "%put DDIWR_CATALOG_SAS_PLATFORM=&sysscp &sysscpl;",
    "%put DDIWR_CATALOG_SAS_ENCODING=%sysfunc(getoption(encoding));",
    "%put DDIWR_CATALOG_RESULT failures=&failures SYSCC=&syscc;",
    "title \"Native SAS catalogs: &sysvlong4 / &sysscp / %sysfunc(getoption(encoding))\";",
    "proc print data=evidence.summary noobs; run; title; proc printto; run;"
)
program_path <- file.path(output, paste0("catalog_", run_id, ".sas"))
writeLines(enc2utf8(program), program_path, useBytes = TRUE)
files <- c(files, program_path)

if (!all(file.exists(files))) {
    stop("The validation bundle is incomplete.")
}

write.csv(data.frame(file = basename(files), bytes = file.info(files)$size,
    md5 = unname(tools::md5sum(files))), file.path(output, "manifest.csv"), row.names = FALSE)
cat("Package:", find.package("DDIwR"), "\nDLL:", getLoadedDLLs()[["DDIwR"]][["path"]],
    "\nBundle:", output, "\n")
