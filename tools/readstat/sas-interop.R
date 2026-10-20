# Generate synthetic exports and assertions for an actual SAS Studio session.
# Usage: Rscript tools/readstat/sas-interop.R [new-output-directory]
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 1L) {
    stop("Supply at most one new output directory.")
}

output <- tempfile("ddiwr-sas-interop-")

if (length(args) == 1L) {
    output <- args[[1L]]
}

if (file.exists(output) || !dir.create(output, recursive = TRUE)) {
    stop("The output directory must be new and creatable.")
}

output <- normalizePath(output)
suppressPackageStartupMessages(library(DDIwR))
write_sas <- getFromNamespace("write_sas", "DDIwR")
read_sas <- getFromNamespace("read_sas", "DDIwR")
run_id <- format(Sys.time(), "%Y%m%d_%H%M%S", tz = "UTC")

sas_quote <- function(value) {
    return(paste0("'", gsub("'", "''", value, fixed = TRUE), "'"))
}

sas_number <- function(value) {
    if (is.na(value)) {
        return(".")
    }

    return(sprintf("%.17g", value))
}

missing_tags <- c(".", "_", LETTERS)
missing_keys <- c(NA_real_, NA_real_, as.numeric(-91:-116))
bytes <- writeBin(NA_real_, raw(), endian = "little")
bytes[5L] <- charToRaw("_")
missing_keys[2L] <- readBin(bytes, "double", n = 1L, endian = "little")
missing_labels <- c("System missing", "Underscore missing", paste("Missing", LETTERS))
values <- list(
    number = c(-2.5, 0, 1.25, 42),
    day = c(-3654, -3653, 0, 20454),
    moment = c(-315619200.125, -315619200, 0, 1767225600.125),
    clock = c(-0.125, 0, 1.25, 90000.5)
)
# Unique names prevent formats from earlier interactive runs masking lookup failures.
format_prefix <- paste0("R", gsub("_", "", run_id, fixed = TRUE))
formats <- setNames(paste0(format_prefix, c("NUMF", "CALF", "STAMPF", "CLOCKF", "TEXTF")),
    c("number", "day", "moment", "clock", "text"))
formats[["text"]] <- paste0("$", formats[["text"]])
offsets <- c(number = 0, day = 3653, moment = 315619200, clock = 0)
data <- data.frame(row_id = as.numeric(seq_len(32L)))

for (name in names(values)) {
    column <- c(values[[name]], rep(NA_real_, 28L))

    if (name == "day") {
        class(column) <- "Date"
    }
    else if (name == "moment") {
        class(column) <- c("POSIXct", "POSIXt")
        attr(column, "tzone") <- "UTC"
    }
    else if (name == "clock") {
        class(column) <- c("hms", "difftime")
        attr(column, "units") <- "secs"
    }

    labels <- c(values[[name]], missing_keys)
    names(labels) <- c(paste(name, seq_len(4L)), missing_labels)
    attr(column, "labels") <- labels
    attr(column, "na_index") <- structure(6:32, names = c("_", as.character(-91:-116)))
    attr(column, "na_values") <- as.numeric(-91:-116)
    attr(column, "sas_metadata") <- list(format = formats[[name]],
        label_tags = c(rep("", 4L), missing_tags))
    data[[name]] <- column
}

data$text <- rep(c("01", "1", " A", "é", "漢字", ""), length.out = nrow(data))
attr(data$text, "labels") <- c(ZeroOne = "01", One = "1", Leading = " A",
    Accent = "é", Unicode = "漢字", Blank = "", Unused = "unused")
attr(data$text, "sas_metadata") <- list(format = formats[["text"]], width = 40L)
attr(data, "label") <- "DDIwR actual SAS interoperability"

members <- paste0(c("mixed_", "empty_", "narrow_"), run_id)
names(members) <- c("mixed", "empty", "narrow")
paths <- file.path(output, paste0(members, ".sas7bdat"))
catalogs <- file.path(output, paste0(members, ".sas7bcat"))
write_sas(data, paths[1L], catalog_file = catalogs[1L])
empty <- read_sas(paths[1L], catalog_file = catalogs[1L], n_max = 0L)
write_sas(empty, paths[2L], catalog_file = catalogs[2L])
narrow <- data.frame(row_id = c(1, 2), number = c(4097, 0),
    day = structure(c(0, 1), class = "Date"))
attr(narrow$number, "labels") <- c(First = 4097, Zero = 0)
attr(narrow$number, "sas_metadata") <- list(format = paste0(format_prefix, "SMALLF"), width = 3L)
attr(narrow$day, "labels") <- c(Epoch = 0, Tomorrow = 1)
attr(narrow$day, "sas_metadata") <- list(format = paste0(format_prefix, "SHORTF"), width = 3L)
write_sas(narrow, paths[3L], catalog_file = catalogs[3L])

program <- c(
    "/* Upload this program and the six generated binary files to Files (Home). */",
    "/* These checks require SAS itself; generating this bundle is not a pass. */",
    "%let input_dir=%sysget(HOME);",
    paste0("%let run_id=", run_id, ";"),
    "%let result_dir=&input_dir/ddiwr_validation_&run_id;",
    "data _null_;",
    "    length path $1024;",
    "    path=dcreate(\"ddiwr_validation_&run_id\", \"&input_dir\");",
    "    if missing(path) then abort cancel;",
    "run;",
    "proc printto log=\"&result_dir/sas.log\" new; run;",
    "options source source2 fmterr nosyntaxcheck noerrorabend;",
    "%let syscc=0;",
    "libname source \"&input_dir\" access=readonly;",
    "libname results \"&result_dir\";",
    "%let failures=0;",
    "%put DDIWR_SAS_VERSION=&sysvlong4;",
    "%put DDIWR_SAS_PLATFORM=&sysscp &sysscpl;",
    "%let session_encoding=%sysfunc(getoption(encoding));",
    "%put DDIWR_SAS_ENCODING=&session_encoding;",
    "%macro fail(message);",
    "    %let failures=%eval(&failures+1);",
    "    %put ERROR: DDIWR_INTEROP_FAIL &message;",
    "%mend;",
    "%macro compare(base,actual,variables,id);",
    "    %if not %sysfunc(exist(&actual)) %then %do;",
    "        %fail(Missing comparison table &actual);",
    "        %return;",
    "    %end;",
    "    proc sort data=&base; by &id; run;",
    "    proc sort data=&actual; by &id; run;",
    "    proc compare base=&base compare=&actual criterion=0 method=absolute;",
    "        id &id; var &variables;",
    "    run;",
    "    /* Lower bits describe metadata, checked separately below. */",
    "    %if &sysinfo >= 64 %then %fail(Comparison &base against &actual SYSINFO=&sysinfo);",
    "%mend;",
    "data work.expected_rows;",
    "    length text $40;"
)

for (index in seq_len(nrow(data))) {
    numbers <- NULL

    if (index <= 4L) {
        numbers <- vapply(names(values), function(name) {
            return(sas_number(values[[name]][index] + offsets[[name]]))
        }, character(1))
    }
    else {
        tag <- missing_tags[index - 4L]
        numbers <- rep(paste0(".", sub("^\\.$", "", tag)), length(values))
        names(numbers) <- names(values)
    }

    assignments <- paste(paste(names(numbers), numbers, sep = "="), collapse = "; ")
    program <- c(program, paste0("    row_id=", index, "; ", assignments,
        "; text=", sas_quote(data$text[index]), "; output;"))
}

program <- c(program, "run;", "data work.expected_labels;",
    "    length fmtname $32 type $1 cstart $40 label $100;", "    nstart=.; cstart='';")

for (name in names(values)) {
    labels <- attr(data[[name]], "labels")

    for (index in seq_along(labels)) {
        key <- NULL

        if (index <= 4L) {
            key <- sas_number(values[[name]][index] + offsets[[name]])
        }
        else {
            key <- paste0(".", sub("^\\.$", "", missing_tags[index - 4L]))
        }

        program <- c(program, paste0("    fmtname=", sas_quote(formats[[name]]),
            "; type='N'; nstart=", key, "; label=", sas_quote(names(labels)[index]), "; output;"))
    }
}

for (index in seq_along(attr(data$text, "labels"))) {
    labels <- attr(data$text, "labels")
    program <- c(program, paste0("    fmtname=", sas_quote(sub("^\\$", "", formats[["text"]])),
        "; type='C'; nstart=.; cstart=",
        sas_quote(unname(labels[index])), "; label=", sas_quote(names(labels)[index]), "; output;"))
}

program <- c(program, "run;",
    "data work.expected_rendered;",
    "    length number_label day_label moment_label clock_label text_label $100;"
)

for (index in seq_len(nrow(data))) {
    assignments <- vapply(names(values), function(name) {
        text <- NULL

        if (index <= 4L) {
            text <- paste(name, index)
        }
        else {
            text <- missing_labels[index - 4L]
        }

        return(paste0(name, "_label=", sas_quote(text)))
    }, character(1))
    text_labels <- attr(data$text, "labels")
    text_label <- names(text_labels)[match(data$text[index], text_labels)]
    program <- c(program, paste0("    row_id=", index, "; ", paste(assignments, collapse = "; "),
        "; text_label=", sas_quote(text_label), "; output;"))
}

program <- c(program, "run;",
    "%macro check(member,expected_count);",
    "    options nofmterr fmtsearch=(source.&member);",
    "    proc contents data=source.&member out=results.columns_&expected_count noprint; run;",
    "    %if &syserr ne 0 %then %fail(PROC CONTENTS &member SYSERR=&syserr);",
    "    data _null_; set results.columns_&expected_count;",
    "        if name='day' and upcase(informat) ne 'DATE' then call execute('%fail(Date informat);');",
    "        if name='moment' and upcase(informat) ne 'DATETIME' then call execute('%fail(Datetime informat);');",
    "        if name='clock' and upcase(informat) ne 'TIME' then call execute('%fail(Time informat);');",
    "    run;",
    "    data work.actual_rows; set source.&member; format _all_; run;",
    "    %if &syserr ne 0 %then %fail(Read &member SYSERR=&syserr);",
    "    %if &expected_count=32 %then %do;",
    "        %compare(work.expected_rows,work.actual_rows,number day moment clock text,row_id);",
    "    %end;",
    "    data _null_;",
    "        dsid=open(\"source.&member\",'I');",
    "        if dsid then do;",
    "            count=attrn(dsid,'NLOBS'); rc=close(dsid);",
    "            if count ne &expected_count then call execute('%fail(Unexpected observation count);');",
    "        end;",
    "        else call execute('%fail(Could not open dataset for observation count);');",
    "    run;",
    "    proc format library=source.&member cntlout=work.catalog; run;",
    "    %if &syserr ne 0 %then %do;",
    "        %fail(PROC FORMAT &member SYSERR=&syserr);",
    "        %return;",
    "    %end;",
    "    data work.actual_labels;",
    "        length fmtname $32 type $1 cstart $40 label $100;",
    "        set work.catalog;",
    "        nstart=.; cstart='';",
    "        if type='N' then nstart=input(start,best32.);",
    "        else if type='C' then cstart=start;",
    "    run;",
    "    %compare(work.expected_labels,work.actual_labels,nstart cstart label,fmtname type nstart cstart);",
    "    %if &expected_count=32 %then %do;",
    "        data work.actual_rendered;",
    "            length number_label day_label moment_label clock_label text_label $100;",
    "            set source.&member;",
    "            number_label=strip(vvalue(number)); day_label=strip(vvalue(day));",
    "            moment_label=strip(vvalue(moment)); clock_label=strip(vvalue(clock)); text_label=strip(vvalue(text));",
    "        run;",
    "        %if &syserr ne 0 %then %fail(Render &member SYSERR=&syserr);",
    "        %compare(work.expected_rendered,work.actual_rendered,number_label day_label moment_label clock_label text_label,row_id);",
    "    %end;",
    "%mend;",
    paste0("%check(", members[["mixed"]], ",32);"),
    paste0("%check(", members[["empty"]], ",0);"),
    paste0("options nofmterr fmtsearch=(source.", members[["narrow"]], ");"),
    paste0("proc contents data=source.", members[["narrow"]], " out=results.narrow_columns noprint; run;"),
    "%if &syserr ne 0 %then %do; %fail(Narrow dataset PROC CONTENTS failed); %end;",
    paste0("proc format library=source.", members[["narrow"]], " cntlout=work.narrow_catalog; run;"),
    "%if &syserr ne 0 %then %do; %fail(Narrow catalog could not be read); %end;",
    "data work.narrow_actual; length number_label day_label $100;",
    paste0("    set source.", members[["narrow"]], ";"),
    "    number_label=strip(vvalue(number)); day_label=strip(vvalue(day));",
    "run;",
    "data work.narrow_expected;",
    "    length number_label day_label $100;",
    "    row_id=1; number=4097; day=3653; number_label='First'; day_label='Epoch'; output;",
    "    row_id=2; number=0; day=3654; number_label='Zero'; day_label='Tomorrow'; output;",
    "run;",
    "%compare(work.narrow_expected,work.narrow_actual,number day number_label day_label,row_id);",
    "data _null_; set results.narrow_columns;",
    "    if name in ('number','day') and length ne 3 then call execute('%fail(Narrow storage length);');",
    "run;",
    "/* Save an independently SAS-created catalog for reciprocal import checks. */",
    "proc format library=results.reference;",
    "    value REFDATE -1='Before SAS epoch' 0='SAS epoch' 3653='R epoch'",
    "        .='System missing' ._='Underscore missing' .A='Missing A' .Z='Missing Z';",
    "    value $REFTEXT '01'='ZeroOne' ' A'='Leading' 'é'='Accent' '漢字'='Unicode' ' '='Blank';",
    "run;",
    "options fmtsearch=(results.reference);",
    "data results.reference;",
    "    length text $40; informat day date9.; format day REFDATE. text $REFTEXT.;",
    "    day=3653; text='01'; output; day=.; text=' '; output;",
    "    day=._; text=' A'; output; day=.A; text='é'; output; day=.Z; text='漢字'; output;",
    "run;",
    "%if &syserr ne 0 %then %do; %fail(Could not create SAS reference fixture); %end;",
    "data results.summary;",
    "    length sas_version $80 platform $80 encoding $40;",
    "    failures=&failures; sas_version=\"&sysvlong4\"; platform=\"&sysscp &sysscpl\"; encoding=\"&session_encoding\";",
    "run;",
    "%put DDIWR_SAS_INTEROP_RESULT failures=&failures SYSCC=&syscc;",
    "proc print data=results.summary; run;",
    "proc printto; run;",
    "%put DDIWR_SAS_INTEROP_RESULTS=&result_dir;"
)
program_path <- file.path(output, paste0("validate_", run_id, ".sas"))
writeLines(enc2utf8(program), program_path, useBytes = TRUE)
files <- c(paths, catalogs, program_path)
manifest <- data.frame(file = basename(files), bytes = file.info(files)$size,
    md5 = unname(tools::md5sum(files)))
write.csv(manifest, file.path(output, "manifest.csv"), row.names = FALSE)
cat("Package:", find.package("DDIwR"), "\nVersion:", as.character(packageVersion("DDIwR")),
    "\nBundle:", output, "\n")
print(manifest, row.names = FALSE)
