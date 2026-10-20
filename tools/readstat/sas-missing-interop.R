# Generate a data-only validation bundle for an actual SAS runtime.
# Usage from the repository root:
# Rscript tools/readstat/sas-missing-interop.R [new-output-directory]
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 1L) {
    stop("Supply at most one new output directory.")
}

fixture <- file.path("tests", "testthat", "fixtures", "readstat", "sas94-m9-reference.sas7bdat")
catalog <- sub("sas7bdat$", "sas7bcat", fixture)

if (!file.exists(fixture) || !file.exists(catalog)) {
    stop("Run from the DDIwR repository root; the SAS-native fixture pair is required.")
}

output <- tempfile("ddiwr-sas-missing-")

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
members <- setNames(paste0(c("native_", "lowlevel_", "public_", paste0("width", 3:8, "_"),
    "dict_", "empty_"), run_id), c("native", "lowlevel", "public", paste0("width", 3:8), "dict", "empty"))
paths <- setNames(file.path(output, paste0(members, ".sas7bdat")), names(members))

if (!file.copy(fixture, paths[["native"]])) {
    stop("Could not copy the independent SAS-native reference.")
}

native <- read_sas(fixture, catalog_file = catalog)
write_sas(native, paths[["lowlevel"]])
convert(native, paths[["public"]])
values <- c(1.25, -1.25, NA_real_, NaN, rep(NA_real_, 27L))
integers <- c(1L, -1L, rep(NA_integer_, 29L))
data <- data.frame(
    row_id = as.numeric(seq_len(31L)),
    number = values, integer = integers,
    day = structure(c(-3653, 0, values[-(1:2)]), class = "Date"),
    integer_day = structure(c(-3653L, 0L, integers[-(1:2)]), class = "Date"),
    moment = structure(c(-315619200.125, 0.125, values[-(1:2)]),
        class = c("POSIXct", "POSIXt"), tzone = "UTC"),
    integer_moment = structure(c(-315619200L, 0L, integers[-(1:2)]),
        class = c("POSIXct", "POSIXt"), tzone = "UTC"),
    clock = structure(c(-0.125, 90000.5, values[-(1:2)]), class = c("hms", "difftime"), units = "secs"),
    flag = c(TRUE, FALSE, rep(NA, 29L))
)
columns <- setdiff(names(data), "row_id")

for (name in setdiff(columns, "flag")) {
    attr(data[[name]], "na_index") <- structure(5:31, names = c("_", as.character(-91:-116)))
    attr(data[[name]], "na_values") <- as.numeric(-91:-116)
}

for (width in 3:8) {
    for (name in columns) {
        attr(data[[name]], "sas_metadata") <- list(width = width)
    }

    write_sas(data, paths[[paste0("width", width)]])
}

write_sas(data, paths[["dict"]], dictionary = data.frame(old = -91:-116, new = rev(letters)))
write_sas(read_sas(paths[["width8"]], n_max = 0L), paths[["empty"]])
program <- c(
    "/* Upload these data files and this program to Files (Home), then run. */",
    "/* Catalogs are deliberately excluded; this checks native data encodings. */",
    "%let input_dir=%sysget(HOME);",
    paste0("%let run_id=", run_id, ";"),
    "%let result_dir=&input_dir/ddiwr_missing_&run_id;",
    "data _null_; path=dcreate(\"ddiwr_missing_&run_id\",\"&input_dir\"); if missing(path) then abort cancel; run;",
    "proc printto log=\"&result_dir/sas.log\" new; run;",
    "options source source2 nosyntaxcheck nofmterr;",
    "%let syscc=0;",
    "libname incoming \"&input_dir\" access=readonly;",
    "libname evidence \"&result_dir\";",
    "%let failures=0;",
    "data evidence.summary; length check $60 status $8 sysinfo 8; stop; run;",
    "%macro record(check,code);",
    "    data work.check; length check $60 status $8 sysinfo 8;",
    "        check=\"&check\"; sysinfo=&code; status=ifc(sysinfo < 64,'PASS','FAIL');",
    "    run;",
    "    proc append base=evidence.summary data=work.check; run;",
    "    %if &code >= 64 %then %do;",
    "        %let failures=%eval(&failures+1);",
    "        %put ERROR: DDIWR_MISSING_FAIL &check SYSINFO=&code;",
    "    %end;",
    "    %else %put DDIWR_MISSING_PASS &check SYSINFO=&code;",
    "%mend;",
    "%macro compare(member,base,variables,check);",
    "    data work.actual; set incoming.&member; format _all_; informat _all_; run;",
    "    %if &syserr ne 0 %then %do; %record(&check,32768); %return; %end;",
    "    proc compare base=&base compare=work.actual criterion=0 method=absolute;",
    "        var &variables;",
    "    run;",
    "    /* Low bits describe metadata; width is checked independently below. */",
    "    %record(&check,&sysinfo);",
    "%mend;",
    paste0("data work.native_expected; set incoming.", members[["native"]], "; format _all_; informat _all_; run;"),
    paste0("%compare(", members[["lowlevel"]], ",work.native_expected,day text,SAS-native fixture via write_sas);"),
    paste0("%compare(", members[["public"]], ",work.native_expected,day text,SAS-native fixture via convert);"),
    "%macro matrix(member,width,reversed);",
    "    data evidence.expected_&member;",
    "        length row_id 8 number integer day integer_day moment integer_moment clock flag &width;",
    "        length tag $1;",
    "        array values[7] number integer day integer_day moment integer_moment clock;",
    "        do row_id=1 to 31;",
    "            if row_id=1 then do;",
    "                number=1.25; integer=1; day=0; integer_day=0;",
    "                moment=-0.125; integer_moment=0; clock=-0.125; flag=1;",
    "            end;",
    "            else if row_id=2 then do;",
    "                number=-1.25; integer=-1; day=3653; integer_day=3653;",
    "                moment=315619200.125; integer_moment=315619200; clock=90000.5; flag=0;",
    "            end;",
    "            else do;",
    "                if row_id in (3,4) then tag='.';",
    "                else if row_id=5 then tag='_';",
    "                else if &reversed then tag=substr('ZYXWVUTSRQPONMLKJIHGFEDCBA',row_id-5,1);",
    "                else tag=substr('ABCDEFGHIJKLMNOPQRSTUVWXYZ',row_id-5,1);",
    "                do index=1 to dim(values);",
    "                    if tag='.' then values[index]=.;",
    "                    else values[index]=input(cats('.',tag),best32.);",
    "                end;",
    "                flag=.;",
    "            end;",
    "            output;",
    "        end;",
    "        drop tag index;",
    "    run;",
    "    %compare(&member,evidence.expected_&member,number integer day integer_day moment integer_moment clock flag,&member);",
    "    proc contents data=incoming.&member out=work.columns noprint; run;",
    "    proc sql noprint; select count(*) into :width_errors trimmed from work.columns",
    "        where name ne 'row_id' and length ne &width; quit;",
    "    %if &width_errors ne 0 %then %record(Storage widths &member,32768);",
    "%mend;"
)

for (width in 3:8) {
    program <- c(program, paste0("%matrix(", members[[paste0("width", width)]], ",", width, ",0);"))
}

program <- c(program,
    paste0("%matrix(", members[["dict"]], ",8,1);"),
    "data _null_;",
    paste0("    dsid=open('incoming.", members[["empty"]], "','I');"),
    "    if dsid then do; count=attrn(dsid,'NLOBS'); rc=close(dsid); end; else count=-1;",
    "    call symputx('empty_count',count);",
    "run;",
    "%macro empty_check;",
    "    %if &empty_count=0 %then %record(Empty data,0); %else %record(Empty data,32768);",
    "%mend; %empty_check;",
    "%put DDIWR_ACTUAL_SAS_VERSION=&sysvlong4;",
    "%put DDIWR_ACTUAL_SAS_PLATFORM=&sysscp &sysscpl;",
    "%put DDIWR_ACTUAL_SAS_ENCODING=%sysfunc(getoption(encoding));",
    "%put DDIWR_NATIVE_MISSING_RESULT failures=&failures SYSCC=&syscc;",
    "title \"Native SAS missing values: &sysvlong4 / &sysscp / %sysfunc(getoption(encoding))\";",
    "proc print data=evidence.summary noobs; run; title;",
    "proc printto; run;",
    "%put DDIWR_NATIVE_MISSING_EVIDENCE=&result_dir;"
)
program_path <- file.path(output, paste0("missing_", run_id, ".sas"))
writeLines(enc2utf8(program), program_path, useBytes = TRUE)
files <- c(paths, program_path)
manifest <- data.frame(file = basename(files), bytes = file.info(files)$size,
    md5 = unname(tools::md5sum(files)))
write.csv(manifest, file.path(output, "manifest.csv"), row.names = FALSE)
cat("Package:", find.package("DDIwR"), "\nDLL:", getLoadedDLLs()[["DDIwR"]][["path"]],
    "\nBundle:", output, "\n")
print(manifest, row.names = FALSE)
