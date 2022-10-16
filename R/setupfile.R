#' @name setupfile
#' @title
#' Create setup files for SPSS, Stata, SAS and R
#'
#' @description
#' Creates a setup file, based on a list of variable and value labels.
#'
#' @details
#' When a path to a metadata directory is specified for the argument **`obj`**,
#' then next argument **`file`** is silently ignored and all created setup files
#' are saved in a directory called "Setup Files" that (if not already found) is
#' created in the working directory.
#'
#' The argument **`file`** expects the name of the final setup file being
#' saved on the disk. If not specified, the name of the object provided for the
#' **`obj`** argument will be used as a filename.
#'
#' If **`file`** is specified, the argument **`type`** is automatically
#' determined from the file's extension, otherwise when **`type = "all"`**, the
#' function produces one setup file for each supported type.
#'
#' If batch processing multiple files, the function will inspect all files in
#' the provided directory, and retain only those with the extension  `.R` or
#' `.r` or DDI versions with the extension `.xml` or `.XML` (it will
#' subsequently generate an error if the .R files do not contain an object list,
#' or if the `.xml` files do not contain a DDI structured metadata file).
#'
#' If the metadata directory contains a subdirectory called `"data"` or
#' `"Data"`, it will match the name of the metadata file with the name of the
#' `.csv` file (their names have to be *exactly* the same, regardless of
#' their extension).
#'
#' The **`csv`** argument can provide a data frame object produced by reading
#' the `.csv` file, or a path to the directory where the `.csv` files are
#' located. If the user doesn't provide something for this argument, the
#' function will check the existence of a subdirectory called `data` in the
#' directory where the metadata files are located.
#'
#' In batch mode, the code starts with the argument **`delim = ","`**, but if
#' the `.csv` file is delimited differently it will also try hard to find other
#' delimiters that will match the variable names in the metadata file. At the
#' initial version 0.1-0, the automatically detected delimiters include `";"`
#' and `"\t"`.
#'
#' The argument `OS` (case insensitive) can be either:\cr
#' `"Windows"` (default), or `"Win"`,\cr
#' `"MacOS"`, `"Darwin"`, `"Apple"`, `"Mac"`,\cr
#' `"Linux"`.\cr
#'
#' The end of line character(s) changes only when the target OS is different
#' from the running OS.
#'
#' @return
#' A setup file to complement the imported raw dataset.
#'
#' @examples
#' codebook <- list(dataDscr = list(
#' ID = list(
#'     label = "Questionnaire ID",
#'     type = "num",
#'     measurement = "interval"
#' ),
#' V1 = list(
#'     label = "Label for the first variable",
#'     labels = c(
#'         "No"             =  0,
#'         "Yes"            =  1,
#'         "Not applicable" = -97,
#'         "Not answered"   = -99),
#'     na_values = c(-99, -97),
#'     type = "cat",
#'     measurement = "nominal"
#' ),
#' V2 = list(
#'     label = "Label for the second variable",
#'     labels = c(
#'         "Very little"    =  1,
#'         "Little"         =  2,
#'         "So, so"         =  3,
#'         "Much"           =  4,
#'         "Very much"      =  5,
#'         "Don't know"     = -98),
#'     na_values = c(-98),
#'     type = "cat",
#'     measurement = "ordinal"
#' ),
#' V3 = list(
#'     label = "Label for the third variable",
#'     labels = c(
#'         "First answer"   = "A",
#'         "Second answer"  = "B",
#'         "Don't know"     = -98),
#'     na_values = c(-98),
#'     type = "cat",
#'     measurement = "nominal"
#' ),
#' V4 = list(
#'     label = "Number of children",
#'     labels = c(
#'         "Don't know"     = -98,
#'         "Not answered"   = -99),
#'     na_values = c(-99, -98),
#'     type = "numcat",
#'     measurement = "ratio"
#' )))
#'
#'
#' \dontrun{
#' # IMPORTANT:
#' # make sure to set the working directory to a directory with
#' # read/write permissions
#' # setwd("/path/to/read/write/directory")
#'
#'
#' setupfile(codebook)
#'
#'
#' # if the csv data file is available
#' setupfile(codebook, csv="/path/to/csv/file.csv")
#'
#'
#' # generating a specific type of setup file
#' setupfile(codebook, file = "codebook.do") # type = "Stata" also works
#'
#'
#' # other types of possible utilizations, using paths to specific files
#' # an XML file containing a DDI metadata object
#'
#' setupfile("/path/to/the/metadata/file.xml", csv="/path/to/csv/file.csv")
#'
#'
#' # or in batch mode, specifying entire directories
#' setupfile("/path/to/the/metadata/directory", csv="/path/to/csv/directory")
#' }
#'
#' @author Adrian Dusa
#'
#' @param obj
#' A data frame, or a list object containing the metadata, or a path to a data
#' file or to a directory where such objects are located, for batch processing
#'
#' @param file Character, the (path to the) setup file to be created
#'
#' @param type
#' The type of setup file, can be: "SPSS", "Stata", "SAS", "R", or "all"
#' (default)
#'
#' @param csv
#' The original dataset, used to create the setup file commands, or a path
#' to the directory where the .csv files are located, for batch processing
#'
#' @param recode
#' Logical, recode missing values to extended .a-.z range
#'
#' @param OS The target operating system, for the eol - end of line character(s)
#'
#' @param stringnum Logical, recode string variables to numeric
#'
#' @param ... Other arguments, see Details below
#'
#' @export

`setupfile` <- function(
    obj, file = "", type = "all", csv = "", recode = TRUE, OS = "",
    stringnum = TRUE, ...
) {

    # TO DO: when obj is a path to a file or directory,
    # it should be (only) XML and not R anymore
    on.exit(suppressWarnings(sink()))
    
    dataDscr_objname <- deparse(substitute(obj))

    if (is.data.frame(obj)) {
        cobj <- obj
        obj <- getMetadata(obj)
        obj$fileDscr <- list(datafile = cobj)
    }

    dots <- list(...)
    type <- toupper(type)
    exts <- c("sps", "do", "sas", "R")
    types <- c("SPSS", "STATA", "SAS", "R")
    catalog <- isTRUE(dots$catalog)
    dictionary <- dots$dictionary

    tp <- NULL

    recall <- isTRUE(dots$recall)
    saveFile <- isTRUE(dots$saveFile)
    script <- isTRUE(dots$script) ## what did I want to do with this...?

    outdir <- identical(file, "") | isTRUE(dots$outdir)

    if (!identical(file, "")) {
        tp <- treatPath(file, check = FALSE)

        if (is.na(tp$fileext)) {
            if (identical(toupper(type), "ALL") & !recall) {
                admisc::stopError("Unknown file type.")
            }
            else {
                if (is.element(type, c(types, "ALL"))) {
                    tp$fileext <- exts[which(types == type)]
                }
                else {
                    admisc::stopError("Unknown destination type.")
                }
            }
        }
        else {
            if (identical(toupper(type), "ALL")) {
                if (is.element(toupper(tp$fileext), toupper(exts))) {
                    type <- types[which(toupper(exts) == toupper(tp$fileext))]
                }
                else {
                    admisc::stopError("Unknown file type.")
                }
            }
            else {
                if ((tp$fileext == "SPS" & toupper(type) != "SPSS") |
                    (tp$fileext == "DO" & toupper(type) != "STATA") |
                    (tp$fileext == "SAS" & toupper(type) != "SAS") |
                    (tp$fileext == "R" & toupper(type) != "R")) {
                    admisc::stopError("Incompatible type and file extension.")
                }
            }
        }

        if (length(tp$filenames) == 1) {
            outdir <- FALSE
        }
    }

    indent <- 4
    if (is.element("indent", names(dots))) {
        indent <- dots$indent
    }

    forcenum  <- c("") # variable names that are to be forced as numeric
    if (is.element("forcenum", names(dots))) {
        forcenum <- dots$forcenum
    }

    delim <- ","
    if (is.element("delim", names(dots))) {
        delim <- dots$delim
    }
    else if (is.element("sep", names(dots))) {
        delim <- dots$sep
    }

    if (OS == "") {
        OS <- Sys.info()[['sysname']]
    }

    csvlist <- NULL

    completePath <- treatPath("test", check = FALSE)$completePath
    singlefile <- isTRUE(dots$singlefile)

    if (singlefile) {
        completePath <- dots$completePath
    }

    if (missing(obj)) {
        admisc::stopError(
            "The argument <obj> is missing, with no default."
        )
    }
    else if (all(is.character(obj))) {
            # all() just in case someone provides a vector by mistake
        if (length(obj) > 1) {
            admisc::stopError(
                paste(
                    "The argument <obj> should contain",
                    "a single path to the object."
                )
            )
        }

        pathtofiles <- FALSE

        labelist <- treatPath(obj, check = FALSE)
        if (length(labelist) == 1) {
            admisc::stopError(labelist)
        }
        else {
            pathtofiles <- TRUE
        }

        singlefile <- length(labelist$fileext) == 1

        outdir <- outdir & !singlefile

        if (singlefile) {
            completePath <- labelist$completePath
        }

        csvdatadir <- FALSE # by default


        # now trying to assess what the csv argument is
        # it can be an object containing csv data, or
        # it can be a string containing a path to the data


        if (all(is.character(csv))) {
            if (csv != "") {
                if (length(csv) > 1) {
                    admisc::stopError(
                        paste(
                            "The argument <csv> should contain",
                            "a single path to the .csv file."
                        )
                    )
                }

                tc <- admisc::tryCatchWEM(csvlist <- treatPath(csv, type = "csv"))
                if (is.null(tc)) {
                    if (length(csvlist$files) > 1) {
                        datadir <- csvlist$completePath
                        csvdatadir <- TRUE
                    }
                }
                else {
                    cat("\nNOTE:", tc$error)
                    # since "err" is now an error message from treatPath()
                }
            }
            else {
                # differentiate between "data" and "Data",
                # some OSs that are case sensitive
                datathere <- file.exists(
                    file.path(
                        labelist$completePath,
                        "data"
                    )
                )

                datadir <- file.path(
                    labelist$completePath,
                    "Data"
                )

                # TODO:
                # return(list(datathere, datadir))

                csvdatadir <- file.exists(datadir)

                if (csvdatadir) {
                    csvlist <- treatPath(datadir, type = "csv")
                    if (length(csvlist) == 1) {
                        csvdatadir <- FALSE
                        cat(paste(
                            paste(
                                "\nNOTE: There is a ",
                                ifelse(
                                    datathere,
                                    "data",
                                    "Data"
                                ),
                                " directory within ",
                                labelist$completePath,
                                ". ",
                                sep = ""
                            ),
                            csvlist
                        ))
                    }
                }
            }
        }



        if (csvdatadir) {
            csvfiles <- csvlist$files
            csvnames <- csvlist$filenames
            csvext <- csvlist$fileext
            if (outdir) {
                cat("Processing (including data directory):\n")
            }
        }
        else {

            if (is.data.frame(csv)) {
                if (length(labelist$files) > 1) {
                    admisc::stopError(
                        paste(
                            "There are multiple files containing labels",
                            "and only one .csv file provided."
                        )
                    )
                }
            }

            if (outdir) {
                cat("Processing (no data directory):\n")
            }
        }

        for (i in seq(length(labelist$files))) {
            if (pathtofiles) {
                obj <- getMetadata(
                    file.path(
                        labelist$completePath,
                        labelist$files[i]
                    ),
                    fromsetupfile = TRUE,
                    embed = TRUE,
                    save = saveFile
                )

                if (!is.null(obj[["fileDscr"]][["datafile"]])) {
                    csv <- obj[["fileDscr"]][["datafile"]]
                }
            }
            else {
                aa <- ls()

                tryCatch(
                    eval(
                        parse(
                            file.path(
                                labelist$completePath,
                                labelist$files[i]
                            )
                        )
                    ), error = function(x) {
                    admisc::stopError(
                        paste(
                            "There is an error associated with the file \"",
                            labelist$files[i],
                            "\", see below:\n       ",
                            gsub(
                                "Error in ",
                                "",
                                as.character(x)
                            ),
                            sep = ""
                        )
                    )
                })

                bb <- ls()
                bb <- setdiff(bb, aa) # bb[-which(bb == "aa")]
            }

            currentdir <- getwd()

            if (csvdatadir) {

                if (is.element(labelist$filenames[i], csvnames)) {

                    if (outdir) {
                        cat(paste(labelist$filenames[i], "\n"))
                    }

                    position <- match(
                        labelist$filenames[i],
                        csvnames
                    )

                    for (j in seq(length(position))) {

                        if (
                            is.element(
                                csvext[position[j]],
                                c("CSV", "CSV.GZ")
                            )
                        ) {

                            csvreadfile <- read.csv(
                                file.path(
                                    datadir,
                                    csvfiles[position[j]]
                                ),
                                # delim is already set from the function's formal argument
                                sep = delim,
                                header = TRUE,
                                as.is = TRUE
                            )

                            if (ncol(csvreadfile) == 1) {
                                delim <- getDelimiter(
                                    file.path(
                                        datadir,
                                        csvfiles[position[j]]
                                    )
                                )

                                if (delim == "unknown") {
                                    admisc::stopError(
                                        paste(
                                            "Unknown column separator for the file",
                                            csvfiles[position[j]],
                                            "\nShould be either \",\" or \";\" or tab separated."
                                        )
                                    )
                                }

                                csvreadfile <- read.csv(
                                    file.path(
                                        datadir,
                                        csvfiles[position[j]]
                                    ),
                                    sep = delim,
                                    header = TRUE,
                                    as.is = TRUE
                                )
                            }


                            if (!pathtofiles) {
                                obj <- get(setdiff(bb, aa))
                            }

                            tryCatch(
                                Recall(
                                    obj,
                                    type = type,
                                    csv = csvreadfile,
                                    delim = delim,
                                    OS = OS,
                                    file = ifelse(identical(file, ""), labelist$filenames[i], file),
                                    outdir = outdir,
                                    stringnum = stringnum,
                                    singlefile = singlefile,
                                    completePath = completePath,
                                    recall = TRUE,
                                    ... = ...
                                ),
                                error = function(x) {
                                    # if no sink() is needed, an invisible warning message will be returned
                                    tryCatch(
                                        sink(),
                                        warning = function(y) {
                                            return(invisible(y))
                                        }
                                    )
                                    setwd(currentdir)
                                    cat(paste(
                                        "     There is an error associated with the file \"",
                                        labelist$files[i],
                                        "\", see below:\n     ",
                                        sep = ""
                                    ))

                                    cat(as.character(x))
                                }
                            )
                        }
                    }
                }
                else {
                    cat(paste(
                        labelist$filenames[i],
                        "(no .csv file)",
                        "\n"
                    ))

                    if (!pathtofiles) {
                        obj <- get(setdiff(bb, aa))
                    }

                    tryCatch(
                        Recall(
                            obj,
                            type = type,
                            delim = delim,
                            OS = OS,
                            file = ifelse(identical(file, ""), labelist$filenames[i], file),
                            outdir = outdir,
                            stringnum = stringnum,
                            singlefile = singlefile,
                            completePath = completePath,
                            recall = TRUE,
                            ... = ...
                        ),
                        error = function(x) {
                            tryCatch(
                                sink(),
                                warning = function(y) {
                                    return(invisible(y))
                                }
                            )

                            setwd(currentdir)
                            cat(paste(
                                "     There is an error associated with the file \"",
                                labelist$files[i],
                                "\", see below:\n     ",
                                sep = ""
                            ))

                            cat(as.character(x))
                        }
                    )
                }
            }
            else {
                if (outdir) {
                    cat(labelist$filenames[i], "\n")
                }

                if (is.data.frame(csv) | length(csvlist) > 1) {
                    if (length(labelist$filenames) == 1) {
                        if (!pathtofiles) {
                            obj <- get(setdiff(bb, aa))
                        }

                        if (!identical(file, "")) {
                            outdir <- FALSE
                            labelist$filenames <- tp$filenames
                        }

                        # return(list(obj = obj,
                        #             type = type,
                        #             csv = csv,
                        #             delim = delim,
                        #             OS = OS,
                        #             file = ifelse(identical(file, ""), labelist$filenames, file),
                        #             outdir = outdir,
                        #             ... = ...))

                        tryCatch(
                            Recall(
                                obj,
                                type = type,
                                csv = csv,
                                delim = delim,
                                OS = OS,
                                file = ifelse(identical(file, ""), labelist$filenames, file),
                                outdir = outdir,
                                stringnum = stringnum,
                                singlefile = singlefile,
                                completePath = completePath,
                                recall = TRUE,
                                ... = ...
                            ),
                            error = function(x) {
                                tryCatch(
                                    sink(),
                                    warning = function(y) {
                                        return(invisible(y))
                                    }
                                )
                                setwd(currentdir)

                                cat(paste(
                                    "     There is an error associated with the file \"",
                                    labelist$files[i],
                                    "\", see below:\n     ",
                                    sep = ""
                                ))

                                cat(as.character(x))
                            }
                        )
                    }
                }
                else {
                    # there is really no csv data
                    if (!pathtofiles) {
                        obj <- get(setdiff(bb, aa))
                    }

                    tryCatch(
                        Recall(
                            obj,
                            type = type,
                            delim = delim,
                            OS = OS,
                            file = ifelse(identical(file, ""), labelist$filenames[i], file),
                            outdir = outdir,
                            stringnum = stringnum,
                            singlefile = singlefile,
                            completePath = completePath,
                            recall = TRUE,
                            ... = ...
                        ),
                        error = function(x) {
                            tryCatch(
                                sink(),
                                warning = function(y) {
                                    return(invisible(y))
                                }
                            )

                            setwd(currentdir)

                            cat(paste(
                                "     There is an error associated with the file \"",
                                labelist$files[i],
                                "\", see below:\n     ",
                                sep = ""
                            ))

                            cat(as.character(x))
                        }
                    )
                }
            }

            if (!pathtofiles) {
                rm(list = c(eval(setdiff(bb, aa)), "bb", "aa"))
            }

        }

        if (outdir) {
            cat(paste(
                "\nSetup file(s) created in:\n",
                file.path(
                    currentdir,
                    "Setup files"
                ),
                "\n\n",
                sep = ""
            ))
        }

        return(invisible())
    }
    else if (is.list(obj)) {
        if (!is.null(obj[["fileDscr"]][["datafile"]])) {
            csv <- obj[["fileDscr"]][["datafile"]]
        }
        dataDscr <- obj[["dataDscr"]]
    
        outdir <- FALSE
    }
    else {
        admisc::stopError("Unknown input for the argument <obj>.")
    }

    anymissing <- any(unlist(lapply(dataDscr, function(x) {
        if (!is.element("labels", names(x))) return(FALSE)
        return(is.element("na_values", names(x)))
    })))



    csvlist <- NULL # initialization
    if (is.character(csv)) {
        if (all(csv != "")) {
            if (length(csv) > 1) {
                admisc::stopError(
                    paste(
                        "The argument <csv> should contain",
                        "a single path to the .csv file."
                    )
                )
            }

            csvlist <- treatPath(csv, type = "CSV")

            if (length(csvlist) > 1) {
                # no error
                if (length(csvlist$files) > 1) {
                    admisc::stopError(
                        paste(
                            "There is only one object containing",
                            "metadata and multiple csv files."
                        )
                    )
                }
            }
            else {
                # There is a single string returned by treatPath(), with an error message
                cat("\nNOTE:", csvlist)
                csv <- "" # back to the default value
            }
        }
    }
    else if (
        is.data.frame(csv) &
        is.null(dictionary) &
        is.element(type, c("SPSS", "STATA", "SAS"))
    ) {
        if (type == "STATA") {
            type <- "Stata"
        }

        dictionary <- recodeMissings(
            make_labelled(csv, obj$dataDscr),
            to = ifelse(type == "SAS", "Stata", type),
            return_dictionary = TRUE
        )
    }

    `checkvarlab` <- function(dataDscr) {
        any(sapply(dataDscr, function(x) {
            is.element("label", names(x))
        }))
    }

    # variables that have missing values, with or without labels
    `variables_missing` <- function(dataDscr) {
        lapply(dataDscr, function(x) {
            return(any(is.element(c("na_values", "na_range"), names(x))))
        })
    }

    # if it has labels, all that refer to missing values
    `labels_missing` <- function(dataDscr) {
        lapply(dataDscr, function(x) {
            if (!is.element("labels", names(x))) return(FALSE)

            labels <- x[["labels"]]
            ismiss <- is.element(labels, x[["na_values"]])

            if (is.element("na_range", names(x)) && admisc::possibleNumeric(labels)) {
                na_range <- x[["na_range"]]
                labels <- as.numeric(labels)
                ismiss <- ismiss | (
                    labels >= na_range[1] & labels <= na_range[2]
                )
            }

            return(ismiss)
        })
    }

    # for recoding into SPSS
    `values_missing` <- function(dataDscr, range = FALSE, numvars = TRUE) {
        lapply(dataDscr, function(x) {
            na_values <- NULL
            if (is.element("na_values", names(x))) {
                na_values <- sort(x[["na_values"]])
            }
            labels <- x[["labels"]]
            na_range <- x[["na_range"]]
            if (length(na_range) > 0) {
                if (range) {
                    if (na_range[1] == -Inf) na_range[1] <- "LO"
                    if (na_range[2] == Inf) na_range[1] <- "HI"
                    na_values <- c(
                        na_values,
                        paste(
                            na_range,
                            collapse = " THRU "
                        )
                    )
                }
                else if (admisc::possibleNumeric(labels)) {
                    labels <- as.numeric(labels)
                    na_values <- c(
                        na_values,
                        labels[labels >= na_range[1] & labels <= na_range[2]]
                    )
                }
            }
            return(na_values)
        })
    }


    if (is.null(names(dataDscr)) | !checkvarlab(dataDscr)) {
        admisc::stopError(
            "The object does not contain variables and / or labels."
        )
    }

    if (!is.element(toupper(type), c(types, "ALL"))) {
        admisc::stopError("Unknown destination type.")
    }

    enter <- getEnter(OS = OS)

    names(dataDscr) <- toupper(names(dataDscr))
    varnames <- names(dataDscr)
    maxchars <- max(nchar(varnames))
    varcheck <- rep(0, length(varnames))
    formats <- FALSE

    csv_is_df <- is.data.frame(csv)

    csv_is_path <- FALSE
    if (length(csv) == 1) {
        # csv is a character vector of length 1, i.e. a path
        if (is.character(csv)) {
            if (csv != "") {
                csv_is_path <- TRUE
            }
        }
    }

    if (csv_is_df | csv_is_path) {

        if (!is.null(csvlist)) {

            csvreadfile <- read.csv(
                file.path(
                    csvlist$completePath,
                    csvlist$files[1]
                ),
                # delim is already set from the function's formal argument
                sep = delim,
                header = TRUE,
                as.is = TRUE
            )

            if (ncol(csvreadfile) == 1) {

                delim <- getDelimiter(
                    file.path(
                        csvlist$completePath,
                        csvlist$files[1]
                    )
                )

                if (delim == "unknown") {
                    admisc::stopError(
                        paste(
                            "Unknown column separator for the file",
                            csvlist$files[1],
                            "\nShould be either \",\" or \";\" or tab separated."
                        )
                    )
                }

                csvreadfile <- read.csv(
                    file.path(
                        csvlist$completePath,
                        csvlist$files[1]
                    ),
                    sep = delim,
                    header = TRUE,
                    as.is = TRUE
                )
            }

            # cat("\n")
            # cat("Found \"", csvlist$files[1], "\" in the directory \"", csvlist$completePath, "\". Using that as the .csv file.\n\n", sep = "")

            csv <- csvreadfile
        }

        colnames(csv) <- toupper(colnames(csv))
        csvnames <- colnames(csv)
        spssformats <- sasformats <- rep("", length(csvnames))
        if (!is.data.frame(csv)) {
            admisc::stopError("The csv file should be a data frame.")
        }

        gofurther <- TRUE

        plusnames <- setdiff(csvnames, varnames)
        # print(plusnames)

        if (length(plusnames) > 0) {
            if (length(plusnames) == length(csvnames)) {
                cat(paste(
                    "    None of the variables in the .csv ",
                    "file have metadata information.\n",
                    "    (perhaps the .csv file doesn't ",
                    "have the variable names in the first row?)\n",
                    sep = ""
                ))
                gofurther <- FALSE
            }
            else {
                cat("    There is no metadata information for the following variables in the .csv file:\n")
                plusnames <- strwrap(paste(plusnames, collapse=", "), 75)
                for (pnms in plusnames) {
                    cat("       ", pnms, "\n")
                }
                cat("\n")
            }
        }


        plusnames <- setdiff(varnames, csvnames)
        if (length(plusnames) > 0) {
            cat(paste(
                "    There is metadata information for the following",
                "variables, but *not* in the .csv file:\n"
            ))
            plusnames <- strwrap(paste(plusnames, collapse=", "), 75)
            for (pnms in plusnames) {
                cat("       ", pnms, "\n")
            }

            if (gofurther) {
                cat(paste(
                    "       ",
                    ifelse(
                        length(plusnames) == 1,
                        "This variable",
                        "These variables"
                    ),
                    "will be omitted.\n"
                ))
            }
            else {
                cat("\n")
            }
        }

        nrowscsv <- nrow(csv)


        if (gofurther) {

            # filter the variable labels
            dataDscr <- dataDscr[is.element(varnames, csvnames)]

            varnames <- names(dataDscr)
            maxchars <- max(nchar(varnames))
            varcheck <- rep(0, length(varnames))
            vartypes <- emptyvars <- vector(mode = "list", length = ncol(csv))
            names(vartypes) <- names(emptyvars) <- csvnames

            printNOTE <- FALSE

            for (i in seq(ncol(csv))) {

                vartypes[i] <- "numeric"
                decimals <- FALSE

                tempvar <- csv[[varnames[i]]]
                emptyvars[[i]] <- all(is.na(tempvar))

                if (is.character(tempvar)) {
                    vartypes[i] <- "string"

                    # %in% performs better than == when NAs are present
                    # any(tempvar == ".") generates an error, while
                    # any(tempvar %in% ".") doesn't generate any error

                    if (any(tempvar %in% ".")) { # Stata type empty cells
                        if (sum(tempvar %in% "") == 0) {
                            # test if there are pure blank cells
                            # if none than a "." must be representing a Stata type missing
                            printNOTE <- TRUE
                            tempvar[tempvar %in% "."] <- NA
                        }
                    }
                }

                if (is.element("labels", names(dataDscr[[csvnames[i]]]))) {
                    if (is.character(dataDscr[[csvnames[i]]][["labels"]])) {
                        vartypes[i] <- "string"
                    }
                }

                spssformats[i] <- getFormat(tempvar, type = "SPSS")
                
                if (vartypes[i] == "string") {
                    sasformats[i] <- " $"
                }

                varcheck[i] <- 1
            }

            if (printNOTE) {
                cat("    NOTE: some variable(s) in this file have a \".\" sign to represent a missing.\n")
                cat("    The .csv file might not import properly in some software.\n\n")
            }

            formats <- all(spssformats != "")
        }

        ## TO check if any of the existing metadata variables is not found in the CSV data file
    }

    charvars <- sapply(dataDscr, function(x) {
        grepl("char", x$type)
    })

    stringvars <- lapply(dataDscr, function(x) {
        sv <- FALSE
        if (is.element("labels", names(x))) {
            sv <- is.character(x[["labels"]])
        }
        return(sv)
    })

    if (outdir && identical(file, "")) {
        file <- dataDscr_objname
    }
    else {
        if (identical(file, "")) {
            if (grepl("\"", dataDscr_objname)) {
                file <- readline("Name for the setup file:\n")
            }
            else {
                file <- dataDscr_objname
            }
        }
        else {
            tp <- treatPath(file, check = FALSE)

            if (singlefile) {
                tp$completePath <- completePath
            }

            if (is.na(tp$fileext)) {
                if (toupper(type) == "ALL") {
                    outdir <- TRUE
                }
                else {
                    file <- file.path(
                        tp$completePath,
                        paste(
                            tp$files,
                            exts[which(types == toupper(type))],
                            sep = "."
                        )
                    )
                }
            }
            else {
                if (!identical(toupper(type), "ALL")) {
                    if ((tp$fileext == "SPS" & toupper(type) != "SPSS") |
                        (tp$fileext == "DO" & toupper(type) != "STATA") |
                        (tp$fileext == "SAS" & toupper(type) != "SAS") |
                        (tp$fileext == "R" & toupper(type) != "R")) {
                        admisc::stopError("Incompatible type and file extension.")
                    }
                }
                else {
                    if (tp$fileext == "SPS") type <- "SPSS"
                    if (tp$fileext == "DO") type <- "STATA"
                    if (tp$fileext == "SAS") type <- "SAS"
                    if (tp$fileext == "R") type <- "R"
                }
            }
        }
    }


    haslabels <- sapply(
        dataDscr,
        function(x) is.element("labels", names(x))
    )

    uniquevals <- unique(
        lapply(
            dataDscr[haslabels],
            function(x) return(x[["labels"]])
        )
    )


    unique_list <- lapply(uniquevals, function(uniques) {
        vars <- sapply(dataDscr[haslabels],
                    function(x) {
                        identical(x[["labels"]], uniques)
                    }
                )
        return(names(vars[vars]))
    })


    # initiate an empty list
    numerical_with_strings <- list()

    if (!identical(forcenum, c(""))) {

        if (!all(is.element(forcenum, csvnames))) {
            cat(paste(
                "The following <numerical> variable ",
                "names were not found in the data:\n",
                "\"",
                paste(
                    setdiff(forcenum, csvnames),
                    collapse = "\", \""
                ),
                "\"\n\n",
                sep = ""
            ))
        }

        which_numerical <- intersect(csvnames, forcenum)

        if (length(which_numerical) > 0) {

            # test which values are not numbers in the respective variables

            # has_strings  <- lapply(subset(csv, select = which_numerical), function(x) {
            #    grep("^-?[0-9]+([.]?[0-9]+)?$", x, perl=TRUE, invert=TRUE)
            # })

            has_strings  <- lapply(
                subset(csv, select = which_numerical),
                # csv[, which_numerical, drop = FALSE],
                function(x) {
                    which(!admisc::possibleNumeric(x, each = TRUE))
                }
            )

            numerical_with_strings  <- has_strings[unlist(lapply(has_strings, length)) > 0]
        }
    }



    if (toupper(type) == "SPSS" | toupper(type) == "ALL") {
        dataDscr2 <- dataDscr
        printMISSING <- FALSE
        currentdir <- getwd()

        if (outdir) {
            if (!file.exists("Setup files")) {
                dir.create("Setup files")
            }

            if (!file.exists(file.path("Setup files", "SPSS"))) {
                dir.create(file.path("Setup files", "SPSS"))
            }

            setwd(file.path("Setup files", "SPSS"))
        }
        
        sink(
            ifelse(
                grepl("\\.sps", file),
                file,
                paste(file, ".sps", sep = "")
            )
        )


        if (script) {
            to <- treatPath(
                dots$to,
                check = FALSE
            )

            cat(paste(
                "GET FILE = \"",
                file.path(to$completePath, to$files[1]),
                "\" .",
                enter,
                enter,
                sep = ""
            ))
        }
        else {
            cat(paste(
                "* ------------------------------------------------------------------------------",
                enter, enter,
                "* --- CONFIGURATION SECTION - START ---",
                enter, enter, enter,
                sep = ""
            ))

            if (formats) {
                cat(paste(
                    "* The following command should contain the complete path and",
                    enter,
                    "* name of the .csv file to be read (e.g. \"C:/file.csv\")",
                    enter,
                    "* Change CSV_DATA_PATH to your filename, below:",
                    enter, enter,
                    "FILE HANDLE csvpath /NAME=\"CSV_DATA_PATH\" .",
                    enter, enter, enter,
                    sep = ""
                ))
            }

            cat(paste(
                "* The following command should contain the complete path and",
                enter,
                "* name of the final .sav file (e.g. \"C:/file.sav\")",
                enter,
                "* Change SAV_DATA_PATH to your filename, below:",
                enter, enter,
                "FILE HANDLE savfile /NAME=\"SAV_DATA_PATH\" .",
                enter, enter, enter,
                "* --- CONFIGURATION SECTION -  END  ---",
                enter, enter,
                "* ------------------------------------------------------------------------------",
                enter, enter, enter, enter,
                "* There should be nothing to change below this line",
                enter,
                "* ------------------------------------------------------------------------------",
                enter, enter, enter, enter,
                sep = ""
            ))


            if (formats) {
                cat(paste(
                    "* -------------- Start Definition Macro --------------",
                    enter, enter,
                    "SET LOCALE = \"English\" .",
                    enter,
                    "SHOW LOCALE .",
                    enter, enter,
                    # SET DECIMAL = COMMA . * (might be another idea)
                    "* --------------     Read Raw Data      --------------",
                    enter, enter,
                    "GET DATA",
                    enter,
                    " /TYPE=TXT",
                    enter,
                    " /FILE=csvpath",
                    enter,
                    " /DELCASE=LINE",
                    enter,
                    " /ARRANGEMENT=DELIMITED",
                    enter,
                    " /DELIMITERS='",
                    ifelse(delim == "\t", "\\t", delim), "'",
                    enter,
                    " /QUALIFIER='\"'",
                    enter,
                    " /FIRSTCASE=2",
                    enter,
                    " /IMPORTCASE=ALL",
                    enter,
                    " /VARIABLES=",
                    enter,
                    sep = ""
                ))

                maxcharcsv <- max(nchar(csvnames))
                for (i in seq(length(csvnames))) {
                    cat(paste(
                        csvnames[i],
                        paste(
                            rep(" ", maxcharcsv - nchar(csvnames[i]) + 1),
                            collapse = ""
                        ),
                        spssformats[i],
                        sep = ""
                    ))

                    if (i == length(csvnames)) {
                        cat(" .")
                    }
                    cat(enter)
                }
                cat(paste(
                    "CACHE .",
                    enter,
                    "EXECUTE .",
                    enter, enter,
                    "* ------------------------------------------------------------------------------",
                    enter, enter, enter,
                    sep = ""
                ))
            }
            else {
                cat(paste(
                    "GET FILE = savfile .",
                    enter, enter, enter,
                    sep = ""
                ))
            }

            numvars <- !unlist(stringvars)

            if (stringnum & any(unlist(stringvars))) {

                numvars <- rep(TRUE, length(dataDscr))

                cat(paste(
                    "* --- Recode string variables with labels, to numeric variables ---",
                    enter, enter, enter
                ))

                stringvars <- stringvars[unlist(stringvars)]

                for (sv in names(stringvars)) {

                    oldvalues <- dataDscr2[[sv]][["labels"]]
                    pN <- admisc::possibleNumeric(oldvalues, each = TRUE)
                    newvalues <- oldvalues
                    newvalues[!pN] <- seq(length(oldvalues))[!pN]
                    nummiss <- logical(length(newvalues))

                    if (is.element("na_values", names(dataDscr2[[sv]]))) {
                        na_values <- dataDscr2[[sv]][["na_values"]]

                        for (j in seq(length(na_values))) {
                            if (admisc::possibleNumeric(na_values[j])) {
                                missval <- admisc::asNumeric(na_values[j])
                                if (!is.element(missval, newvalues)) {
                                    mj <- match(na_values[j], oldvalues)
                                    newvalues[mj] <- missval
                                    nummiss[mj] <- TRUE
                                }
                            }
                        }
                    }


                    # if (csv_is_df | csv_is_path) {
                        if (emptyvars[[sv]]) {
                            cat(paste(
                                "* Variable ",
                                toupper(sv),
                                " is completely empty, recoding command skipped.",
                                enter, enter,
                                "ALTER TYPE ",
                                toupper(sv),
                                "(F",
                                max(nchar(newvalues)),
                                ".0) .",
                                enter, enter,
                                sep = ""
                            ))
                        }
                    # }
                    # else {
                    #     cat("--++--\nbla\n")
                        precommand <- paste(
                            "RECODE ",
                            toupper(sv),
                            " ",
                            sep = ""
                        )
                        postcommand <- "(\""
                        command <- paste(precommand, postcommand, sep = "")

                        for (j in seq(length(newvalues[!nummiss]))) {
                            jlabels <- dataDscr2[[sv]][["labels"]]
                            
                            postcommand <- paste(
                                postcommand,
                                jlabels[j],
                                "\"", " = ",
                                newvalues[!nummiss][j],
                                sep = ""
                            )

                            command <- paste(
                                command,
                                jlabels[j],
                                "\"", " = ",
                                newvalues[!nummiss][j],
                                sep = ""
                            )

                            if (j == length(newvalues[!nummiss])) {
                                command <- paste(
                                    command,
                                    ") (else = copy) INTO TEMPVRBL .",
                                    enter,
                                    "EXECUTE .",
                                    enter, enter,
                                    sep = ""
                                )
                            }
                            else {
                                if (nchar(postcommand) > 70) {
                                    postcommand <- paste(
                                        paste(
                                            rep(" ",
                                            nchar(precommand)),
                                            collapse = ""
                                        ),
                                        "(\"",
                                        sep = ""
                                    )

                                    command <- paste(
                                        command,
                                        ")",
                                        enter,
                                        paste(
                                            rep(" ", nchar(precommand)),
                                            collapse = ""
                                        ),
                                        "(\"",
                                        sep = ""
                                    )
                                }
                                else {
                                    command <- paste(command, ") (\"", sep = "")
                                }
                            }
                        }

                        cat(command)

                        cat(paste(
                            "ALTER TYPE ",
                            toupper(sv),
                            "(F",
                            max(nchar(newvalues)),
                            ".0) .",
                            enter, enter,
                            "COMPUTE ",
                            toupper(sv),
                            " = TEMPVRBL .",
                            enter,
                            "EXECUTE .",
                            enter, enter,
                            sep = ""
                        ))

                        cat(paste(
                            "DELETE VARIABLES TEMPVRBL .",
                            enter,
                            "EXECUTE .",
                            enter, enter,
                            sep = ""
                        ))
                    # }

                    names(newvalues) <- names(oldvalues)
                    dataDscr2[[sv]][["labels"]] <- newvalues

                    if (is.element("na_values", names(dataDscr2[[sv]]))) {
                        dataDscr2[[sv]][["na_values"]] <- newvalues[
                            match(
                                dataDscr2[[sv]][["na_values"]],
                                oldvalues
                            )
                        ]
                    }
                }

                cat(paste(enter, enter, sep = ""))
            }


            if (length(numerical_with_strings) > 0) {
                cat(paste(
                    "* --- Force variables as numeric --- ",
                    enter, enter,
                    sep = ""
                ))

                for (nws in names(numerical_with_strings)) {

                    tempvar <- csv[, nws]

                    ##### code to recode and transform into numeric
                    cat(paste(
                        "RECODE ",
                        toupper(i),
                        " (\"",
                        paste(
                            sort(
                                unique(
                                    tempvar[numerical_with_strings[[nws]]]
                                )
                            ),
                            collapse = "\" = \"\") (\""
                        ),
                        "\" = \"\") .",
                        enter,
                        "EXECUTE .",
                        enter, enter,
                        sep = ""
                    ))

                    tempvar <- tempvar[-numerical_with_strings[[nws]]]
                    tempvar <- as.numeric(tempvar)

                    cat(paste(
                        "ALTER TYPE ",
                        toupper(i),
                        " (F",
                        max(nchar(tempvar)),
                        ".",
                        ifelse(
                            any(tempvar - floor(tempvar) > 0),
                            2,
                            0
                        ),
                        ") .",
                        enter,
                        "EXECUTE .",
                        enter, enter,
                        sep = ""
                    ))

                }
                cat(enter)
            }


            cat(paste(
                "* --- Add variable labels --- ",
                enter, enter,
                "VARIABLE LABELS",
                enter,
                sep = ""
            ))

            for (i in seq(length(varnames))) {
                cat(paste(
                    varnames[i],
                    paste(
                        rep(" ", maxchars - nchar(varnames[i])),
                        collapse = ""
                    ),
                    " \"",
                    dataDscr2[[i]][["label"]][1],
                    "\"",
                    sep = ""
                ))

                if (i == length(varnames)) {
                    cat(paste(" .", enter, "EXECUTE .", sep = ""))
                }
                cat(enter)
            }

            cat(paste(enter, enter, sep = ""))

            cat(paste(
                "* --- Add value labels --- ",
                enter, enter,
                "VALUE LABELS",
                enter,
                sep = ""
            ))


            for (i in seq(length(unique_list))) {
                n <- unique_list[[i]][1]

                cat(paste(
                    splitrows(unique_list[[i]], enter, 80),
                    enter,
                    sep = ""
                ))

                #if (all(is.character(dataDscr2[[n]][["labels"]]))) {
                #    cat(paste(paste("\"", dataDscr2[[n]][["labels"]], "\" \"", names(dataDscr2[[n]][["labels"]]), "\"", sep = ""), collapse="\n"))
                #}
                #else {
                    cat(paste(
                        paste(
                            dataDscr2[[n]][["labels"]],
                            " \"",
                            names(dataDscr2[[n]][["labels"]]),
                            "\"",
                            sep = ""
                        ),
                        collapse = enter
                    ))
                #}
                if (i == length(unique_list)) {
                    cat(paste(
                        " .",
                        enter,
                        "EXECUTE .",
                        enter,
                        sep = ""
                    ))
                }
                else {
                    cat(enter, "/", enter, sep = "")
                }
            }

            cat(paste(enter, enter, sep = ""))

        }


        # return(numvars)

        if (anymissing) {

            makeMissingValues <- function(uniqueMissList, missvaLs, nms, finalize = TRUE) {
                for (i in seq(length(uniqueMissList))) {
                    if (i > 1) {
                        cat("/")
                    }

                    if (any(grepl("THRU", missvaLs[[i]]))) {
                        cat(
                            splitrows(
                                toupper(uniqueMissList[[i]]),
                                enter,
                                80
                            )
                        )

                        if (length(missvaLs[[i]]) <= 2) {
                            # at most one discrete missing value and a missing range
                            cat(paste(
                                " (",
                                paste(
                                    if (finalize) {
                                        missvaLs[[i]]
                                    } else {
                                        paste("\"", paste(missvaLs[[i]], collapse = "\", \""), "\"", sep = "")
                                    },
                                    collapse = ", "
                                ),
                                ")",
                                sep = ""
                            ))
                        }
                        else {
                            cat(paste(
                                " (",
                                paste(
                                    missvaLs[[i]][c(1, which(grepl("THRU", missvaLs[[i]])))],
                                    collapse = ", "
                                ),
                                ")",
                                sep = ""
                            ))

                            # cat(paste(
                            #     "  * more than one distinct missing value",
                            #     "found, next to a missing range"
                            # ))
                        }
                    }
                    else {

                        if (length(missvaLs[[i]]) < 4) {
                            cat(
                                splitrows(
                                    toupper(uniqueMissList[[i]]),
                                    enter,
                                    80
                                )
                            )

                            cat(paste(
                                " (",
                                paste(
                                    if (finalize) {
                                        missvaLs[[i]]
                                    } else {
                                        paste("\"", paste(missvaLs[[i]], collapse = "\", \""), "\"", sep = "")
                                    },
                                    collapse = ", "
                                ),
                                ")",
                                sep = ""
                            ))
                        }
                        else {
                            absrange <- abs(range(missvaLs[[i]]))

                            if (all(missvaLs[[i]] < 0)) {
                                cat(
                                    splitrows(
                                        toupper(uniqueMissList[[i]]),
                                        enter,
                                        80
                                    )
                                )
                                cat(paste(
                                    " (LOWEST THRU ",
                                    max(missvaLs[[i]]),
                                    ")",
                                    sep = ""
                                ))
                            }
                            else {
                                # check if the missing values range doesn't contain
                                # any other (non-missing) values
                                checklist <- list()
                                for (mv in uniqueMissList[[i]]) {
                                    allvalues <- dataDscr2[[mv]][["labels"]]
                                    nonmiss <- setdiff(allvalues, missvaLs[[i]])
                                    checklist[[mv]] <- any(
                                        is.element(
                                            nonmiss,
                                            seq(
                                                min(missvaLs[[i]]),
                                                max(missvaLs[[i]])
                                            )
                                        )
                                    )
                                }

                                checklist <- unlist(checklist)

                                # print(checklist)

                                if (any(checklist)) {
                                    # at least one variable has a non-missing value within the range of the missing values
                                    printMISSING <- TRUE

                                    # now trying to see if at least some of the variables can be "rescued"
                                    if (any(!checklist)) {
                                        ###
                                        # is this working...? TO TEST
                                        cat(
                                            splitrows(
                                                names(checklist)[!checklist],
                                                enter,
                                                80
                                            )
                                        )

                                        # cat(paste(names(checklist)[!checklist], collapse=", "))
                                        ###
                                        cat(paste(
                                            " (",
                                            min(missvaLs[[i]]),
                                            " TO ",
                                            max(missvaLs[[i]]),
                                            ")",
                                            enter,
                                            sep = ""
                                        ))
                                        checklist <- checklist[checklist]
                                    }

                                    ###
                                    # is this working...? TO TEST
                                    cat(
                                        splitrows(
                                            names(checklist),
                                            enter,
                                            80
                                        )
                                    )
                                    # cat(paste(names(checklist), collapse=", "))
                                    ###
                                    cat(paste(
                                        " (",
                                        paste(
                                            missvaLs[[i]][1:3],
                                            collapse = ", "
                                        ),
                                        ")",
                                        sep = ""
                                    ))

                                    cat(ifelse(i == length(uniqueMissList), " .", ""))
                                    # cat("  * more than three distinct missing values found")
                                }
                                else {
                                    cat(
                                        splitrows(
                                            toupper(uniqueMissList[[i]]),
                                            enter,
                                            80
                                        )
                                    )
                                    cat(paste(
                                        " (",
                                        min(missvaLs[[i]]),
                                        " TO ",
                                        max(missvaLs[[i]]),
                                        ")",
                                        sep = ""
                                    ))
                                }
                            }
                        }
                    }

                    if (finalize) {
                        cat(ifelse(i == length(uniqueMissList), " .", ""))
                    }
                    cat(enter)
                }
            }


            cat(paste(
                "* --- Add missing values --- ",
                enter, enter,
                "MISSING VALUES",
                enter,
                sep = ""
            ))


            if (sum(numvars) != length(dataDscr)) {
                missvaRs <- labels_missing(dataDscr2[!numvars])
                withmiss <- unlist(lapply(missvaRs, any))

                missvaLs <- values_missing(dataDscr2[!numvars][withmiss]) # range is FALSE by default, so no THRU
                nms <- names(missvaLs)
                uniqueMissList <- lapply(unique(missvaLs), function(x) {
                    nms[unlist(lapply(missvaLs, function(y) {
                        identical(x, y)
                    }))]
                })

                missvaLs <- unique(missvaLs)

                makeMissingValues(uniqueMissList, missvaLs, nms, FALSE)
            }

            missvaRs <- labels_missing(dataDscr2[numvars])
            withmiss <- unlist(lapply(missvaRs, any))

            missvaLs <- values_missing(dataDscr2[numvars][withmiss], range = TRUE)
            nms <- names(missvaLs)
            uniqueMissList <- lapply(unique(missvaLs), function(x) {
                nms[unlist(lapply(missvaLs, function(y) {
                    identical(x, y)
                }))]
            })

            missvaLs <- unique(missvaLs)

            makeMissingValues(uniqueMissList, missvaLs, nms)

            # sink()
            # setwd(currentdir)
            # return(list(dataDscr2=dataDscr2, haslabels=haslabels, missvaRs=missvaRs, withmiss=withmiss, missing = missing))
            # return(list(missvaLs=missvaLs, uniqueMissList=uniqueMissList))

            cat(enter, enter, sep = "")
        }

        outfile <- ifelse(
            script,
            paste(
                "\"",
                file.path(to$completePath, to$files[1]),
                "\"",
                sep = ""
            ),
            "savfile"
        )

        cat(paste(
            "* --- Save the .sav file --- ",
            enter, enter,
            "SAVE OUTFILE=",
            outfile,
            enter,
            "/KEEP",
            enter,
            sep = ""
        ))


        if (formats) {
            for (n in csvnames) {
                cat(paste(toupper(n), enter, sep = ""))
            }
        }
        else {
            for (n in names(dataDscr2)) {
                cat(paste(toupper(n), enter, sep = ""))
            }
        }

        cat(paste(
            "  /COMPRESSED .",
            enter,
            "EXECUTE .",
            enter,
            sep = ""
        ))

        # finish writing and close the .sps file
        sink()

        if (printMISSING) {
            # this would be printed on the screen
            cat("    For some variables, more than 3 distinct missing values were found.\n")
            cat("    Only the first three were used.\n\n")
        }

        setwd(currentdir)

    }


    if (toupper(type) == "STATA" | toupper(type) == "ALL") {

        dataDscr2 <- dataDscr
        currentdir <- getwd()

        if (isTRUE(outdir)) {
            if (!file.exists("Setup files")) {
                dir.create("Setup files")
            }

            if (!file.exists(file.path("Setup files", "Stata"))) {
                dir.create(file.path("Setup files", "Stata"))
            }

            setwd(file.path("Setup files", "Stata"))
        }

        sink(
            ifelse(
                grepl("\\.do", file),
                file,
                paste(file, ".do", sep = "")
            )
        )

        if (script) {
            to <- treatPath(dots$to, check = FALSE)
            cat(paste(
                "use \"",
                file.path(
                    to$completePath,
                    to$files[1]
                ),
                "\"",
                enter, enter, enter,
                sep = ""
            ))
        }
        else {
            cat(paste(
                "/* Initialization commands */", enter,
                "clear", enter,
                "capture log close", enter,
                "set more off", enter,
                "version 12.0", enter,
                "set linesize 250", enter,
                "set varabbrev off", enter,
                "set memory 1G // not necessary in Stata 12",
                enter, enter, enter,
                "* ----------------------------------------------------------------------------",
                enter, enter,
                "* --- CONFIGURATION SECTION - START ---",
                enter, enter,
                "* The following command should contain the complete path and",
                enter,
                "* name of the Stata log file.",
                enter,
                "* Change LOG_FILENAME to your filename, below:",
                enter,
                "local log_file \"LOG_FILENAME\"",
                enter, enter, enter,
                "* The following command should contain the complete path and",
                enter,
                "* name of the CSV file, usual file extension \".csv\"",
                enter,
                "* Change CSV_DATA_PATH to your filename, below:",
                enter,
                "local csvpath \"CSV_DATA_PATH\"",
                enter, enter, enter,
                "* The following command should contain the complete path and",
                enter,
                "* name of the final STATA file, usual file extension \".dta\"",
                enter,
                "* Change STATA_DATA_PATH to your filename, below:",
                enter,
                "local filepath \"STATA_DATA_PATH\"",
                enter, enter, enter,
                "* --- CONFIGURATION SECTION - END ---",
                enter, enter,
                "* ----------------------------------------------------------------------------",
                enter, enter, enter, enter,
                "* There should be nothing to change below this line",
                enter,
                "* ----------------------------------------------------------------------------",
                enter, enter, enter, enter,
                "log using \"`log_file'\", replace text", enter, enter,
                sep = ""
            ))

            if (formats) {
                cat(paste(
                    "insheet using \"`csvpath'\", comma names case",
                    enter, enter,
                    "* Note that some variables in the csv raw data file might be in lowercase",
                    enter,
                    "* To ensure that the dataset contains only variable names in uppercase",
                    enter, enter,
                    "foreach var of varlist _all {",
                    enter,
                    "    local newname = upper(\"`var'\")",
                    enter,
                    "    cap rename \`var\' \`newname\'",
                    enter,
                    "}",
                    enter, enter, enter,
                    sep = ""
                ))
            }
            else {
                cat(paste(
                    "use \"`filepath'\"",
                    enter, enter, enter,
                    sep = ""
                ))
            }
        }


        if (any(unlist(stringvars))) {
            cat(paste(
                "* Recode string variables with labels, to numeric variables",
                enter, enter,
                sep = ""
            ))

            stringvars <- stringvars[unlist(stringvars)]

            for (i in names(stringvars)) {
                oldvalues <- dataDscr2[[i]][["labels"]]

                # recode every letter to a number, but keep the potentially numbers
                # something like "A", "B" and "-9" will be recoded to 1, 2 and -9
                # and someting like "A", "B" and "2" will be recoded to 1, 3 and 2
                newvalues <- suppressWarnings(as.numeric(as.character(oldvalues)))
                newvalues[is.na(newvalues)] <- setdiff(
                    seq(1000),
                    newvalues
                )[seq(sum(is.na(newvalues)))]

                names(newvalues) <- names(oldvalues)

                wel <- which(is.element(dictionary, dataDscr2[[i]][["na_values"]]))

                if (length(wel) > 0) {
                    nmsv <- names(dictionary)[wel]
                    for (w in seq(length(wel))) {
                        dictionary[wel[w]] <- newvalues[oldvalues == dictionary[wel[w]]]
                    }
                    names(dictionary)[wel] <- nmsv
                }


                # the recode command cannot be used because it only allows numeric variables
                cat(paste("generate TEMPVAR = .", enter, sep = ""))

                for (j in seq(length(newvalues))) {
                    cat(paste(
                        "replace TEMPVAR = ", newvalues[j],
                        " if ", toupper(i), " == \"", oldvalues[j], "\"",
                        enter,
                        sep = ""
                    ))
                }

                cat(paste(
                    "drop ", toupper(i),
                    enter,
                    "rename TEMPVAR ", toupper(i),
                    enter, enter,
                    sep = ""
                ))

                dataDscr2[[i]][["labels"]] <- newvalues

                # just in case the old missing values were not numbers
                if (is.element("na_values", names(dataDscr2[[i]]))) {
                    dataDscr2[[i]][["na_values"]] <- newvalues[
                        match(
                            dataDscr2[[i]][["na_values"]],
                            oldvalues
                        )
                    ]
                }
            }

            if (!script) {
                cat(paste(enter, enter, sep = ""))
            }
        }

        if (length(numerical_with_strings) > 0) {
            # This has to do with the (now deprecated) argument "forcenum"

            cat(paste(
                "* Force variables as numeric",
                enter, enter,
                sep = ""
            ))

            for (nws in names(numerical_with_strings)) {

                tempvar <- csv[, nws]

                for (tnws in sort(unique(tempvar[numerical_with_strings[[nws]]]))) {
                    cat(paste(
                        "replace ", toupper(nws), " = \"\" if ", toupper(nws), " == \"", tnws,
                        enter,
                        sep = ""
                    ))
                }

                cat(paste(
                    "destring ", toupper(nws), ", replace",
                    enter, enter,
                    sep = ""
                ))

            }
            cat(enter)
        }



        if (anymissing & recode) {
            missvaRs <- labels_missing(dataDscr2)
            withmiss <- unlist(lapply(missvaRs, any))

            uniquemiss <- sort(unique(unlist(
                mapply(
                    function(x, y) {
                        x[["labels"]][y]
                    },
                    dataDscr2[withmiss],
                    missvaRs[withmiss],
                    SIMPLIFY = FALSE
                )
            )))

            if (is.null(dictionary)) {
                nms <- paste(
                    ".",
                    letters[seq(length(uniquemiss))],
                    sep = ""
                )
            }
            else {
                dictionary <- dictionary[is.element(dictionary, uniquemiss)]

                nms <- names(dictionary)
                if (is.character(nms)) {
                    unms <- unique(nms)
                    for (i in seq(length(unms))) {
                        nms[nms == unms[i]] <- letters[i]
                    }
                }
                # print(dots$dictionary)
                # print(missvaRs[withmiss])

                nms <- paste(".", nms, sep = "")
                names(dictionary) <- nms
            }

            # sink()
            # return(list(dataDscr2=dataDscr2, withmiss=withmiss, missvaRs=missvaRs))

            # we need a third dataDscr because dataDscr2 might have been altered when recoding the strings to numerical
            dataDscr3 <- dataDscr2

            dataDscr3[withmiss] <- mapply(
                function(x, y) {
                    x[["labels"]][y] <- nms[match(x[["labels"]][y], dictionary)]
                    x[["na_values"]] <- nms[match(x[["na_values"]], dictionary)]
                    return(x)
                },
                dataDscr2[withmiss],
                missvaRs[withmiss],
                SIMPLIFY = FALSE
            )

            cat(paste(
                enter,
                "* Recode missing values",
                enter, enter,
                sep = ""
            ))


            for (wm in names(dataDscr2[withmiss])) {
                for (j in which(missvaRs[[wm]])) {
                    cat(paste(
                        "replace ", wm, " = ",
                        dataDscr3[[wm]][["labels"]][j],
                        " if ", wm, " == ",
                        dataDscr2[[wm]][["labels"]][j],
                        enter,
                        sep = ""
                    ))
                }
            }

            dataDscr2 <- dataDscr3

            cat(paste(enter, enter))

        }



        maxchars <- max(nchar(names(dataDscr2)))

        cat(paste(
            "* Definition of variable labels",
            enter, enter,
            sep = ""
        ))

        # cat(maxchars, enter)

        for (n in names(dataDscr)) {
            cat(paste(
                "label variable ", toupper(n),
                paste(
                    rep(" ", maxchars - nchar(n)),
                    collapse = ""
                ),
                " \"", dataDscr2[[n]][["label"]][1], "\"",
                enter,
                sep = ""
            ))
        }
        cat(paste(enter, enter, sep = ""))

        cat(paste(
            "* Definition of category labels",
            enter, enter,
            sep = ""
        ))

        for (i in seq(length(unique_list))) {
            n <- unique_list[[i]][1]

            # header_label <- paste("label define LABELS_GROUP_", i, sep = "")

            cat(paste(
                paste(
                    "label define LABELS_GROUP_",
                    i,
                    " ",
                    sep = ""
                ),
                paste(
                    paste(
                        paste(
                            dataDscr2[[n]][["labels"]],
                            " \"",
                            names(dataDscr2[[n]][["labels"]]),
                            "\"",
                            sep = ""
                        ),
                        collapse = " "
                    ),
                    enter,
                    sep = ""
                ),
                sep = ""
            ))
        }

        cat(paste(enter, enter, sep = ""))
        cat(paste(
            "* Attachment of category labels to variables",
            enter, enter,
            sep = ""
        ))

        for (i in seq(length(unique_list))) {
            for (ul in unique_list[[i]]) {
                cat(paste(
                    "label values ", toupper(ul),
                    paste(
                        rep(" ", maxchars - nchar(ul)),
                        collapse = ""
                    ),
                    " LABELS_GROUP_", i,
                    enter,
                    sep = ""
                ))
            }
        }

        if (script) {
            cat(paste(
                enter,
                "save \"",
                file.path(to$completePath, to$files[1]),
                "\", replace",
                enter,
                sep = ""
            ))
        }
        else  {
            cat(paste(
                enter,
                "compress",
                enter,
                "save \"`filepath'\", replace",
                enter, enter,
                "log close",
                enter,
                "set more on",
                enter,
                sep = ""
            ))
        }

        sink()
        setwd(currentdir)
    }


    if (toupper(type) == "SAS" | toupper(type) == "ALL") {
        dataDscr2 <- dataDscr
        currentdir <- getwd()

        if (isTRUE(outdir)) {
            if (!file.exists("Setup files")) {
                dir.create("Setup files")
            }

            if (!file.exists(file.path("Setup files", "SAS"))) {
                dir.create(file.path("Setup files", "SAS"))
            }

            setwd(file.path("Setup files", "SAS"))
        }

        sink(
            ifelse(
                grepl("\\.sas", file),
                file,
                paste(file, ".sas", sep = "")
            )
        )

        if (script) {
            to <- treatPath(dots$to, check = FALSE)
            cat(paste(
                "libname datadir \"",
                to$completePath,
                "\";",
                enter, enter,
                sep = ""
            ))

            sasimport <- paste("datadir", to$filenames[1], sep = ".")
        }
        else {
            if (catalog) {
                sasimport <- paste("datadir", tp$filenames, sep = ".")
                cat(paste(
                    "libname datadir \"",
                    tp$completePath,
                    "\";",
                    enter, enter,
                    sep = ""
                ))
            }
            else {
                sasimport <- "datadir.&sasfile"
                cat(paste(
                    "* ------------------------------------------------------------------------------ ;",
                    enter, enter,
                    "* --- CONFIGURATION SECTION - START ---                                          ;",
                    enter, enter, enter,
                    sep = ""
                ))

                if (formats) {
                    cat(paste(
                        "* The following command should contain the complete path and                     ;",
                        enter,
                        "* name of the .csv file to be read (e.g. \"C:/file.csv\")                          ;",
                        enter,
                        "* Change CSV_DATA_PATH to your filename, below                                   ;",
                        enter, enter,
                        "filename csvpath \"CSV_DATA_PATH\";",
                        enter, enter, enter,
                        sep = ""
                    ))

                # cat("* It is assumed the data file was created under Windows (end of line is CRLF);", enter,
                #     "* If the csv file was created under Unix,  change eol=LF;", enter,
                #     "* If the csv file was created under MacOS, change eol=CR below;", enter, enter,
                #     "%LET eol=CRLF;", enter, enter, enter, sep = "")
                }

                cat(paste(
                    "* The following command should contain the complete path of the                  ;",
                    enter,
                    "* directory where the final file will be saved (e.g. \"C:/Data\")                  ;",
                    enter,
                    "* Change SAS_DATA_FOLDER to your directory name, below                           ;",
                    enter, enter,
                    "libname datadir \"SAS_DATA_FOLDER\";",
                    enter, enter, enter,
                    sep = ""
                ))

                cat(paste(
                    "* The following command should contain the name of the output SAS file only      ;",
                    enter,
                    "* (without quotes, and without the .sas7bdat extension)                          ;",
                    enter,
                    "* Change SAS_FILE_NAME to your output file name, below                           ;",
                    enter, enter,
                    "%let sasfile = SAS_FILE_NAME;",
                    enter, enter, enter,
                    "* --- CONFIGURATION SECTION -  END ---                                           ;",
                    enter, enter,
                    "* ------------------------------------------------------------------------------ ;",
                    enter, enter, enter, enter,
                    "* There should be nothing to change below this line;",
                    enter,
                    "* ------------------------------------------------------------------------------ ;",
                    enter, enter, enter, enter,
                    sep = ""
                ))

                if (formats & !catalog) {
                    cat(paste(
                        "* --- Read the raw data file --- ;",
                        enter, enter,
                        "data ", sasimport, ";",
                        enter, enter,
                        "infile csvpath",
                        enter,
                        "     dlm=",
                        ifelse(
                            delim == "\t",
                            "'09'X",
                            paste("\"", delim, "\"", sep = "")
                        ),
                        enter,
                        "     firstobs=2",
                        enter,
                        #### "     TERMSTR=&eol", enter, #### line commented out
                        "     dsd",
                        enter,
                        "     truncover",
                        enter,
                        "     lrecl=512",
                        enter,
                        "     ;",
                        enter, enter,
                        "input  ", toupper(csvnames[1]),
                        sasformats[1],
                        enter,
                        sep = ""
                    ))

                    for (i in seq(2, length(csvnames))) {
                        cat(paste(
                            "       ",
                            csvnames[i],
                            sasformats[i],
                            enter,
                            sep = ""
                        ))
                    }
                    cat(paste("       ;", enter, sep = ""))
                    cat(paste("run;", enter, enter, sep = ""))
                        # "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")
                }
            }

        }

        # cat("DATA ", sasimport, ";", enter, enter, enter, sep = "")

        if (any(unlist(stringvars)) & !catalog) {
            cat(paste(
                "* --- Recode string variables with labels, to numeric variables --- ;",
                enter, enter,
                sep = ""
            ))

            for (sv in names(stringvars)) {
                if (stringvars[[sv]]) {

                    oldvalues <- dataDscr2[[sv]][["labels"]]

                    # recode every letter to a number, but keep the potentially numbers
                    # something like "A", "B" and "-9" will be recoded to 1, 2 and -9
                    # and someting like "A", "B" and "2" will be recoded to 1, 3 and 2
                    newvalues <- suppressWarnings(
                        as.numeric(as.character(oldvalues))
                    )

                    newvalues[is.na(newvalues)] <- setdiff(
                        seq(1000),
                        newvalues
                    )[
                        seq(sum(is.na(newvalues)))
                    ]

                    names(newvalues) <- names(oldvalues)

                    cat(paste(
                        "data ", sasimport, ";",
                        enter, enter,
                        "    set ", sasimport, ";",
                        enter, enter,
                        "    tempvar = input(", sv, ", ?? best.);",
                        enter,
                        sep = ""
                    ))

                    for (j in seq(length(newvalues))) {
                        cat(paste(
                            "    if (", sv, " = '", oldvalues[j],
                            "') then tempvar = ", newvalues[j], ";",
                            enter,
                            sep = ""
                        ))
                    }

                    # cat(enter, "RUN;", enter, enter, "DATA ", sasimport, ";", enter, enter,
                    #     "    SET ", sasimport, ";", enter, enter,
                    cat(paste(
                        "    drop ", sv, ";", enter,
                        "    rename tempvar = ", sv, ";", enter, enter,
                        "run;", enter, enter,
                        sep = ""
                    ))

                    # just in case the old missing values were not numbers
                    dataDscr2[[sv]][["labels"]] <- newvalues
                    if (is.element("na_values", names(dataDscr2[[sv]]))) {
                        dataDscr2[[sv]][["na_values"]] <- newvalues[
                            match(
                                dataDscr2[[sv]][["na_values"]],
                                oldvalues
                            )
                        ]
                    }
                }
            }
        }


        if (length(numerical_with_strings) > 0) {

            cat(paste(
                "* --- Force variables as numeric --- ;",
                enter, enter,
                sep = ""
            ))

            for (nws in toupper(names(numerical_with_strings))) {
                cat(paste(
                    "data ", sasimport, "; set ", sasimport, ";",
                    enter, enter,
                    "    TEMPVAR = input(", nws, ", ?? best.);",
                    enter,
                    "    drop ", nws, ";",
                    enter,
                    "    rename TEMPVAR = ", nws, ";",
                    enter, enter,
                    "run;",
                    enter, enter,
                    sep = ""
                ))
            }
            # for (i in toupper(names(numerical_with_strings))) {
            #     cat("TEMPVAR = INPUT(", i, ", ?? best.);", enter, enter,
            #         "DROP ", i, ";", enter,
            #         "RENAME TEMPVAR = ", i, ";", enter, enter, sep = "")
            # }

            # cat("* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")
        }


        if ((any(unlist(stringvars)) & !catalog) | length(numerical_with_strings) > 0) {
            cat(paste(
                "* --- Reorder the variables in their original positions --- ;",
                enter, enter,
                "data ", sasimport, ";",
                enter, enter,
                "    retain ",
                gsub(
                    ",",
                    "",
                    splitrows(
                        toupper(names(dataDscr2)),
                        enter,
                        70,
                        "           "
                    )
                ), ";",
                enter, enter,
                "    set ", sasimport, ";",
                enter, enter,
                "run;",
                enter, enter,
                sep = ""
            ))

            #     "RETAIN ", gsub(",", "", splitrows(toupper(names(dataDscr2)), enter, 70, "           ")), ";", enter, enter, sep = "")
                # "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")
        }


        if (anymissing & recode) {
            missvaRs <- labels_missing(dataDscr2)
            # SAS does not accept character variables, so !charvars
            withmiss <- unlist(lapply(missvaRs, any)) & !charvars

            uniquemiss <- sort(unique(unlist(
                mapply(
                    function(x, y) {
                        x[["labels"]][y]
                    },
                    dataDscr2[withmiss],
                    missvaRs[withmiss],
                    SIMPLIFY = FALSE
                )
            )))

            if (is.null(dictionary)) {
                nms <- paste(
                    ".",
                    letters[seq(length(uniquemiss))],
                    sep = ""
                )
            }
            else {
                dictionary <- dictionary[is.element(dictionary, uniquemiss)]
                nms <- character(0) # just to initialize

                if (length(dictionary) > 0) {
                    nms <- names(dictionary)
                    if (nms[1] != "a") {
                        unms <- unique(nms)
                        for (i in seq(length(unms))) {
                            nms[nms == unms[i]] <- letters[i]
                        }
                    }
                    # print(dots$dictionary)
                    # print(missvaRs[withmiss])

                    nms <- paste(".", nms, sep = "")
                }
            }


            if (any(withmiss)) {
                dataDscr3 <- dataDscr2

                dataDscr3[withmiss] <- mapply(
                    function(x, y) {
                        x[["labels"]][y] <- nms[match(x[["labels"]][y], dictionary)]
                        x[["na_values"]] <- nms[match(x[["na_values"]], dictionary)]
                        return(x)
                    },
                    dataDscr2[withmiss],
                    missvaRs[withmiss],
                    SIMPLIFY = FALSE
                )

                cat(paste(
                    "* --- Recode missing values --- ;",
                    enter, enter,
                    "data ", sasimport, "; set ", sasimport, ";",
                    enter, enter,
                    sep = ""
                ))

                for (wm in names(dataDscr2[withmiss])) {
                    cat(paste("select ", wm, ";", enter, sep = ""))
                    for (j in which(missvaRs[[wm]])) {
                        cat(paste(
                            "    when (",
                            dataDscr2[[wm]][["labels"]][j],
                            ") ", wm, " = ",
                            dataDscr3[[wm]][["labels"]][j],
                            ";", enter,
                            sep = ""
                        ))
                    }
                    cat("end;", enter, sep = "")
                }

                cat(paste(enter, "run;", enter, enter, sep = ""))

                dataDscr2 <- dataDscr3
            }
        }


        if (!script & !catalog) {
            cat(paste(
                "* --- Add variable labels --- ;",
                enter, enter,
                "data ", sasimport, "; set ", sasimport, ";",
                enter, enter,
                sep = ""
            ))

            for (i in seq(length(varnames))) {
                cat(paste(
                    "    label ", varnames[i],
                    paste(
                        rep(" ", maxchars - nchar(varnames[i]) + 1),
                        collapse = ""
                    ),
                    " = ", "\"", dataDscr2[[i]][["label"]][1], "\";",
                    enter,
                    sep = ""
                ))
            }

            cat(paste(enter, "run;", enter, enter, sep = ""))
            #     "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")
        }

        cat(paste(
            "* --- Create value labels groups --- ;",
            enter, enter,
            "proc format library = datadir;",
            enter, enter,
            sep = ""
        ))

        for (i in seq(length(unique_list))) {

            n <- unique_list[[i]][1]
            labels <- dataDscr2[[n]]$labels
            char <- charvars[n]

            cat(paste(
                paste(
                    "value ",
                    ifelse(char, "$", ""),
                    "labels_", i, "_group",
                    enter,
                    sep = ""
                ),
                paste(
                    paste(
                        paste(
                            "    ",
                            ifelse(char, "\"", ""),
                            labels,
                            ifelse(char, "\"", ""),
                            " = \"",
                            names(labels),
                            "\"",
                            sep = ""
                        ),
                        collapse = enter
                    ),
                    ";",
                    enter, enter,
                    sep = ""
                ),
                sep = ""
            ))
        }

        cat(paste(
            "run;",
            enter, enter,
            sep = ""
        ))
        # "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")

        cat(paste(
            "* --- Format variables with value labels --- ;",
            enter, enter,
            "data ", sasimport, "; set ", sasimport, ";",
            enter, enter,
            "    format",
            enter,
            sep = ""
        ))

        for (i in seq(length(unique_list))) {
            n <- unique_list[[i]][1]
            for (j in unique_list[[i]]) {
                cat(paste(
                    "    ",
                    toupper(j),
                    paste(
                        rep(" ", maxchars - nchar(j)),
                        collapse = ""
                    ),
                    " labels_", i, "_group", ".",
                    enter,
                    sep = ""
                ))
            }
        }

        cat(paste(
            "    ;",
            enter, enter,
            "run;",
            enter, enter,
            sep = ""
        ))
        # "* ------------------------------------------------------------------------------ ;", enter, enter, enter, sep = "")

        if (!script & !catalog) {
            cat(paste(
                "* --- Save data to a sas type file --- ;",
                enter, enter,
                "data datadir.&sasfile;",
                enter, enter,
                "    set ", sasimport, ";",
                enter, enter,
                "run;",
                enter,
                sep = ""
            ))
        }


        sink()
        setwd(currentdir)
    }


    if (toupper(type) == "R" | toupper(type) == "ALL") {
        printMISSING <- FALSE
        currentdir <- getwd()

        if (outdir) {
            if (!file.exists("Setup files")) {
                dir.create("Setup files")
            }

            if (!file.exists(file.path("Setup files", "R"))) {
                dir.create(file.path("Setup files", "R"))
            }

            setwd(file.path("Setup files", "R"))
        }

        sink(
            ifelse(
                grepl("\\.R", file),
                file,
                paste(file, ".R", sep = "")
            )
        )

        cat(paste(
            "# ------------------------------------------------------------------------------",
            enter, enter,
            "# --- CONFIGURATION SECTION - START ---",
            enter, enter, enter,
            sep = ""
        ))

        # if (formats) {
            cat(paste(
                "# The following command should contain the complete path to",
                enter,
                "# the .csv file to be read (e.g. \"C:/CIS 2008/Data/file.csv\")",
                enter,
                "# Change CSV_DATA_PATH to your filename, below:",
                enter, enter,
                "csvpath <- \"CSV_DATA_PATH\"",
                enter, enter, enter,
                sep = ""
            ))
        # }

        cat(paste(
            "# The following command should contain the complete path to",
            enter,
            "# the .rds file to be saved (e.g. \"C:/CIS 2008/Data/file.rds\")",
            enter,
            "# Change RDS_PATH to your filename, below:",
            enter, enter,
            "rdspath <- \"RDS_PATH\"",
            enter, enter, enter,
            sep = ""
        ))


        # if (formats) {
            cat(paste(
                "# --- Read the raw data ---",
                enter, enter,
                "rdatafile <- read.csv(csvpath)",
                enter, enter,
                "# all variable names to upper case",
                enter,
                "names(rdatafile) <- toupper(names(rdatafile))",
                enter, enter, enter,
                sep = ""
            ))
        # }
        # else {
        #     cat("# \"rdatafile\" should be an R data.frame (usually read from a .csv file)\n\n")
        # }

        cat(paste(
            "# --- CONFIGURATION SECTION -  END  ---",
            enter, enter,
            "# There should be nothing to change below this line",
            enter,
            "# ------------------------------------------------------------------------------",
            enter, enter, enter, enter,
            sep = ""
        ))


        if (length(numerical_with_strings) > 0) {

            cat(paste(
                "# --- Force variables as numeric ---",
                enter, enter,
                sep = ""
            ))

            maxnchars <- max(nchar(names(numerical_with_strings)))

            for (i in toupper(names(numerical_with_strings))) {
                cat(paste(
                    "rdatafile[ , \"", i, "\"]",
                    paste(
                        rep(" ", maxnchars - nchar(i)),
                        collapse = ""
                    ),
                    " <- suppressWarnings(as.numeric(rdatafile[ , \"", i, "\"]", "))",
                    enter,
                    sep = ""
                ))
            }
        }

        cat(paste(
            "# --- Set the variable metadata attributes --- ",
            enter,
            "# package declared needs to be installed",
            enter, enter,
            "if (system.file(package = \"declared\") == \"\") {",
            enter,
            "    install.packages(\"declared\")",
            enter,
            "}",
            enter, enter,
            "library(declared)",
            enter, enter,
            sep = ""
        ))

        writeMetadata(dataDscr, OS = OS, indent = indent)

        cat(paste(enter, enter, sep = ""))

        cat(paste(
            "# --- Save the R data file --- ",
            enter, enter,
            "saveRDS(rdatafile, file = rdspath)",
            enter, enter, enter, enter,
            "# ------------------------------------------------------------------------------",
            enter, enter,
            "# --- Clean up the working space --- ",
            enter, enter,
            "rm(rdatafile, csvpath, rdspath",
            sep = ""
        ))

        # if (any(unlist(stringvars))) {
        #     cat(", tempvar")
        # }

        cat(paste(")", enter, sep = ""))



        # finish writing and close the .R file
        sink()

        setwd(currentdir)

    }
}
