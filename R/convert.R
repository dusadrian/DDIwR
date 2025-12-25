#' @name convert
#'
#' @title
#' Converts a dataset from one statistical software to another
#'
#' @description
#' This function converts (or transfers) between R, Stata, SPSS, SAS, Excel and
#' DDI XML files. Unlike the regular import / export functions from packages
#' \bold{\pkg{haven}} or \bold{\pkg{rio}}, this function uses the DDI standard
#' as an exchange platform and facilitates a consistent conversion of the
#' missing values.
#'
#' @details
#' When the argument **`to`** specifies a certain statistical package
#' (`"R"`, `"Stata"`, `"SPSS"`, `"SAS"`, `"XPT"`) or `"Excel"`, the name of the
#' destination file will be identical to the one in the argument **`from`**,
#' with an automatically added software specific extension.
#'
#' SPSS portable file (with the extension `".por"`) can only be read, but not
#' written.
#'
#' The argument **`to`** can also be specified as a path to a specific file,
#' in which case the software package is determined from its file extension.
#' The following extentions are currently recognized: `.xml` for DDI,
#' `.rds` for R, `.dta` for Stata, `.sav` for SPSS, `.xpt` for SAS, and
#' `.xlsx` for Excel.
#'
#' Additional parameters can be specified via the three dots argument
#' **`...`**, that are passed to the respective functions from packages
#' \bold{\pkg{haven}} and \bold{\pkg{readxl}}. For instance the function
#' \bold{\code{\link[haven]{write_dta}()}} has an additional argument called
#' **`version`** when writing a Stata file.
#'
#' The most important argument to consider is called **`user_na`**, part of
#' the function \bold{\code{\link[haven]{read_sav}()}}. Defaulted to `FALSE` in
#' package \bold{\pkg{haven}}, in package \bold{\pkg{DDIwR}} it is used as
#' having the value of `TRUE`, and it can be deactivated by explicitly
#' specifying `user_na = FALSE` in function **`convert()`**.
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
#' The current version reads and creates DDI Codebook version 2.6, with future
#' versions to extend the functionality for DDI Lifecycle versions 3.x and link
#' to the future package \bold{DDI4R} for the UML model based version 4. It
#' extends the standard DDI Codebook by offering the possibility to embed a
#' serialized version of the R dataset into the XML file containing the
#' Codebook, within a `notes` child of the `fileDscr` component. This type of
#' generated codebook is unique to this package and automatically detected when
#' converting to another statistical software. This will likely be replaced with
#' a time insensitive text version.
#'
#' Converting to SAS is experimental, and it relies on the same package
#' \bold{\pkg{haven}} that uses the ReadStat C library. The safest way to
#' convert, which at the same time consistently converts the missing values, is
#' to export the data to a CSV file and create a setup file produced by function
#' \bold{\code{\link{setupfile}()}} and run the commands manually.
#'
#' Converting data from SAS is possible, however reading the metadata is also
#' experimental (the current version of \bold{haven} only partially imports the
#' metadata). Either specify the path to the catalog file using the argument
#' **`catalog_file`** from the function \bold{\code{\link[haven]{read_sas}()}},
#' or have the catalog file in the same directory as the data set, with the same
#' file name and the extension `.sas7bcat`
#'
#' The argument **`recode`** controls how missing values are treated. If the
#' input file has SPSS like numeric codes, they will be recoded to extended
#' (a-z) missing types when converting to Stata or SAS. If the input has Stata
#' like extended codes, they will be recoded to SPSS like numeric codes.
#'
#' The character **`encoding`** is usually passed to the corresponding functions
#' from package \bold{\pkg{haven}}. It can be set to \code{NULL} to reset at the
#' default in that package.
#'
#' Converting to SPSS works with numerical and character labelled vectors, with
#' or without labels. Date/Time variables are partially supported by package
#' **haven**: either having such a variable with no labels and missing values,
#' or if labels and missing values are declared the variable is automatically
#' coerced to numeric, and users may have to make the proper settings in SPSS.
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
#' @param ... Additional parameters passed to other functions, see the
#' Details section
#'
#' @export

`convert` <- function(
    from, to = NULL, declared = TRUE, chartonum = FALSE, recode = TRUE,
    encoding = "UTF-8", csv = NULL, ...
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
    embed <- !isFALSE(dots$embed)

    file_extension <- dots$file_extension
    file_name <- dots$file_name
    file_id <- dots$file_id
    dots$file_extension <- NULL
    dots$file_name <- NULL
    dots$file_id <- NULL

    codeBook <- NULL
    dictionary <- dots$dictionary

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
        #         paste(tp_from$filenames, "sas7bdat", sep = ".")
        #     )
        # }
        # else if (identical(toupper(to), "XPT")) {
        #     to <- file.path(
        #         tp_from$completePath,
                paste(tp_from$filenames, "xpt", sep = ".")
            )
        }
        else if (!grepl("[.]", to)) {
            admisc::stopError(
                "Unknown destination software."
            )
        }

        tp_to <- treatPath(to, single = TRUE, check = FALSE)

        if (tp_to$fileext == "XLS") {
            tp_to$fileext <- "XLSX"
        }

        if (is.na(tp_to$fileext)) {
            admisc::stopError(
                "Cannot determine the destination software without a file extension."
            )
        }
        else if (tp_to$fileext == "SAS7BDAT") {
            admisc::stopError(
                "The extension .sas7bdat is deprecated, please use .xpt instead."
            )
        }
        else if (
            !is.element(
                tp_to$fileext,
                c("RDS", "SAV", "DTA", "XML", "XPT", "XLSX")
            )
        ) {
            admisc::stopError("Unknown destination software.")
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

            variables <- lapply(xmlvars, XMLtoRmetadata, dns = dns)

            xpath <- sprintf("/%scodeBook/%sdataDscr/%svar/@name", dns, dns, dns)
            names(variables) <- admisc::trimstr(
                xml2::xml_text(xml2::xml_find_all(xml, xpath))
            )

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

            if (!is.null(hashes)) {
                checkhashes <- getHashes(xmlvars)

                if (!identical(hashes, checkhashes)) {
                    different <- which(hashes != checkhashes)

                    for (i in different) {
                        metadata <- XMLtoRmetadata(xmlvars[i], dns = dns)
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
        tc <- admisc::tryCatchWEM(
            data <- do.call(haven::read_sav, arglist) # haven_labelled variables
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
        data <- do.call(haven::read_por, arglist)
    }
    else if (tp_from$fileext == "DTA") {
        fargs <- names(formals(read_dta))
        arglist <- dots[is.element(names(dots), fargs)]
        arglist$file <- from
        arglist$encoding <- encoding
        data <- do.call(haven::read_dta, arglist)

        # return(list(data = data, to = to, dictionary = dictionary, chartonum = chartonum, to_declared = FALSE, error_null = FALSE))
        if (recode) {
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

        if (is.null(arglist$catalog_file)) {
            cats <- treatPath(tp_from$completePath, type = "sas7bcat")
            if (any(is.element(cats$filenames, tp_from$filenames))) {
                arglist$catalog_file <- file.path(
                    tp_from$completePath,
                    paste(tp_from$filenames, "sas7bcat", sep = ".")
                )
            }
        }

        data <- do.call(haven::read_sas, arglist)
        if (recode) {
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
        fargs <- names(formals(haven::read_xpt))
        arglist <- dots[is.element(names(dots), fargs)]
        arglist$file <- from
        # arglist$encoding <- encoding
        data <- do.call(haven::read_xpt, arglist)

        if (recode) {
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
            data <- declared::as.declared(data)
            class(data) <- "data.frame"
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

    variables <- collectRMetadata(data)

    if (tp_to$fileext == "XML") {

        if (admisc::anyTagged(data)) {
            admisc::stopError("DDI does not support extended missing codes")
        }

        codeBook <- makeElement("codeBook")

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
    else if (identical(tp_to$fileext, "SAV")) {
        data[] <- lapply(data, function(x) {
            if (!is.element("format.spss", names(attributes(x)))) {
                attr(x, "format.spss") <- getFormat(x, type = "SPSS")
            }

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

        # return(data)
        haven::write_sav(declared::as.haven(data), to)
    }
    else if (identical(tp_to$fileext, "DTA")) {
        data <- declared::as.haven(data)

        colnms <- colnames(data)
        arglist <- list(data = data)

        rechars <- c()
        err <- paste(
            "Stata does not allow labels for non-integer variables",
            "(e.g. \"%s\")."
        )

        for (i in seq(ncol(data))) {
            x <- data[[colnms[i]]]
            metadata <- getElement(variables, colnms[i])
            labels <- getElement(metadata, "labels")

            if (is.null(labels)) {
                labels <- attr(x, "labels", exact = TRUE)
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
                        x <- recodeCharcat(
                            declared::as.declared(x),
                            metadata = metadata
                        )
                    }
                    else {
                        label <- attr(x, "label", exact = TRUE)
                        x <- as.character(x)
                        attr(x, "label") <- label
                    }

                    data[[colnms[i]]] <- x
                }
            }
        }

        if (recode) {
            data <- recodeMissings(
                dataset = data,
                to = "Stata",
                dictionary = dictionary,
                to_declared = FALSE,
                error_null = FALSE
            )
        }

        data[] <- lapply(data, function(x) {
            attr(x, "format.spss") <- NULL
            if (is.null(attr(x, "format.stata"))) {
                attr(x, "format.stata") <- getFormat(x, type = "Stata")
            }
            return(x)
        })

        arglist <- list(data = data)

        if (is.element("version", names(dots))) {
            arglist$version <- dots$version
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

            do.call(haven::write_dta, arglist)
            # return(invisible(arglist$data))
        # }
    }
    else if (identical(tp_to$fileext, "RDS")) {
        if (declared) {
            data <- declared::as.declared(data)
            class(data) <- "data.frame"
            saveRDS(data, to)
        }
        else {
            saveRDS(data, to)
        }
    }
    else if (identical(tp_to$fileext, "XLSX")) {
        var_labels <- sapply(variables, function(x) {
            lbl <- getElement(x, "label")
            if (is.null(lbl)) {
                lbl <- ""
            }
            return(lbl)
        })

        excel <- list(
            data = data,
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

                excel$values <- rbind(excel$values, temp)
            }
        }

        writexl::write_xlsx(excel, path = to)
    }
    else {
        # if (identical(tp_to$fileext, "SAS7BDAT")) {
        #     fargs <- names(formals(haven::write_sas))
        #     arglist <- dots[is.element(names(dots), fargs)]
        #     arglist$data <- declared::as.haven(data)
        #     arglist$path <- to
        #     do.call(haven::write_sas, arglist)
        # }
        # else if (identical(tp_to$fileext, "XPT")) {
            lnms <- nchar(colnames(data))
            if (any(lnms > 8)) {
                admisc::stopError("SAS .xpt files do not allow more than 8 characters for column names.")
            }
            fargs <- names(formals(haven::write_xpt))
            arglist <- dots[is.element(names(dots), fargs)]
            arglist$data <- declared::as.haven(data)
            arglist$path <- to
            if (is.null(arglist$version)) {
                # hardcode XPT version 5, since 8 doesn't work
                arglist$version <- 5
            }
            do.call(haven::write_xpt, arglist)
        # }

        to <- file.path(
            tp_from$completePath,
            paste(tp_from$filenames, "sas", sep = ".")
        )

        if (is.null(dictionary) & recode) {
            dictionary <- recodeMissings(
                dataset = arglist$data,
                to = "SAS",
                return_dictionary = TRUE
            )
        }

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
