#' @name exportDDI
#' @title
#' Export to a DDI metadata file
#'
#' @description
#' Create a DDI Codebook version 2.5, XML file structure.
#'
#' @details
#' #' The information object can either be a data file (includign an R data
#' frame) or a list having two main list components:
#'
#' - **`fileDscr`**, if the data is provided in a subcomponent named
#' **`datafile`**
#'
#' - **`dataDscr`**, having as many components as the number of variables in the
#' (meta)data. For each variable, there should a mandatory subcomponent called
#' **`label`** (that contains the variable's label) and, if the variable is of a
#' categorical type, another subcomponent called **`labels`**.
#'
#' Additional informations about the variables can be specified as further
#' subcomponents, combining DDI specific data but also other information that
#' might not be covered by DDI:
#'
#' - **`measurement`** is the equivalent of the specific DDI attribute
#' **`nature`** of the **`var`** element, which accepts these values:
#' `"nominal"`, `"ordinal"`, `"interval"`, `"ratio"`, `"percent"`, and
#' `"other"`.
#'
#' - **`type`** is useful for multiple reasons. A first one, if the variable is
#' numerical, is to differentiate between `discrete` and `continuous` values of
#' the attribute **`intrvl`** from the same DDI element **`var`**. Another
#' reason is to help identifying pure string variables (containing text), when
#' the subcomponent **`type`** is equal to `"char"`. It is also used for the
#' subelement **`varFormat`** of the element **`var`**. Finally, another reason
#' is to differentiate between pure categorical (`"cat"`) and pure numerical
#' (`"num"`) variables, as well as mixed ones, among which `"numcat"` referring
#' to a numerical variable with very few values (such as the number of
#' children), for which it is possible to also produce a table of frequencies
#' along the numerical summaries. There are also categorical variables that can
#' be interpreted as numeric (`"catnum"`), such as a Likert type response scale
#' with 7 values, where numerical summaries are also routinely performed along
#' with the usual table of frequencies.
#'
#' - **`missing`** is an important subcomponent, indicating which of the values
#' in the variable are going to be treated as missing values, and it is going to
#' be exported as the attribute `missing` of the DDI subelement **`catgry`**.
#'
#' There are many more possible attributes and DDI elements to be added in the
#' information object, future versions of this function will likely expand.
#'
#' For the moment, only DDI codebook version 2.5 is exported, and DDI Lifecycle
#' is planned for future releases.
#'
#' Argument **`xmlang`** expects a two letter ISO country coding, for instance
#' `"en"` to indicate English, or `"ro"` to indicate Romanian etc.
#'
#' If the document is monolang, this argument is placed a single time for the
#' entire document, in the attributes of the `codeBook` element. For
#' multilingual documents, it is placed in the attributes of various other
#' (sub)elements, for instance `abstract` as an obvious one, or the study
#' title, name of the distributing institution, variable labels etc.
#'
#' The argument **`OS`** can be either:\cr
#' `"windows"` (default), or `"Windows"`, `"Win"`, `"win"`,\cr
#' `"MacOS"`, `"Darwin"`, `"Apple"`, `"Mac"`, `"mac"`,\cr
#' `"Linux"`, `"linux"`.
#'
#' The end of line separator changes only when the target OS is different from
#' the running OS.
#'
#' The argument **`indent`** controls how many spaces will be used in the XML
#' file, to indent the different subelements.
#'
#' A small number of required DDI specific elements and attributes have generic
#' default values but they may be specified using the three dots **`...`**
#' argument. For the current version, these are: `IDNo`, `titl`, `agency`, `URI`
#' (for the `holdings` element), `distrbtr`, `abstract` and `level` (for the
#' `otherMat` element).
#'
#' @return
#' An XML file containing a DDI version 2.5 metadata.
#'
#' @seealso
#' \url{https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html}
#'
#' @examples
#' codeBook <- list(dataDscr = list(
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
#' ),
#' V5 = list(
#'     label = "Political party reference",
#'     type = "char",
#'     txt = "When the respondent indicated his political party reference,
#'         his/her open response was recoded on a scale of 1-99 with parties
#'         with a left-wing orientation coded on the low end of the scale and
#'         parties with a right-wing orientation coded on the high end of the
#'         scale. Categories 90-99 were reserved miscellaneous responses."
#' )))
#'
#' \dontrun{
#' exportDDI(codeBook, file = "codebook.xml")
#'
#' # using a namespace
#' exportDDI(codeBook, file = "codebook.xml", xmlns = "ddi")
#' }
#'
#' @author Adrian Dusa
#'
#' @param codebook
#' A list object containing the metadata, or a path to a directory
#' where these objects are located, for batch processing
#'
#' @param file
#' either a character string naming a file or a connection open for
#' writing. "" indicates output to the console
#'
#' @param embed
#' Logical, embed the CSV datafile in the XML file, if present
#'
#' @param OS
#' The target operating system, for the eol - end of line character(s)
#'
#' @param indent Indent width, in number of spaces
#'
#' @param monolang Logical, monolang or multilingual document
#'
#' @param xmlang ISO two letter code for the language used in the DDI elements
#'
#' @param xmlns
#' Character, namespace for the XML file (ignored if already present
#' in the codebook object)
#'
#' @param ... Other arguments, mainly for internal use
#'
#' @export

`exportDDI` <- function(
    codebook, file = "", embed = TRUE, OS = "", indent = 4,
    monolang = FALSE, xmlang = "en", xmlns = "", ...
) {
    # https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html

    # validation procedure:
    # https://ddialliance.org/Specification/DDI-Codebook/2.5/
    # schema <- read_xml("path/to/ddi_2_5_1/schemas/codebook.xsd")
    # doc <- read_xml("path/to/filetovalidate.xml")
    # xml_validate(doc, schema)

    `check_arg` <- function(x, vx, type = "c") {
        valid <- is.atomic(vx) && length(vx) == 1
        if (valid & type == "c") { # character
            valid <- is.character(vx) & !admisc::possibleNumeric(vx)
        }

        if (!valid) {
            admisc::stopError(
                sprintf(
                    "Argument '%s' should be a%s vector of length 1",
                    x,
                    ifelse(type == "c", " character", "")
                )
            )
        }
    }

    `check_dots` <- function(x, default, type = "c") {
        if (is.element(x, names(dots))) {
            dotsx <- dots[[x]]
            check_arg(x, dotsx, type = type)
            return(dotsx)
        }
        return(default)
    }

    `generateUUID` <- function(x) {
        toreturn <- rep(NA, x)
        first <- sample(c(LETTERS, letters), x, replace = TRUE)
        for (i in seq(x)) {
            toreturn[i] <- paste(
                c(
                    first[i],
                    sample(c(LETTERS, letters, 0:9), 15, replace = TRUE)
                ),
                collapse = ""
            )
        }
        return(toreturn)
        # a safer way is to use unique() but it is highly unlikely this would be needed
        # toreturn <- unique(toreturn)
        # if (length(toreturn) == x) return(toreturn)
    }

    `repeatSpace` <- function(times) {
        paste(rep(" ", times*indent), collapse = "")
    }

    `replaceChars` <- function(x) {
        x <- replaceTicks(x)
        x <- gsub(
            "<", "&lt;",
            gsub(
                ">", "&gt;",
                gsub(
                    "&", "&amp;",
                    x
                )
            )
        )

        tc <- admisc::tryCatchWEM({
            # use case: StatConverter in Electron, the terminal that opens R
            # probably doesn't have a suitable locale and it outputs and error
            x <- gsub(rawToChar(as.raw(c(194, 160))), " ", x)
        })
        
        return(x)
    }

    dots <- list(...)

    IDNo <- check_dots("IDNo", "S0000", type = "any")
    titl <- check_dots("titl", "Generic title")
    agency <- check_dots("agency", "default")
    URI <- check_dots("URI", "http://www.default.eu")
    distrbtr <- check_dots("distrbtr", "Name of the distributing institution")
    abstract <- check_dots("abstract", "Study abstract")
    level <- check_dots("level", "0")

    check_arg("xmlang", xmlang)

    xmlang <- paste0(
        ifelse(isTRUE(monolang), "", " "),
        "xml:lang=\"", xmlang, "\""
    )

    ns <- codebook[["xmlns"]]
    if (is.null(ns)) ns <- xmlns
    check_arg("xmlns", ns, "c")


    s0 <- repeatSpace(0)
    s1 <- repeatSpace(1)
    s2 <- repeatSpace(2)
    s3 <- repeatSpace(3)
    s4 <- repeatSpace(4)
    s5 <- repeatSpace(5)

    # `catText` <- function(x, ...) {
    #     cat(paste(
    #         repeatSpace(x),
    #         paste(
    #             unlist(list(...)),
    #             collapse = ""
    #         ),
    #         enter,
    #         sep = ""
    #     ))
    # }

    if (OS == "") {
        OS <- Sys.info()[["sysname"]]
    }

    enter <- getEnter(OS = OS)

    data <- codebook[["fileDscr"]][["datafile"]]
    stdyDscr <- codebook[["stdyDscr"]]
    dataDscr <- codebook[["dataDscr"]]
    pN <- logical(length(dataDscr))

    # uuid for all variables
    uuid <- generateUUID(length(dataDscr))

    prodDate <- as.character(Sys.time())
    version <- as.character(packageVersion("DDIwR"))
    varnames <- names(dataDscr)

    if (!identical(file, "")) {
        sink(file)
        on.exit(sink())
    }

    cat(paste(
        s0, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
        enter,
        sep = ""
    ))

    cat(paste(
        s0, "<", ns, ifelse(identical(ns, ""), "", ":"), "codeBook version=\"2.5\"",
        enter,
        # Apparently, Nesstar interprets this ID as the Study Description ID
        # "ID=\"", generateUUID(1), "\"",
        # enter,
        ifelse(isTRUE(monolang), paste0(xmlang, enter), ""),
        "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
        enter,
        "xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"",
        enter,
        "xsi:schemaLocation=\"",
        "ddi:codebook:2_5 https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd\"",
        enter,
        paste0("xmlns", ifelse(identical(ns, ""), "", ":"), ns, "=\"ddi:codebook:2_5\">"),
        enter,
        sep = ""
    ))

    if (!identical(ns, "")) ns <- paste0(ns, ":")

    if (isTRUE(monolang)) {
        xmlang <- ""
    }

    # Document description
    cat(paste(s1, "<", ns, "docDscr>", enter, sep = ""))
    cat(paste(s2, "<", ns, "citation>", enter, sep = ""))
    cat(paste(s3, "<", ns, "titlStmt>", enter, sep = ""))
    cat(paste(
        s4, "<", ns, "titl", xmlang, ">", titl, "</", ns, "titl>",
        enter,
        sep = ""
    ))
    cat(paste(
        s4, "<", ns, "IDNo agency=\"", agency,"\">", generateUUID(1), "</", ns, "IDNo>",
        enter,
        sep = ""
    ))
    cat(paste(s3, "</", ns, "titlStmt>", enter, sep = ""))
    cat(paste(s3, "<", ns, "prodStmt>", enter, sep = ""))
    cat(paste(
        s4, "<", ns, "prodDate date=\"", prodDate, "\">",
        prodDate, "</", ns, "prodDate>",
        enter,
        sep = ""
    ))
    cat(paste(
        s4, "<", ns, "software version=\"", version,
        "\">R package DDIwR</", ns, "software>",
        enter, sep = ""
    ))
    cat(paste(s3, "</", ns, "prodStmt>", enter, sep = ""))
    cat(paste(s2, "</", ns, "citation>", enter, sep = ""))
    cat(paste(s1, "</", ns, "docDscr>", enter, sep = ""))


    # Study description
    if (is.null(stdyDscr)) {
        cat(paste(s1, "<", ns, "stdyDscr>", enter, sep = ""))
        cat(paste(s2, "<", ns, "citation>", enter, sep = ""))

        cat(paste(s3, "<", ns, "titlStmt>", enter, sep = ""))
        cat(paste(
            s4, "<", ns, "titl", xmlang, ">", titl, "</", ns, "titl>",
            enter,
            sep = ""
        ))
        cat(paste(
            s4, "<", ns, "IDNo agency=\"", agency,"\">", IDNo, "</", ns, "IDNo>",
            enter,
            sep = ""
        ))
        cat(paste(s3, "</", ns, "titlStmt>", enter, sep = ""))

        cat(paste(s3, "<", ns, "distStmt>", enter, sep = ""))
        cat(paste(
            s4, "<", ns, "distrbtr", xmlang, ">", distrbtr,
            "</", ns, "distrbtr>",
            enter, sep = ""
        ))
        cat(paste(s3, "</", ns, "distStmt>", enter, sep = ""))

        cat(paste(
            s3, "<", ns, "holdings URI=\"", URI,
            "\">Description of the study holdings</", ns, "holdings>",
            enter, sep = ""
        ))

        cat(paste(s2, "</", ns, "citation>", enter, sep = ""))
        cat(paste(s2, "<", ns, "stdyInfo>", enter, sep = ""))
        cat(paste(
            s3,
            "<", ns, "abstract", xmlang, ">", abstract, "</", ns, "abstract>",
            enter, sep = ""
        ))

        cat(paste(s2, "</", ns, "stdyInfo>", enter, sep = ""))

        cat(paste(s1, "</", ns, "stdyDscr>", enter, sep = ""))
    }
    else {

        cat(paste(makeXML(stdyDscr, 1, indent, ns, enter), collapse = ""))

        ## when no xml namespace is needed, this works:
        # stdyDscr <- as_xml_document(list(stdyDscr = stdyDscr))

        # addns <- function(x) {
        #     if (!is.null(names(x))) {
        #         names(x) <- paste0(ns, names(x))
        #         if (is.list(x)) {
        #             x <- lapply(x, function(x) {
        #                 addns(x)
        #             })
        #         }
        #     }
        #     return(x)
        # }

        # stdyDscr <- lapply(list(stdyDscr), addns)
        # names(stdyDscr) <- paste0(ns, "stdyDscr")
        # stdyDscr <- as_xml_document(stdyDscr) # error can not find namespace
        # cat(as.character(stdyDscr))
    }

    fileDscrUUID <- generateUUID(1)
    cat(paste(
        s1, "<", ns, "fileDscr ID=\"", fileDscrUUID, "\">",
        enter,
        sep = ""
    ))

    if (!is.null(data)) {
        if (!is.data.frame(data)) {
            admisc::stopError(
                "The 'datafile' component should be a data frame."
            )
        }
        else if (!identical(toupper(names(data)), toupper(names(dataDscr)))) {
            admisc::stopError(
                paste(
                    "Variables in the data do not match",
                    "the variables in the data description."
                )
            )
        }

        cat(paste(s2, "<", ns, "fileTxt>", enter, sep = ""))
        if (!is.null(fileName <- codebook[["fileDscr"]][["fileName"]])) {
            cat(paste(
                s3,
                "<", ns, "fileName>", fileName, "</", ns, "fileName>",
                enter,
                sep = ""
            ))
        }
        cat(paste(s3, "<", ns, "dimensns>", enter, sep = ""))
        cat(paste(
            s4,
            "<", ns, "caseQnty>", nrow(data), "</", ns, "caseQnty>",
            enter,
            sep = ""
        ))
        cat(paste(
            s4,
            "<", ns, "varQnty>", ncol(data), "</", ns, "varQnty>",
            enter,
            sep = ""
        ))
        cat(paste(s3, "</", ns, "dimensns>", enter, sep = ""))
        if (!is.null(fileType <- codebook[["fileDscr"]][["fileType"]])) {
            cat(paste(
                s3,
                "<", ns, "fileType>", fileType, "</", ns, "fileType>",
                enter,
                sep = ""
            ))
        }
        cat(paste(s2, "</", ns, "fileTxt>", enter, sep = ""))

        if (embed) {
            cat(paste(s2, "<", ns, "notes level=\"file\" subject=\"CSV dataset\">", enter, sep = ""))
            cat(paste(
                s3, "<![CDATA[# start data #",
                enter,
                sep = ""
            ))

            tt <- tempfile()
            # sink()
            suppressWarnings(
                write.table(
                    undeclare(data, drop = TRUE),
                    file = tt,
                    sep = ",",
                    na = "",
                    # append = TRUE,
                    row.names = FALSE
                )
            )
            # sink(file, append = TRUE)

            cat(
                paste(s3, readLines(tt), collapse = enter, sep = ""),
                enter,
                sep = ""
            )

            cat(paste(s3,
                "# end data #]]>",
                enter,
                sep = ""
            ))
            cat(paste(s2, "</", ns, "notes>", enter, sep = ""))
        }

        pN <- sapply(data[names(dataDscr)], function(x) {
            admisc::possibleNumeric(unclass(x))
        })

        aN <- lapply(
            subset(
                data,
                select = is.element(
                    names(data),
                    names(pN)[pN]
                )
            ),
            # data[, names(pN)[pN], drop = FALSE],
            function(x) admisc::asNumeric(unclass(x))
        )

    }

    cat(paste(s1, "</", ns, "fileDscr>", enter, sep = ""))
    cat(paste(s1, "<", ns, "dataDscr>", enter, sep = ""))

    for (i in seq(length(dataDscr))) {
        dcml <- ""
        if (!is.null(data) && pN[i]) {
            dcml <- paste0(
                " dcml=\"",
                admisc::numdec(na.omit(aN[[varnames[i]]])),
                "\""
            )
        }
    
        nature <- ""
        if (is.element("measurement", names(dataDscr[[i]]))) {
            nature <- paste0(
                " nature=\"",
                gsub(
                    "categorical|quantitative|, ", "",
                    dataDscr[[i]]$measurement,
                ),
                "\""
            )
        }

        cat(paste0(
            s2, "<", ns, "var ID=\"", uuid[i], "\"",
            " name=\"", varnames[i], "\"",
            " files=\"", fileDscrUUID, "\"",
            dcml, nature, ">",
            enter
        ))

        if (!is.null(dataDscr[[i]][["label"]])) {
            if (!is.na(dataDscr[[i]][["label"]])) {
                cat(paste(
                    s3, "<", ns, "labl", xmlang, ">",
                    replaceChars(
                        dataDscr[[i]][["label"]]
                    ),
                    "</", ns, "labl>",
                    enter,
                    sep = ""
                ))
            }
        }


        na_values <- NULL
        if (is.element("na_values", names(dataDscr[[i]]))) {
            na_values <- dataDscr[[i]]$na_values
        }

        na_range <- NULL
        if (is.element("na_range", names(dataDscr[[i]]))) {
            na_range <- dataDscr[[i]]$na_range
        }


        if (length(na_range) > 0) {
            cat(paste(s3, "<", ns, "invalrng>", enter, sep = ""))

            if (any(is.element(na_range, c(-Inf, Inf)))) {
                if (identical(na_range[1], -Inf)) {
                    cat(paste(
                        s4,
                        sprintf(
                            "<%srange UNITS=\"INT\" max=\"%s\"/>",
                            ns, na_range[2]
                        ),
                        enter,
                        sep = ""
                    ))
                }
                else {
                    cat(paste(
                        s4,
                        sprintf(
                            "<%srange UNITS=\"INT\" min=\"%s\"/>",
                            ns, na_range[1]
                        ),
                        enter,
                        sep = ""
                    ))
                }
            }
            else {
                cat(paste(
                    s4,
                    sprintf(
                        "<%srange UNITS=\"INT\" min=\"%s\" max=\"%s\"/>",
                        ns, na_range[1], na_range[2]
                    ),
                    enter,
                    sep = ""
                ))
            }

            cat(paste(s3, "</", ns, "invalrng>", enter, sep = ""))
        }

        lbls <- dataDscr[[i]][["labels"]]
        # if (!is.null(lbls)) {
        #     # non-ASCII also means non multibyte characters
        #     lbls <- setNames(
        #         admisc::trimstr(
        #             iconv(lbls, "UTF-8", "ASCII", sub = "")
        #         ),
        #         names(lbls)
        #     )
        # }

        type <- dataDscr[[i]]$type

        # even if the data is not present, pN is FALSE for all variables
        if (pN[i]) {
            vals <- aN[[names(dataDscr)[i]]]

            if (!is.null(lbls)) {
                ismiss <- is.element(lbls, na_values)
                if (length(na_range) > 0) {
                    ismiss <- ismiss | (lbls >= na_range[1] & lbls <= na_range[2])
                }
                vals[is.element(vals, lbls[ismiss])] <- NA
            }

            vals <- na.omit(vals)

            # this is a test if a variable truly is numeric
            # (not just categorical using numbers)
            # if it has at least four(?) values different from the labels
            printnum <- length(setdiff(vals, lbls)) > 4

            if (!is.null(type)) {
                # at least two non missing values are needed to calculate sd()
                printnum <- printnum | (length(vals) > 2 & grepl("num", type))
            }

            if (printnum) { # numeric variable
                cat(paste(
                    s3,
                    "<", ns, "sumStat type=\"min\">",
                    format(
                        min(vals, na.rm = TRUE),
                        scientific = FALSE
                    ),
                    "</", ns, "sumStat>",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s3,
                    "<", ns, "sumStat type=\"max\">",
                    format(
                        max(vals, na.rm = TRUE),
                        scientific = FALSE
                    ),
                    "</", ns, "sumStat>",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s3,
                    "<", ns, "sumStat type=\"mean\">",
                    format(
                        mean(vals, na.rm = TRUE),
                        scientific = FALSE
                    ),
                    "</", ns, "sumStat>",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s3,
                    "<", ns, "sumStat type=\"medn\">",
                    format(
                        median(vals, na.rm = TRUE),
                        scientific = FALSE
                    ),
                    "</", ns, "sumStat>",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s3,
                    "<", ns, "sumStat type=\"stdev\">",
                    format(
                        sd(vals, na.rm = TRUE),
                        scientific = FALSE
                    ),
                    "</", ns, "sumStat>",
                    enter,
                    sep = ""
                ))

            }
        }


        if (!is.null(lbls)) {

            # what is the difference from data[[i]] ?
            tbl <- table(data[[names(dataDscr)[i]]])

            for (v in seq(length(lbls))) {

                ismiss <- is.element(lbls[v], na_values)
                if (length(na_range) > 0 & pN[i]) {
                    ismiss <- ismiss | (
                        lbls[v] >= na_range[1] & lbls[v] <= na_range[2]
                    )
                }

                cat(paste(
                    s3,
                    "<", ns, "catgry",
                    ifelse(ismiss, " missing=\"Y\"", ""), ">",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s4,
                    "<", ns, "catValu>",
                    replaceChars(lbls[v]),
                    "</", ns, "catValu>",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s4,
                    "<", ns, "labl", xmlang, ">",
                    replaceChars(names(lbls)[v]),
                    "</", ns, "labl>",
                    enter,
                    sep = ""
                ))

                if (!is.null(data)) {
                    freq <- tbl[match(lbls[v], names(tbl))]
                    cat(paste(
                        s4,
                        "<", ns, "catStat type=\"freq\">",
                        ifelse(
                            is.na(freq),
                            0,
                            format(freq, scientific = FALSE)
                        ),
                        "</", ns, "catStat>",
                        enter,
                        sep = ""
                    ))
                }

                cat(paste(s3, "</", ns, "catgry>", enter, sep = ""))
            }
        }

        if (any(grepl("type", names(dataDscr[[i]])))) {
            varFormat <- dataDscr[[i]]$varFormat[1] # SPSS
            cat(paste(
                s3,
                "<", ns, "varFormat type=\"",
                ifelse(
                    grepl("char", dataDscr[[i]]$type),
                    "character",
                    "numeric"
                ),
                "\" schema=\"other\" formatname=\"",
                substr(varFormat, 1, 1),
                "\">",
                varFormat,
                "</", ns, "varFormat>",
                enter,
                sep = ""
            ))
        }

        if (any(grepl("txt", names(dataDscr[[i]])))) {
            cat(paste(s3, "<", ns, "txt>", enter, sep = ""))
            cat(paste(
                s0,
                "<![CDATA[", dataDscr[[i]]$txt, "]]>",
                enter,
                sep = ""
            ))
            cat(paste(s3, "</", ns, "txt>", enter, sep = ""))
        }

        cat(paste(s2, "</", ns, "var>", enter, sep = ""))
    }

    cat(paste(s1, "</", ns, "dataDscr>", enter, sep = ""))
    cat(paste(
        s1,
        "<", ns, "otherMat level=\"", level, "\"></", ns, "otherMat>",
        enter,
        sep = ""
    ))
    cat(paste(s0, "</", ns, "codeBook>", enter, sep = ""))

}
