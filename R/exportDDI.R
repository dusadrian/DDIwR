`exportDDI` <- function(
    codebook, file = "", embed = TRUE, OS = "", indent = 4,
    monolang = FALSE, xmlang = "en", xmlns = "", ...
) {
    # https://ddialliance.org/Specification/DDI-Codebook/2.5/
    # https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html
    
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
        x <- gsub("&", "&amp;", x)
        x <- gsub("<", "&lt;", x)
        x <- gsub(">", "&gt;", x)
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

    `catText` <- function(x, ...) {
        cat(paste(
            repeatSpace(x),
            paste(
                unlist(list(...)),
                collapse = ""
            ),
            enter,
            sep = ""
        ))
    }
    
    if (OS == "") {
        OS <- Sys.info()[["sysname"]]
    }

    enter <- getEnter(OS = OS)

    data <- codebook[["fileDscr"]][["datafile"]]
    obj  <- codebook[["dataDscr"]]
    
    uuid <- generateUUID(length(obj) + 1)
    
    prodate <- as.character(Sys.time())
    version <- as.character(packageVersion("DDIwR"))
    varnames <- names(obj)
    
    if (!identical(file, "")) {
        sink(file)
        on.exit(sink())
    }

    prodDate <- as.character(Sys.time())
    version <- as.character(packageVersion("DDIwR"))
    varnames <- names(obj)
    cat(paste(
        s0, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
        enter,
        sep = ""
    ))

    cat(paste(
        s0, "<", ns, ifelse(identical(ns, ""), "", ":"), "codeBook version=\"2.5\"",
        enter,
        "ID=\"", generateUUID(1), "\"",
        enter,
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
    
    cat(paste(s1, "<", ns, "docDscr>", enter, sep = ""))
    cat(paste(s2, "<", ns, "citation>", enter, sep = ""))
    cat(paste(s3, "<", ns, "titlStmt>", enter, sep = ""))
    cat(paste(
        s4, "<", ns, "titl", xmlang, ">", titl, "</", ns, "titl>",
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
        s3, "<", ns, "abstract", xmlang, ">", abstract, "</", ns, "abstract>",
        enter, sep = ""
    ))

    cat(paste(s2, "</", ns, "stdyInfo>", enter, sep = ""))

    cat(paste(s1, "</", ns, "stdyDscr>", enter, sep = ""))
    
    cat(paste(
        s1, "<", ns, "fileDscr ID=\"", uuid[length(uuid)], "\">",
        enter,
        sep = ""
    ))

    if (!is.null(data)) {
        if (!is.data.frame(data)) {
            admisc::stopError(
                "The 'datafile' component should be a data frame."
            )
        }
        else if (!identical(toupper(names(data)), toupper(names(obj)))) {
            admisc::stopError(
                "Variables in the data do not match the variables in the data description."
            )
        }

        cat(paste(s2, "<", ns, "fileTxt>", enter, sep = ""))
        if (!is.null(fileName <- codebook[["fileDscr"]][["fileName"]])) {
            cat(paste(s3, "<", ns, "fileName>", fileName, "</", ns, "fileName>", enter, sep = ""))
        }
        cat(paste(s3, "<", ns, "dimensns>", enter, sep = ""))
        cat(paste(s4, "<", ns, "caseQnty>", nrow(data), "</", ns, "caseQnty>", enter, sep = ""))
        cat(paste(s4, "<", ns, "varQnty>", ncol(data), "</", ns, "varQnty>", enter, sep = ""))
        cat(paste(s3, "</", ns, "dimensns>", enter, sep = ""))
        if (!is.null(fileType <- codebook[["fileDscr"]][["fileType"]])) {
            cat(paste(s3, "<", ns, "fileType>", fileType, "</", ns, "fileType>", enter, sep = ""))
        }
        cat(paste(s2, "</", ns, "fileTxt>", enter, sep = ""))

        if (embed) {
            cat(paste(s2, "<", ns, "notes>", enter, sep = ""))
            cat(paste(
                s0, "<![CDATA[# start data #",
                enter,
                readr::format_csv(declared::as.haven(data), na = ""),
                # vroom::vroom_format(data, delim = ",", na = ""),
                "# end data #", enter, "]]>",
                enter,
                sep = ""
            ))
            cat(paste(s2, "</", ns, "notes>", enter, sep = ""))
        }

        pN <- unlist(lapply(
            data[names(obj)],
            function(x) admisc::possibleNumeric(unclass(x))
        ))

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
    for (i in seq(length(obj))) {
        dcml <- ""
        if (!is.null(data)) {
            dcml <- paste0(
                " dcml=\"",
                ifelse(
                    pN[[names(obj)[i]]],
                    getDecimals(na.omit(aN[[names(obj)[i]]])),
                    0
                ),
                "\""
            )
        }
        
        nature <- ""
        if(any(grepl("measurement", names(obj[[i]])))) {
            nature <- paste0(" nature=\"", obj[[i]]$measurement, "\"")
        }
                         
        
        cat(paste0(
            s2, "<", ns, "var ID=\"", uuid[i], "\"",
            " name=\"", varnames[i], "\"",
            " files=\"", uuid[length(uuid)], "\"",
            dcml, nature, ">",
            enter
        ))
        
        if (!is.null(obj[[i]][["label"]])) {
            if (!is.na(obj[[i]][["label"]])) {
                cat(paste(
                    s3, "<", ns, "labl", xmlang, ">",
                    replaceChars(
                        obj[[i]][["label"]]
                    ),
                    "</", ns, "labl>",
                    enter,
                    sep = ""
                ))
            }
        }
        

        na_values <- NULL
        if (is.element("na_values", names(obj[[i]]))) {
            na_values <- obj[[i]]$na_values
        }

        na_range <- NULL
        if (is.element("na_range", names(obj[[i]]))) {
            na_range <- obj[[i]]$na_range
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

        lbls <- obj[[i]][["labels"]]
        type <- obj[[i]]$type
        
        if (!is.null(data)) {
            if (pN[i]) {
                vals <- aN[[names(obj)[i]]]

                if (!is.null(lbls)) {
                    ismiss <- is.element(lbls, na_values)
                    if (length(na_range) > 0) {
                        ismiss <- ismiss | (lbls >= na_range[1] & lbls <= na_range[2])
                    }
                    vals[is.element(vals, lbls[ismiss])] <- NA
                }

                printnum <- length(setdiff(vals[!is.na(vals)], lbls)) > 4
                if (!is.null(type)) printnum <- printnum | grepl("num", type)
                
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
        }
        
        if (any(grepl("labels", names(obj[[i]])))) {

            tbl <- table(data[[names(obj)[i]]])
            
            for (v in seq(length(lbls))) {
                
                ismiss <- is.element(lbls[v], na_values)
                if (length(na_range) > 0) {
                    ismiss <- ismiss | (lbls[v] >= na_range[1] & lbls[v] <= na_range[2])
                }

                cat(paste(
                    s3,
                    "<", ns, "catgry", ifelse(ismiss, " missing=\"Y\"", ""), ">",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s4,
                    "<", ns, "catValu>", replaceChars(lbls[v]), "</", ns, "catValu>",
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

        if (any(grepl("type", names(obj[[i]])))) {
            cat(paste(
                s3,
                "<", ns, "varFormat type=\"",
                ifelse(
                    grepl("char", obj[[i]]$type),
                    "character",
                    "numeric"
                ),
                "\"/>",
                enter,
                sep = ""
            ))
        }
        
        if (any(grepl("txt", names(obj[[i]])))) {
            cat(paste(s3, "<", ns, "txt>", enter, sep = ""))
            cat(paste(
                s0,
                "<![CDATA[", obj[[i]]$txt, "]]>",
                enter,
                sep = ""
            ))
            cat(paste(s3, "</", ns, "txt>", enter, sep = ""))
        }
        
        cat(paste(s2, "</", ns, "var>", enter, sep = ""))
    }
    
    cat(paste(s1, "</", ns, "dataDscr>", enter, sep = ""))
    cat(paste(s1, "<", ns, "otherMat level=\"", level, "\"></", ns, "otherMat>", enter, sep = ""))
    cat(paste(s0, "</", ns, "codeBook>", enter, sep = ""))
    
}
