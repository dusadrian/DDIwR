`exportDDI` <- function(
    codebook, file = "", embed = TRUE, OS = "", indent = 4, monolingual = FALSE,
    xmlang = "en", IDNo = "S0000", agency = "default", distrbtr = "default",
    URI = "http://www.default.edu"
) {
    
    # https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html
    

    ddi <- ""

    # dots <- list(...)
    # if (
    #     is.element("ddi", names(dots)) && is.atomic(dots$ddi) && 
    #     length(dots$ddi) == 1 && is.character(dots$ddi)
    # ) {
    #     ddi <- paste(dots$ddi, ":", sep = "")
    # }

    if (!is.atomic(xmlang) || !is.character(xmlang) || length(xmlang) != 1) {
        admisc::stopError(
            "Argument 'xmlang' should be a character vector of length 1"
        )
    }

    if (!is.atomic(agency) || !is.character(agency) || length(agency) != 1) {
        admisc::stopError(
            "Argument 'agency' should be a character vector of length 1"
        )
    }

    if (!is.atomic(URI) || !is.character(URI) || length(URI) != 1) {
        admisc::stopError(
            "Argument 'URI' should be a character vector of length 1"
        )
    }

    xmlang <- paste0(ifelse(isTRUE(monolingual), "", " "), "xml:lang=\"", xmlang, "\"")
    agency <- paste0("\"", agency, "\"")
    URI <- paste0("\"", URI, "\"")

    `generateUUID` <- function(x) {
        toreturn <- rep(NA, x)
        first <- sample(c(LETTERS, letters), x, replace = TRUE)
        for (i in seq(x)) {
            toreturn[i] <- paste(c(first[i], sample(c(LETTERS, letters, 0:9), 15, replace = TRUE)), collapse = "")
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
        x <- gsub("&", "&amp;", x)
        x <- gsub("<", "&lt;", x)
        x <- gsub(">", "&gt;", x)
        return(x)
    }

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
        s0, "<", ddi, "codeBook",
        enter,
        "version=\"2.5\"",
        enter,
        ifelse(isTRUE(monolingual), paste0(xmlang, enter), ""),
        "xmlns:ddi=\"ddi:codebook:2_5\"",
        enter,
        "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"",
        enter, 
        "xsi:schemaLocation=\"",
        "ddi:codebook:2_5 https://ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd",
        "\">",
        enter,
        sep = ""
    ))

    if (isTRUE(monolingual)) {
        xmlang <- ""
    }
    
    cat(paste(s1, "<", ddi, "docDscr>", enter, sep = ""))
    cat(paste(s2, "<", ddi, "citation>", enter, sep = ""))
    cat(paste(s3, "<", ddi, "titlStmt>", enter, sep = ""))
    cat(paste(
        s4, "<", ddi, "titl", xmlang, ">Generic title</", ddi, "titl>",
        enter,
        sep = ""
    ))
    cat(paste(s3, "</", ddi, "titlStmt>", enter, sep = ""))
    cat(paste(s3, "<", ddi, "prodStmt>", enter, sep = ""))
    cat(paste(
        s4, "<", ddi, "prodDate date=\"", prodDate, "\">",
        prodDate, "</", ddi, "prodDate>",
        enter,
        sep = ""
    ))
    cat(paste(
        s4, "<", ddi, "software version=\"", version,
        "\">R package DDIwR</", ddi, "software>",
        enter, sep = ""
    ))
    cat(paste(s3, "</", ddi, "prodStmt>", enter, sep = ""))
    cat(paste(s2, "</", ddi, "citation>", enter, sep = ""))
    cat(paste(s1, "</", ddi, "docDscr>", enter, sep = ""))

    cat(paste(s1, "<", ddi, "stdyDscr>", enter, sep = ""))
    cat(paste(s2, "<", ddi, "citation>", enter, sep = ""))

    cat(paste(s3, "<", ddi, "titlStmt>", enter, sep = ""))
    cat(paste(
        s4, "<", ddi, "titl", xmlang, ">Generic title</", ddi, "titl>",
        enter,
        sep = ""
    ))
    cat(paste(
        s4, "<", ddi, "IDNo agency=", agency,">", IDNo, "</", ddi, "IDNo>",
        enter,
        sep = ""
    ))
    cat(paste(s3, "</", ddi, "titlStmt>", enter, sep = ""))
    
    cat(paste(s3, "<", ddi, "distStmt>", enter, sep = ""))
    cat(paste(
        s4, "<", ddi, "distrbtr", xmlang, ">", distrbtr,
        "</", ddi, "distrbtr>",
        enter, sep = ""
    ))
    cat(paste(s3, "</", ddi, "distStmt>", enter, sep = ""))
    cat(paste(
        s3, "<", ddi, "holdings URI=", URI,
        ">Description of the study holdings</", ddi, "holdings>",
        enter, sep = ""
    ))

    cat(paste(s2, "</", ddi, "citation>", enter, sep = ""))
    cat(paste(s2, "<", ddi, "stdyInfo>", enter, sep = ""))
    cat(paste(
        s3, "<", ddi, "abstract", xmlang, ">Study abstract</", ddi, "abstract>",
        enter, sep = ""
    ))

    cat(paste(s2, "</", ddi, "stdyInfo>", enter, sep = ""))

    cat(paste(s1, "</", ddi, "stdyDscr>", enter, sep = ""))
    
    cat(paste(
        s1, "<", ddi, "fileDscr ID=\"", uuid[length(uuid)], "\">",
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

        data <- declared::as.haven(data)

        cat(paste(s2, "<", ddi, "fileTxt>", enter, sep = ""))
        if (!is.null(fileName <- codebook[["fileDscr"]][["fileName"]])) {
            cat(paste(s3, "<", ddi, "fileName>", fileName, "</", ddi, "fileName>", enter, sep = ""))
        }
        cat(paste(s3, "<", ddi, "dimensns>", enter, sep = ""))
        cat(paste(s4, "<", ddi, "caseQnty>", nrow(data), "</", ddi, "caseQnty>", enter, sep = ""))
        cat(paste(s4, "<", ddi, "varQnty>", ncol(data), "</", ddi, "varQnty>", enter, sep = ""))
        cat(paste(s3, "</", ddi, "dimensns>", enter, sep = ""))
        cat(paste(s2, "</", ddi, "fileTxt>", enter, sep = ""))

        if (embed) {
            cat(paste(s2, "<", ddi, "notes>", enter, sep = ""))
            cat(paste(
                s0, "<![CDATA[# start data #",
                enter,
                readr::format_csv(data, na = ""),
                "# end data #", enter, "]]>",
                enter,
                sep = ""
            ))
            cat(paste(s2, "</", ddi, "notes>", enter, sep = ""))
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

    cat(paste(s1, "</", ddi, "fileDscr>", enter, sep = ""))
    cat(paste(s1, "<", ddi, "dataDscr>", enter, sep = ""))
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
            s2, "<", ddi, "var ID=\"", uuid[i], "\"",
            " name=\"", varnames[i], "\"",
            " files=\"", uuid[length(uuid)], "\"",
            dcml, nature, ">",
            enter
        ))
        
        if (!is.null(obj[[i]][["label"]])) {
            if (!is.na(obj[[i]][["label"]])) {
                cat(paste(
                    s3, "<", ddi, "labl", xmlang, ">",
                    replaceChars(
                        obj[[i]][["label"]]
                    ),
                    "</", ddi, "labl>",
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
            cat(paste(s3, "<", ddi, "invalrng>", enter, sep = ""))

            if (any(is.element(na_range, c(-Inf, Inf)))) {
                if (identical(na_range[1], -Inf)) {
                    cat(paste(
                        s4,
                        sprintf(
                            "<%srange UNITS=\"INT\" max=\"%s\"/>",
                            ddi, na_range[2]
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
                            ddi, na_range[1]
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
                        ddi, na_range[1], na_range[2]
                    ),
                    enter,
                    sep = ""
                ))
            }
                
            cat(paste(s3, "</", ddi, "invalrng>", enter, sep = ""))
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
                        "<", ddi, "sumStat type=\"min\">",
                        format(
                            min(vals, na.rm = TRUE),
                            scientific = FALSE
                        ),
                        "</", ddi, "sumStat>",
                        enter,
                        sep = ""
                    ))

                    cat(paste(
                        s3,
                        "<", ddi, "sumStat type=\"max\">",
                        format(
                            max(vals, na.rm = TRUE),
                            scientific = FALSE
                        ),
                        "</", ddi, "sumStat>",
                        enter,
                        sep = ""
                    ))
                    
                    cat(paste(
                        s3,
                        "<", ddi, "sumStat type=\"mean\">",
                        format(
                            mean(vals, na.rm = TRUE),
                            scientific = FALSE
                        ),
                        "</", ddi, "sumStat>",
                        enter,
                        sep = ""
                    ))
                    
                    cat(paste(
                        s3,
                        "<", ddi, "sumStat type=\"medn\">",
                        format(
                            median(vals, na.rm = TRUE),
                            scientific = FALSE
                        ),
                        "</", ddi, "sumStat>",
                        enter,
                        sep = ""
                    ))
                    
                    cat(paste(
                        s3,
                        "<", ddi, "sumStat type=\"stdev\">",
                        format(
                            sd(vals, na.rm = TRUE),
                            scientific = FALSE
                        ),
                        "</", ddi, "sumStat>",
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
                    "<", ddi, "catgry", ifelse(ismiss, " missing=\"Y\"", ""), ">",
                    enter,
                    sep = ""
                ))

                cat(paste(
                    s4,
                    "<", ddi, "catValu>", replaceChars(lbls[v]), "</", ddi, "catValu>",
                    enter,
                    sep = ""
                ))
                
                cat(paste(
                    s4,
                    "<", ddi, "labl", xmlang, ">",
                    replaceChars(names(lbls)[v]),
                    "</", ddi, "labl>",
                    enter,
                    sep = ""
                ))
                
                if (!is.null(data)) {
                    freq <- tbl[match(lbls[v], names(tbl))]
                    cat(paste(
                        s4,
                        "<", ddi, "catStat type=\"freq\">", 
                        ifelse(
                            is.na(freq),
                            0,
                            format(freq, scientific = FALSE)
                        ), 
                        "</", ddi, "catStat>",
                        enter,
                        sep = ""
                    ))
                }

                cat(paste(s3, "</", ddi, "catgry>", enter, sep = ""))
            }
        }

        if (any(grepl("type", names(obj[[i]])))) {
            cat(paste(
                s3,
                "<", ddi, "varFormat type=\"",
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
            cat(paste(s3, "<", ddi, "txt>", enter, sep = ""))
            cat(paste(
                s0,
                "<![CDATA[", obj[[i]]$txt, "]]>",
                enter,
                sep = ""
            ))
            cat(paste(s3, "</", ddi, "txt>", enter, sep = ""))
        }
        
        cat(paste(s2, "</", ddi, "var>", enter, sep = ""))
    }
    
    cat(paste(s1, "</", ddi, "dataDscr>", enter, sep = ""))
    cat(paste(s1, "<", ddi, "otherMat></", ddi, "otherMat>", enter, sep = ""))
    cat(paste(s0, "</", ddi, "codeBook>", enter, sep = ""))
    
}
