`exportDDI` <- function(codebook, file = "", embed = TRUE, OS = "", indent = 4) {
    
    # http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html

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
        paste(rep(" ", times*indent), collapse="")
    }

    s0 <- repeatSpace(0)
    s1 <- repeatSpace(1)
    s2 <- repeatSpace(2)
    s3 <- repeatSpace(3)
    s4 <- repeatSpace(4)
    s5 <- repeatSpace(5)

    `catText` <- function(x, ...) {
        cat(repeatSpace(x), paste(unlist(list(...)), collapse = ""), enter, sep = "")
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
    }

    prodDate <- as.character(Sys.time())
    version <- as.character(packageVersion("DDIwR"))

    varnames <- names(obj)
    cat(s0, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>", enter, sep = "")
    cat(s0, "<codeBook", enter,
        "version=\"2.5\"", enter,
        "xmlns=\"ddi:codebook:2_5\"", enter,
        "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"", enter, 
        "xsi:schemaLocation=\"http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd\">", enter, sep = "")
    cat(s1, "<docDscr>", enter, sep = "")
    cat(s2, "<citation>", enter, sep = "")
    cat(s3, "<titlStmt>", enter, sep = "")
    cat(s4, "<titl>Generic title</titl>", enter, sep = "")
    cat(s3, "</titlStmt>", enter, sep = "")
    cat(s3, "<prodStmt>", enter, sep = "")
    cat(s4, "<prodDate date=\"", prodDate, "\">", prodDate, "</prodDate>", enter, sep = "")
    cat(s4, "<software version=\"", version,"\">R package DDIwR</software>", enter, sep = "")
    cat(s3, "</prodStmt>", enter, sep = "")
    cat(s2, "</citation>", enter, sep = "")
    cat(s1, "</docDscr>", enter, sep = "")

    cat(s1, "<stdyDscr>", enter, sep = "")
    cat(s2, "<citation>", enter, sep = "")
    cat(s3, "<titlStmt>", enter, sep = "")
    cat(s4, "<titl>Generic title</titl>", enter, sep = "")
    cat(s3, "</titlStmt>", enter, sep = "")
    cat(s2, "</citation>", enter, sep = "")
    cat(s1, "</stdyDscr>", enter, sep = "")
    
    cat(s1, "<fileDscr ID=\"", uuid[length(uuid)], "\">", enter, sep = "")

    if (!is.null(data)) {
        if (!is.data.frame(data)) {
            cat("\n")
            stop("The 'datafile' component should be a data frame.\n\n", call. = FALSE)
        }
        else if (!identical(toupper(names(data)), toupper(names(obj)))) {
            cat("\n")
            stop("Variables in the data do not match the variables in the data description.\n\n", call. = FALSE)
        }

        cat(s2, "<fileTxt>", enter, sep = "")
        cat(s3, "<dimensns>", enter, sep = "")
        cat(s4, "<caseQnty>", nrow(data), "</caseQnty>", enter, sep = "")
        cat(s4, "<varQnty>", ncol(data), "</varQnty>", enter, sep = "")
        cat(s3, "</dimensns>", enter, sep = "")
        cat(s2, "</fileTxt>", enter, sep = "")

        if (embed) {
            cat(s2, "<notes>", enter, sep = "")
            cat(s0, "<![CDATA[# start data #", enter,
                readr::format_csv(data, na = ""),
                "# end data #", enter, "]]>", enter, sep = "")
            cat(s2, "</notes>", enter, sep = "")
        }

        pN <- unlist(lapply(data[names(obj)], function(x) admisc::possibleNumeric(x)))
        aN <- lapply(data[, names(pN)[pN]], function(x) admisc::asNumeric(x))

    }

    cat(s1, "</fileDscr>", enter, sep = "")

    cat(s1, "<dataDscr>", enter, sep = "")

    for (i in seq(length(obj))) {
        
        dcml <- ""
        if (!is.null(data)) {
            dcml <- c(" dcml=\"",
                      ifelse(pN[names(obj)[i]], getDecimals(na.omit(aN[[names(obj)[i]]])),  0),
                      "\"")
        }
        
        nature <- ""
        if(any(grepl("measurement", names(obj[[i]])))) {
            nature <- c(" nature=\"", obj[[i]]$measurement, "\"")
        }
                         
        
        cat(s2, "<var ID=\"", uuid[i], "\" name=\"", varnames[i], "\" files=\"", uuid[length(uuid)], "\"", dcml, nature, ">", enter, sep = "")
        
        if (!is.null(obj[[i]][["label"]])) {
            if (!is.na(obj[[i]][["label"]])) {
                cat(s3, "<labl>", obj[[i]][["label"]], "</labl>", enter, sep = "")
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
            cat(s3, "<invalrng>", enter, sep = "")

            if (any(is.element(na_range, c(-Inf, Inf)))) {
                if (identical(na_range[1], -Inf)) {
                    cat(s4, sprintf("<range UNITS=\"INT\" max=\"%s\"/>", na_range[2]), enter, sep = "")
                }
                else {
                    cat(s4, sprintf("<range UNITS=\"INT\" min=\"%s\"/>", na_range[1]), enter, sep = "")
                }
            }
            else {
                cat(s4, sprintf("<range UNITS=\"INT\" min=\"%s\" max=\"%s\"/>", na_range[1], na_range[2]), enter, sep = "")
            }
                
            cat(s3, "</invalrng>", enter, sep = "")
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
                    cat(s3, "<sumStat type=\"min\">", format(min(vals, na.rm = TRUE), scientific = FALSE), "</sumStat>", enter, sep = "")
                    cat(s3, "<sumStat type=\"max\">", format(max(vals, na.rm = TRUE), scientific = FALSE), "</sumStat>", enter, sep = "")
                    cat(s3, "<sumStat type=\"mean\">", format(mean(vals, na.rm = TRUE), scientific = FALSE), "</sumStat>", enter, sep = "")
                    cat(s3, "<sumStat type=\"medn\">", format(median(vals, na.rm = TRUE), scientific = FALSE), "</sumStat>", enter, sep = "")
                    cat(s3, "<sumStat type=\"stdev\">", format(sd(vals, na.rm = TRUE), scientific = FALSE), "</sumStat>", enter, sep = "")
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

                cat(s3, "<catgry", ifelse(ismiss, " missing=\"Y\"", ""), ">", enter, sep = "")
                cat(s4, "<catValu>",  lbls[v],  "</catValu>", enter, sep = "")
                cat(s4, "<labl>",  names(lbls)[v],  "</labl>", enter, sep = "")
                
                if (!is.null(data)) {
                    freq <- tbl[match(lbls[v], names(tbl))]
                    cat(s4, "<catStat type=\"freq\">", 
                            ifelse(is.na(freq), 0, format(freq, scientific = FALSE)), 
                            "</catStat>", enter, sep = "")
                }

                cat(s3, "</catgry>", enter, sep = "")
            }
        }

        if (any(grepl("type", names(obj[[i]])))) {
            cat(s3, "<varFormat type=\"", ifelse(grepl("char", obj[[i]]$type), "character", "numeric"), "\"/>", enter, sep = "")
        }
        
        if (any(grepl("txt", names(obj[[i]])))) {
            cat(s3, "<txt>", enter, sep = "")
            cat(s0, "<![CDATA[", obj[[i]]$txt, "]]>", enter, sep = "")
            cat(s3, "</txt>", enter, sep = "")
        }
        
        cat(s2, "</var>", enter, sep = "")
    }
    
    cat(s1, "</dataDscr>", enter, sep = "")
    cat(s0, "</codeBook>", enter, sep = "")
    
    if (!identical(file, "")) {
        sink()
    }
}
