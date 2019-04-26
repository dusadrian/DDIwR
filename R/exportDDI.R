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
    catText(0, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
    catText(0, "<codeBook", enter,
        "version=\"2.5\"", enter,
        "xmlns=\"ddi:codebook:2_5\"", enter,
        "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"", enter, 
        "xsi:schemaLocation=\"http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd\">")
    catText(1, "<docDscr>")
    catText(2, "<citation>")
    catText(3, "<titlStmt>")
    catText(4, "<titl>Generic title</titl>")
    catText(3, "</titlStmt>")
    catText(3, "<prodStmt>")
    catText(4, "<prodDate date=\"", prodDate, "\">", prodDate, "</prodDate>")
    catText(4, "<software version=\"", version,"\">R package DDIwR</software>")
    catText(3, "</prodStmt>")
    catText(2, "</citation>")
    catText(1, "</docDscr>")

    catText(1, "<stdyDscr>")
    catText(2, "<citation>")
    catText(3, "<titlStmt>")
    catText(4, "<titl>Generic title</titl>")
    catText(3, "</titlStmt>")
    catText(2, "</citation>")
    catText(1, "</stdyDscr>")
    
    catText(1, "<fileDscr ID=\"", uuid[length(uuid)], "\">")

    if (!is.null(data)) {
        if (!is.data.frame(data)) {
            cat("\n")
            stop("The 'datafile' component should be a data frame.\n\n", call. = FALSE)
        }
        else if (!identical(toupper(names(data)), toupper(names(obj)))) {
            cat("\n")
            stop("Variables in the data do not match the variables in the data description.\n\n", call. = FALSE)
        }

        catText(2, "<fileTxt>")
        catText(3, "<dimensns>")
        catText(4, "<caseQnty>", nrow(data), "</caseQnty>")
        catText(4, "<varQnty>", ncol(data), "</varQnty>")
        catText(3, "</dimensns>")
        catText(2, "</fileTxt>")

        if (embed) {
            catText(2, "<notes>")
            catText(0, "<![CDATA[# start data #\n",
                readr::format_csv(data, na = ""),
                "# end data #\n]]>")
            catText(2, "</notes>")
        }
    }

    catText(1, "</fileDscr>")

    catText(1, "<dataDscr>")

    for (i in seq(length(obj))) {
        
        dcml <- ""
        if (!is.null(data)) {
            dcml <- c(" dcml=\"",
                      ifelse(admisc::possibleNumeric(data[[i]]), getDecimals(admisc::asNumeric(na.omit(data[[i]]))),  0),
                      "\"")
        }
        
        nature <- ""
        if(any(grepl("measurement", names(obj[[i]])))) {
            nature <- c(" nature=\"", obj[[i]]$measurement, "\"")
        }
                         
        
        catText(2, "<var ID=\"", uuid[i], "\" name=\"", varnames[i], "\" files=\"", uuid[length(uuid)], "\"", dcml, nature, ">")
        
        if (!is.null(obj[[i]]$label)) {
            if (!is.na(obj[[i]]$label)) {
                catText(3, "<labl>", obj[[i]]$label, "</labl>")
            }
        }
        

        missing <- NULL
        if (is.element("missing", names(obj[[i]]))) {
            missing <- obj[[i]]$missing
        }

        missrange <- NULL
        if (is.element("missrange", names(obj[[i]]))) {
            missrange <- obj[[i]]$missrange
        }

        
        if (length(missrange) > 0) {
            catText(3, "<invalrng>")

            if (any(is.element(missrange, c(-Inf, Inf)))) {
                if (identical(missrange[1], -Inf)) {
                    catText(4, sprintf("<range UNITS=\"INT\" max=\"%s\"/>", missrange[2]))
                }
                else {
                    catText(4, sprintf("<range UNITS=\"INT\" min=\"%s\"/>", missrange[1]))
                }
            }
            else {
                catText(4, sprintf("<range UNITS=\"INT\" min=\"%s\" max=\"%s\"/>", missrange[1], missrange[2]))
            }
                
            catText(3, "</invalrng>")
        }

        lbls <- obj[[i]]$values
        type <- obj[[i]]$type
        
        if (!is.null(data)) {
            vals <- data[[names(obj)[i]]]
            if (admisc::possibleNumeric(vals)) {
                vals <- admisc::asNumeric(vals)

                if (!is.null(lbls)) {
                    ismiss <- is.element(lbls, missing)
                    if (length(missrange) > 0) {
                        ismiss <- ismiss | (lbls >= missrange[1] & lbls <= missrange[2])
                    }
                    vals[is.element(vals, lbls[ismiss])] <- NA
                }

                printnum <- length(setdiff(vals[!is.na(vals)], lbls)) > 4
                if (!is.null(type)) printnum <- printnum | grepl("num", type)
                
                if (printnum) { # numeric variable
                    catText(3, "<sumStat type=\"min\">", format(min(vals, na.rm = TRUE), scientific = FALSE), "</sumStat>")
                    catText(3, "<sumStat type=\"max\">", format(max(vals, na.rm = TRUE), scientific = FALSE), "</sumStat>")
                    catText(3, "<sumStat type=\"mean\">", format(mean(vals, na.rm = TRUE), scientific = FALSE), "</sumStat>")
                    catText(3, "<sumStat type=\"medn\">", format(median(vals, na.rm = TRUE), scientific = FALSE), "</sumStat>")
                    catText(3, "<sumStat type=\"stdev\">", format(sd(vals, na.rm = TRUE), scientific = FALSE), "</sumStat>")
                }
            }
        }
        
        if (any(grepl("values", names(obj[[i]])))) {

            tbl <- table(data[[names(obj)[i]]])
            
            for (v in seq(length(lbls))) {
                
                ismiss <- is.element(lbls[v], missing)
                if (length(missrange) > 0) {
                    ismiss <- ismiss | (lbls[v] >= missrange[1] & lbls[v] <= missrange[2])
                }

                catText(3, "<catgry", ifelse(ismiss, " missing=\"Y\"", ""), ">")
                catText(4, "<catValu>",  lbls[v],  "</catValu>")
                catText(4, "<labl>",  names(lbls)[v],  "</labl>")
                
                if (!is.null(data)) {
                    freq <- tbl[match(lbls[v], names(tbl))]
                    catText(4, "<catStat type=\"freq\">", 
                            ifelse(is.na(freq), 0, format(freq, scientific = FALSE)), 
                            "</catStat>")
                }

                catText(3, "</catgry>")
            }
        }

        if (any(grepl("type", names(obj[[i]])))) {
            catText(3, "<varFormat type=\"", ifelse(grepl("char", obj[[i]]$type), "character", "numeric"), "\"/>")
        }
        
        if (any(grepl("txt", names(obj[[i]])))) {
            catText(3, "<txt>")
            catText(0, "<![CDATA[", obj[[i]]$txt, "]]>")
            catText(3, "</txt>")
        }
        
        catText(2, "</var>")
    }
    
    catText(1, "</dataDscr>")
    catText(0, "</codeBook>")
    
    if (!identical(file, "")) {
        sink()
    }
}
