`getMetadata` <- 
function(x, OS = "windows", saveFile = FALSE, ...) {
    
    # TODO: detect DDI version or ask the version through a dedicated argument
    
    `possibleNumeric` <- function(x) {
        # as.character converts everything (especially factors)
        return(!any(is.na(suppressWarnings(as.numeric(na.omit(as.character(x)))))) & !all(is.na(x)))
    }
    
    `asNumeric` <- function(x) {
        return(suppressWarnings(as.numeric(as.character(x))))
    }

    `trimstr` <- function(x, what = " ", side = "both") {
        what <- ifelse(what == " ", "[[:space:]]", ifelse(what == "*", "\\*", what))
        pattern <- switch(side,
        both = paste("^", what, "+|", what, "+$", sep = ""),
        left = paste("^", what, "+", sep = ""),
        right = paste(what, "+$", sep = "")
        )
        gsub(pattern, "", x)
    }
    
    `cleanup` <- function(x, cdata = TRUE) {
        x <- gsub("^[[:space:]]+|[[:space:]]+$", "", x)
        x <- gsub("\"", "'", x)
        for (l in letters) {
            x <- gsub(sprintf("\\\\+%s", l), sprintf("/%s", l), x)
        }
        x <- gsub("\\\\", "/", x)
        if (cdata) {
            x <- gsub("<\\!\\[CDATA\\[|\\]\\]>", "", x)
        }
        return(x)
    }

    `extract` <- function(data) {
        lapply(data, function(x) {
            toreturn <- list(label = attr(x, "label"))
            labels <- attr(x, "labels")
            if (!is.null(labels)) toreturn$values <- labels
            
            missing <- attr(x, "na_values")
            if (!is.null(missing)) toreturn$missing <- sort(missing)
            
            missrange <- attr(x, "na_range")
            if (!is.null(missrange)) {
                toreturn$missrange <- missrange
            }
            
            toreturn$type <- DDIwR::checkType(x, labels, possibleNumeric(x))

            return(toreturn)
        })
    }
    
    if (is.data.frame(x)) {
        if (is.element("tbl_df", class(x))) {
            return(invisible(extract(x)))
        }
        else {
            cat("\n")
            stop("The argument 'x' should be a tibble.\n\n", call. = FALSE)
        }
    }


    other.args <- list(...)
    
    enter <- getEnter(OS)
    
    fromsetupfile <- FALSE
    if (is.element("fromsetupfile", names(other.args))) {
        fromsetupfile <- other.args$fromsetupfile
    }
    
    embed <- FALSE
    if (is.element("embed", names(other.args))) {
        embed <- other.args$embed
    }
    
    tp <- DDIwR::treatPath(x, type = "*")
    
    singlefile <- length(tp$files) == 1
    notes <- NA
    
    if (!fromsetupfile & !singlefile) {
        cat("Processing:\n")
    }

    data <- NULL
    
    for (ff in seq(length(tp$files))) {
        if (!fromsetupfile & !singlefile) {
            cat(tp$files[ff], "\n")
        }
        
        if (tp$fileext[ff] == "XML") {

            xml <- getXML(file.path(tp$completePath, tp$files[ff]))
            
            
            # lapply(xml_find_all(xml, "/d1:codeBook/d1:dataDscr/d1:var"), function(x) {
            #     list(label = trimstr(xml_text(xml_find_first(x, "d1:labl"))))
            # })
            
            xmlns <- xml2::xml_ns(xml)
            # d1  <-> http://www.icpsr.umich.edu/DDI
            # xsi <-> http://www.w3.org/2001/XMLSchema-instance
            
            # <d>efault <n>ame <s>pace
            dns <- ifelse(is.element("d1", names(xmlns)), "d1:", "")
            
            xpath <- sprintf("/%scodeBook/%sdataDscr/%svar/%slabl", dns, dns, dns, dns)
            varlab <- cleanup(xml2::xml_text(xml2::xml_find_all(xml, xpath)))
            
            xpath <- sprintf("/%scodeBook/%sfileDscr/%snotes", dns, dns, dns)
            notes <- xml2::xml_text(xml2::xml_find_first(xml, xpath))
            
            dataDscr <- lapply(varlab, function(x) list(label = x))
            
            xpath <- sprintf("/%scodeBook/%sdataDscr/%svar/@name", dns, dns, dns)
            names(dataDscr) <- trimstr(xml2::xml_text(xml2::xml_find_all(xml, xpath)))
            
            for (i in seq(length(dataDscr))) {
                # nms <- xml_name(xml_contents(xml_find_all(xml, sprintf("/d1:codeBook/d1:dataDscr/d1:var[%s]", i))))
                
                xpath <- sprintf("/%scodeBook/%sdataDscr/%svar[%s]", dns, dns, dns, i)
                vars <- xml2::xml_find_first(xml, xpath)

                measurement <- xml2::xml_attr(vars, "nature")
                missng <- NULL
                missrange <- NULL
                xpath <- sprintf("%sinvalrng/%srange", dns, dns)
                missrange[1] <- asNumeric(xml2::xml_attr(xml2::xml_find_first(vars, xpath), "min"))
                missrange[2] <- asNumeric(xml2::xml_attr(xml2::xml_find_first(vars, xpath), "max"))
                if (all(is.na(missrange))) {
                    missrange <- NULL
                }
                else {
                    if (is.na(missrange[1])) missrange[1] <- -Inf
                    if (is.na(missrange[2])) missrange[2] <- Inf
                }
                
                xpath <- sprintf("%scatgry/%scatValu", dns, dns)
                values <- cleanup(xml2::xml_text(xml2::xml_find_all(vars, xpath)))

                xpath <- sprintf("%svarFormat", dns)
                type <- xml2::xml_attr(xml2::xml_find_first(vars, xpath), "type")
                
                if (length(values) > 0) {
                    
                    catgry <- xml2::xml_find_all(vars, sprintf("%scatgry", dns))
                    
                    missng <- c(missng, values[unlist(lapply(catgry, function(x) {
                        grepl("Y", xml2::xml_attr(x, "missing"))
                    }))])
                    
                    labl <- unlist(lapply(catgry, function(x) {
                        xml2::xml_text(xml2::xml_find_first(x, sprintf("%slabl", dns)))
                    }))
                    
                    values <- values[!is.na(labl)]
                    labl <- cleanup(labl[!is.na(labl)])
                    
                    if (possibleNumeric(values)) {
                        values <- asNumeric(values)
                    }
                    
                    dataDscr[[i]]$values <- values
                    names(dataDscr[[i]]$values) <- labl
                }
                
                
                if (length(missng) > 0) {
                    if (possibleNumeric(missng)) {
                        missng <- asNumeric(missng)
                    }
                    missng <- sort(unique(missng))

                    missng <- missng[missng < missrange[1] | missng > missrange[2]]

                    if (length(missng) > 0) dataDscr[[i]]$missing <- missng
                }

                if (!is.null(missrange)) {
                    dataDscr[[i]]$missrange <- missrange
                }

                if (!is.na(measurement)) {
                    if (is.element(measurement, c("nominal", "ordinal"))) {
                        dataDscr[[i]]$type <- "cat"
                    }
                    else if (is.element(measurement, c("interval", "ratio"))) {
                        dataDscr[[i]]$type <- "num"
                    }
                    else if (!is.na(type)) {
                        dataDscr[[i]]$type <- type
                    }

                    dataDscr[[i]]$measurement <- measurement
                }
                else {
                    if (!is.na(type)) {
                        dataDscr[[i]]$type <- "num" # default

                        if (type == "character") {
                            dataDscr[[i]]$type <- "char"
                        }
                        else if (length(values) > 0) {
                            if (length(setdiff(values, missng)) > 0) {
                                dataDscr[[i]]$type <- "cat"
                            }
                        }
                    }
                }

                if (identical(type, "character")) {
                    xpath <- sprintf("%stxt", dns)
                    txt <- cleanup(xml_text(xml_find_first(vars, xpath)))
                    if (!is.na(txt)) {
                        dataDscr[[i]]$txt <- txt
                    }
                }
            }
        }
        else if (tp$fileext == "SAV") {
            data <- haven::read_spss(file.path(tp$completePath, tp$files[ff]), user_na = TRUE)
            dataDscr <- extract(data)
        }
        
        if (saveFile) {
            currentdir <- getwd()
            setwd(tp$completePath)
            
            indent <- 4
            if (is.element("indent", names(other.args))) {
                indent <- other.args$indent
            }
            
            sink(paste(tp$filenames[ff], "R", sep = "."))
            cat("codeBook <- list(dataDscr = list(", enter)
            writeRlist(dataDscr, OS = OS, indent = indent)
            cat("))", enter)
            sink()
            setwd(currentdir)
        }
    }
    
    if (singlefile) {
        codeBook <- list()
        
        if (!is.na(notes)) {
            if (grepl("# start data #", notes)) {
                notes <- unlist(strsplit(notes, split = "\\n"))
                data <- notes[seq(which(grepl("# start data #", notes)) + 1, which(grepl("# end data #", notes)) - 1)]
                data <- suppressMessages(readr::read_csv(paste(data, collapse = "\n")))
                codeBook$fileDscr$datafile <- convertibble(data, dataDscr)
                # codeBook$data <- readr::read_csv(paste(data, collapse = "\n"))
            }
        }

        codeBook$dataDscr <- dataDscr
        if (embed & !is.null(data)) {
            codeBook$fileDscr$datafile <- data
        }
        
        return(invisible(codeBook))
    }
}

