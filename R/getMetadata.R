`getMetadata` <- 
function(x, save = FALSE, OS = "Windows", ...) {
    
    # TODO: detect DDI version or ask the version through a dedicated argument
    # http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html
    
    
    error <- TRUE
    
    if (is.data.frame(x)) {
        i <- 1
        while (i <= ncol(x) & error) {
            attrx <- attributes(x[[i]])
            if (any(is.element(c("label", "labels", "na_value", "na_range"), names(attrx)))) {
                error <- FALSE
            }
            i <- i + 1
        }
        if (!error) {
            codeBook <- list()
            codeBook$dataDscr <- collectMetadata(x)
            return(invisible(codeBook))
        }
    }

    if (error) {
        cat("\n")
        stop("The input does not seem to contain any metadata.\n\n", call. = FALSE)
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

            codeBook <- list()

            xml <- getXML(file.path(tp$completePath, tp$files[ff]))
            
            
            # lapply(xml_find_all(xml, "/d1:codeBook/d1:dataDscr/d1:var"), function(x) {
            #     list(label = admisc::trimstr(xml_text(xml_find_first(x, "d1:labl"))))
            # })
            
            xmlns <- xml2::xml_ns(xml)
            # d1  <-> http://www.icpsr.umich.edu/DDI
            # xsi <-> http://www.w3.org/2001/XMLSchema-instance
            
            # <d>efault <n>ame <s>pace
            dns <- ifelse(is.element("d1", names(xmlns)), "d1:", "")
            
            ### Unfortunately this does not work because some variables don't always have labels
            ### and we'll end up having a vector of labels that is shorter than the number of variables
            # xpath <- sprintf("/%scodeBook/%sdataDscr/%svar/%slabl", dns, dns, dns, dns)
            # varlab <- cleanup(xml2::xml_text(xml2::xml_find_all(xml, xpath)))
            ###

            xpath <- sprintf("/%scodeBook/%sdataDscr/%svar", dns, dns, dns)
            vars <- xml2::xml_find_all(xml, xpath)
            varlab <- cleanup(xml2::xml_text(xml2::xml_find_first(vars, sprintf("%slabl", dns))))

            xpath <- sprintf("/%scodeBook/%sfileDscr/%snotes", dns, dns, dns)
            notes <- xml2::xml_text(xml2::xml_find_first(xml, xpath))

            codeBook$dataDscr <- lapply(varlab, function(x) list(label = x))
            
            xpath <- sprintf("/%scodeBook/%sdataDscr/%svar/@name", dns, dns, dns)
            names(codeBook$dataDscr) <- admisc::trimstr(xml2::xml_text(xml2::xml_find_all(xml, xpath)))
            
            for (i in seq(length(codeBook$dataDscr))) {
                # nms <- xml_name(xml_contents(xml_find_all(xml, sprintf("/d1:codeBook/d1:dataDscr/d1:var[%s]", i))))
                
                # xpath <- sprintf("/%scodeBook/%sdataDscr/%svar[%s]", dns, dns, dns, i)
                # vars_i <- xml2::xml_find_first(xml, xpath)

                measurement <- xml2::xml_attr(vars[i], "nature")
                na_values <- NULL
                na_range <- NULL
                xpath <- sprintf("%sinvalrng/%srange", dns, dns)
                na_range[1] <- admisc::asNumeric(xml2::xml_attr(xml2::xml_find_first(vars[i], xpath), "min"))
                na_range[2] <- admisc::asNumeric(xml2::xml_attr(xml2::xml_find_first(vars[i], xpath), "max"))
                if (all(is.na(na_range))) {
                    na_range <- NULL
                }
                else {
                    if (is.na(na_range[1])) na_range[1] <- -Inf
                    if (is.na(na_range[2])) na_range[2] <- Inf
                }
                
                xpath <- sprintf("%scatgry/%scatValu", dns, dns)
                values <- cleanup(xml2::xml_text(xml2::xml_find_all(vars[i], xpath)))

                xpath <- sprintf("%svarFormat", dns)
                type <- xml2::xml_attr(xml2::xml_find_first(vars[i], xpath), "type")
                
                if (length(values) > 0) {
                    
                    catgry <- xml2::xml_find_all(vars[i], sprintf("%scatgry", dns))
                    
                    na_values <- c(na_values, values[unlist(lapply(catgry, function(x) {
                        grepl("Y", xml2::xml_attr(x, "missing"))
                    }))])
                    
                    labl <- unlist(lapply(catgry, function(x) {
                        xml2::xml_text(xml2::xml_find_first(x, sprintf("%slabl", dns)))
                    }))
                    
                    values <- values[!is.na(labl)]
                    labl <- cleanup(labl[!is.na(labl)])
                    
                    if (admisc::possibleNumeric(values)) {
                        values <- admisc::asNumeric(values)
                    }
                    
                    codeBook$dataDscr[[i]][["labels"]] <- values
                    names(codeBook$dataDscr[[i]][["labels"]]) <- labl
                }
                
                
                if (length(na_values) > 0) {
                    if (admisc::possibleNumeric(na_values)) {
                        na_values <- admisc::asNumeric(na_values)
                    }
                    na_values <- sort(unique(na_values))

                    na_values <- na_values[na_values < na_range[1] | na_values > na_range[2]]

                    if (length(na_values) > 0) codeBook$dataDscr[[i]]$na_values <- na_values
                }

                if (!is.null(na_range)) {
                    codeBook$dataDscr[[i]]$na_range <- na_range
                }

                if (!is.na(measurement)) {
                    if (is.element(measurement, c("nominal", "ordinal"))) {
                        codeBook$dataDscr[[i]]$type <- "cat"
                    }
                    else if (is.element(measurement, c("interval", "ratio"))) {
                        codeBook$dataDscr[[i]]$type <- "num"
                    }
                    else if (!is.na(type)) {
                        codeBook$dataDscr[[i]]$type <- type
                    }

                    codeBook$dataDscr[[i]]$measurement <- measurement
                }
                else {
                    if (!is.na(type)) {
                        codeBook$dataDscr[[i]]$type <- "num" # default

                        if (type == "character") {
                            codeBook$dataDscr[[i]]$type <- "char"
                        }
                        else if (length(values) > 0) {
                            if (length(setdiff(values, na_values)) > 0) {
                                codeBook$dataDscr[[i]]$type <- "cat"
                            }
                        }
                    }
                }

                if (identical(type, "character")) {
                    xpath <- sprintf("%stxt", dns)
                    txt <- cleanup(xml2::xml_text(xml2::xml_find_first(vars[i], xpath)))
                    if (!is.na(txt)) {
                        codeBook$dataDscr[[i]]$txt <- txt
                    }
                }
            }
        }
        else {
            if (tp$fileext[ff] == "SAV") {
                data <- haven::read_spss(file.path(tp$completePath, tp$files[ff]), user_na = TRUE)
            }
            else if (tp$fileext[ff] == "POR") {
                data <- haven::read_por(file.path(tp$completePath, tp$files[ff]), user_na = TRUE)
            }
            else if (tp$fileext[ff] == "DTA") {
                data <- haven::read_dta(file.path(tp$completePath, tp$files[ff]))
            }
            else if (tp$fileext[ff] == "RDS") {
                data <- readr::read_rds(file.path(tp$completePath, tp$files[ff]))
            }
            # not sure about SAS, as far as I understand the metadata is not embedded in the datafile
            # sometimes it might sit into a separate, catalog file or something (need to investigate)
            # else if (tp$fileext[ff] == "SAS7BDAT") {
            #     data <- haven::read_sas(file.path(tp$completePath, tp$files[ff]))
            # }
            
            codeBook <- list()
            codeBook$dataDscr <- collectMetadata(x)
        }
        
        
        if (save) {
            
            indent <- 4
            if (is.element("indent", names(other.args))) {
                indent <- other.args$indent
            }
            
            writeRlist(codeBook$dataDscr, OS = OS, indent = indent, dirpath = tp$completePath, filename = tp$filenames[ff])
            
        }
    }
    
    
    if (singlefile) {
        if (!is.na(notes)) {
            if (grepl("# start data #", notes)) {
                spss <- ifelse(is.element("spss", names(other.args)), other.args$spss, TRUE)
                notes <- unlist(strsplit(notes, split = "\\n"))
                data <- notes[seq(which(grepl("# start data #", notes)) + 1, which(grepl("# end data #", notes)) - 1)]
                #----------------
                # don't remember why read_csv was not working
                # data <- suppressMessages(readr::read_csv(paste(data, collapse = "\n")))
                # and instead:
                data <- read.csv(text = paste(data, collapse = "\n"), as.is = TRUE)
                data <- make_labelled(data, codeBook$dataDscr, spss = spss)
                #----------------
                embed <- TRUE
            }
        }

        if (embed & !is.null(data)) {
            codeBook$fileDscr$datafile <- data
        }
        
        return(invisible(codeBook))
    }
}
