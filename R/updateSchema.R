#' @name updateSchema
#'
#' @title Updates the internal DDI Codebook schema object.
#'
#' @description Rebuilds the internal schema object, from a (newer) XML Schema codebook.xsd file.
#'
#' @param xsd A path to the Codebook XML Schema file.
#' @param return Return an R object representing the schema instead of updating the internal one.
#'
#' @details Releasing a new stable version of the DDI Codebook takes about 10 years. There are
#' numerous elements and attributes that have to work together, and most importantly the Codebook
#' has to obey the backward compatibility rule. Until a new version is released, the Codebook
#' schema is incrementally modified by the DDI Alliance, in their GitHub repository.
#'
#' This function is intended to update the internal DDI Codebook schema object, parsing the
#' latest version of the Codebook XML Schema file.
#'
#' Unless the codebook.xsd file is provided by the user, the function will attempt to read it
#' from the DDI Alliance GitHub repository, located at:
#' \url{https://github.com/ddialliance/ddi-c_2}
#'
#' @author Adrian Dusa
#'
# #' @export
`updateSchema` <- function(xsd = NULL, return = FALSE) {

    # aa <- readLines("Lucru/_R/DDIwR/R/DDI_Codebook_2.6.R")
    # bb <- tools::showNonASCII(aa)

    if (is.null(xsd)) {
        xsd <- "https://raw.githubusercontent.com/ddialliance/ddi-c_2/master/schemas/codebook.xsd"
    }

    tc <- admisc::tryCatchWEM(schema <- xml2::read_xml(xsd))

    if (!is.null(tc$error)) {
        # message not error because this could be called at package load,
        # if persistent caching is set
        message(paste("Could not read the schema file:", tc$error))
        invisible(return(NULL))
    }

    # Parse the .xsd file
    xsd_doc <- xml2::read_xml("~/Documents/GitHub/ddi-c_26/schemas/codebook.xsd")

    xsdlist <- xml2::as_list(xsd_doc)[[1]]
    elements <- xsdlist[names(xsdlist) == "element"]
    complex <- xsdlist[names(xsdlist) == "complexType"]


    element_names <- unname(sapply(elements, function(x) {
        attr(x, "name")
    }))

    complex_names <- unname(sapply(complex, function(x) {
        attr(x, "name")
    }))

    types <- unname(sapply(elements, function(x) {
        attr(x, "type")
    }))

    # not formally part of the DDI Codebook schema, but useful for validation
    # purposes, akin to the CESSDA Metadata Validator
    recommended <- list(
        elements = c(
            "abstract", "anlyUnit", "AuthEnty", "collDate", "concept",
            "fileName", "IDNo", "nation", "restrctn", "topcClas", "universe"
        ),
        attributes = list(
            date = c("collDate"),
            vocab = c("concept", "keyword", "topcClas"),
            vocabURI = c("concept", "topcClas"),
            xmlang = c("AuthEnty", "fileName", "holdings", "IDNo"),
            abbr = c("nation")
        )
    )

    getpieces <- function(complex_name, what = "element") {
        comp <- complex[[which(complex_names == complex_name)]]
        nms <- names(comp)

        extension <- NULL
        pieces <- NULL

        if (is.element("complexContent", nms)) {
            comp <- comp$complexContent
            nms <- names(comp)

            if (is.element("extension", nms)) {
                comp <- comp$extension
                extension <- attr(comp, "base")
            } else if (is.element("restriction", nms)) {
                comp <- comp$restriction
            }
            nms <- names(comp)

            pieces <- comp[is.element(nms, what)]

            if (!is.null(extension)) {
                pieces <- c(getpieces(extension, what = what), pieces)
            }

            if (what == "element") {
                if (is.element("choice", nms)) {
                    comp <- comp$choice
                    nms <- names(comp)
                }

                if (is.element("sequence", nms)) {
                    comp <- comp$sequence
                    nms <- names(comp)
                }

                # choice within a sequence
                if (is.element("choice", nms)) {
                    comp <- comp$choice
                    nms <- names(comp)
                }

                pieces <- c(pieces, comp[is.element(nms, "element")])
            }
        }

        return(pieces)
    }

    # this is manually set by inspecting the XML Schema for element that contain
    # the word "deprecated" in their documentation
    deprecated <- list(
        elements = c("ExtLink", "Link"),
        attributes = list(
            type = c(
                "dataAppr", "instrumentDevelopment", "collectorTraining", "dataKind", "codingInstructions",
                "dataProcessing", "otherMat", "resInstru", "setAvail", "stdyClas", "developmentActivity",
                "exPostEvaluation"
            ),
            unit = c("anlyUnit"),
            freq = c("frequenc"),
            method = c("timeMeth"),
            nCube = c("varGrp")
        )
    )

    elnames <- function(x) {
        children <- sapply(x, function(e) {
            elname <- attr(e, "ref")
            if (is.null(elname)) {
                elname <- attr(e, "name")
            }

            names(elname) <- paste(
                ifelse(is.null(attr(e, "minOccurs")), 1, 0),
                ifelse(is.null(attr(e, "maxOccurs")), 1, "n"),
                sep = "-"
            )
            return(elname)
        })
        names(children) <- gsub("element\\.", "", names(children))
        return(children)
    }

    meta <- lapply(element_names, function(x) {

        el <- elements[[which(element_names == x)]]
        ldiv <- length(el$annotation$documentation$div)

        title <- c()
        if (ldiv > 0) {
            title <- el$annotation$documentation$div$h1[[1]]
        }

        documentation <- c()
        if (ldiv > 1) {
            documentation <- unname(unlist(el$annotation$documentation$div[[2]]$div))
            if (x == "abstract") {
                documentation <- gsub(
                    "\"source\" and \"date\"",
                    "\"date\" and (the global) \"source\"",
                    documentation
                )
                documentation <- gsub(
                    "maps to Dublin Core Creator element",
                    "maps to Dublin Core element \"Creator\"",
                    documentation
                )
            }
            if (is.element(x, c("collDate", "nation", "sumDscr"))) {
                documentation <- gsub(
                    "Maps to Dublin Core Coverage element",
                    "Maps to Dublin Core element \"Coverage\"",
                    documentation
                )
            }
            if (x == "IDNo") {
                documentation <- gsub(
                    "Dublin Core Identifier element",
                    "Dublin Core element \"Identifier\"",
                    documentation
                )
            }
            if (x == "othId") {
                documentation <- gsub(
                    "Dublin Core Contributor element",
                    "Dublin Core element \"Contributor\"",
                    documentation
                )
            }
            if (x == "producer") {
                documentation <- gsub(
                    "Dublin Core Publisher element",
                    "Dublin Core element \"Publisher\"",
                    documentation
                )
            }
            if (x == "qstn") {
                documentation <- gsub(
                    "The attribute \"ID\"",
                    "The global attribute \"ID\"",
                    documentation
                )
            }
        }

        examples <- c()
        if (ldiv > 2) {
            examples <- unname(sapply(
                el$annotation$documentation$div[[3]]$div,
                function(x) {
                    gsub(
                        ">(\\s+)<",
                        "><",
                        gsub(
                            intToUtf8(157),
                            "",
                            gsub(
                                intToUtf8(252),
                                "\\u00fc",
                                admisc::trimstr(x[[1]])
                            )
                        )
                    )
                }
            ))

            examples <- examples[examples != ""]
        }

        children <- c()
        attributes <- c()

        cpos <- which(complex_names == types[[which(element_names == x)]])

        if (length(cpos) > 0) {
            comp <- complex[[cpos]]
            nms <- names(comp)

            extension <- NULL

            if (
                any(is.element(c("simpleContent", "complexContent"), nms)) &
                length(nms == 1)
            ) {
                comp <- comp[[1]]
                nms <- names(comp)

                if (is.element("extension", nms)) {
                    comp <- comp$extension
                    extension <- attr(comp, "base")
                } else if (is.element("restriction", nms)) {
                    comp <- comp$restriction
                }
                nms <- names(comp)
            }

            ats <- comp[is.element(nms, "attribute")]
            els <- NULL
            if (!is.null(extension)) {
                ats <- c(getpieces(extension, "attribute"), ats)
                els <- getpieces(extension, "element")
            }

            if (is.element("choice", nms)) {
                comp <- comp$choice
                nms <- names(comp)
            }

            if (is.element("sequence", nms)) {
                comp <- comp$sequence
                nms <- names(comp)
            }

            els <- c(els, comp[is.element(nms, "element")])

            # choice within a sequence
            choice <- list()

            if (is.element("choice", nms)) {
                pos <- which(nms == "choice") - 1
                comp <- comp$choice
                nms <- names(comp)

                if (any(nms == "element")) {
                    choice <- comp[is.element(nms, "element")]
                    for (i in seq(length(choice))) {
                        if (is.null(attr(choice[[i]], "minOccurs"))) {
                            attr(choice[[i]], "minOccurs") <- attr(comp, "minOccurs")
                        }
                        if (is.null(attr(choice[[i]], "maxOccurs"))) {
                            attr(choice[[i]], "maxOccurs") <- attr(comp, "maxOccurs")
                        }
                    }

                }
            }

            if (length(els) > 0) {
                els <- elnames(els)

                children <- vector("list", length(els))
                for (i in seq(length(els))) {
                    children[[i]] <- els[i]
                }
            }

            if (length(choice) > 0) {
                choice <- elnames(choice)

                if (length(choice) > 1) {
                    choice <- list(choice = choice)
                }

                children <- append(children, choice, after = pos)
            }

            if (length(ats) > 0) {
                attributes <- lapply(ats, function(a) {
                    atname <- gsub("-", "_", attr(a, "name")) # wgt-var de ex.
                    optional <- !identical(attr(a, "use"), "required")
                    recommended <- is.element(x, recommended$attributes[[atname]])

                    type <- attr(a, "type")
                    default <- attr(a, "default")
                    values <- c()
                    deprecated <- is.element(x, deprecated$attributes[[atname]])
                    description <- "" # TODO
                    if (deprecated) {
                        description <- paste(
                            description,
                            "DEPRECATED.",
                            sep = ifelse (description == "", "", " ")
                        )
                    }

                    if (is.element("simpleType", names(a))) {
                        a <- a$simpleType$restriction
                        type <- attr(a, "base")
                        values <- unname(sapply(
                            a[names(a) == "enumeration"],
                            function(x) {
                                return(attr(x, "value"))
                            }
                        ))
                    }

                    if (type == "xs:boolean") {
                        values <- c("true", "false")
                    }

                    return(list(
                        # type = gsub("xs:", "", type),
                        type = type,
                        description = description,
                        values = values,
                        default = default,
                        optional = optional,
                        recommended = recommended,
                        deprecated = deprecated
                    ))
                })

                names(attributes) <- sapply(ats, function(x) {
                    return(attr(x, "name"))
                })
            }
        }

        return(list(
            type = attr(el, "type"),
            title = title,
            # double quote from Word
            description = gsub(
                paste(intToUtf8(8220), intToUtf8(8221), sep = "|"),
                "\"",
                admisc::trimstr(documentation)
            ),
            examples = gsub(
                paste(intToUtf8(8220), intToUtf8(8221), sep = "|"),
                "\"",
                admisc::trimstr(examples)
            ),
            children = children,
            attributes = attributes,
            parents = c()
        ))
    })
    names(meta) <- element_names

    els <- unlist(lapply(meta, function(x) {
        x <- unlist(x$children)
        return(x)
    }))

    nms <- gsub(".*\\.", "", names(els)[!duplicated(els)])
    els <- unname(els[!duplicated(els)])

    # return(list(els, nms))

    for (e in seq_along(els)) {
        oprep <- unlist(strsplit(nms[e], split = "-"))
        if (!is.element(els[e], element_names)) {
            extratypes <- c(
                digitalFingerprintValue = "xs:string",
                algorithmSpecification = "xs:string",
                algorithmVersion = "xs:string",
                description = "simpleTextType",
                outcome = "simpleTextType",
                otherQualityStatement = "simpleTextType",
                complianceDescription = "simpleTextType"
            )
            meta[[els[e]]] <- list(
                type = extratypes[names(extratypes) == els[e]], # TODO type pentru elementele extra, cum le iau?
                title = "",
                description = "",
                examples = c(),
                children = c(),
                attributes = list(),
                parents = c()
            )
            element_names <- c(element_names, els[e])
        }

        meta[[els[e]]]$optional <- oprep[1] == "0"
        meta[[els[e]]]$repeatable <- oprep[2] == "n"
        meta[[els[e]]]$recommended <- is.element(els[e], recommended$elements)
    }

    for (element in element_names) {
        meta[[element]]$deprecated <- is.element(element, deprecated$elements)
        meta[[element]]$children <- lapply(
            meta[[element]]$children,
            function(x) {
                names(x) <- NULL
                return(x)
            }
        )
        parents <- sapply(meta, function(x) {
            is.element(element, unlist(x$children))
        })

        if (any(parents)) {
            meta[[element]]$parents <- names(parents[parents])
        }
    }

    if (return) {
        return(meta)
    }


    # DDIC <- get("DDIC", envir = cacheEnv)
    # Code to modify the DDIC object
    # then
    assign("DDIC", meta, envir = cacheEnv)
}



sinklist <- function(DDIC) {

    on.exit(suppressWarnings(sink()))
    nms <- names(DDIC)
    sink("DDICtest.R")
    cat("DDIC <- list(\n")
    for (i in seq(length(DDIC))) {

        # if (length(DDIC[[i]]$parents) > 0) {
            cat("    ")
            cat(nms[i])
            cat(" = list(\n")
            cat(sprintf(
                "        type = \"%s\",\n",
                DDIC[[i]]$type
            ))
            cat(sprintf(
                "        optional = %s,\n",
                ifelse(isTRUE(DDIC[[i]]$optional), "TRUE", "FALSE")
            ))
            cat(sprintf(
                "        repeatable = %s,\n",
                ifelse(isTRUE(DDIC[[i]]$repeatable), "TRUE", "FALSE")
            ))
            cat(sprintf(
                "        recommended = %s,\n",
                ifelse(isTRUE(DDIC[[i]]$recommended), "TRUE", "FALSE")
            ))
            cat(sprintf(
                "        deprecated = %s,\n",
                ifelse(isTRUE(DDIC[[i]]$deprecated), "TRUE", "FALSE")
            ))

            attributes <- DDIC[[i]]$attributes
            cat("        attributes = list(")
            if (length(attributes) > 0) {
                cat("\n")
                nmsa <- names(attributes)
                for (j in seq(length(nmsa))) {
                    n <- gsub("-", "_", nmsa[j])

                    cat(paste(
                        "           ",
                        ifelse(grepl("\\:", n), paste0("'", n, "'"), n),
                        "= list(\n"
                    ))

                    cat("                ")
                    cat(paste0("type = \"", attributes[[n]]$type, "\",\n"))

                    attr_description <- gsub("\"", "\\\\\"", attributes[[n]]$description)
                    cat(paste0(
                        "                ",
                        "description = ",
                        ifelse(length(attr_description) > 1, "c(\"", "\""),
                        paste(attr_description, collapse = "\", \""),
                        ifelse(length(attr_description) > 1, "\")", "\""),
                        ",\n"
                    ))

                    values <- attributes[[n]]$values
                    cat(paste0(
                        "                ",
                        "values = "
                    ))
                    if (length(values) == 0) {
                        cat("c(),\n")
                    }
                    else {
                        cat(paste0(
                            ifelse(length(values) > 1, "c(\"", "\""),
                            paste(values, collapse = "\", \""),
                            ifelse(length(values) > 1, "\")", "\""),
                            ",\n"
                        ))
                    }

                    default <- attributes[[n]]$default
                    cat("                ")
                    if (length(default) == 0) {
                        cat("default = c(),\n")
                    }
                    else {
                        cat(paste0("default = \"", default, "\",\n"))
                    }

                    cat(paste0(
                        "                optional = ",
                        ifelse(isTRUE(attributes[[n]]$optional), "TRUE", "FALSE"),
                        ",\n"
                    ))

                    cat(paste0(
                        "                recommended = ",
                        ifelse(isTRUE(attributes[[n]]$recommended), "TRUE", "FALSE"),
                        ",\n"
                    ))

                    cat(paste0(
                        "                deprecated = ",
                        ifelse(isTRUE(attributes[[n]]$deprecated), "TRUE", "FALSE"),
                        "\n"
                    ))

                    cat("            ")
                    cat(ifelse(j < length(nmsa), "),\n", ")\n"))
                }
                cat("        ),")
            }
            else {
                cat("),")
            }

            cat("\n        parents = c(")
            if (length(DDIC[[i]]$parents) > 0) cat("\"")
            cat(paste(DDIC[[i]]$parents, collapse = "\", \""))
            if (length(DDIC[[i]]$parents) > 0) cat("\"")
            cat("),")

            children <- unname(unlist(DDIC[[i]]$children))
            cat("\n        children = ")

            # cat(ifelse(length(children) > 1, "c(\"", "\""))
            # cat(paste(children, collapse = "\", \""))
            # cat(ifelse(length(children) > 1, "\"),", "\","))

            if (length(children) == 0) {
                cat("list(),")
            }
            else {
                cat("list(")
                nmsc <- names(children)

                for (cd in seq(length(children))) {
                    # if (cd == 1) {
                    #     cat("\n")
                    # }

                    many <- length(children[[cd]]) > 1
                    choice <- FALSE
                    tc <- admisc::tryCatchWEM({
                        choice <- nmsc[cd] == "choice"
                    })

                    if (!is.null(tc$error)) {
                        sink()
                        print(nms[i])
                        stop()
                    }

                    if (length(choice) && choice) {
                        # cat("            choice = ")
                        cat("choice = ")
                        cat(paste0(
                            ifelse(many, "c(\"", "\""),
                            paste(children[[cd]], collapse = "\", \""),
                            ifelse(many, "\")", "\"")
                        ))
                    }
                    else {
                        # cat("            ")
                        cat(paste0("\"", children[[cd]], "\""))
                    }

                    if (cd < length(children)) {
                        cat(", ")
                    }
                    # cat("\n")
                }
                # cat("        ),")
                cat("),")
            }

            cat("\n        title = ")
            if (length(DDIC[[i]]$title) == 0) {
                cat("c(),")
            }
            else {
                cat(paste0("\"", DDIC[[i]]$title, "\","))
            }

            cat("\n        description = ")

            ld <- length(DDIC[[i]]$description)
            if (length(DDIC[[i]]$description) == 0) {
                cat("c(),")
            }
            else {
                cat(ifelse(ld > 1, "c(\n", ""))
                cat(ifelse(ld > 1, "            \"", "\""))
                cat(paste(gsub("\"", "\\\\\"", DDIC[[i]]$description), collapse =  "\",\n            \""))
                cat(ifelse(ld > 1, "\"\n        ),", "\","))
            }


            cat("\n        examples = ")
            le <- length(DDIC[[i]]$example)

            if (le == 0) {
                cat("c()")
            }
            else {
                cat(ifelse(le > 1, "c(\n", ""))
                cat(ifelse(le > 1, "            \"", "\""))
                cat(paste(gsub("\"", "\\\\\"", DDIC[[i]]$example), collapse =  "\",\n            \""))
                cat(ifelse(le > 1, "\"\n        )", "\""))
            }

            cat("\n    )")
            cat(ifelse(i < length(DDIC), ",\n", "\n"))

        # }
    }

    cat(")\n")
    sink()


}






# atts <- sapply(DDIC, function(x) {
#     if (length(x$attributes)) {
#         desc <- sapply(x$attributes, function(y) {
#             return(all(y$description != ""))
#         })
#         return(any(desc))
#     }
#     return(FALSE)
# })

# atts <- atts[atts]
# nms <- names(atts)

# sink("DDI attributes.R")
# for (i in seq(length(atts))) {
#     cat("\n----------------\n")
#     element <- nms[i]
#     elatts <- DDIC[[element]]$attributes
#     atnms <- names(elatts)

#     cat("Element: ", element, "\n\n")

#     cat("<xhtml:div class=\"description\">")
#     if (length(DDIC[[element]]$description) > 1) {
#         cat("\n")
#         for (d in seq(length(DDIC[[element]]$description))) {
#             cat("   <xhtml:p>")
#             cat(DDIC[[element]]$description[d])
#             cat("</xhtml:p>\n")
#         }
#         cat("   ")
#     } else {
#         cat(DDIC[[element]]$description)
#     }
#     cat("</xhtml:div>\n\n")
#     cat("------\n")
#     cat("\nAttributes:\n")

#     for (j in seq(length(elatts))) {
#         cat(paste(
#             "<xs:attribute name=\"",
#             atnms[j],
#             "\" type=\"xs:",
#             elatts[[j]]$type,
#             "\">\n",
#             sep = ""
#         ))
#         cat("   <xs:annotation>\n")
#         cat("      <xs:documentation>\n")
#         cat("         <xhtml:div class=\"description\">")
#         if (length(elatts[[j]]$description) > 1) {
#             cat("\n")
#             for (d in seq(length(elatts[[j]]$description))) {
#                 cat("            <xhtml:p>")
#                 cat(elatts[[j]]$description[d])
#                 cat("</xhtml:p>\n")
#             }
#             cat("         ")
#         } else {
#             cat(elatts[[j]]$description)
#         }
#         cat("</xhtml:div>\n")
#         cat("      </xs:documentation>\n")
#         cat("   </xs:annotation>\n")
#         cat("</xs:attribute>\n")
#     }
#     cat("\n\n")
# }
# sink()





# sapply(DDIC, function(x) {
#     if (!length(x$attributes)) {
#         return(0)
#     }

#     att <- sapply(x$attributes, function(y) {
#         return(length(y$values))
#     })

#     return(max(att))
# })


