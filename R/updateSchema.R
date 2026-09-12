#' @name updateSchema
#' @title Update the internal DDI Codebook schema object.
#' @description Extract structural facts from a Codebook XSD while preserving
#' the existing R naming conventions and curated descriptive metadata.
#' @param xsd Path or URL to codebook.xsd. Defaults to the pinned 2.6 release.
#' @param return Return the proposed list without changing the in-memory cache.
#' @details The extractor follows type inheritance, restrictions, local elements
#' and attribute groups. Each element receives a parent-specific contentModel
#' containing sequence/choice particles and numeric min/max occurrence bounds.
#' PHRASE, FORM, imported markup groups and wildcards remain outside the
#' simplified child model. Existing descriptions, examples, recommendations and
#' deprecation flags are preserved; new records use schema documentation.
#'
#' Inspect updateSchema(xsd, return = TRUE) before applying a different schema.
#' Structural facts are refreshed, including removed/prohibited attributes.
#' Legacy element-wide optional/repeatable flags are retained for existing
#' records; validation uses contentModel. No source files are overwritten.
#' @author Adrian Dusa
updateSchema <- function(xsd = NULL, return = FALSE) {
    if (is.null(xsd)) {
        xsd <- paste0(
            "https://raw.githubusercontent.com/ddialliance/ddi-c_2/",
            "94006d85b995c4013a07cd4e50a66df51aead33e/schemas/codebook.xsd"
        )
    }
    schema <- tryCatch(xml2::read_xml(xsd), error = function(e) {
        message("Could not read the schema file: ", conditionMessage(e))
        NULL
    })
    if (is.null(schema)) {
        return(invisible(NULL))
    }
    ns <- c(xs = "http://www.w3.org/2001/XMLSchema", h = "http://www.w3.org/1999/xhtml")
    nodes <- function(node, xpath) xml2::xml_find_all(node, xpath, ns)
    at <- function(node, name, default = NULL) {
        value <- xml2::xml_attr(node, name)
        if (is.na(value)) {
            default
        } else {
            value
        }
    }
    index <- function(xpath) {
        result <- nodes(schema, xpath)
        setNames(as.list(result), xml2::xml_attr(result, "name"))
    }
    types <- index("/xs:schema/xs:complexType")
    simple <- index("/xs:schema/xs:simpleType")
    groups <- index("/xs:schema/xs:attributeGroup")
    declaredAttributes <- index("/xs:schema/xs:attribute")
    strip <- function(x) sub("^.*:", "", x)
    rname <- function(x) {
        if (x == "xml:lang") {
            "xmlang"
        } else {
            gsub("-", "_", x, fixed = TRUE)
        }
    }
    emptyModel <- function() list(kind = "sequence", min = 1, max = 1, particles = list())
    occurs <- function(node, name) {
        value <- at(node, name, "1")
        if (value == "unbounded" && name == "maxOccurs") {
            return(Inf)
        }
        number <- suppressWarnings(as.numeric(value))
        if (is.na(number) || number < 0 || number != floor(number)) {
            stop("Invalid ", name, ": ", value)
        }
        number
    }
    particle <- function(node) {
        kind <- xml2::xml_name(node)
        # These content groups were deliberately not expanded in the R list.
        if (is.element(kind, c("group", "any"))) {
            return(emptyModel())
        }
        if (!is.element(kind, c("sequence", "choice", "element"))) {
            stop("Unsupported content particle: ", kind)
        }
        result <- list(kind = kind, min = occurs(node, "minOccurs"), max = occurs(node, "maxOccurs"))
        if (result$min > result$max) {
            stop("minOccurs exceeds maxOccurs.")
        }
        if (kind == "element") {
            result$name <- at(node, "ref", at(node, "name"))
            if (is.null(result$name)) {
                stop("Element particle has no name.")
            }
        } else {
            result$particles <- lapply(nodes(node, "xs:sequence|xs:choice|xs:element|xs:group|xs:any|xs:all"), particle)
        }
        result
    }
    groupAttributes <- function(name, seen = character()) {
        if (is.element(name, seen) || is.null(groups[[name]])) {
            stop("Unresolved/cyclic attribute group: ", name)
        }
        node <- groups[[name]]
        inherited <- unlist(lapply(nodes(node, "xs:attributeGroup"), function(g) {
            groupAttributes(at(g, "ref"), c(seen, name))
        }), recursive = FALSE)
        c(inherited, as.list(nodes(node, "xs:attribute")))
    }
    resolved <- new.env(parent = emptyenv())
    resolveType <- function(name, seen = character()) {
        if (is.null(name) || startsWith(name, "xs:") || is.element(name, names(simple))) {
            return(list(model = emptyModel(), attributes = list()))
        }
        if (is.element(name, seen) || is.null(types[[name]])) {
            stop("Unresolved/cyclic complex type: ", name)
        }
        if (exists(name, resolved, inherits = FALSE)) {
            return(get(name, resolved))
        }
        node <- types[[name]]
        derivation <- nodes(node, "xs:complexContent/xs:extension|xs:complexContent/xs:restriction|xs:simpleContent/xs:extension|xs:simpleContent/xs:restriction")
        base <- list(model = emptyModel(), attributes = list())
        extension <- FALSE

        if (length(derivation)) {
            node <- derivation[[1]]
            base <- resolveType(at(node, "base"), c(seen, name))
            extension <- xml2::xml_name(node) == "extension"
        }

        own <- lapply(nodes(node, "xs:sequence|xs:choice|xs:group|xs:all"), particle)
        model <- if (length(own) == 1) own[[1]] else {
            value <- emptyModel()
            value$particles <- own
            value
        }
        if (extension && length(ddiModelNames(base$model))) {
            value <- emptyModel()
            value$particles <- c(list(base$model), own)
            model <- value
        }
        attributes <- base$attributes
        inherited <- unlist(lapply(nodes(node, "xs:attributeGroup"), function(g) {
            groupAttributes(at(g, "ref"))
        }), recursive = FALSE)
        for (a in c(inherited, as.list(nodes(node, "xs:attribute")))) {
            key <- at(a, "name", at(a, "ref"))
            if (is.null(key)) {
                stop("Attribute has no name.")
            }
            if (identical(at(a, "use"), "prohibited")) {
                attributes[[key]] <- NULL
            } else {
                attributes[[key]] <- a
            }
        }
        result <- list(model = model, attributes = attributes)
        assign(name, result, resolved)
        result
    }
    text <- function(node, xpath) {
        values <- xml2::xml_text(nodes(node, xpath))
        unique(trimws(gsub("[[:space:]]+", " ", values)))
    }
    attributeRecord <- function(node, old = NULL) {
        ref <- at(node, "ref")
        definition <- node
        if (!is.null(ref) && ref != "xml:lang") {
            definition <- declaredAttributes[[ref]]
            if (is.null(definition)) {
                stop("Unresolved attribute: ", ref)
            }
        }
        restrictions <- nodes(definition, "xs:simpleType/xs:restriction")
        type <- at(definition, "type", if (identical(ref, "xml:lang")) "xs:language" else NULL)
        if (length(restrictions)) {
            type <- at(restrictions[[1]], "base")
        }
        if (is.null(type)) {
            stop("Attribute has no supported type.")
        }
        if (length(restrictions)) {
            values <- xml2::xml_attr(nodes(restrictions[[1]], "xs:enumeration"), "value")
        } else {
            values <- character()
        }
        if (type == "xs:boolean") {
            values <- c("true", "false")
        }
        description <- text(definition, "xs:annotation/xs:documentation")
        if (!length(description)) {
            description <- ""
        }
        result <- list(
            type = strip(type), description = description,
            values = if (length(values)) values else NULL,
            default = at(node, "default", at(definition, "default")),
            optional = !identical(at(node, "use", at(definition, "use")), "required"),
            recommended = FALSE, deprecated = any(grepl("deprecated", description, ignore.case = TRUE))
        )
        if (!is.null(old)) {
            for (field in c("description", "recommended", "deprecated")) {
                if (is.element(field, names(old))) {
                    result[field] <- old[field]
                } else {
                    result[[field]] <- NULL
                }
            }
            # Keep representation-only type aliases when the XSD type agrees.
            equivalent <- identical(strip(old$type), result$type) ||
                (identical(old$type, "dateSimple") && result$type == "dateSimpleType") ||
                (identical(strip(old$type), "string") && result$type == "NMTOKEN" &&
                 identical(old$values, result$values))
            if (equivalent) {
                result$type <- old$type
            }
        }
        result
    }
    old <- get("DDIC", envir = cacheEnv)
    globals <- get("DDIC_global_attributes", envir = cacheEnv)
    # Global attributes remain factored out; preserve their editorial metadata.
    if (!is.null(groups[["GLOBALS"]])) {
        globalNodes <- groupAttributes("GLOBALS")
        nextGlobals <- list()
        for (a in globalNodes) {
            key <- at(a, "name", at(a, "ref"))
            if (key == "xml-lang" || identical(at(a, "use"), "prohibited")) {
                next
            }
            key <- rname(key)
            nextGlobals[[key]] <- attributeRecord(a, globals[[key]])
        }
    } else {
        nextGlobals <- globals
    }

    declarations <- nodes(schema, "//xs:element[@name]")
    declarationNames <- xml2::xml_attr(declarations, "name")
    if (anyDuplicated(declarationNames)) {
        stop("Repeated local element names need a context-specific representation; cache unchanged.")
    }
    declarations <- setNames(as.list(declarations), declarationNames)
    # Keep the curated order, appending any newly declared elements.
    namesInOrder <- c(intersect(names(old), declarationNames), setdiff(declarationNames, names(old)))
    meta <- setNames(vector("list", length(namesInOrder)), namesInOrder)
    for (name in namesInOrder) {
        el <- declarations[[name]]
        type <- at(el, "type")
        if (is.null(type)) {
            stop("Unsupported anonymous element type: ", name)
        }
        definition <- resolveType(type)
        previous <- old[[name]]
        record <- if (!is.null(previous)) previous else list(
            type = type, optional = TRUE, repeatable = FALSE,
            recommended = FALSE, deprecated = FALSE, attributes = list(),
            parents = NULL, children = list(),
            title = text(el, "xs:annotation/xs:documentation//h:h1"),
            description = text(el, "xs:annotation/xs:documentation//h:div[@class='description']"),
            examples = NULL
        )
        if (is.null(previous)) {
            record$examples <- trimws(xml2::xml_text(nodes(el, "xs:annotation/xs:documentation//h:samp")))
            record$examples <- gsub(">[[:space:]]+<", "><", record$examples)
            if (!length(record$title)) {
                record$title <- name
            }
            if (!length(record$description)) {
                record$description <- ""
            }
        }
        record$type <- type
        record$contentModel <- definition$model
        children <- ddiModelNames(definition$model)
        if (!identical(unname(unlist(record$children)), children)) {
            record$children <- as.list(children)
        }
        attributes <- list()
        for (key in names(definition$attributes)) {
            if (key == "xml-lang") {
                next
            }
            rkey <- rname(key)
            # Keep existing element-specific overrides for globals, e.g. codeBook.
            if (is.element(rkey, names(nextGlobals)) && is.null(previous$attributes[[rkey]])) {
                next
            }
            attributes[[rkey]] <- attributeRecord(definition$attributes[[key]], previous$attributes[[rkey]])
        }
        if (name == "codeBook") {
            synthetic <- intersect(c("xmlns", "xmlns:xsd", "xmlns:xsi", "xsi:schemaLocation"), names(previous$attributes))
            attributes <- c(previous$attributes[synthetic], attributes)
        }
        # Preserve the existing attribute order for reviewable subsequent diffs.
        order <- c(intersect(names(previous$attributes), names(attributes)), setdiff(names(attributes), names(previous$attributes)))
        record$attributes <- attributes[order]
        meta[[name]] <- record
    }
    for (name in names(meta)) {
        parents <- names(meta)[vapply(meta, function(x) {
            is.element(name, ddiModelNames(x$contentModel))
        }, logical(1))]
        previousParents <- old[[name]]$parents
        parents <- c(intersect(previousParents, parents), setdiff(parents, previousParents))
        if (length(parents)) {
            meta[[name]]["parents"] <- list(parents)
        } else {
            meta[[name]]["parents"] <- list(NULL)
        }
        if (is.null(old[[name]]) && length(parents)) {
            bounds <- lapply(parents, function(p) ddiModelBounds(meta[[p]]$contentModel, name))
            meta[[name]]$optional <- any(vapply(bounds, function(b) b[1] == 0, logical(1)))
            meta[[name]]$repeatable <- any(vapply(bounds, function(b) b[2] > 1, logical(1)))
        }
        unknown <- setdiff(ddiModelNames(meta[[name]]$contentModel), names(meta))
        if (length(unknown)) {
            stop("Unresolved child declarations: ", paste(unknown, collapse = ", "))
        }
    }
    if (return) {
        # Carry proposed global changes with the preview so it is fully reviewable.
        attr(meta, "global_attributes") <- nextGlobals
        return(meta)
    }
    assign("DDIC", meta, envir = cacheEnv)
    assign("DDIC_global_attributes", nextGlobals, envir = cacheEnv)
    invisible(meta)
}

# Export a reviewable R expression. dput handles quoting, nested choices,
# content models and empty values without changing names or losing fields.
sinklist <- function(DDIC, file = "DDICtest.R") {
    globals <- attr(DDIC, "global_attributes")
    attr(DDIC, "global_attributes") <- NULL
    connection <- file(file, open = "wt")
    on.exit(close(connection))
    writeLines("DDIC <-", connection)
    dput(DDIC, connection)
    if (!is.null(globals)) {
        writeLines("DDIC_global_attributes <-", connection)
        dput(globals, connection)
    }
    invisible(file)
}
