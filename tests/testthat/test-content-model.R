test_that("occurrence limits follow the immediate parent", {
    title <- makeElement("titl", content = "Title")

    expect_error(makeElement("titlStmt", children = list(title, title)), "at most 1")

    table <- makeElement("table", children = list(title, title))

    expect_equal(sum(names(table) == "titl"), 2)
    expect_match(paste(testValid(makeElement("titlStmt"))$mandatory, collapse = " "), "mandatory child titl")
    expect_length(testValid(makeElement("titlStmt", children = list(title)))$mandatory, 0)

    citation <- makeElement("citation", children = list(makeElement("titlStmt", children = list(title))))

    study <- makeElement("stdyDscr", children = list(citation, citation))

    expect_length(testValid(study)$mandatory, 0)
    expect_error(makeElement("docDscr", children = list(citation, citation)), "at most 1")
    expect_match(paste(testValid(makeElement("stdyDscr"))$mandatory, collapse = " "), "mandatory child citation")

    source <- makeElement("sources")

    expect_equal(sum(names(makeElement("sources", children = list(source, source))) == "sources"), 2)
    expect_error(makeElement("dataColl", children = list(source, source)), "at most 1")

    for (name in c("caseQnty", "varQnty", "logRecL")) {
        child <- makeElement(name, content = "1")
        expect_error(makeElement("recDimnsn", children = list(child, child)), "at most 1")
        expect_equal(sum(names(makeElement("dimensns", children = list(child, child))) == name), 2)
    }

    desc <- makeElement("drvdesc", content = "Derivation")

    expect_error(makeElement("fileCommand", children = list(desc, desc)), "at most 1")
    expect_equal(sum(names(makeElement("derivation", children = list(desc, desc))) == "drvdesc"), 2)
    expect_match(paste(testValid(makeElement("fileCommand"))$mandatory, collapse = " "), "mandatory child drvcmd")
    expect_length(testValid(makeElement("derivation"))$mandatory, 0)
    expect_output(showDescription("titl"), "depends on parent")
    expect_output(showRelations("titlStmt"), "titl \\[1..1\\]")
})

test_that("repeated choices accept mixed alternatives and retain their order", {
    question <- makeElement("qstn", children = list(
        makeElement("qstnLit", content = "First"),
        makeElement("preQTxt", content = "Context"),
        makeElement("qstnLit", content = "Second")
    ))
    expect_identical(names(question), c("qstnLit", "preQTxt", "qstnLit", ".extra"))
    expect_length(testValid(question)$mandatory, 0)
    expect_equal(question[[3]]$.extra$index, 2)
    conceptual <- makeElement("weight", children = list(
        makeElement("concept", content = "Sampling"), makeElement("txt", content = "Details")
    ))

    expect_length(testValid(conceptual)$mandatory, 0)

    usage <- makeElement("usage", children = list(makeElement("selector", content = "//var")))

    expect_error(addChildren(makeElement("specificElements"), usage, overwrite = FALSE), "choice")

    for (name in c("valrng", "invalrng")) {
        expect_match(paste(testValid(makeElement(name))$mandatory, collapse = " "), "choice")

        value <- makeElement(name, children = list(makeElement("range", attributes = c(min = "1", max = "5"))))
        expect_length(testValid(value)$mandatory, 0)
    }
})

test_that("validation visits every repeated and recursively nested instance", {
    title <- makeElement("titl", content = "Title")
    citation <- makeElement("citation", children = list(makeElement("titlStmt", children = list(title))))
    book <- makeElement("stdyDscr", children = list(citation, makeElement("citation")))

    expect_match(paste(testValid(book)$mandatory, collapse = " "), "citation\\[2\\].*titlStmt")
    nested <- makeElement("otherMat", children = list(makeElement("otherMat")))

    expect_s3_class(testValid(nested), "validation")

    # Direct list edits cannot bypass full validation of occurrence limits.
    invalid <- makeElement("titlStmt", children = list(title))
    invalid <- append(invalid, list(titl = title))

    expect_match(paste(testValid(invalid)$mandatory, collapse = " "), "at most 1")
})

test_that("schema previews resolve restrictions, inheritance and local declarations", {
    before <- get("DDIC", cacheEnv)
    globals <- get("DDIC_global_attributes", cacheEnv)
    schema <- test_path("fixtures", "content-model.xsd")

    result <- updateSchema(schema, return = TRUE)

    expect_identical(get("DDIC", cacheEnv), before)
    expect_identical(get("DDIC_global_attributes", cacheEnv), globals)
    expect_setequal(names(result), c("root", "a", "b", "detail", "pairs"))

    expect_identical(result$detail$type, "xs:string")
    expect_setequal(names(result$root$attributes), c("wgt_var", "mode"))
    expect_match(result$root$attributes$wgt_var$description, "weight variable")
    expect_identical(result$root$attributes$mode$values, c("brief", "full"))
    expect_identical(result$root$attributes$mode$default, "brief")

    expect_true(ddiModelMatches(result$root$contentModel, c("b", "a", "detail")))
    expect_false(ddiModelMatches(result$root$contentModel, "a"))
    expect_true(ddiModelMatches(result$root$contentModel, "a", complete = FALSE))
    expect_false(ddiModelMatches(result$root$contentModel, rep("a", 4), complete = FALSE))

    output <- tempfile(fileext = ".R")
    on.exit(unlink(output), add = TRUE)

    sinklist(result, output)

    written <- new.env(parent = baseenv())
    sys.source(output, written)
    attr(result, "global_attributes") <- NULL

    expect_identical(written$DDIC, result)
})

test_that("content model matching agrees with XSD for bounded choices", {
    schema <- test_path("fixtures", "content-model.xsd")
    model <- updateSchema(schema, return = TRUE)$root$contentModel
    xsd <- xml2::read_xml(schema)

    cases <- list(character(), "a", c("a", "b"), c("b", "a", "b"),
        rep("a", 4), c("a", "b", "detail"), c("a", "detail", "b"),
        c("a", "b", rep("detail", 2)), c("a", "b", rep("detail", 3)))

    for (children in cases) {
        xml <- paste0("<root>", paste0("<", children, "/>", collapse = ""), "</root>")
        if (!length(children)) xml <- "<root/>"
        valid <- isTRUE(xml2::xml_validate(xml2::read_xml(xml), xsd))
        expect_identical(ddiModelMatches(model, children), valid, info = xml)
    }
})

test_that("schema refresh preserves curation and changes cache only after extraction", {
    original <- get("DDIC", cacheEnv)
    globals <- get("DDIC_global_attributes", cacheEnv)

    on.exit({
        assign("DDIC", original, cacheEnv)
        assign("DDIC_global_attributes", globals, cacheEnv)
    })

    schema <- tempfile(fileext = ".xsd")
    on.exit(unlink(schema), add = TRUE)

    lines <- readLines(test_path("fixtures", "content-model.xsd"))
    writeLines(sub('name="root"', 'name="concept"', lines, fixed = TRUE), schema)

    proposed <- updateSchema(schema, return = TRUE)

    for (field in c("title", "description", "examples", "recommended", "deprecated")) {
        expect_identical(proposed$concept[[field]], original$concept[[field]])
    }

    expect_identical(get("DDIC", cacheEnv), original)
    expect_identical(proposed$concept$type, "rootType")

    updateSchema(schema)

    expect_identical(get("DDIC", cacheEnv)$concept$contentModel, proposed$concept$contentModel)

    # A failed extraction must not partially change either cache object.
    current <- get("DDIC", cacheEnv)
    writeLines(sub('name="b"', 'name="a"', lines, fixed = TRUE), schema)

    expect_error(updateSchema(schema), "Repeated local element names")
    expect_identical(get("DDIC", cacheEnv), current)
    expect_identical(get("DDIC_global_attributes", cacheEnv), globals)

    writeLines("not XML", schema)

    expect_message(expect_null(updateSchema(schema)), "Could not read")
    expect_identical(get("DDIC", cacheEnv), current)
})

test_that("nested repeated sequences preserve order and agree with XSD", {
    path <- test_path("fixtures", "content-model.xsd")
    model <- updateSchema(path, return = TRUE)$pairs$contentModel
    schema <- xml2::read_xml(path)

    cases <- list(character(), "a", c("a", "b"), c("b", "a"),
        c("a", "a", "b", "b"), rep(c("a", "b"), 2), rep(c("a", "b"), 3),
        rep(c("a", "b"), 4))

    for (children in cases) {
        body <- if (length(children)) paste0("<", children, "/>", collapse = "") else ""
        xml <- paste0("<pairs>", body, "</pairs>")
        valid <- isTRUE(xml2::xml_validate(xml2::read_xml(xml), schema))
        expect_identical(ddiModelMatches(model, children), valid, info = xml)
        expect_identical(children[ddiModelOrder(model, children)], children)
    }

    expect_true(ddiModelMatches(model, c("a", "a"), complete = FALSE))
    expect_false(ddiModelMatches(model, rep("a", 4), complete = FALSE))
})
