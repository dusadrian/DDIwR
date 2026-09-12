test_that("translation metadata can be created, edited and exported", {
    label <- makeElement("labl", content = "Age", attributes = c(
        xmlang = "en", isTranslated = "true", isTranslatable = "true",
        translationSourceLanguage = "ro", translationDate = "2026-04-15"
    ))
    label <- changeAttributes(
        c(translationSourceLanguage = "ro fr"), from = label, overwrite = FALSE
    )
    variable <- makeElement("var", attributes = c(name = "age"), children = list(label))
    book <- makeElement("codeBook", children = list(
        makeElement("stdyDscr", fill = TRUE),
        makeElement("dataDscr", children = list(variable))
    ))
    path <- tempfile(fileext = ".xml")
    on.exit(unlink(path))
    exportCodebook(book, path, monolang = FALSE)
    doc <- xml2::read_xml(path)
    node <- xml2::xml_find_first(doc, "//*[local-name()='labl']")
    expect_equal(xml2::xml_text(node), "Age")
    expect_equal(xml2::xml_attr(node, "isTranslated"), "true")
    expect_equal(xml2::xml_attr(node, "translationSourceLanguage"), "ro fr")
    expect_equal(xml2::xml_attr(node, "translationDate"), "2026-04-15")
    expect_equal(xml2::xml_find_chr(node, "string(@xml:lang)"), "en")

    # Translation attributes belong to textual types, not to every DDI element.
    for (name in c("codeBook", "var", "dataDscr", "algorithmVersion")) {
        expect_error(
            makeElement(name, attributes = c(isTranslated = "true")),
            "do not belong"
        )
    }
})

test_that("agent and grant metadata use the released attribute names", {
    provider <- makeElement("accsPlac", content = "Archive", attributes = c(
        affiliation = "University", abbr = "UA", agentIdentifier = "archive-1",
        typeOfAgentIdentifier = "local", isPersistentIdentifier = "false",
        agentType = "organization"
    ))
    expect_equal(attr(provider, "agentIdentifier"), "archive-1")
    expect_equal(attr(provider, "agentType"), "organization")

    for (name in c(
        "AuthEnty", "authorizingAgency", "contact", "custodian", "dataCollector",
        "depositr", "distrbtr", "evaluator", "fundAg", "origArch", "othId",
        "participant", "producer", "verResp"
    )) {
        agent <- makeElement(name, attributes = c(isPersistentIdentifier = "true"))
        expect_equal(attr(agent, "isPersistentIdentifier"), "true")
        expect_error(
            makeElement(name, attributes = c(isPersistantIdentifier = "true")),
            "do not belong"
        )
    }
    grant <- makeElement("grantNo", content = "G1", attributes = c(
        fundAgRefs = "agency1 agency2", URI = "https://example.org/program"
    ))
    expect_equal(attr(grant, "fundAgRefs"), "agency1 agency2")
    expect_equal(attr(grant, "URI"), "https://example.org/program")
})

test_that("local elements have usable help and textual metadata", {
    for (name in c(
        "digitalFingerprintValue", "algorithmSpecification", "algorithmVersion",
        "description", "outcome", "otherQualityStatement", "complianceDescription"
    )) {
        expect_output(showDetails(name), "[Ff]ingerprint|development activity|study")
    }
    for (name in c("description", "outcome", "otherQualityStatement", "complianceDescription")) {
        element <- makeElement(name, content = "Translated text", attributes = c(
            isTranslated = "true", translationSourceLanguage = "ro"
        ))
        expect_equal(attr(element, "isTranslated"), "true")
    }
})

test_that("formatting restrictions distinguish prohibited and inherited attributes", {
    for (name in c("div", "emph", "hi", "label", "p", "itm")) {
        expect_error(makeElement(name, attributes = c(type = "test")), "do not belong")
        element <- makeElement(name, attributes = c(n = "1", rend = "bold"))
        expect_equal(attr(element, "rend"), "bold")
    }
    heading <- makeElement("head", attributes = c(type = "section", n = "1"))
    expect_equal(attr(heading, "type"), "section")
    element <- makeElement("list", attributes = c(type = "bulleted"))
    expect_equal(attr(element, "type"), "bulleted")
})

test_that("minimal validation accepts unweighted variables and requires vocabulary usage", {
    variable <- makeElement("var", attributes = c(name = "age"), children = list(
        makeElement("sumStat", content = "30", attributes = c(type = "mean")),
        makeElement("catgry", children = list(
            makeElement("catValu", content = "1"),
            makeElement("catStat", content = "10", attributes = c(type = "freq"))
        ))
    ))
    expect_false(any(grepl("wgt_var", unlist(testValid(variable)))))

    vocabulary <- makeElement("controlledVocabUsed")
    expect_true(any(grepl("mandatory child usage", unlist(testValid(vocabulary)))))
    vocabulary <- addChildren(makeElement("usage"), to = vocabulary, overwrite = FALSE)
    expect_false(any(grepl("mandatory child usage", unlist(testValid(vocabulary)))))
})
