
dataDscr <- makeElement("dataDscr")

#-------

variable <- makeElement("var", attributes = c(name = "ID"))
addChildren(
    makeElement("labl", content = "Questionnaire ID"),
    to = variable
)
addChildren(variable, to = dataDscr)

#-------

variable <- makeElement("var", attributes = c(name = "V1"))
addChildren(
    makeElement("labl", content = "Label for the first variable"),
    to = variable
)

metadata <- list(
    labels = c("No" = 0, "Yes" = 1, "Not applicable" = -97, "Not answered" = -99),
    na_values = c(-99, -97)
)
variable$.extra <- append(variable$.extra, metadata)
addChildren(makeCategories(metadata), to = variable)
addChildren(variable, to = dataDscr)

#-------

variable <- makeElement("var", attributes = c(name = "V2"))
addChildren(
    makeElement("labl", content = "Label for the second variable"),
    to = variable
)

metadata <- list(
    labels = c("Very little" = 1, "Little" = 2, "So, so" = 3, "Much" = 4, "Very much" = 5, "Don't know" = -98),
    na_values = -98
)
variable$.extra <- append(variable$.extra, metadata)
addChildren(makeCategories(metadata), to = variable)
addChildren(variable, to = dataDscr)

#-------

variable <- makeElement("var", attributes = c(name = "V3"))
addChildren(
    makeElement("labl", content = "Label for the third variable"),
    to = variable
)

metadata <- list(
    labels = c("First answer" = "A", "Second answer" = "B", "Don't know" = -98),
    na_values = -98
)
variable$.extra <- append(variable$.extra, metadata)
addChildren(makeCategories(metadata), to = variable)
addChildren(variable, to = dataDscr)

#-------

variable <- makeElement("var", attributes = c(name = "V4"))
addChildren(
    makeElement("labl", content = "Number of children"),
    to = variable
)

metadata <- list(
    labels = c("Don't know" = -98, "Not answered" = -99),
    na_values = c(-99, -98)
)
variable$.extra <- append(variable$.extra, metadata)
addChildren(makeCategories(metadata), to = variable)
addChildren(variable, to = dataDscr)


#-------

variables <- list(
    ID = list(
        label = "Questionnaire ID",
        type = "num",
        measurement = "interval"
    ),
    V1 = list(
        label = "Label for the first variable",
        labels = c(
            "No"             =  0,
            "Yes"            =  1,
            "Not applicable" = -97,
            "Not answered"   = -99),
        na_values = c(-99, -97),
        type = "cat",
        measurement = "nominal"
    ),
    V2 = list(
        label = "Label for the second variable",
        labels = c(
            "Very little"    =  1,
            "Little"         =  2,
            "So, so"         =  3,
            "Much"           =  4,
            "Very much"      =  5,
            "Don't know"     = -98),
        na_values = c(-98),
        type = "cat",
        measurement = "ordinal"
    ),
    V3 = list(
        label = "Label for the third variable",
        labels = c(
            "First answer"   = "A",
            "Second answer"  = "B",
            "Don't know"     = -98),
        na_values = c(-98),
        type = "cat",
        measurement = "nominal"
    ),
    V4 = list(
        label = "Number of children",
        labels = c(
            "Don't know"     = -98,
            "Not answered"   = -99),
        na_values = c(-99, -98),
        type = "numcat",
        measurement = "ratio"
    )#,
    # V5 = list(
    #     type = "char"
    # )
)

testdf <- data.frame(
    ID = 1:100,
    V1 = sample(c(0, 1, -97, -99), 100, replace = TRUE),
    V2 = sample(c(1:5, -98), 100, replace = TRUE),
    V3 = sample(c("A", "B", -98), 100, replace = TRUE),
    V4 = sample(c(1:10, -98, -99), 100, replace = TRUE)
    # V5 = sample(LETTERS, 100, replace = TRUE)
)


test_that("makeLabelled() works", {
    expect_true(is.data.frame(makeLabelled(testdf, dataDscr)))
    expect_true(is.data.frame(makeLabelled(testdf, dataDscr, declared = FALSE)))
})
