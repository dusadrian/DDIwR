test_that("variable analysis preserves serial results across worker limits", {
    old <- options(DDIwR.variable_threads = 1L)
    on.exit(options(old))

    data <- list(
        numeric = rep(c(-3.5, 0, 1.25, 6, 99, NA_real_, NaN), 3000),
        integer = rep(c(1:6, NA_integer_), 3000),
        logical = rep(c(TRUE, FALSE, NA), 7000),
        text = rep(c("é", "a", "missing", NA_character_), 5250),
        numeric_text = rep(c("1", "2.5", "3", "4", "5", "6", NA), 3000),
        date = as.Date(c("2020-01-01", NA, "2020-01-03")),
        datetime = as.POSIXct(c("2020-01-01 12:34:56", NA), tz = "UTC"),
        factor = factor(c("a", "b", NA)),
        missing = c(NA_real_, NaN),
        empty = numeric()
    )
    attr(data$numeric, "labels") <- c("Low" = -3.5, "Missing" = 99)
    attr(data$numeric, "na_range") <- c(98, Inf)
    attr(data$text, "labels") <- c("Accent" = "é", "Missing" = "missing")
    attr(data$text, "na_values") <- "missing"

    variables <- lapply(data, function(column) {
        return(list(
            labels = attr(column, "labels", exact = TRUE),
            na_values = attr(column, "na_values", exact = TRUE),
            na_range = attr(column, "na_range", exact = TRUE),
            type = "num"
        ))
    })
    dates <- unname(vapply(data, inherits, logical(1), what = "Date"))
    expected_stats <- collectDataDscrStatsC(data, variables, dates)
    expected_metadata <- collectRMetadataC(data)
    expected_single <- collectDataDscrStatsC(data[1], variables[1], dates[1])

    expect_equal(expected_stats$sum_valid[1], 12000)
    expect_equal(expected_stats$sum_invalid[1], 9000)
    expect_equal(expected_stats$cat_freq[1:2], c(3000, 3000))
    expect_identical(expected_metadata$date$varFormat, "date")
    expect_identical(expected_metadata$datetime$varFormat, c("DATETIME", "%tc"))

    for (workers in c(0L, 2L, 4L, 12L, 256L)) {
        options(DDIwR.variable_threads = workers)

        expect_identical(collectDataDscrStatsC(data, variables, dates), expected_stats)
        expect_identical(collectRMetadataC(data), expected_metadata)
        expect_identical(
            collectDataDscrStatsC(data[1], variables[1], dates[1]),
            expected_single
        )
    }
})

test_that("invalid variable worker options fail before analysis", {
    old <- options(DDIwR.variable_threads = NULL)
    on.exit(options(old))

    data <- list(x = 1:6)
    variables <- list(list(type = "num"))

    for (value in list(NA_integer_, NaN, Inf, -1, 1.5, 257, "4", numeric())) {
        options(DDIwR.variable_threads = value)

        expect_error(collectDataDscrStatsC(data, variables, FALSE), "DDIwR.variable_threads", fixed = TRUE)
        expect_error(collectRMetadataC(data), "DDIwR.variable_threads", fixed = TRUE)
    }

    options(DDIwR.variable_threads = NULL)
    expect_silent(collectDataDscrStatsC(data, variables, FALSE))
    expect_silent(collectRMetadataC(data))
})

test_that("metadata-only collection is independent of observations", {
    short <- list(
        number = structure(1.25, label = "Value", labels = c(Low = 1),
            na_values = 99, na_range = c(98, 99), xmlang = "en", id = "value"),
        text = structure("a", label = "Text"),
        category = factor("a", levels = c("a", "b", "unused")),
        date = as.Date("2020-01-01")
    )
    long <- lapply(short, function(column) {
        result <- rep(column, 2000L)
        attributes(result) <- attributes(column)

        return(result)
    })
    empty <- lapply(short, function(column) {
        result <- column[FALSE]
        attributes(result) <- attributes(column)

        return(result)
    })
    long$number[1L] <- 123456.789
    long$text[1L] <- "a considerably longer value"
    metadata <- collectRMetadataC(short, FALSE)

    expect_identical(collectRMetadataC(long, FALSE), metadata)
    expect_identical(collectRMetadataC(empty, FALSE), metadata)
    expect_identical(metadata$category$labels, c(a = 1L, b = 2L, unused = 3L))
    expect_false(identical(
        collectRMetadataC(short)$text$varFormat,
        collectRMetadataC(long)$text$varFormat
    ))

    formats <- collectRMetadataC(long)
    metadata_from_formats <- lapply(formats, function(variable) {
        variable$varFormat <- NULL

        return(variable)
    })

    expect_identical(metadata_from_formats, metadata)
})

test_that("shared analysis preserves checkType classification", {
    data <- data.frame(
        many = seq_len(20),
        few = rep(c(1, 2, 2, NA_real_), 5),
        numeric_text = rep(c("1", "2.5", "3", NA_character_), 5),
        text = rep(c("a", "b", NA_character_, "a"), 5),
        category = factor(rep(c("a", "b", NA_character_, "a"), 5)),
        logical = rep(c(TRUE, FALSE, NA, FALSE), 5),
        all_missing = rep(c(NA_real_, NaN), 10),
        check.names = FALSE
    )
    attr(data$many, "labels") <- c(low = 1, high = 20)
    attr(data$few, "labels") <- c(one = 1, two = 2)
    attr(data$text, "labels") <- c(A = "a", B = "b")
    attr(data$numeric_text, "na_values") <- "3"

    variables <- collectRMetadata(data, infer_type = FALSE, include_formats = FALSE)
    dates <- rep(FALSE, ncol(data))
    analysis <- collectDataDscrStatsC(data, variables, dates, include_projection = TRUE)
    expected <- unname(vapply(seq_along(data), function(index) {
        variable <- variables[[index]]

        return(checkType(
            data[[index]],
            getElement(variable, "labels"),
            getElement(variable, "na_values"),
            getElement(variable, "na_range")
        ))
    }, character(1)))

    expect_identical(analysis$variable_type, expected)
})

test_that("shared classification matches randomized supported inputs", {
    set.seed(20260919)

    for (iteration in seq_len(150)) {
        kind <- sample(c("numeric", "integer", "character", "factor", "logical"), 1)
        size <- sample(0:40, 1)

        if (identical(kind, "numeric")) {
            column <- sample(c(-3:20, NA_real_, NaN), size, replace = TRUE)
        }
        else if (identical(kind, "integer")) {
            column <- sample(c(-3:20, NA_integer_), size, replace = TRUE)
        }
        else if (identical(kind, "character")) {
            column <- sample(c(as.character(-3:20), letters[1:4], NA_character_),
                size, replace = TRUE)
        }
        else if (identical(kind, "factor")) {
            column <- factor(sample(c(letters[1:5], NA_character_), size, replace = TRUE),
                levels = letters[1:5])
        }
        else {
            column <- sample(c(TRUE, FALSE, NA), size, replace = TRUE)
        }

        if (runif(1) < 0.65) {
            if (is.factor(column)) {
                pool <- seq_along(levels(column))
            }
            else if (is.character(column)) {
                pool <- c(as.character(-3:8), letters[1:3])
            }
            else {
                pool <- -3:8
            }

            labels <- sample(pool, sample(seq_len(min(6, length(pool))), 1))
            names(labels) <- paste0("Label ", seq_along(labels))
            attr(column, "labels") <- labels
        }

        if (runif(1) < 0.3) {
            attr(column, "na_values") <- if (is.character(column)) {
                sample(c("-3", "a"), 1)
            }
            else {
                sample(-3:3, 1)
            }
        }

        data <- structure(
            list(value = column),
            class = "data.frame",
            row.names = .set_row_names(length(column))
        )
        variables <- collectRMetadata(data, infer_type = FALSE, include_formats = FALSE)
        variable <- variables[[1]]
        analysis <- collectDataDscrStatsC(
            data,
            variables,
            FALSE,
            include_projection = TRUE
        )
        expected <- checkType(
            column,
            getElement(variable, "labels"),
            getElement(variable, "na_values"),
            getElement(variable, "na_range")
        )

        expect_identical(analysis$variable_type, expected,
            info = sprintf("random iteration %d (%s)", iteration, kind))
    }
})

test_that("shared analysis returns reusable weight facts", {
    data <- data.frame(
        positive = c(0.5, 1, 2, NA_real_),
        negative = c(-1, 0, 1, NA_real_),
        labelled = c(1, 2, 1, NA_real_),
        numeric_text = c("1", "2", NA_character_, "3"),
        empty = rep(NA_real_, 4),
        factor = factor(c("a", "b", NA_character_, "a"))
    )
    attr(data$labelled, "labels") <- c(one = 1, two = 2)
    attr(data$negative, "na_values") <- -1

    variables <- collectRMetadata(data, infer_type = FALSE, include_formats = FALSE)
    analysis <- collectDataDscrStatsC(
        data,
        variables,
        rep(FALSE, ncol(data)),
        include_projection = TRUE
    )

    expect_identical(
        analysis$weight_numeric_compatible,
        c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
    )
    expect_identical(
        analysis$weight_has_labels,
        c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
    )
    expect_identical(
        analysis$weight_has_observed,
        c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE)
    )
    expect_identical(
        analysis$weight_has_negative,
        c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
    )
})
