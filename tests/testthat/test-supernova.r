# Helper functions --------------------------------------------------------

sse <- function(fit) sum(fit$residuals^2)
ssr <- function(fit) sum((fit$fitted.values - mean(fit$model[[1]]))^2)
sst <- function(fit) var(fit$model[, 1], na.rm = TRUE) * (nrow(fit$model) - 1)
formula_to_string <- function(formula) Reduce(paste, deparse(formula))

# Retrieves partial row data for multiple regression from the model cache
get_partials <- function(model, type) {
  ivs <- frm_terms(model)
  if (length(ivs) < 2) {
    return(NULL)
  }

  anova_tbl <- if (type == 1) {
    anova(model)
  } else {
    suppressMessages(car::Anova) # ignore masking lme4 methods
    suppressWarnings(car::Anova(model, type = type))
  }

  anova_tbl <- anova_tbl[, c("Sum Sq", "Df", "F value", "Pr(>F)")]
  anova_tbl <- setNames(anova_tbl, c("SS", "df", "F", "p"))
  anova_tbl$term <- rownames(anova_tbl)

  anova_tbl[which(anova_tbl$term %in% ivs), ]
}

get_data_with_missing <- function() {
  df_missing <- mtcars
  df_missing[1, ]$hp <- NA_real_
  df_missing[2:3, ]$disp <- NA_real_
  df_missing
}


# Custom expectations -----------------------------------------------------

#' Test a column of a `data.frame`
#'
#' @param object The `data.frame` the column-to-be-tested is in.
#' @param col_name The name of the column in the data.frame as a string.
#' @param expected A vector of the expected values for the column.
#' @param ... Additional arguments used to control specifics of comparison.
#'
#' @return Invisibly returns the original `data.frame` object for piping.
expect_col_equal <- function(object, col_name, expected, ...) {
  column <- object[[col_name]]
  act <- testthat::quasi_label(rlang::enquo(column), sprintf("obj$%s", col_name))
  exp <- testthat::quasi_label(rlang::enquo(expected))
  comp <- testthat::compare(act$val, exp$val, ...)
  expect(comp$equal, sprintf(
    "%s not equal to %s.\n%s\nActual: %s\nExpected: %s",
    act$lab, exp$lab, comp$message, data.frame(act$val), data.frame(exp$val)
  ))
  invisible(object)
}

#' Test that a model is correctly `supernova`'d
#'
#' For all models *except* null (empty) models, test the `supernova` table
#' against values computed with either base R (for overall model fit) or
#' `car::Anova()` (partial regressors in multiple regression).
#'
#' @param object An object created by `supernova()`.
#' @param type The type of calculation used to compute the sums of squares.
#' @param ... Additional arguments used to control specifics of comparison.
#'
#' @return Invisibly returns the original `supernova` object for piping.
expect_supernova <- function(object, type = 3, ...) {
  expect_model(object)
  if (nrow(object$tbl) > 3) {
    expect_partials(object, type, ...)
  }
  invisible(object)
}

#' Test the overall model rows in a `supernova` table
#'
#' Test that a \code{\link{supernova}} object has the correct values for
#' evaluation of the overall model being tested. That is, the regression
#' row, the error row, and the total row of the table are checked against
#' standard calculations.
#'
#' @param object An object created by `supernova()`.
#' @param ... Additional arguments used to control specifics of comparison.
#'
#' @return Invisibly returns the original `supernova` object for piping.
expect_model <- function(object, ...) {
  f_stats <- summary(object$fit)[["fstatistic"]]
  p_value <- pf(f_stats[[1]], f_stats[[2]], f_stats[[3]], lower.tail = FALSE)
  pre <- summary(object$fit)[["r.squared"]]

  expected <- data.frame(
    SS = c(ssr(object$fit), sse(object$fit), sst(object$fit)),
    df = c(f_stats[[2]], f_stats[[3]], nrow(object$fit$model) - 1),
    `F` = c(f_stats[[1]], NA_real_, NA_real_),
    PRE = c(pre, NA_real_, NA_real_),
    p = c(p_value, NA_real_, NA_real_)
  )
  expected["MS"] <- expected["SS"] / expected["df"]
  expected <- expected[c("SS", "df", "MS", "F", "PRE", "p")]

  model_rows <- c(1, nrow(object$tbl) - 1, nrow(object$tbl))
  act_model_rows <- object$tbl[model_rows, 3:8]
  expect_table_data(act_model_rows, expected, object$fit, attr(object, "type"))
  invisible(object)
}

#' Test the partial (predictor) rows in a `supernova` table
#'
#' Test that a [`supernova`] object has the correct values for evaluation of the partial terms in
#' the model (each predictor in a multiple regression model).
#'
#' @param object An object created by `supernova()`.
#' @param type The type of calculation used to compute the sums of squares.
#' @param ... Additional arguments used to control specifics of comparison.
#'
#' @return Invisibly returns the original `supernova` object for piping.
expect_partials <- function(object, type = 3, ...) {
  act_partial_rows <- object$tbl[2:(nrow(object$tbl) - 2), 3:8]

  expected <- get_partials(object$fit, type = type)
  expected["MS"] <- expected["SS"] / expected["df"]
  expected["PRE"] <- expected["SS"] / (sse(object$fit) + expected["SS"])
  expected <- expected[c("SS", "df", "MS", "F", "PRE", "p")]

  expect_table_data(act_partial_rows, expected, object$fit, attr(object, "type"))
  invisible(object)
}

#' Compare the data in two tables.
#'
#' Custom expectation for comparing table data with better print methods in the
#' error results display.
#'
#' @param object The table data to test.
#' @param expected The expected table data.
#' @param ... Additional arguments used to control specifics of comparison.
#'
#' @return Invisibly returns the tested object.
expect_table_data <- function(object, expected, model = "???", type = "???", ...) {
  model_string <- if (is.character(model)) model else deparse(formula(model))
  testthat_table <- function(table) {
    output <- capture.output(print(table, row.names = FALSE))
    paste0("\n", paste0(output, collapse = "\n"))
  }
  comp <- testthat::compare(object, expected, check.attributes = FALSE, ...)
  expect(comp$equal, sprintf(
    "%s\n\nModel: %s\nType: %s\n\nActual: %s\n\nExpected: %s\n",
    comp$message, model_string, type, testthat_table(object), testthat_table(expected)
  ))
  invisible(object)
}


# Structure tests ---------------------------------------------------------

test_that("supernova object has table, fit, and models", {
  fit <- lm(mpg ~ NULL, mtcars)
  obj <- supernova(fit, type = 3)

  obj %>%
    expect_s3_class("supernova")

  obj$fit %>%
    expect_s3_class("lm") %>%
    expect_identical(fit)

  obj$models %>%
    expect_s3_class("comparison_models") %>%
    expect_identical(suppressWarnings(generate_models(fit, 3)))
})

test_that("supernova table structure is well-formed", {
  obj <- supernova(lm(mpg ~ NULL, mtcars))$tbl
  expect_vector(obj, data.frame(
    term = character(),
    description = character(),
    SS = double(),
    df = integer(),
    MS = double(),
    `F` = double(),
    PRE = double(),
    p = double(),
    stringsAsFactors = FALSE
  ))

  # TODO: what about verbose table structure?
})

test_that("supernova works when lm() is piped in", {
  lm(mpg ~ NULL, mtcars) %>%
    supernova() %>%
    expect_s3_class("supernova")
})

test_that("supernova works when data is piped into lm() is piped in", {
  # Believe it or not, this might not work. Do not remove or re-factor test.
  # When stats::update() tries to get the call, the data object is just "."
  # supernova has to middle-man with supernova::update() to get this to work
  mtcars %>%
    lm(mpg ~ NULL, data = .) %>%
    supernova() %>%
    expect_s3_class("supernova")
})

test_that("it can handle datasets with function name collisions", {
  # subset is a base R function
  subset <- mtcars
  expect_error(supernova(lm(mpg ~ hp, data = subset)), NA)
})


# ANOVA values ------------------------------------------------------------

test_that("supernova calcs. (quant. ~ NULL) ANOVA correctly", {
  model <- lm(mpg ~ NULL, data = mtcars)
  actual <- supernova(model)$tbl
  expected <- anova(model)
  numbers_only <- function(x) unname(as.matrix(x))

  expect_identical(
    numbers_only(actual[3, c("SS", "df", "MS", "F", "p")]),
    numbers_only(expected[c("Sum Sq", "Df", "Mean Sq", "F value", "Pr(>F)")])
  )
})

test_that("it can handle models that don't use `data =` when fitting with `lm()`", {
  actual <- supernova(lm(mtcars$mpg ~ NULL))
  expected <- supernova(lm(mpg ~ NULL, data = mtcars))
  expect_identical(actual$tbl, expected$tbl)
})

models_to_test <- list(
  `q ~ q` = lm(mpg ~ hp, data = mtcars),
  `q ~ q + q` = lm(mpg ~ hp + disp, data = mtcars),
  `q ~ q * q` = lm(mpg ~ hp * disp, data = mtcars),
  `q ~ c` = lm(mpg ~ factor(cyl), data = mtcars),
  `q ~ c + c` = lm(mpg ~ factor(cyl) + factor(am), data = mtcars),
  `q ~ c * c` = lm(mpg ~ factor(cyl) * factor(am), data = mtcars),
  `q ~ c + c + c` = lm(mpg ~ factor(cyl) + factor(am) + factor(gear), data = mtcars),
  `q ~ q + c` = lm(mpg ~ hp + factor(cyl), data = mtcars),
  `q ~ q * c` = lm(mpg ~ hp * factor(cyl), data = mtcars),
  `q ~ q * c + c` = lm(mpg ~ hp * factor(cyl) + factor(am), data = mtcars),
  `q ~ q * c * c` = lm(mpg ~ hp * factor(cyl) * factor(am), data = mtcars)
)

test_that("supernova calculates ANOVAs properly (including different SS types)", {
  anova_tester <- function(model, type) {
    obj <- supernova(model, type)
    expect_supernova(obj, type)
  }

  anova_tester_type_1 <- function(model) anova_tester(model, type = 1)
  lapply(models_to_test, anova_tester_type_1)

  anova_tester_type_2 <- function(model) anova_tester(model, type = 2)
  lapply(models_to_test, anova_tester_type_2)

  anova_tester_type_3 <- function(model) anova_tester(model, type = 3)
  lapply(models_to_test, anova_tester_type_3)
})

test_that("supernova SS types can be specified as numeric, numeral, character", {
  model <- models_to_test[[1]]
  expect_identical(supernova(model, type = 1), supernova(model, type = "I"))
  expect_identical(supernova(model, type = 1), supernova(model, type = "sequential"))

  expect_identical(supernova(model, type = 2), supernova(model, type = "II"))
  expect_identical(supernova(model, type = 2), supernova(model, type = "hierarchical"))

  expect_identical(supernova(model, type = 3), supernova(model))
  expect_identical(supernova(model, type = 3), supernova(model, type = "III"))
  expect_identical(supernova(model, type = 3), supernova(model, type = "orthogonal"))

  expect_identical(supernova(model, type = 3), supernova(model, type = " 3 "))
  expect_identical(supernova(model, type = 3), supernova(model, type = " iIi"))
})


# Unbalanced and missing data ---------------------------------------------

test_that("supernova uses listwise deletion for missing data", {
  df_missing <- get_data_with_missing()

  # one-way
  df_total <- sum(!is.na(df_missing$hp)) - 1
  one_way <- suppressMessages({
    fit <- lm(mpg ~ hp, df_missing)
    supernova(fit)
  })
  expect_supernova(one_way)
  # explicitly test correct df
  # this needs to be checked because otherwise the total row will show
  # num. rows - 1 for df instead of looking at only complete cases
  expect_col_equal(one_way$tbl, "df", c(1, df_total - 1, df_total))

  # two-way
  df_total <- nrow(na.omit(df_missing[c("mpg", "hp", "disp")])) - 1
  two_way <- suppressMessages({
    fit <- lm(mpg ~ hp * disp, df_missing)
    supernova(fit)
  })
  expect_supernova(two_way)
  expect_col_equal(two_way$tbl, "df", c(3, 1, 1, 1, df_total - 3, df_total))
})

test_that("supernova makes correct calculations for unbalanced data", {
  one_way <- suppressMessages(supernova(lm(uptake ~ Treatment, data = CO2[1:80, ])))
  expect_supernova(one_way)

  two_way <- suppressMessages(supernova(lm(uptake ~ Treatment * Type, data = CO2[1:80, ])))
  expect_supernova(two_way)
})

test_that("message is given for number of rows deleted due to missing cases", {
  df_missing <- get_data_with_missing()

  # 1 case removed
  expect_snapshot(supernova(lm(mpg ~ hp, df_missing)))
  # 2 cases removed
  expect_snapshot(supernova(lm(mpg ~ disp, df_missing)))
  # 3 cases removed
  expect_snapshot(supernova(lm(mpg ~ hp * disp, df_missing)))
})


# Printing -------------------------------------------------------------------

test_that("null model tables are beautifully formatted", {
  model <- lm(mpg ~ NULL, data = mtcars)
  expect_snapshot(supernova(model))
})

test_that("single predictor tables are beautifully formatted", {
  model <- lm(mpg ~ hp, data = mtcars)
  expect_snapshot(supernova(model))
})

test_that("multiple predictor tables are beautifully formatted", {
  model <- lm(mpg ~ hp + disp, data = mtcars)
  expect_snapshot(supernova(model))
})

test_that("non-verbose tables do not have a description column", {
  model <- lm(mpg ~ NULL, data = mtcars)
  expect_snapshot(supernova(model))
})

test_that("perfectly fit models print 1.000 for PRE and p", {
  model <- lm(c(1:10) ~ c(11:20))
  # f statistic can be unreliable to reproduce
  supernova(model) %>%
    print() %>%
    expect_output("1.0000 .0000\n")
})
