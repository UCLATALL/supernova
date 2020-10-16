context("supernova: Values, independent designs")

# Notes -------------------------------------------------------------------

# When checking multiple regression, the data are sanity checked against the
# output from car::Anova(type = 3). The car package is NOT included in the
# supernova package, so these data are cached in
# "./tests/testthat/model_cache.Rds". This cache is accessed every time there
# are more than one predictor in the model, and the model is being tested using
# expect_supernova(). If you are going to test a new model using
# expect_supernova() and it has more than one predictor, you need to add that
# model in the "cache_test_data.R" script.


# Helper functions --------------------------------------------------------

pre <- function(ssr, sse) ssr / (ssr + sse)
ssr <- function(model) sum((predict(model) - mean(model$model[,1])) ^ 2, na.rm = TRUE)
sse <- function(model) sum(resid(model) ^ 2, na.rm = TRUE)
sst <- function(model) var(model$model[,1], na.rm = TRUE) * (nrow(model$model) - 1)
formula_to_string <- function(formula) Reduce(paste, deparse(formula))

# Retrieves partial row data for multiple regression from the model cache
get_partials <- function(model, type) {
  ivs <- formula(model) %>% terms() %>% labels()
  if (length(ivs) < 2) return(NULL)

  if (!exists("model_cache")) {
    model_cache <<- paste0("./cache/model_cache_type_", 1:3, ".Rds") %>%
      lapply(readRDS)
  }

  cache_name <- formula_to_string(model$call)

  anova_tbl <- model_cache[[type]][[cache_name]] %>%
    .[c('Sum Sq', 'Df', 'F value', 'Pr(>F)')] %>%
    setNames(c('SS', 'df', 'F', 'p')) %>%
    { .$term <- rownames(.); . }

  anova_tbl[which(anova_tbl$term %in% ivs), ]
}

# This needs to match with df.missing in cache_test_data.R
get_data_with_missing <- function() {
  df.missing <- mtcars
  df.missing[1,]$hp <- NA_real_
  df.missing[2:3,]$disp <- NA_real_
  return(df.missing)
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
  act <- quasi_label(rlang::enquo(column), sprintf("obj$%s", col_name))
  exp <- quasi_label(rlang::enquo(expected))
  comp <- compare(act$val, exp$val, ...)
  expect(comp$equal, sprintf(
    "%s not equal to %s.\n%s\nActual: %s\n", "Expected: %s",
    act$lab, exp$lab, comp$message, tibble::tibble(act$val), tibble::tibble(exp$val)
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
  if (nrow(object$tbl) > 3) expect_partials(object, type, ...)
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
    F = c(f_stats[[1]], NA_real_, NA_real_),
    PRE = c(pre, NA_real_, NA_real_),
    p = c(p_value, NA_real_, NA_real_)
  )
  expected['MS'] <- expected['SS'] / expected['df']
  expected <- expected[c('SS', 'df', 'MS', 'F', 'PRE', 'p')]

  act_model_rows <- object$tbl %>% .[c(1, nrow(.) - 1, nrow(.)), 3:8]
  expect_table_data(act_model_rows, expected)
  invisible(object)
}

#' Test the partial (predictor) rows in a `supernova` table
#'
#' Test that a \code{\link{supernova}} object has the correct values for
#' evaluation of the partial terms in the model (each predictor in a multiple
#' regression model).
#'
#' @param object An object created by `supernova()`.
#' @param type The type of calculation used to compute the sums of squares.
#' @param ... Additional arguments used to control specifics of comparison.
#'
#' @return Invisibly returns the original `supernova` object for piping.
expect_partials <- function(object, type = 3, ...) {
  act_partial_rows <- object$tbl %>% .[2:(nrow(.) - 2), 3:8]

  expected <- get_partials(object$fit, type = type)
  expected['MS'] <- expected['SS'] / expected['df']
  expected['PRE'] <- pre(expected['SS'], sse(object$fit))
  expected <- expected[c('SS', 'df', 'MS', 'F', 'PRE', 'p')]

  expect_table_data(act_partial_rows, expected)
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
expect_table_data <- function(object, expected, ...) {
  testthat_table <- function(table) {
    output <- capture.output(print(table, row.names = FALSE))
    paste0("\n", paste0(output, collapse = "\n"))
  }
  comp <- compare(object, expected, check.attributes = FALSE, ...)
  expect(comp$equal, sprintf(
    "%s\n\nActual: %s\n\nExpected: %s\n",
    comp$message, testthat_table(object), testthat_table(expected)
  ))
  invisible(object)
}


# Structure tests ---------------------------------------------------------

test_that("superanova is an alias of supernova", {
  expect_identical(superanova, supernova)
})

test_that("supernova object has table, fit, and models", {
  fit <- lm(mpg ~ NULL, mtcars)
  obj <- supernova(fit, type = 3)

  obj %>%
    expect_is("supernova")

  obj$tbl %>%
    expect_is("data.frame")

  obj$fit %>%
    expect_is("lm") %>%
    expect_identical(fit)

  obj$models %>%
    expect_is("comparison_models") %>%
    expect_identical(suppressWarnings(generate_models(fit, 3)))
})

test_that("supernova table structure is well-formed", {
  obj <- supernova(lm(mpg ~ NULL, mtcars))$tbl %>% expect_is("data.frame")
  expect_named(obj, c("term", "description", "SS", "df", "MS", "F", "PRE", "p"))
  expect_is(obj[["term"]], "character")
  expect_is(obj[["description"]], "character")
  expect_is(obj[["SS"]], "numeric")
  expect_is(obj[["df"]], "integer")
  expect_is(obj[["MS"]], "numeric")
  expect_is(obj[["F"]], "numeric")
  expect_is(obj[["PRE"]], "numeric")
  expect_is(obj[["p"]], "numeric")
})

test_that("magrittr can pipe lm() to supernova", {
  lm(mpg ~ NULL, mtcars) %>% supernova() %>%
    expect_is("supernova")
})

test_that("magrittr can pipe data to lm() to supernova", {
  # Believe it or not, this might not work. Do not remove or refactor test.
  # When stats::update() tries to get the call, the data object is just "."
  # supernova has to middle-man with supernova::update() to get this to work
  mtcars %>%
    lm(mpg ~ NULL, data = .) %>%
    supernova() %>%
    expect_is("supernova")
})


# ANOVA values ------------------------------------------------------------

test_that("supernova calcs. (quant. ~ NULL) ANOVA correctly", {
  model <- lm(Thumb ~ NULL, Fingers)
  expected <- anova(model)
  expected[c('SS', 'df', 'MS')] <- expected[c('Sum Sq', 'Df', 'Mean Sq')]
  expected[c('F', 'PRE', 'p')] <- NA_real_
  expected <- expected[c('SS', 'df', 'MS', 'F', 'PRE', 'p')]

  supernova(model)$tbl %>%
    .[nrow(.), 3:8] %>%
    expect_table_data(expected)
})

models_to_test <- list(
  `q ~ q`         = lm(Thumb ~ Weight, Fingers),
  `q ~ c`         = lm(Thumb ~ RaceEthnic, Fingers),
  `q ~ q + q`     = lm(Thumb ~ Weight + Height, Fingers),
  `q ~ c + q`     = lm(Thumb ~ RaceEthnic + Weight, Fingers),
  `q ~ c + c`     = lm(Thumb ~ RaceEthnic + Sex, Fingers),
  `q ~ c + c + c` = lm(Thumb ~ RaceEthnic + Weight + Sex, Fingers),
  `q ~ q * q`     = lm(Thumb ~ Weight * Height, Fingers),

  `q ~ c * q`     = lm(Thumb ~ RaceEthnic * Weight, Fingers),
  `q ~ c * c`     = lm(Thumb ~ RaceEthnic * Sex, Fingers),
  `q ~ c + q * c` = lm(Thumb ~ RaceEthnic + Weight * Sex, Fingers),
  `q ~ c * q * c` = lm(Thumb ~ RaceEthnic * Weight * Sex, Fingers)
)

test_that("supernova can calculate different SS types sequential (type 1) SS", {
  anova_tester <- function(model, type) expect_supernova(supernova(model, type), type)

  anova_tester_type_1 <- function(model) anova_tester(model, type = 1)
  lapply(models_to_test, anova_tester_type_1)

  anova_tester_type_2 <- function(model) anova_tester(model, type = 2)
  lapply(models_to_test, anova_tester_type_2)

  anova_tester_type_3 <- function(model) anova_tester(model, type = 3)
  lapply(models_to_test, anova_tester_type_3)
})

test_that("supernova SS types can be specified as numeric, numeral, character", {
  model <- models_to_test[[length(models_to_test)]]
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
  df.missing <- get_data_with_missing()

  # one-way
  df_total <- sum(!is.na(df.missing$hp)) - 1
  supernova(lm(mpg ~ hp, df.missing)) %>%
    expect_supernova() %>%
    # explicitly test correct df
    # this needs to be checked because otherwise the total row will show
    # nrow() - 1 for df instead of looking at only complete cases
    .$tbl %>% expect_col_equal("df", c(1, df_total - 1, df_total))

  # two-way
  df_total <- nrow(na.omit(df.missing[c('mpg', 'hp', 'disp')])) - 1
  anova <- supernova(lm(mpg ~ hp * disp, df.missing))
  expect_supernova(anova)
  expect_col_equal(anova$tbl, "df", c(3, 1, 1, 1, df_total - 3, df_total))
})

test_that("message is given for number of rows deleted due to missing cases", {
  df.missing <- get_data_with_missing()

  expect_message(
    supernova(lm(mpg ~ hp, mtcars)),
    NA)
  expect_message(
    supernova(lm(mpg ~ hp, df.missing)),
    "Note: 1 case removed due to missing value(s).",
    fixed = TRUE)
  expect_message(
    supernova(lm(mpg ~ disp, df.missing)),
    "Note: 2 cases removed due to missing value(s).",
    fixed = TRUE)
  expect_message(
    supernova(lm(mpg ~ hp * disp, df.missing)),
    "Note: 3 cases removed due to missing value(s).",
    fixed = TRUE)
})

test_that("supernova makes correct calculations for unbalanced data", {
  expect_supernova(supernova(lm(uptake ~ Treatment, data = CO2[1:80, ])))
  expect_supernova(supernova(lm(uptake ~ Treatment * Type, data = CO2[1:80, ])))
})
