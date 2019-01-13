context("ANOVA values")
library(glue)
library(dplyr)
library(supernova)

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

# calcs. overall model SS
calc_ss <- function(model) {
  null_model <- update(model, . ~ NULL)
  sse <- sum(resid(model) ^ 2)
  sst <- sum(resid(null_model) ^ 2)
  list(ssr = sst - sse, sse = sse, sst = sst)
}

calc_pre <- function(ssr, sse) {
  ssr / (ssr + sse)
}


# Custom expectations -----------------------------------------------------

# Test a named column of a data.frame against a vector of expected values
expect_col_equal <- function(object, col_name, expected, ...) {
  column <- object[[col_name]]
  act <- quasi_label(rlang::enquo(column), glue("obj${col_name}"))
  exp <- quasi_label(rlang::enquo(expected))
  comp <- compare(act$val, exp$val, ...)
  expect(comp$equal, glue(
    "{act$lab} not equal to {exp$lab}.\n", "{comp$message}\n",
    "Actual: {tibble(act$val)}\n", "Expected: {tibble(exp$val)}"
  ))
  invisible(object)
}

# Test a values of a supernova data.frame against vectors of expected values
# Tolerances are set to be within the printed output's rounded values
expect_data <- function(object, SS, df, MS, F, PRE, p) {
  tol <- c(SS = .0001, df = .1, MS = .0001, F = .0001, PRE = .00001, p = .00001)
  for (i in seq_along(tol)) {
    name <- names(tol)[[i]]
    expect_col_equal(object, name, get(name), tol[[i]])
  }
  invisible(object)
}

# Retrieves partial row data for multiple regression from the model cache
get_partials <- function(model) {
  ivs <- labels(terms(formula(model)))
  if (length(ivs) < 2) return(NULL)

  cache_name <- Reduce(paste, deparse(model$call))
  if (!exists("model_cache")) {
    model_cache <<- readRDS("./model_cache.Rds")
  }
  model_cache[[cache_name]] %>%
    setNames(c("SS", "df", "F", "p")) %>%
    mutate(term = rownames(.)) %>%
    filter(term %in% ivs)
}

# Test the regression, error, and total rows of a supernova table
# Will work for all except null models
expect_supernova <- function(model) {
  object <- supernova(model)

  ss <- calc_ss(model)
  f <- summary(model)$fstatistic
  p <- pf(f[[1]], f[[2]], f[[3]], lower.tail = FALSE)

  exp <- tibble(SS = ss$ssr, df = f[[2]], F = f[[1]], p = p) %>%
    bind_rows(., get_partials(model)) %>%
    mutate(PRE = calc_pre(SS, calc_ss(model)$sse)) %>%  # PRE only in these rows
    # add error and total rows
    add_row(SS = ss$sse, df = f[[3]]) %>%
    add_row(SS = ss$sst, df = sum(f[2:3])) %>%
    mutate(MS = SS / df)

  expect_data(object$tbl, exp$SS, exp$df, exp$MS, exp$F, exp$PRE, exp$p)
  invisible(object)
}


# Structure tests ---------------------------------------------------------

test_that("superanova is an alias of supernova", {
  expect_identical(superanova, supernova)
})

test_that("supernova object has table and fit", {
  fit <- lm(mpg ~ NULL, mtcars)
  obj <- supernova(fit)
  obj %>% expect_is("supernova")
  obj$tbl %>% expect_is("data.frame")
  obj$fit %>%
    expect_is("lm") %>%
    expect_identical(fit)
})

test_that("supernova table structure is well-formed", {
  obj <- supernova(lm(mpg ~ NULL, mtcars))$tbl %>%
    expect_is("data.frame") %>%
    expect_named(c("term", "description", "SS", "df", "MS", "F", "PRE", "p"))
  expect_true(
    all(sapply(obj, class) == c(rep("character", 2), rep("numeric", 6)))
  )
})

test_that("magrittr can pipe lm() to supernova", {
  lm(mpg ~ NULL, mtcars) %>%
    supernova() %>%
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


# Variable extraction -----------------------------------------------------

test_that("variables returns the variables in a model", {
  expect_identical(
    variables(lm(mpg ~ NULL, mtcars)),
    list(outcome = "mpg", predictor = character(0))
  )
  expect_identical(
    variables(lm(mpg ~ hp, mtcars)),
    list(outcome = "mpg", predictor = "hp")
  )
  expect_identical(
    variables(lm(mpg ~ hp + disp, mtcars)),
    list(outcome = "mpg", predictor = c("hp", "disp"))
  )
  expect_identical(
    variables(lm(mpg ~ hp * disp, mtcars)),
    list(outcome = "mpg", predictor = c("hp", "disp", "hp:disp"))
  )
  expect_identical(
    variables(lm(mpg + hp ~ disp, mtcars)),
    list(outcome = c("mpg", "hp"), predictor = c("disp"))
  )
})

test_that("variables works with bare formulae", {
  expect_identical(
    variables(mpg ~ NULL),
    list(outcome = "mpg", predictor = character(0))
  )
})

test_that("variables works with supernova object", {
  expect_identical(
    variables(supernova(lm(mpg ~ NULL, mtcars))),
    list(outcome = "mpg", predictor = character(0))
  )
})

# Regression values -------------------------------------------------------

test_that("supernova calcs. (quant. ~ NULL) ANOVA correctly", {
  model <- lm(Thumb ~ NULL, Fingers)
  e <- anova(model)
  supernova(model)$tbl %>%
    tail(1) %>%
    expect_data(e$`Sum Sq`, e$Df, e$`Mean Sq`,NA_real_, NA_real_, NA_real_)
})

test_that("supernova correctly calcs. ANOVA Type 3 SS for multiple regression", {
  # q ~ q
  expect_supernova(lm(Thumb ~ Weight, Fingers))

  # q ~ c
  expect_supernova(lm(Thumb ~ RaceEthnic, Fingers))

  # q ~ q + q
  expect_supernova(lm(Thumb ~ Weight + Height, Fingers))

  # q ~ c + q
  expect_supernova(lm(Thumb ~ RaceEthnic + Weight, Fingers))

  # q ~ c + c
  expect_supernova(lm(Thumb ~ RaceEthnic + Sex, Fingers))

  # q ~ c + c + c
  expect_supernova(lm(Thumb ~ RaceEthnic + Weight + Sex, Fingers))

  # q ~ q * q
  expect_supernova(lm(Thumb ~ Weight * Height, Fingers))

  # q ~ c * q
  expect_supernova(lm(Thumb ~ RaceEthnic * Weight, Fingers))

  # q ~ c * c
  expect_supernova(lm(Thumb ~ RaceEthnic * Sex, Fingers))

  # q ~ c + q * c
  expect_supernova(lm(Thumb ~ RaceEthnic + Weight * Sex, Fingers))

  # q ~ c * q * c
  expect_supernova(lm(Thumb ~ RaceEthnic * Weight * Sex, Fingers))
})


# Unbalanced and missing data ---------------------------------------------

# This needs to match with df.missing in cache_test_data.R
get_data_with_missing <- function() {
  df.missing <- mtcars
  df.missing[1,]$hp <- NA_real_
  df.missing[2:3,]$disp <- NA_real_
  return(df.missing)
}

test_that("update() inherits na.action from lm() fit", {
  # no missing data
  model <- lm(mpg ~ hp * disp, mtcars)
  updated <- update(model, . ~ NULL)
  expect_length(resid(updated), length(resid(model)))

  # missing data
  df.missing <- get_data_with_missing()

  # na.omit (default)
  model <- lm(mpg ~ hp * disp, df.missing)
  updated <- update(model, . ~ NULL)
  expect_length(resid(updated), length(resid(model)))

  # na.omit (explicit)
  model <- lm(mpg ~ hp * disp, df.missing, na.action = na.omit)
  updated <- update(model, . ~ NULL)
  expect_length(resid(updated), length(resid(model)))

  # na.exclude
  model <- lm(mpg ~ hp * disp, df.missing, na.action = na.exclude)
  updated <- update(model, . ~ NULL)
  expect_length(resid(updated), length(resid(model)))
})

test_that("supernova uses listwise deletion for missing data", {
  df.missing <- get_data_with_missing()

  # one-way
  df_total <- sum(!is.na(df.missing$hp)) - 1
  expect_supernova(lm(mpg ~ hp, df.missing))  %>%
    # explicitly test correct df
    # this needs to be checked because otherwise the total row will show
    # nrow() - 1 for df instead of looking at only complete cases
    .$tbl %>% expect_col_equal("df", c(1, df_total - 1, df_total))

  # two-way
  df_total <- nrow(na.omit(select(df.missing, mpg, hp, disp))) - 1
  expect_supernova(lm(mpg ~ hp * disp, df.missing)) %>%
    .$tbl %>% expect_col_equal("df", c(3, 1, 1, 1, df_total - 3, df_total))
})

test_that("message is given for number of rows deleted due to missing cases", {
  df.missing <- get_data_with_missing()

  expect_message(supernova(lm(mpg ~ hp, mtcars)), NA)
  expect_message(
    supernova(lm(mpg ~ hp, df.missing)),
    "Note: 1 case removed due to missing value(s). Row number: 1",
    fixed = TRUE)
  expect_message(
    supernova(lm(mpg ~ disp, df.missing)),
    "Note: 2 cases removed due to missing value(s). Row numbers: 2, 3",
    fixed = TRUE)
  expect_message(
    supernova(lm(mpg ~ hp * disp, df.missing)),
    "Note: 3 cases removed due to missing value(s). Row numbers: 1, 2, 3",
    fixed = TRUE)
})

test_that("supernova makes correct calculations for unbalanced data", {
  expect_supernova(lm(uptake ~ Treatment, data = CO2[1:80,]))
  expect_supernova(lm(uptake ~ Treatment * Type, data = CO2[1:80,]))
})
