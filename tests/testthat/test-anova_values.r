context("ANOVA values")
library(supernova)
library(glue)
library(dplyr)


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

# Test the regression, error, and total rows of a supernova table
# Will work for all except null models
expect_supernova <- function(model) {
  object <- supernova(model)

  ss <- calc_ss(model)
  f <- summary(model)$fstatistic
  p <- pf(f[[1]], f[[2]], f[[3]], lower.tail = FALSE)
  ivs <- labels(terms(formula(model)))

  partials <- if (length(ivs) < 2) NULL else {
    car::Anova(model, type = 3) %>%
      setNames(c("SS", "df", "F", "p")) %>%
      mutate(term = rownames(.)) %>%
      filter(term %in% ivs)
    }

  exp <- tibble(SS = ss$ssr, df = f[[2]], F = f[[1]], p = p) %>%
    bind_rows(., partials) %>%
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
  fit <- lm(mpg ~ NULL, data = mtcars)
  obj <- supernova(fit)
  obj %>% expect_is("supernova")
  obj$tbl %>% expect_is("data.frame")
  obj$fit %>%
    expect_is("lm") %>%
    expect_identical(fit)
})

test_that("supernova table structure is well-formed", {
  obj <- supernova(lm(mpg ~ NULL, data = mtcars))$tbl %>%
    expect_is("data.frame") %>%
    expect_named(c("term", "description", "SS", "df", "MS", "F", "PRE", "p"))
  expect_true(
    all(sapply(obj, class) == c(rep("character", 2), rep("numeric", 6)))
  )
})

test_that("magrittr can pipe lm() to supernova", {
  lm(mpg ~ NULL, data = mtcars) %>%
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
  model <- lm(mpg ~ NULL, data = mtcars)
  e <- anova(model)
  supernova(model)$tbl %>%
    tail(1) %>%
    expect_data(e$`Sum Sq`, e$Df, e$`Mean Sq`,NA_real_, NA_real_, NA_real_)
})

test_that("supernova correctly calcs. ANOVA Type 3 SS for multiple regression", {
  # q ~ q
  expect_supernova(lm(mpg ~ hp, data = mtcars))

  # q ~ c
  expect_supernova(lm(Thumb ~ RaceEthnic, data = Fingers))

  # q ~ q + q
  expect_supernova(lm(mpg ~ hp + disp, mtcars))

  # q ~ c + q
  expect_supernova(lm(Thumb ~ RaceEthnic + Weight, data = Fingers))

  # q ~ c + c
  expect_supernova(lm(Thumb ~ RaceEthnic + Sex, data = Fingers))

  # q ~ c + c + c
  expect_supernova(lm(Thumb ~ RaceEthnic + Weight + Sex, data = Fingers))

  # q ~ q * q
  expect_supernova(lm(mpg ~ hp * disp, data = mtcars))

  # q ~ c * q
  expect_supernova(lm(Thumb ~ RaceEthnic * Weight, data = Fingers))

  # q ~ c * c
  expect_supernova(lm(Thumb ~ RaceEthnic * Sex, data = Fingers))

  # q ~ c + q * c
  expect_supernova(lm(Thumb ~ RaceEthnic + Weight * Sex, data = Fingers))

  # q ~ c * q * c
  expect_supernova(lm(Thumb ~ RaceEthnic * Weight * Sex, data = Fingers))
})


# Unbalanced and missing data ---------------------------------------------

get_data_with_missing <- function() {
  mtcopy <- mtcars
  mtcopy[1,]$hp <- NA_real_
  mtcopy[2:3,]$disp <- NA_real_
  return(mtcopy)
}

test_that("update() inherits na.action from lm() fit", {
  # no missing data
  model <- lm(mpg ~ hp * disp, data = mtcars)
  updated <- update(model, . ~ NULL)
  expect_length(resid(updated), length(resid(model)))

  # missing data
  mtcopy <- get_data_with_missing()

  # na.omit (default)
  model <- lm(mpg ~ hp * disp, data = mtcopy)
  updated <- update(model, . ~ NULL)
  expect_length(resid(updated), length(resid(model)))

  # na.omit (explicit)
  model <- lm(mpg ~ hp * disp, data = mtcopy, na.action = na.omit)
  updated <- update(model, . ~ NULL)
  expect_length(resid(updated), length(resid(model)))

  # na.exclude
  model <- lm(mpg ~ hp * disp, data = mtcopy, na.action = na.exclude)
  updated <- update(model, . ~ NULL)
  expect_length(resid(updated), length(resid(model)))
})

test_that("supernova uses listwise deletion for missing data", {
  mtcopy <- get_data_with_missing()

  # one-way
  df_total <- sum(!is.na(mtcopy$hp)) - 1
  expect_supernova(lm(mpg ~ hp, mtcopy))  %>%
    # explicitly test correct df
    # this needs to be checked because otherwise the total row will show
    # nrow() - 1 for df instead of looking at only complete cases
    .$tbl %>% expect_col_equal("df", c(1, df_total - 1, df_total))

  # two-way
  df_total <- (mtcopy %>% select(mpg, hp, disp) %>% na.omit %>% nrow) - 1
  expect_supernova(lm(mpg ~ hp * disp, mtcopy)) %>%
    .$tbl %>% expect_col_equal("df", c(3, 1, 1, 1, df_total - 3, df_total))
})

test_that("message is given for number of rows deleted due to missing cases", {
  mtcopy <- get_data_with_missing()

  expect_message(supernova(lm(mpg ~ hp, data = mtcars)), NA)
  expect_message(
    supernova(lm(mpg ~ hp, data = mtcopy)),
    "Note: 1 case removed due to missing value(s). Row number: 1",
    fixed = TRUE)
  expect_message(
    supernova(lm(mpg ~ disp, data = mtcopy)),
    "Note: 2 cases removed due to missing value(s). Row numbers: 2, 3",
    fixed = TRUE)
  expect_message(
    supernova(lm(mpg ~ disp * hp, data = mtcopy)),
    "Note: 3 cases removed due to missing value(s). Row numbers: 1, 2, 3",
    fixed = TRUE)
})

test_that("supernova makes correct calculations for unbalanced data", {
  expect_supernova(lm(uptake ~ Treatment, data = CO2[1:80,]))
  expect_supernova(lm(uptake ~ Treatment * Type, data = CO2[1:80,]))
})
