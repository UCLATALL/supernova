context("ANOVA values")
library(supernova)
library(dplyr)
library(glue)


# Helper functions --------------------------------------------------------

# calcs. overall model SS
calc_ss <- function(model) {
  null_model <- update(model, . ~ NULL)
  ssr <- sum((predict(model) - predict(null_model)) ^ 2)
  sst <- sum(resid(null_model) ^ 2)
  list(ssr = ssr, sse = sst - ssr, sst = sst)
}

calc_pre <- function(ssr, sse) {
  ssr / (ssr + sse)
}

# Test a named column of a data.frame against a vector of expected values
expect_col_equal <- function(object, col_name, expected, ...) {
  column <- object[[col_name]]
  act <- quasi_label(rlang::enquo(column), glue("obj${col_name}"))
  exp <- quasi_label(rlang::enquo(expected))
  comp <- compare(act$val, exp$val, ...)
  expect(comp$equal, glue("{act$lab} not equal to {exp$lab}.\n{comp$message}"))
  invisible(object)
}

# Test a values of a supernova data.frame against vectors of expected values
# Tolerances are set to be within the printed output's rounded values
expect_data <- function(object, ss, df, ms, f, pre, p) {
  object %>% 
    expect_col_equal("SS", ss, tolerance = .0001) %>% 
    expect_col_equal("df", df) %>% 
    expect_col_equal("MS", ms, tolerance = .0001) %>% 
    expect_col_equal("F", f, tolerance = .0001) %>% 
    expect_col_equal("PRE", pre, tolerance = .00001) %>% 
    expect_col_equal("p", p, tolerance = .00001)
}

# Test the regression, error, and total rows of a supernova table
# Will work for all except null models
expect_data_overall_model <- function(object, model) {
  f <- summary(model)$fstatistic
  p <- pf(f[[1]], f[[2]], f[[3]], lower.tail = FALSE)
  df <- tibble(
    ss = as.numeric(calc_ss(model)), 
    df = c(f[["numdf"]], df.residual(model), sum(anova(model)$Df)),
    ms = ss / df,
    f = c(f[["value"]], NA, NA),
    pre = c(summary(model)$r.squared, NA, NA),
    p = c(p, NA, NA)
  )
  object[c(1, nrow(object) - 1, nrow(object)),] %>% 
    expect_data(df$ss, df$df, df$ms, df$f, df$pre, df$p)
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


# Simple regression -------------------------------------------------------

test_that("supernova calcs. (quant. ~ NULL) ANOVA correctly", {
  model <- lm(mpg ~ NULL, data = mtcars)
  e <- anova(model)
  supernova(model)$tbl %>%
    tail(1) %>% 
    expect_data(e$`Sum Sq`, e$Df, e$`Mean Sq`, NA_real_, NA_real_, NA_real_)
})

test_that("supernova calcs. (quant. ~ quant.) ANOVA correctly", {
  model <- lm(mpg ~ hp, data = mtcars)
  supernova(model)$tbl %>% 
    expect_data_overall_model(model)
})

test_that("supernova calcs. (quant. ~ cat.) ANOVA correctly", {
  model <- lm(uptake ~ Type, data = CO2)
  supernova(model)$tbl %>% 
    expect_data_overall_model(model)
})


# Additive multiple regression --------------------------------------------

# NOTE: All hard-coded test values below are for the partials, and they were
# found by car::Anova(model, type = 3). The car package is widely
# known and used by many, and can be considered trustworthy.

test_that("supernova calcs. (quant. ~ quant. + quant.) ANOVA Type 3 SS", {
  model <- lm(mpg ~ hp + disp, data = mtcars)
  df <- tibble(
    ss = c(33.66525, 164.18088), df = c(1, 1), ms = ss / df,
    f = c(3.4438, 16.7949), p = c(0.07368, 0.00031),
    pre = calc_pre(ss, calc_ss(model)$sse)
  )
  
  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    slice(2:3) %>% 
    expect_data(df$ss, df$df, df$ms, df$f, df$pre, df$p)
})

test_that("supernova calcs. (quant. ~ cat. + quant.) ANOVA Type 3 SS", {
  model <- lm(Thumb ~ RaceEthnic + Weight, data = Fingers)
  df <- tibble(
    ss = c(347.5909, 1411.9531), df = c(4, 1), ms = ss / df,
    f = c(1.3381, 21.7425), p = c(0.2584, 0.000006808),
    pre = calc_pre(ss, calc_ss(model)$sse)
  )
  
  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    slice(2:3) %>% 
    expect_data(df$ss, df$df, df$ms, df$f, df$pre, df$p)
})

test_that("supernova calcs. (quant. ~ cat. + cat.) ANOVA Type 3 SS", {
  model <- lm(Thumb ~ RaceEthnic + Sex, data = Fingers)
  df <- tibble(
    ss = c(542.1871, 1214.1), df = c(4, 1), ms = ss / df,
    f = c(2.045974, 18.325244), p = c(0.09076844, 0.00003295751),
    pre = calc_pre(ss, calc_ss(model)$sse)
  )
  
  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    slice(2:3) %>% 
    expect_data(df$ss, df$df, df$ms, df$f, df$pre, df$p)
})

test_that("supernova calcs. additive 3-way mixed model ANOVA Type 3 SS", {
  model <- lm(Thumb ~ RaceEthnic + Weight + Sex, data = Fingers)
  df <- tibble(
    ss = c(327.1720, 509.7228, 311.8257), df = c(4, 1, 1), ms = ss / df,
    f = c(1.292271, 8.053257, 4.926625),
    p = c(0.2756319, 0.005171346, 0.02794657),
    pre = calc_pre(ss, calc_ss(model)$sse)
  )
  
  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    slice(2:4) %>% 
    expect_data(df$ss, df$df, df$ms, df$f, df$pre, df$p)
})

# Interactive multiple regression -----------------------------------------

# NOTE: All hard-coded test values below are for the partials, and they were
# found by car::Anova(model, type = 3). The car package is widely
# known and used by many, and can be considered trustworthy.

test_that("supernova calcs. (quant. ~ quant. * quant.) ANOVA Type 3 SS", {
  model <- lm(mpg ~ hp * disp, data = mtcars)
  df <- tibble(
    ss = c(113.39272, 188.44895, 80.63539),
    df = c(1, 1, 1),
    ms = ss / df,
    f = c(15.651, 26.011, 11.130),
    p = c(0.0004725, 0.00002109, 0.0024070),
    pre = calc_pre(ss, calc_ss(model)$sse)
  )
  
  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    slice(2:4) %>% 
    expect_data(df$ss, df$df, df$ms, df$f, df$pre, df$p)
})

test_that("supernova calcs. (quant. ~ cat. * quant.) ANOVA Type 3 SS", {
  model <- lm(Thumb ~ RaceEthnic * Weight, data = Fingers)
  df <- tibble(
    ss = c(237.8776, 599.5416, 150.4405),
    df = c(4, 1, 4),
    ms = ss / df,
    f = c(0.9053925, 9.1277270, 0.5725956),
    p = c(0.4625566, 0.002970283, 0.6829344),
    pre = calc_pre(ss, calc_ss(model)$sse)
  )
  
  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    slice(2:4) %>% 
    expect_data(df$ss, df$df, df$ms, df$f, df$pre, df$p)
})

test_that("supernova calcs. (quant. ~ cat. * cat.) ANOVA Type 3 SS", {
  model <- lm(Thumb ~ RaceEthnic * Sex, data = Fingers)
  df <- tibble(
    ss = c(720.1771, 919.3382, 364.9248),
    df = c(4, 1, 4),
    ms = ss / df,
    f = c(2.745803, 14.020560, 1.391340),
    p = c(0.03061602, 0.0002589060, 0.239772),
    pre = calc_pre(ss, calc_ss(model)$sse)
  )
  
  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    slice(2:4) %>% 
    expect_data(df$ss, df$df, df$ms, df$f, df$pre, df$p)
})

test_that("supernova calcs. interactive model with addl. regressor Type 3 SS", {
  model <- lm(Thumb ~ RaceEthnic + Weight * Sex, data = Fingers)
  df <- tibble(
    ss = c(327.29294, 485.92344, 177.56371, 80.05167),
    df = c(4, 1, 1, 1),
    ms = ss / df,
    f = c(1.29505, 7.69091, 2.81037, 1.26701),
    p = c(0.2745969, 0.0062597, 0.0957535, 0.2621380),
    pre = calc_pre(ss, calc_ss(model)$sse)
  )
  
  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    slice(2:5) %>% 
    expect_data(df$ss, df$df, df$ms, df$f, df$pre, df$p)
})

test_that("supernova calcs. interactive 3-way mixed model ANOVA Type 3 SS", {
  model <- lm(Thumb ~ RaceEthnic * Weight * Sex, data = Fingers)
  df <- tibble(
    ss = c(137.94928, 8.89483, 6.84062, 91.58906, 9.16352, 3.04586, 22.27521),
    df = c(4, 1, 1, 4, 4, 1, 4),
    ms = ss / df,
    f = c(0.52429, 0.13522, 0.10399, 0.34809, 0.03483, 0.04630, 0.08466),
    p = c(0.71803, 0.71364, 0.74758, 0.84499, 0.99765, 0.82994, 0.98704),
    pre = calc_pre(ss, calc_ss(model)$sse)
  )
    
  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    slice(2:8) %>% 
    expect_data(df$ss, df$df, df$ms, df$f, df$pre, df$p)
})

