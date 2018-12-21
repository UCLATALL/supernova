context("ANOVA values")
library(supernova)
library(magrittr)
library(glue)


# Helper functions --------------------------------------------------------

# calculates overall model SS
calc_ss <- function(model) {
  null_model <- update(model, . ~ NULL)
  ssr <- sum((predict(model) - predict(null_model)) ^ 2)
  sst <- sum(resid(null_model) ^ 2)
  list(ssr = ssr, sse = sst - ssr, sst = sst)
}

calc_pre <- function(ssr, sse) {
  ssr / (ssr + sse)
}

calc_p <- function(model) {
  f <- summary(model)$fstatistic
  pf(f[[1]], f[[2]], f[[3]], lower.tail = FALSE)
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

# Test a numbered row from a supernova data.frame (e.g. data[1,])
expect_data_row <- function(object, row, ss, df, ms, f, pre, p) {
  expect_data(object[row, ], ss, df, ms, f, pre, p)
  invisible(object)
}

# Test the regression, error, and total rows of a supernova table
# Will work for all except null models
expect_data_overall_model <- function(object, model) {
  expected <- anova(model)
  expected_SS <- as.numeric(calc_ss(model))
  expected_f <- summary(model)$fstatistic
  expected_df <- c(expected_f[["numdf"]], df.residual(model), sum(expected$Df))
  object[c(1, nrow(object) - 1, nrow(object)), ] %>% 
    expect_data(
      expected_SS,
      expected_df,
      expected_SS / expected_df,
      c(expected_f[['value']], NA, NA),
      c(summary(model)$r.squared, NA, NA),
      c(calc_p(model), NA, NA)
    )
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

test_that("supernova prints correct values for NULL model ANOVA", {
  model <- lm(mpg ~ NULL, data = mtcars)
  expected <- anova(model)
  supernova(model)$tbl %>%   
    tail(1) %>% 
    expect_data(
      expected$`Sum Sq`, expected$Df, expected$`Mean Sq`, 
      NA_real_, NA_real_, NA_real_
    )
})

test_that("supernova prints correct values for single predictor regrssion", {
  model <- lm(mpg ~ hp, data = mtcars)
  supernova(model)$tbl %>% 
    expect_data_overall_model(model)
})


# Multiple regression -----------------------------------------------------

# NOTE: All hard-coded test values below are for the partials, and they were
# found by car::Anova(model, type = 3). The car package is widely
# known and used by many, and can be considered trustworthy.

test_that("supernova prints correct values for additive multiple regression", {
  model <- lm(mpg ~ hp + disp, data = mtcars)
  ss <- calc_ss(model)
  hp <- list(ssr = 33.66525, f = 3.4438, p = 0.07368) %>% 
    c(pre = calc_pre(.$ssr, ss$sse))
  disp <- list(ssr = 164.18088, f = 16.7949, p = 0.00031) %>% 
    c(pre = calc_pre(.$ssr, ss$sse))
  
  supernova(model)$tbl %>% 
    expect_data_overall_model(model) %>%
    expect_data_row(2, hp$ssr, 1, hp$ssr / 1, hp$f, hp$pre, hp$p) %>% 
    expect_data_row(3, disp$ssr, 1, disp$ssr / 1, disp$f, disp$pre, disp$p)
})

test_that("supernova gives Type III sums of squares for interaction models", {
  model <- lm(mpg ~ hp * disp, data = mtcars)
  ss <- calc_ss(model)
  hp   <- list(ssr = 113.39272, f = 15.651, p = 0.0004725) %>% 
    c(pre = calc_pre(.$ssr, ss$sse))
  disp <- list(ssr = 188.44895, f = 26.011, p = 0.00002109) %>% 
    c(pre = calc_pre(.$ssr, ss$sse))
  int  <- list(ssr =  80.63539, f = 11.130, p = 0.0024070) %>% 
    c(pre = calc_pre(.$ssr, ss$sse))
  
  supernova(model)$tbl %>% 
    expect_data_overall_model(model) %>%
    expect_data_row(2, hp$ssr, 1, hp$ssr / 1, hp$f, hp$pre, hp$p) %>% 
    expect_data_row(3, disp$ssr, 1, disp$ssr / 1, disp$f, disp$pre, disp$p) %>% 
    expect_data_row(4, int$ssr, 1, int$ssr / 1, int$f, int$pre, int$p)
})
