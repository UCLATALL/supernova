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

test_that("supernova calculates (quant. ~ NULL) ANOVA correctly", {
  model <- lm(mpg ~ NULL, data = mtcars)
  expected <- anova(model)
  supernova(model)$tbl %>%   
    tail(1) %>% 
    expect_data(
      expected$`Sum Sq`, expected$Df, expected$`Mean Sq`, 
      NA_real_, NA_real_, NA_real_
    )
})

test_that("supernova calculates (quant. ~ quant.) ANOVA correctly", {
  model <- lm(mpg ~ hp, data = mtcars)
  supernova(model)$tbl %>% 
    expect_data_overall_model(model)
})

test_that("supernova calculates (quant. ~ cat.) ANOVA correctly", {
  model <- lm(uptake ~ Type, data = CO2)
  supernova(model)$tbl %>% 
    expect_data_overall_model(model)
})


# Additive multiple regression --------------------------------------------

# NOTE: All hard-coded test values below are for the partials, and they were
# found by car::Anova(model, type = 3). The car package is widely
# known and used by many, and can be considered trustworthy.

test_that("supernova calculates (quant. ~ quant. + quant.) ANOVA Type 3 SS", {
  model <- lm(mpg ~ hp + disp, data = mtcars)
  ss <- calc_ss(model)
  x1 <- list(ssr = 33.66525, f = 3.4438, p = 0.07368) %>%
    c(pre = calc_pre(.$ssr, ss$sse))
  x2 <- list(ssr = 164.18088, f = 16.7949, p = 0.00031) %>%
    c(pre = calc_pre(.$ssr, ss$sse))

  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    expect_data_row(2, x1$ssr, 1, x1$ssr / 1, x1$f, x1$pre, x1$p) %>%
    expect_data_row(3, x2$ssr, 1, x2$ssr / 1, x2$f, x2$pre, x2$p)
})

test_that("supernova calculates (quant. ~ cat. + quant.) ANOVA Type 3 SS", {
  model <- lm(Thumb ~ RaceEthnic + Weight, data = Fingers)
  ss <- calc_ss(model)
  x1 <- list(ssr =  347.5909, df = 4, f =  1.3381, p = 0.2584) %>%
    c(pre = calc_pre(.$ssr, ss$sse))
  x2 <- list(ssr = 1411.9531, df = 1, f = 21.7425, p = 6.808e-06) %>%
    c(pre = calc_pre(.$ssr, ss$sse))
  
  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    expect_data_row(2, x1$ssr, x1$df, x1$ssr / x1$df, x1$f, x1$pre, x1$p) %>%
    expect_data_row(3, x2$ssr, x2$df, x2$ssr / x2$df, x2$f, x2$pre, x2$p)
})

test_that("supernova calculates (quant. ~ cat. + cat.) ANOVA Type 3 SS", {
  model <- lm(Thumb ~ RaceEthnic + Sex, data = Fingers)
  ss <- calc_ss(model)
  x1 <- list(ssr = 542.1871, df = 4, f =  2.045974, p = 9.076844e-02) %>%
    c(pre = calc_pre(.$ssr, ss$sse))
  x2 <- list(ssr = 1214.1,   df = 1, f = 18.325244, p = 3.295751e-05) %>%
    c(pre = calc_pre(.$ssr, ss$sse))
  
  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    expect_data_row(2, x1$ssr, x1$df, x1$ssr / x1$df, x1$f, x1$pre, x1$p) %>%
    expect_data_row(3, x2$ssr, x2$df, x2$ssr / x2$df, x2$f, x2$pre, x2$p)
})


# Interactive multiple regression -----------------------------------------

# NOTE: All hard-coded test values below are for the partials, and they were
# found by car::Anova(model, type = 3). The car package is widely
# known and used by many, and can be considered trustworthy.

test_that("supernova calculates (quant. ~ quant. * quant.) ANOVA Type 3 SS", {
  model <- lm(mpg ~ hp * disp, data = mtcars)
  ss <- calc_ss(model)
  x1   <- list(ssr = 113.39272, f = 15.651, p = 0.0004725) %>%
    c(pre = calc_pre(.$ssr, ss$sse))
  x2 <- list(ssr = 188.44895, f = 26.011, p = 0.00002109) %>%
    c(pre = calc_pre(.$ssr, ss$sse))
  int  <- list(ssr =  80.63539, f = 11.130, p = 0.0024070) %>%
    c(pre = calc_pre(.$ssr, ss$sse))
  
  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    expect_data_row(2, x1$ssr, 1, x1$ssr / 1, x1$f, x1$pre, x1$p) %>%
    expect_data_row(3, x2$ssr, 1, x2$ssr / 1, x2$f, x2$pre, x2$p) %>%
    expect_data_row(4, int$ssr, 1, int$ssr / 1, int$f, int$pre, int$p)
})

test_that("supernova calculates (quant. ~ cat. * quant.) ANOVA Type 3 SS", {
  model <- lm(Thumb ~ RaceEthnic * Weight, data = Fingers)
  ss <- calc_ss(model)
  x1 <-  list(ssr = 237.8776, df = 4, f = 0.9053925, p = 4.625566e-01) %>%
    c(pre = calc_pre(.$ssr, ss$sse))
  x2 <-  list(ssr = 599.5416, df = 1, f = 9.1277270, p = 2.970283e-03) %>%
    c(pre = calc_pre(.$ssr, ss$sse))
  int <- list(ssr = 150.4405, df = 4, f = 0.5725956, p = 6.829344e-01) %>%
    c(pre = calc_pre(.$ssr, ss$sse))

  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    expect_data_row(2, x1$ssr, x1$df, x1$ssr / x1$df, x1$f, x1$pre, x1$p) %>%
    expect_data_row(3, x2$ssr, x2$df, x2$ssr / x2$df, x2$f, x2$pre, x2$p) %>% 
    expect_data_row(4, int$ssr, int$df, int$ssr / int$df, int$f, int$pre, int$p)
})

test_that("supernova calculates (quant. ~ cat. * cat.) ANOVA Type 3 SS", {
  model <- lm(Thumb ~ RaceEthnic * Sex, data = Fingers)
  ss <- calc_ss(model)
  x1 <-  list(ssr = 720.1771, df = 4, f =  2.745803, p = 3.061602e-02) %>%
    c(pre = calc_pre(.$ssr, ss$sse))
  x2 <-  list(ssr = 919.3382, df = 1, f = 14.020560, p = 2.589060e-04) %>%
    c(pre = calc_pre(.$ssr, ss$sse))
  int <- list(ssr = 364.9248, df = 4, f =  1.391340, p = 2.397720e-01) %>%
    c(pre = calc_pre(.$ssr, ss$sse))

  supernova(model)$tbl %>%
    expect_data_overall_model(model) %>%
    expect_data_row(2, x1$ssr, x1$df, x1$ssr / x1$df, x1$f, x1$pre, x1$p) %>% 
    expect_data_row(3, x2$ssr, x2$df, x2$ssr / x2$df, x2$f, x2$pre, x2$p) %>% 
    expect_data_row(4, int$ssr, int$df, int$ssr / int$df, int$f, int$pre, int$p)
})
