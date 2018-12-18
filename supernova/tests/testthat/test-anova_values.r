context("ANOVA values")
library(supernova)

calc_ss <- function(model) {
  null_model <- update(model, . ~ NULL)
  SSR <- sum((predict(model) - predict(null_model)) ^ 2)
  SST <- sum(resid(null_model) ^ 2)
  list(SSR = SSR, SSE = SST - SSR, SST = SST)
}

test_that("superanova is an alias of supernova", {
  expect_identical(superanova, supernova)
})

test_that("supernova object has table and fit", {
  fit <- lm(mpg ~ NULL, data = mtcars)
  obj <- supernova(fit)
  expect_is(obj, "supernova")
  expect_is(obj$tbl, "data.frame")
  expect_is(obj$fit, "lm")
  expect_identical(obj$fit, fit)
})

test_that("supernova prints correct values for NULL model ANOVA", {
  null_model <- lm(mpg ~ NULL, data = mtcars)
  actual_anova <- supernova(null_model)$tbl
  expected_anova <- anova(null_model)
  
  expect_equivalent(actual_anova$SS[[3]], expected_anova$`Sum Sq`)
  expect_equivalent(actual_anova$df[[3]], expected_anova$Df)
  expect_equivalent(actual_anova$MS[[3]], expected_anova$`Mean Sq`)
  expect_equivalent(actual_anova$F[[3]], NA_real_)
  expect_equivalent(actual_anova$PRE[[3]], NA_real_)
  expect_equivalent(actual_anova$p[[3]], NA_real_)
})

test_that("supernova prints correct values for single predictor regrssion", {
  model <- lm(mpg ~ hp, data = mtcars)
  actual_anova <- supernova(model)$tbl
  expected_anova <- anova(model)
  
  exp_SS <- c(expected_anova$`Sum Sq`, sum(expected_anova$`Sum Sq`))
  exp_df <- c(expected_anova$Df, sum(expected_anova$Df))
  exp_f <- c(expected_anova$`F value`, NA)
  exp_pre <- c(summary(model)$r.squared, NA, NA)
  exp_p <- c(expected_anova$`Pr(>F)`, NA)
  
  expect_equivalent(actual_anova$SS, exp_SS)
  expect_equivalent(actual_anova$df, exp_df)
  expect_equivalent(actual_anova$MS, exp_SS / exp_df)
  expect_equivalent(actual_anova$F, exp_f)
  expect_equivalent(actual_anova$PRE, exp_pre)
  expect_equivalent(actual_anova$p, exp_p)
})

test_that("supernova prints correct values for additive multiple regression", {
  # NOTE: All hard-coded test values below are for the partials, and they were
  # found by car::Anova(model, type = 3). The car package is widely
  # known and used by many, and can be considered trustworthy.
  
  model <- lm(mpg ~ hp + disp, data = mtcars)
  actual <- supernova(model)$tbl
  
  ss <- calc_ss(model)
  exp_ss <- c(ss$SSR, 33.66525, 164.18088, ss$SSE, ss$SST)
  expect_equivalent(actual$SS, exp_ss, tolerance = .0001)
  
  exp_df <- c(2, 1, 1, df.residual(model), 2 + df.residual(model))
  expect_equivalent(actual$df, exp_df)
  
  exp_ms <- exp_ss / exp_df
  expect_equivalent(actual$MS, exp_ms, tolerance = .0001)
  
  exp_f <- c(summary(model)$fstatistic["value"], 3.4438, 16.7949, NA, NA)
  expect_equivalent(actual$F, exp_f, tolerance = .0001)
  
  exp_pre <- c(ss$SSR / (ss$SSR + ss$SSE), 
               33.66525 / (33.66525 + ss$SSE), 
               164.18088 / (164.18088 + ss$SSE), NA, NA)
  expect_equivalent(actual$PRE, exp_pre, tolerance = .00001)
  
  exp_p <- c(pf(exp_f[[1]], exp_df[[1]], exp_df[[4]], lower.tail = FALSE), 
             0.07368, 0.00031, NA, NA)
  expect_equivalent(actual$p, exp_p, tolerance = .00001)
})

test_that("supernova gives Type III sums of squares for interaction models", {
  # NOTE: All hard-coded test values below are for the partials, and they were
  # found by car::Anova(model, type = 3). The car package is widely
  # known and used by many, and can be considered trustworthy.
  
  model <- lm(mpg ~ hp * disp, data = mtcars)
  actual <- supernova(model)$tbl
  
  ss <- calc_ss(model)
  car_ss <- as.double(c(hp = 113.39272, disp = 188.44895, int = 80.63539))
  exp_ss <- c(ss$SSR, car_ss, ss$SSE, ss$SST)
  expect_equivalent(actual$SS, exp_ss, tolerance = .0001)
  
  exp_df <- c(3, 1, 1, 1, df.residual(model), df.residual(model) + 3)
  expect_equivalent(actual$df, exp_df)
  
  exp_ms <- exp_ss / exp_df
  expect_equivalent(actual$MS, exp_ms, tolerance = .0001)
  
  exp_f <- c(summary(model)$fstatistic["value"], 15.651, 26.011, 11.130, NA, NA)
  expect_equivalent(actual$F, exp_f, tolerance = .0001)
  
  exp_pre <- c(ss$SSR / (ss$SSR + ss$SSE),
               113.39272 / (113.39272 + ss$SSE), 
               188.44895 / (188.44895 + ss$SSE), 
               80.63539 / (80.63539 + ss$SSE), NA, NA)
  expect_equivalent(actual$PRE, exp_pre, tolerance = .00001)
  
  exp_p <- exp_p <- c(pf(exp_f[[1]], exp_df[[1]], exp_df[[5]], lower.tail = FALSE), 
                      0.0004725, 0.00002109, .0024070, NA, NA)
  expect_equivalent(actual$p, exp_p, tolerance = .0001)
})
