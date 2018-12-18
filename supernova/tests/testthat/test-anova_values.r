context("ANOVA values")
library(supernova)

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

test_that("supernova prints correct values for single predictor regression", {
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
  
  type_1_anova <- anova(model)
  null_model <- update(model, . ~ NULL)
  
  # SS
  SSR <- sum((predict(model) - predict(null_model)) ^ 2)
  SST <- sum(resid(null_model) ^ 2)
  SSE = SST - SSR
  exp_ss <- c(SSR, 33.66525, 164.18088, SSE, SST)
  expect_equivalent(actual$SS, exp_ss, tolerance = .0001)
  
  # df
  predictors <- labels(terms(formula(model)))
  n_pred <- length(predictors)
  exp_df <- c(n_pred, rep(1, n_pred), df.residual(model), n_pred + df.residual(model))
  expect_equivalent(actual$df, exp_df)
  
  # MS
  exp_ms <- exp_ss / exp_df
  expect_equivalent(actual$MS, exp_ms, tolerance = .0001)
  
  # F
  exp_f <- c(summary(model)$fstatistic["value"], 3.4438, 16.7949, NA, NA)
  expect_equivalent(actual$F, exp_f, tolerance = .0001)
  
  # PRE
  exp_pre <- c(SSR / (SSR + SSE), 33.66525 / (33.66525 + SSE), 
               164.18088 / (164.18088 + SSE), NA, NA)
  expect_equivalent(actual$PRE, exp_pre, tolerance = .00001)
  
  # p
  exp_p <- c(pf(exp_f[[1]], exp_df[[1]], exp_df[[4]], lower.tail = FALSE), 
             0.07368, 0.00031, NA, NA)
  expect_equivalent(actual$p, exp_p, tolerance = .00001)
})
