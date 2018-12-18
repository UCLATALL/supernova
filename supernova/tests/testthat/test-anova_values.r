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
  regression_model <- lm(mpg ~ hp, data = mtcars)
  actual_anova <- supernova(regression_model)$tbl
  expected_anova <- anova(regression_model)
  
  exp_SS <- c(expected_anova$`Sum Sq`, sum(expected_anova$`Sum Sq`))
  exp_df <- c(expected_anova$Df, sum(expected_anova$Df))
  exp_f <- c(expected_anova$`F value`, NA)
  exp_pre <- c(summary(regression_model)$r.squared, NA, NA)
  exp_p <- c(expected_anova$`Pr(>F)`, NA)
  
  expect_equivalent(actual_anova$SS, exp_SS)
  expect_equivalent(actual_anova$df, exp_df)
  expect_equivalent(actual_anova$MS, exp_SS / exp_df)
  expect_equivalent(actual_anova$F, exp_f)
  expect_equivalent(actual_anova$PRE, exp_pre)
  expect_equivalent(actual_anova$p, exp_p)
})
