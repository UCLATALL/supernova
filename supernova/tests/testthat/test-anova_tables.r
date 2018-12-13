context("ANOVA tables")
library(supernova)


# Data --------------------------------------------------------------------

test_that("supernova object has table and fit", {
  fit <- lm(mpg ~ NULL, data = mtcars)
  obj <- supernova(fit)
  expect_is(obj, "supernova")
  expect_is(obj$f, "data.frame")
  expect_is(obj$fit, "lm")
  expect_identical(obj$fit, fit)
})

test_that("supernova prints correct values for NULL model ANOVA", {
  null_model <- lm(mpg ~ NULL, data = mtcars)
  actual_anova <- supernova(null_model)$f
  expected_anova <- anova(null_model)
  
  expect_similar(actual_anova$SS[[3]], expected_anova$`Sum Sq`)
  expect_similar(actual_anova$df[[3]], expected_anova$Df)
  expect_similar(actual_anova$MS[[3]], expected_anova$`Mean Sq`)
  expect_similar(actual_anova$F[[3]], NA)
  expect_similar(actual_anova$PRE[[3]], NA)
  expect_similar(actual_anova$p[[3]], NA)
})

test_that("supernova prints correct values for one-way ANOVA", {
  regression_model <- lm(mpg ~ hp, data = mtcars)
  actual_anova <- supernova(regression_model)$f
  expected_anova <- anova(regression_model)
  
  exp_SS <- c(expected_anova$`Sum Sq`, sum(expected_anova$`Sum Sq`))
  exp_df <- c(expected_anova$Df, sum(expected_anova$Df))
  exp_f <- c(expected_anova$`F value`, NA)
  exp_pre <- c(summary(regression_model)$r.squared, NA, NA)
  exp_p <- c(expected_anova$`Pr(>F)`, NA)
  
  expect_similar(actual_anova$SS, exp_SS)
  expect_similar(actual_anova$df, exp_df)
  expect_similar(actual_anova$MS, exp_SS / exp_df)
  expect_similar(actual_anova$F, exp_f)
  expect_similar(actual_anova$PRE, exp_pre)
  expect_similar(actual_anova$p, exp_p)
})

test_that("superanova is an alias of supernova", {
  regression_model <- lm(mpg ~ hp, data = mtcars)
  expect_identical(superanova(regression_model), supernova(regression_model))
})


# Formatting --------------------------------------------------------------

test_that("tables are beautifully formatted", {
  # TODO only tests a specific one-way model; would be good to create custom 
  # expectation that tests all models given to it, then add a few more models
  
  regression_model <- lm(mpg ~ hp, data = mtcars)
  printed <- capture.output(supernova(regression_model))
  
  # table info
  expect_match(printed[[1]], "Analysis of Variance Table")
  expect_match(printed[[2]], paste0("Outcome variable: ", names(regression_model$model)[[1]]))
  expect_match(printed[[3]], "mpg ~ hp, data = mtcars")
  expect_identical(printed[[4]], paste(""))
  
  # header names
  expect_match(printed[[5]], "SS")
  expect_match(printed[[5]], "df")
  expect_match(printed[[5]], "MS")
  expect_match(printed[[5]], "F")
  expect_match(printed[[5]], "PRE")
  expect_match(printed[[5]], "p")
  
  # vertical lines
  # should appear at position 24 on value rows, regardless of table
  expect_true(all(regexpr("\\|", 
    printed[c(7:(length(printed)-2), length(printed))]) == 24
  ))
  
  # horizontal lines
  # note: number of dashes is dependent on getOption("digits")
  # TODO update to not be hard-coded
  expect_match(printed[[6]], " ----- ----------------- ------- -- ------- ----- ------ -----")
  expect_identical(printed[[9]], printed[[6]])
})
