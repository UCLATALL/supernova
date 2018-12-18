context("ANOVA tables")
library(supernova)


# Data --------------------------------------------------------------------

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
  
  expect_similar(actual_anova$SS[[3]], expected_anova$`Sum Sq`)
  expect_similar(actual_anova$df[[3]], expected_anova$Df)
  expect_similar(actual_anova$MS[[3]], expected_anova$`Mean Sq`)
  expect_similar(actual_anova$F[[3]], NA)
  expect_similar(actual_anova$PRE[[3]], NA)
  expect_similar(actual_anova$p[[3]], NA)
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
  
  expect_similar(actual_anova$SS, exp_SS)
  expect_similar(actual_anova$df, exp_df)
  expect_similar(actual_anova$MS, exp_SS / exp_df)
  expect_similar(actual_anova$F, exp_f)
  expect_similar(actual_anova$PRE, exp_pre)
  expect_similar(actual_anova$p, exp_p)
})

test_that("supernova prints correct values for additive multiple regression", {
  regression_model <- lm(mpg ~ hp + disp, data = mtcars)
})

test_that("superanova is an alias of supernova", {
  regression_model <- lm(mpg ~ hp, data = mtcars)
  expect_identical(superanova(regression_model), supernova(regression_model))
})


# Formatting --------------------------------------------------------------

dashes <- function(data_col, decimals = 0) {
  strrep("-", max(nchar(round(data_col, 0)), na.rm = TRUE) + if (decimals > 0 ) 1 + decimals else 0)
}

test_that("null model tables are beautifully formatted", {
  model <- lm(mpg ~ NULL, data = mtcars)
  printed <- capture.output(supernova(model))
  
  s_tbl <- supernova(model)$tbl
  outcome_variable <- all.vars(formula(model))[[1]]
  horizontal_rule <- paste(" -----", "-----------------", dashes(s_tbl$SS, 3), 
                           "---", dashes(s_tbl$MS, 3), "--- --- ---")

  expect_length(printed, 4 + 1 + 3 + 2)  # header, colnames, rows, rules
  expect_match(printed[[1]], "Analysis of Variance Table")
  expect_match(printed[[2]], paste0("Outcome variable: ", outcome_variable))
  expect_match(printed[[3]], deparse(formula(model)), fixed = TRUE)
  expect_identical(printed[[4]], paste(""))
  expect_match(printed[[5]], "^\\s+SS\\s+df\\s+MS\\s+F\\s+PRE\\s+p$")
  expect_identical(printed[[6]], horizontal_rule)
  expect_match(printed[[7]], "^Model (error reduced) |(\\s+---){3}$")
  expect_match(printed[[8]], "^Error (from model)    |(\\s+---){3}$")
  expect_identical(printed[[9]], horizontal_rule)
  expect_match(printed[[10]], "^Total (empty model)  |\\s+\\d+\\.\\d{3}\\s+\\d+\\s+\\d+\\.\\d{3}\\s+$")
})

test_that("single predictor tables are beautifully formatted", {
  model <- lm(mpg ~ hp, data = mtcars)
  printed <- capture.output(supernova(model))
  
  s_tbl <- supernova(model)$tbl
  outcome_variable <- all.vars(formula(model))[[1]]
  horizontal_rule <- paste(" -----", "-----------------", dashes(s_tbl$SS, 3), 
                           dashes(s_tbl$df, 0), dashes(s_tbl$MS, 3), 
                           dashes(s_tbl$F, 3), "------ -----")
  
  expect_length(printed, 4 + 1 + 3 + 2)  # header, colnames, rows, rules
  expect_match(printed[[1]], "Analysis of Variance Table")
  expect_match(printed[[2]], paste0("Outcome variable: ", outcome_variable))
  expect_match(printed[[3]], deparse(formula(model)), fixed = TRUE)
  expect_identical(printed[[4]], paste(""))
  expect_match(printed[[5]], "^\\s+SS\\s+df\\s+MS\\s+F\\s+PRE\\s+p$")
  expect_identical(printed[[6]], horizontal_rule)
  t_e_pattern <- "\\s+\\d+\\.\\d{3}\\s+\\d+\\s+\\d+\\.\\d{3}\\s+"
  expect_match(printed[[7]], paste0("^Model (error reduced) |", t_e_pattern, "\\d+\\.\\d{3}\\s+0\\.\\d{4}\\s+\\.\\d{4}$"))
  expect_match(printed[[8]], paste0("^Error (from model)    |", t_e_pattern, "$"))
  expect_identical(printed[[9]], horizontal_rule)
  expect_match(printed[[10]], paste0("^Total (empty model)  |", t_e_pattern, "$"))
})

test_that("multiple predictor tables are beautifully formatted", {
  model <- lm(mpg ~ hp + disp, data = mtcars)
  printed <- capture.output(supernova(model))
  
  s_tbl <- supernova(model)$tbl
  outcome_variable <- all.vars(formula(model))[[1]]
  predictors <- labels(terms(formula(model)))
  horizontal_rule <- paste0(" ", paste(
    strrep("-", max(nchar(s_tbl$term))), 
    "-----------------", 
    dashes(s_tbl$SS, 3), dashes(s_tbl$df, 0), dashes(s_tbl$MS, 3), dashes(s_tbl$F, 3), 
    "------ -----"))
  
  expect_length(printed, 4 + 1 + 5 + 2)  # header, colnames, rows, rules
  expect_match(printed[[1]], "Analysis of Variance Table")
  expect_match(printed[[2]], paste0("Outcome variable: ", outcome_variable))
  expect_match(printed[[3]], deparse(formula(model)), fixed = TRUE)
  expect_identical(printed[[4]], paste(""))
  expect_match(printed[[5]], "^\\s+SS\\s+df\\s+MS\\s+F\\s+PRE\\s+p$")
  expect_identical(printed[[6]], horizontal_rule)
  t_e_pattern <- "\\s+\\d+\\.\\d{3}\\s+\\d+\\s+\\d+\\.\\d{3}\\s+"
  expect_match(printed[[7]], paste0("^Model (error reduced) |", t_e_pattern, "\\d+\\.\\d{3}\\s+0\\.\\d{4}\\s+\\.\\d{4}$"))
  expect_match(printed[[8]], paste0("^", predictors[[1]], "\\s+|", t_e_pattern, "\\d+\\.\\d{3}\\s+0\\.\\d{4}\\s+\\.\\d{4}$"))
  expect_match(printed[[9]], paste0("^Model (error reduced) |", t_e_pattern, "\\d+\\.\\d{3}\\s+0\\.\\d{4}\\s+\\.\\d{4}$"))
  expect_match(printed[[10]], paste0("^Error (from model)    |", t_e_pattern, "$"))
  expect_identical(printed[[11]], horizontal_rule)
  expect_match(printed[[12]], paste0("^Total (empty model)  |", t_e_pattern, "$"))
})
