context("ANOVA formatting")
library(supernova)

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
