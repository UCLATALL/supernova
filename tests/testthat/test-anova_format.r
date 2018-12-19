context("ANOVA formatting")
library(supernova)


# Helpers -----------------------------------------------------------------

dashes <- function(data_col, decimals = 0) {
  strrep("-", max(nchar(round(data_col, 0)), na.rm = TRUE) + if (decimals > 0 ) 1 + decimals else 0)
}
pat_3_dig <- "\\d+\\.\\d{3}"
pat_int <- "\\d+"
pat_PRE <- "0\\.\\d{4}"
pat_p <- "\\.\\d{4}$"
pat_model_end <- paste(pat_3_dig, pat_int, pat_3_dig, pat_3_dig, pat_PRE, pat_p, sep = "\\s+")
pat_model <- paste("^Model (error reduced) |", pat_model_end, sep = "\\s+")
pat_et <- paste(pat_3_dig, pat_int, pat_3_dig, "$", sep = "\\s+")
pat_error <- paste("^Error (from model)    |", pat_et, sep = "\\s+")
pat_total <- paste("^Total (empty model)   |", pat_et, sep = "\\s+")


# Tests -------------------------------------------------------------------

test_that("null model tables are beautifully formatted", {
  model <- lm(mpg ~ NULL, data = mtcars)
  printed <- capture.output(supernova(model))
  
  s_tbl <- supernova(model)$tbl
  outcome_variable <- all.vars(formula(model))[[1]]
  horizontal_rule <- paste(" -----", "-----------------", dashes(s_tbl$SS, 3), 
                           "---", dashes(s_tbl$MS, 3), "--- --- ---")
  
  expect_length(printed, 4 + 1 + 3 + 2)  # header, colnames, rows, rules
  expect_match(printed[[1]], "Analysis of Variance Table")
  expect_match(printed[[2]], paste("Outcome variable:", outcome_variable))
  expect_match(printed[[3]], paste("Model:", deparse(formula(model))), fixed = TRUE)
  expect_identical(printed[[4]], paste(""))
  expect_match(printed[[5]], "^\\s+SS\\s+df\\s+MS\\s+F\\s+PRE\\s+p$")
  expect_identical(printed[[6]], horizontal_rule)
  expect_match(printed[[7]], "^Model (error reduced) |(\\s+---){3}$")
  expect_match(printed[[8]], "^Error (from model)    |(\\s+---){3}$")
  expect_identical(printed[[9]], horizontal_rule)
  expect_match(printed[[10]], pat_total)
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
  expect_match(printed[[2]], paste("Outcome variable:", outcome_variable))
  expect_match(printed[[3]], paste("Model:", deparse(formula(model))), fixed = TRUE)
  expect_identical(printed[[4]], paste(""))
  expect_match(printed[[5]], "^\\s+SS\\s+df\\s+MS\\s+F\\s+PRE\\s+p$")
  expect_identical(printed[[6]], horizontal_rule)
  expect_match(printed[[7]], pat_model)
  expect_match(printed[[8]], pat_error)
  expect_identical(printed[[9]], horizontal_rule)
  expect_match(printed[[10]], pat_total)
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
  expect_match(printed[[2]], paste("Outcome variable:", outcome_variable))
  expect_match(printed[[3]], paste("Model:", deparse(formula(model))), fixed = TRUE)
  expect_identical(printed[[4]], paste(""))
  expect_match(printed[[5]], "^\\s+SS\\s+df\\s+MS\\s+F\\s+PRE\\s+p$")
  expect_identical(printed[[6]], horizontal_rule)
  expect_match(printed[[7]], pat_model)
  expect_match(printed[[8]], paste("^", predictors[[1]], "|", pat_model_end, sep = "\\s+"))
  expect_match(printed[[9]], paste("^", predictors[[2]], "|", pat_model_end, sep = "\\s+"))
  expect_match(printed[[10]], pat_error)
  expect_identical(printed[[11]], horizontal_rule)
  expect_match(printed[[12]], pat_total)
})
