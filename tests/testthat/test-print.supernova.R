# Helper functions --------------------------------------------------------

get_data <- function(name) {
  prefix <- if (interactive()) "./tests/testthat/" else "./"
  readRDS(file.path(prefix, "data", paste0(name, ".Rds")))
}

fit_lmer <- function(formula, data) {
  lme4::lmer(
    formula,
    data = data,
    na.action = na.omit,
    subset = NULL,
    weights = NULL,
    offset = NULL
  )
}


# Tests -------------------------------------------------------------------

test_that("supernova tables object has explanatory header for lm", {
  model <- lm(mpg ~ hp * disp, data = mtcars)
  printed <- capture.output(supernova(model, type = 1))
  expect_identical(printed[1:2], c(
    " Analysis of Variance Table (Type I SS)",
    " Model: mpg ~ hp * disp"
  ))

  printed <- capture.output(supernova(model, type = 2))
  expect_identical(printed[[1]], " Analysis of Variance Table (Type II SS)")

  printed <- capture.output(supernova(model, type = 3))
  expect_identical(printed[[1]], " Analysis of Variance Table (Type III SS)")
})

test_that("columns have general ANOVA table names", {
  model <- lm(mpg ~ hp * disp, data = mtcars)
  printed <- capture.output(supernova(model, type = 1))
  expect_match(printed[[4]], "^\\s+SS\\s+df\\s+MS\\s+F\\s+PRE\\s+p$")
})

test_that("null model tables are beautifully formatted", {
  model <- lm(mpg ~ NULL, data = mtcars)
  printed <- capture.output(supernova(model))
  expect_identical(printed[4:9], c(
    "                               SS  df     MS   F PRE   p",
    " ----- --------------- | -------- --- ------ --- --- ---",
    " Model (error reduced) |      --- ---    --- --- --- ---",
    " Error (from model)    |      --- ---    --- --- --- ---",
    " ----- --------------- | -------- --- ------ --- --- ---",
    " Total (empty model)   | 1126.047  31 36.324            "
  ))
})

test_that("single predictor tables are beautifully formatted", {
  model <- lm(mpg ~ hp, data = mtcars)
  printed <- capture.output(supernova(model))
  expect_identical(printed[4:9], c(
    "                               SS df      MS      F    PRE     p",
    " ----- --------------- | -------- -- ------- ------ ------ -----",
    " Model (error reduced) |  678.373  1 678.373 45.460 0.6024 .0000",
    " Error (from model)    |  447.674 30  14.922                    ",
    " ----- --------------- | -------- -- ------- ------ ------ -----",
    " Total (empty model)   | 1126.047 31  36.324                    "
  ))
})

test_that("multiple predictor tables are beautifully formatted", {
  model <- lm(mpg ~ hp + disp, data = mtcars)
  printed <- capture.output(supernova(model))
  expect_identical(printed[4:11], c(
    "                               SS df      MS      F    PRE     p",
    " ----- --------------- | -------- -- ------- ------ ------ -----",
    " Model (error reduced) |  842.554  2 421.277 43.095 0.7482 .0000",
    "    hp                 |   33.665  1  33.665  3.444 0.1061 .0737",
    "  disp                 |  164.181  1 164.181 16.795 0.3667 .0003",
    " Error (from model)    |  283.493 29   9.776                    ",
    " ----- --------------- | -------- -- ------- ------ ------ -----",
    " Total (empty model)   | 1126.047 31  36.324                    "
  ))
})

test_that("non-verbose tables do not have a description column", {
  model <- lm(mpg ~ NULL, data = mtcars)
  printed <- capture.output(supernova(model, verbose = FALSE))
  expect_identical(printed[4:9], c(
    "               SS  df     MS   F PRE   p",
    " ----- | -------- --- ------ --- --- ---",
    " Model |      --- ---    --- --- --- ---",
    " Error |      --- ---    --- --- --- ---",
    " ----- | -------- --- ------ --- --- ---",
    " Total | 1126.047  31 36.324            "
  ))
})

test_that("nested repeated measures tables are beautifully formatted", {
  model <- fit_lmer(
    value ~ instructions + (1 | group),
    data = get_data("jmr_ex11.1")
  )

  printed <- capture.output(supernova(model))
  expect_identical(printed, c(
    " Analysis of Variance Table (Type III SS)",
    " Model: value ~ instructions + (1 | group)",
    "",
    "                              SS df     MS     F    PRE     p",
    " ---------------------- | ------ -- ------ ----- ------ -----",
    " Between Subjects       |                                    ",
    "   instructions         | 12.500  1 12.500 4.687 0.5396 .0963",
    " Error                  | 10.667  4  2.667                   ",
    " Total                  | 23.167  5  4.633                   ",
    " ---------------------- | ------ -- ------ ----- ------ -----",
    " Within Subjects        |                                    ",
    " Total                  |  5.333 12  0.444                   ",
    " ---------------------- | ------ -- ------ ----- ------ -----",
    " Total                  | 28.500 17  1.676                   "
  ))
})

test_that("crossed repeated measures tables are beautifully formatted", {
  model <- fit_lmer(
    rating ~ sex * yearsmarried * children + (1 | couple),
    data = get_data("jmr_ex11.22")
  )

  printed <- capture.output(supernova(model))
  expect_identical(printed, c(
    " Analysis of Variance Table (Type III SS)",
    " Model: rating ~ sex * yearsmarried * children + (1 | couple)",
    "",
    "                                   SS df     MS     F    PRE     p",
    " --------------------------- | ------ -- ------ ----- ------ -----",
    " Between Subjects            |                                    ",
    "   yearsmarried              | 10.125  1 10.125 9.529 0.4426 .0094",
    "   children                  |  0.500  1  0.500 0.471 0.0377 .5058",
    "   yearsmarried:children     | 10.125  1 10.125 9.529 0.4426 .0094",
    " Error                       | 12.750 12  1.063                   ",
    " Total                       | 33.500 15  2.233                   ",
    " --------------------------- | ------ -- ------ ----- ------ -----",
    " Within Subjects             |                                    ",
    "   sex                       |  3.125  1  3.125 8.824 0.4237 .0117",
    "   sex:yearsmarried          |  2.000  1  2.000 5.647 0.3200 .0350",
    "   sex:children              |  0.125  1  0.125 0.353 0.0286 .5635",
    "   sex:yearsmarried:children |  0.500  1  0.500 1.412 0.1053 .2577",
    " Error                       |  4.250 12  0.354                   ",
    " Total                       | 10.000 16  0.625                   ",
    " --------------------------- | ------ -- ------ ----- ------ -----",
    " Total                       | 43.500 31  1.403                   "
  ))
})
