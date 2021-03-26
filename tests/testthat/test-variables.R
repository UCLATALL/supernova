
# Helper functions --------------------------------------------------------


get_data <- function(name) {
  prefix <- if (interactive()) "./tests/testthat/" else "./"
  readRDS(file.path(prefix, "data", paste0(name, ".Rds")))
}


fit_lmer <- function(frm, data) {
  lme4::lmer(frm, data = data, na.action = na.omit, subset = NULL, weights = NULL, offset = NULL)
}



# General tests -----------------------------------------------------------


test_that("variables are extracted from bare formulae", {
  expect_identical(variables(mpg ~ hp), list(
    outcome = "mpg",
    predictor = "hp",
    group = character(0),
    within = character(0),
    between = character(0)
  ))
})


test_that("variables are extracted from compact formulae", {
  expect_identical(variables(mpg ~ hp * disp), list(
    outcome = "mpg",
    predictor = c("hp", "disp", "hp:disp"),
    group = character(0),
    within = character(0),
    between = character(0)
  ))
})


test_that("variables are extracted from lm objects with data =", {
  expect_identical(variables(lm(mpg ~ hp, data = mtcars)), list(
    outcome = "mpg",
    predictor = "hp",
    group = character(0),
    within = character(0),
    between = "hp"
  ))
})


test_that("variables are extracted from lm objects where variables are extracted from data frame", {
  expect_identical(variables(lm(mtcars$mpg ~ mtcars$hp)), list(
    outcome = "mtcars$mpg",
    predictor = "mtcars$hp",
    group = character(0),
    within = character(0),
    between = "mtcars$hp"
  ))
})


test_that("variables are extracted from supernova objects", {
  expect_identical(variables(supernova(lm(mpg ~ hp, data = mtcars))), list(
    outcome = "mpg",
    predictor = "hp",
    group = character(0),
    within = character(0),
    between = "hp"
  ))
})



# Specific types of models ------------------------------------------------


test_that("variables are extracted from null models", {
  expect_identical(variables(lm(mpg ~ NULL, data = mtcars)), list(
    outcome = "mpg",
    predictor = character(0),
    group = character(0),
    within = character(0),
    between = character(0)
  ))

  expect_identical(variables(lm(mtcars$mpg ~ NULL)), list(
    outcome = "mtcars$mpg",
    predictor = character(0),
    group = character(0),
    within = character(0),
    between = character(0)
  ))
})


test_that("variables are extracted from one-way between models", {
  expect_identical(
    variables(lm(mpg ~ hp, data = mtcars)),
    list(
      outcome = "mpg",
      predictor = "hp",
      group = character(0),
      within = character(0),
      between = "hp"
    )
  )
})


test_that("variables are extracted from complex between models with interactions", {
  expect_identical(
    variables(lm(mpg ~ hp * disp, data = mtcars)),
    list(
      outcome = "mpg",
      predictor = c("hp", "disp", "hp:disp"),
      group = character(0),
      within = character(0),
      between = c("hp", "disp", "hp:disp")
    )
  )
})


test_that("variables are extracted from simple nested models", {
  model <- fit_lmer(value ~ instructions + (1 | group), data = get_data("jmr_ex11.1"))
  expect_identical(
    variables(model),
    list(
      outcome = "value",
      predictor = "instructions",
      group = "group",
      within = character(0),
      between = "instructions"
    )
  )
})


test_that("variables are extracted from simple crossed models", {
  model <- fit_lmer(puzzles_completed ~ condition + (1 | subject), data = get_data("jmr_ex11.9"))
  expect_identical(
    variables(model),
    list(
      outcome = "puzzles_completed",
      predictor = "condition",
      group = "subject",
      within = "condition",
      between = character(0)
    )
  )
})


test_that("variables are extracted from simple crossed models with interactions", {
  model <- fit_lmer(recall ~ type * time + (1 | subject), data = get_data("jmr_ex11.17"))
  expect_identical(
    variables(model),
    list(
      outcome = "recall",
      predictor = c("type", "time", "type:time"),
      group = "subject",
      within = c("type", "time", "type:time"),
      between = character(0)
    )
  )
})


test_that("variables are extracted from models with multiple crossed variables", {
  model <- fit_lmer(
    recall ~ time * type + (1 | subject) + (1 | time:subject) + (1 | type:subject),
    data = get_data("jmr_ex11.17")
  )
  expect_identical(
    variables(model),
    list(
      outcome = "recall",
      predictor = c("time", "type", "time:type"),
      group = "subject",
      within = c("time", "type", "time:type"),
      between = character(0)
    )
  )
})


test_that("variables are extracted from mixed models with interactions", {
  model <- fit_lmer(
    rating ~ sex * yearsmarried * children + (1 | couple),
    data = get_data("jmr_ex11.22")
  )
  expect_identical(
    variables(model),
    list(
      outcome = "rating",
      predictor = c(
        "sex", "yearsmarried", "children",
        "sex:yearsmarried", "sex:children", "yearsmarried:children",
        "sex:yearsmarried:children"
      ),
      group = "couple",
      within = c(
        "sex",
        "sex:yearsmarried", "sex:children",
        "sex:yearsmarried:children"
      ),
      between = c(
        "yearsmarried", "children",
        "yearsmarried:children"
      )
    )
  )
})
