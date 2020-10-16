context("Variable extraction")

get_data <- function(name) {
  prefix <- if (interactive()) "./tests/testthat/" else "./"
  readRDS(file.path(prefix, "cache", name))
}

test_that("variables can extract from null model", {
  model <- lm(mpg ~ NULL, data = mtcars)
  expect_identical(
    variables(model),
    list(
      outcome = "mpg",
      predictor = character(0),
      group = character(0),
      within = character(0),
      between = character(0)
    )
  )
})

test_that("variables can extract from bare formulae", {
  model <- mpg ~ NULL
  expect_identical(
    variables(model),
    list(
      outcome = "mpg",
      predictor = character(0),
      group = character(0),
      within = character(0),
      between = character(0)
    )
  )
})

test_that("variables can extract from supernova object", {
  model <- supernova(lm(mpg ~ NULL, mtcars))
  expect_identical(
    variables(model),
    list(
      outcome = "mpg",
      predictor = character(0),
      group = character(0),
      within = character(0),
      between = character(0)
    )
  )
})

test_that("variables can extract from simple between model", {
  model <- lm(mpg ~ hp, data = mtcars)
  expect_identical(
    variables(model),
    list(
      outcome = "mpg",
      predictor = "hp",
      group = character(0),
      within = character(0),
      between = "hp"
    )
  )
})

test_that("variables can extract from multiple between model", {
  model <- lm(mpg ~ hp * disp, data = mtcars)
  expect_identical(
    variables(model),
    list(
      outcome = "mpg",
      predictor = c("hp", "disp", "hp:disp"),
      group = character(0),
      within = character(0),
      between = c("hp", "disp", "hp:disp")
    )
  )
})

test_that("variables can extract from simple nested model", {
  model <- lme4::lmer(
    value ~ instructions + (1 | group),
    data = get_data("jmr_ex11.1.Rds")
  )
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

test_that("variables can extract from simple crossed model", {
  model <- lme4::lmer(
    puzzles_completed ~ condition + (1 | subject),
    data = get_data("jmr_ex11.9.Rds")
  )
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

test_that("variables can extract from complex crossed model", {
  model <- lme4::lmer(
    recall ~ type * time + (1 | subject),
    data = get_data("jmr_ex11.17.Rds")
  )
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

test_that("variables can extract from mixed model", {
  model <- lme4::lmer(
    rating ~ sex * yearsmarried * children + (1 | couple),
    data = get_data("jmr_ex11.22.Rds"),
  )
  expect_identical(
    variables(model),
    list(
      outcome = "rating",
      predictor = c(
        "sex", "yearsmarried", "children", "sex:yearsmarried", "sex:children",
        "yearsmarried:children", "sex:yearsmarried:children"
      ),
      group = "couple",
      within = c(
        "sex", "sex:yearsmarried", "sex:children", "sex:yearsmarried:children"
      ),
      between = c("yearsmarried", "children", "yearsmarried:children")
    )
  )
})
