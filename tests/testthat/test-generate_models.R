
# Helpers -------------------------------------------------------------------------------------

test_formula <- y ~ a * b * c
test_terms <- c("Full Model", "a", "b", "c", "a:b", "a:c", "b:c", "a:b:c")

remove_attributes <- function(obj) {
  attr(obj, "type") <- NULL
  attr(obj, "model") <- NULL
  attr(obj, "class") <- NULL
  obj
}


# Tests ---------------------------------------------------------------------------------------

test_that("model has attributes for type and model specifications", {
  purrr::walk(1:3, function(type) {
    actual <- generate_models(test_formula, type)
    expect_equal(attr(actual, "type"), paste0(rep("I", type), collapse = ""))
    expect_equal(attr(actual, "model"), test_formula)
  })
})

test_that("null models return empty list", {
  model <- lm(mpg ~ NULL, data = mtcars)
  expect_length(generate_models(model, type = 1), 0)
})

test_that("generated models are organized with the key as the term", {
  generate_models(test_formula, 1) %>%
    expect_length(length(test_terms)) %>%
    expect_named(test_terms)
})

test_that("each term has a key for each of its complex and simple models", {
  generate_models(test_formula, 1) %>%
    purrr::flatten() %>%
    expect_named(rep(c("complex", "simple"), times = length(test_terms)))
})

test_that("the returned formulae are in the same environment as the given formulae", {
  expect_identical(
    environment(generate_models(test_formula, 1)$`Full Model`$complex),
    environment(test_formula)
  )
  expect_identical(
    environment(generate_models(test_formula, 2)$`Full Model`$complex),
    environment(test_formula)
  )
  expect_identical(
    environment(generate_models(test_formula, 3)$`Full Model`$complex),
    environment(test_formula)
  )
})

test_that("sequential models are returned for Type 1", {
  expected <- list(
    `Full Model` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ NULL
    ),
    `a` = list(
      complex = y ~ a,
      simple = y ~ NULL
    ),
    `b` = list(
      complex = y ~ a + b,
      simple = y ~ a
    ),
    `c` = list(
      complex = y ~ a + b + c,
      simple = y ~ a + b
    ),
    `a:b` = list(
      complex = y ~ a + b + c + a:b,
      simple = y ~ a + b + c
    ),
    `a:c` = list(
      complex = y ~ a + b + c + a:b + a:c,
      simple = y ~ a + b + c + a:b
    ),
    `b:c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c,
      simple = y ~ a + b + c + a:b + a:c
    ),
    `a:b:c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ a + b + c + a:b + a:c + b:c
    )
  )

  actual <- generate_models(test_formula, type = 1)
  expect_equal(actual, expected, ignore_attr = TRUE)
})

test_that("hierarchical models are returned for Type 2", {
  expected <- list(
    `Full Model` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ NULL
    ),
    `a` = list(
      complex = y ~ a + b + c + b:c,
      simple = y ~ b + c + b:c
    ),
    `b` = list(
      complex = y ~ a + b + c + a:c,
      simple = y ~ a + c + a:c
    ),
    `c` = list(
      complex = y ~ a + b + c + a:b,
      simple = y ~ a + b + a:b
    ),
    `a:b` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c,
      simple = y ~ a + b + c + a:c + b:c
    ),
    `a:c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c,
      simple = y ~ a + b + c + a:b + b:c
    ),
    `b:c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c,
      simple = y ~ a + b + c + a:b + a:c
    ),
    `a:b:c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ a + b + c + a:b + a:c + b:c
    )
  )

  actual <- generate_models(test_formula, type = 2)
  expect_equal(actual, expected, ignore_attr = TRUE)
})

test_that("orthogonal models are returned for Type 3", {
  expected <- list(
    `Full Model` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ NULL
    ),
    `a` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ b + c + a:b + a:c + b:c + a:b:c
    ),
    `b` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ a + c + a:b + a:c + b:c + a:b:c
    ),
    `c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ a + b + a:b + a:c + b:c + a:b:c
    ),
    `a:b` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ a + b + c + a:c + b:c + a:b:c
    ),
    `a:c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ a + b + c + a:b + b:c + a:b:c
    ),
    `b:c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ a + b + c + a:b + a:c + a:b:c
    ),
    `a:b:c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ a + b + c + a:b + a:c + b:c
    )
  )

  actual <- generate_models(test_formula, type = 3)
  expect_equal(actual, expected, ignore_attr = TRUE)
})

test_that("linear models are refit with the resulting formulae", {
  # this test depends on data being complete (no missing data)
  # missing data is tested next
  model <- lm(mpg ~ hp + disp, data = mtcars)
  expected <- list(
    `Full Model` = list(
      complex = lm(mpg ~ hp + disp, data = mtcars),
      simple = lm(mpg ~ 1, data = mtcars)
    ),
    `hp` = list(
      complex = lm(mpg ~ hp, data = mtcars),
      simple = lm(mpg ~ 1, data = mtcars)
    ),
    `disp` = list(
      complex = lm(mpg ~ hp + disp, data = mtcars),
      simple = lm(mpg ~ hp, data = mtcars)
    )
  )

  actual <- generate_models(model, type = 1)
  expect_equal(actual, expected, ignore_attr = TRUE)
})

test_that("type 3 models properly exclude lower-level terms", {
  model <- lm(mpg ~ hp * disp, data = mtcars)
  models <- generate_models(model, type = 3)
  expect_false("hp" %in% names(coefficients(models$hp$simple)))
})

test_that("the updated models remove missing cases according to the largest model", {
  df_missing <- mtcars
  df_missing[1, ]$hp <- NA_real_
  df_missing[2:3, ]$disp <- NA_real_
  model <- lm(mpg ~ hp * disp, data = df_missing)

  models <- suppressMessages(generate_models(model, type = 1))
  most_complex <- models$`Full Model`$complex
  least_complex <- models$`Full Model`$simple
  middle_complex <- models$disp$complex

  expect_equal(nrow(most_complex$model), nrow(model$model))
  expect_equal(nrow(least_complex$model), nrow(model$model))
  expect_equal(nrow(middle_complex$model), nrow(model$model))
})

test_that("it prints nicely for supported models", {
  expect_snapshot(generate_models(test_formula, type = 1))
  expect_snapshot(generate_models(test_formula, type = 2))
  expect_snapshot(generate_models(test_formula, type = 3))

  test_lm <- lm(mpg ~ hp * disp, data = mtcars)
  expect_snapshot(generate_models(test_lm, type = 1))
  expect_snapshot(generate_models(test_lm, type = 2))
  expect_snapshot(generate_models(test_lm, type = 3))
})
