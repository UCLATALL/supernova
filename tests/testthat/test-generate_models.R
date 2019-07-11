context("Generate models for different SS types")

library(dplyr)

test_formula <- y ~ a + b + c * a:b + a:c + b:c + a:b:c

test_that("generating Type 3 models emits a message regarding `anova()`", {
  expect_warning(generate_models(test_formula, 3))
})

test_that("generated models are organized with the key as the term", {
  expected <- c("a", "b", "c", "a:b", "a:c", "b:c", "a:b:c")
  expect_identical(names(type_1_models(test_formula)), expected)
  expect_identical(names(type_2_models(test_formula)), expected)
  expect_identical(names(type_3_models(test_formula)), expected)
})

test_that("sequential models are returned for Type 1", {
  expected <- list(
    augmented = list(
      `a`     = y ~                               a,
      `b`     = y ~ a +                           b,
      `c`     = y ~ a + b +                       c,
      `a:b`   = y ~ a + b + c +                   a:b,
      `a:c`   = y ~ a + b + c + a:b +             a:c,
      `b:c`   = y ~ a + b + c + a:b + a:c +       b:c,
      `a:b:c` = y ~ a + b + c + a:b + a:c + b:c + a:b:c
    ),
    compact = list(
      `a`     = y ~ NULL,
      `b`     = y ~ a,
      `c`     = y ~ a + b,
      `a:b`   = y ~ a + b + c,
      `a:c`   = y ~ a + b + c + a:b,
      `b:c`   = y ~ a + b + c + a:b + a:c,
      `a:b:c` = y ~ a + b + c + a:b + a:c + b:c
    )
  )
  expect_equal(generate_models(test_formula, type = 1), expected)
})

test_that("hierarchical models are returned for Type 2", {
  expected <- list(
    augmented = list(
      `a`     = y ~     b + c +             b:c + a,
      `b`     = y ~ a +     c +       a:c +       b,
      `c`     = y ~ a + b +     a:b +             c,
      `a:b`   = y ~ a + b + c +       a:c + b:c + a:b,
      `a:c`   = y ~ a + b + c + a:b +       b:c + a:c,
      `b:c`   = y ~ a + b + c + a:b + a:c +       b:c,
      `a:b:c` = y ~ a + b + c + a:b + a:c + b:c + a:b:c
    ),
    compact = list(
      `a`     = y ~     b + c +             b:c,
      `b`     = y ~ a +     c +       a:c,
      `c`     = y ~ a + b +     a:b,
      `a:b`   = y ~ a + b + c +       a:c + b:c,
      `a:c`   = y ~ a + b + c + a:b +       b:c,
      `b:c`   = y ~ a + b + c + a:b + a:c,
      `a:b:c` = y ~ a + b + c + a:b + a:c + b:c
    )
  )
  expect_equal(generate_models(test_formula, type = 2), expected)
})

test_that("orthogonal models are returned for Type 3", {
  expected <- list(
    augmented = list(
      `a`     = y ~     b + c + a:b + a:c + b:c + a:b:c + a,
      `b`     = y ~ a +     c + a:b + a:c + b:c + a:b:c + b,
      `c`     = y ~ a + b +     a:b + a:c + b:c + a:b:c + c,
      `a:b`   = y ~ a + b + c +       a:c + b:c + a:b:c + a:b,
      `a:c`   = y ~ a + b + c + a:b +       b:c + a:b:c + a:c,
      `b:c`   = y ~ a + b + c + a:b + a:c +       a:b:c + b:c,
      `a:b:c` = y ~ a + b + c + a:b + a:c + b:c +         a:b:c
    ),
    compact = list(
      `a`     = y ~     b + c + a:b + a:c + b:c + a:b:c,
      `b`     = y ~ a +     c + a:b + a:c + b:c + a:b:c,
      `c`     = y ~ a + b +     a:b + a:c + b:c + a:b:c,
      `a:b`   = y ~ a + b + c +       a:c + b:c + a:b:c,
      `a:c`   = y ~ a + b + c + a:b +       b:c + a:b:c,
      `b:c`   = y ~ a + b + c + a:b + a:c +       a:b:c,
      `a:b:c` = y ~ a + b + c + a:b + a:c + b:c
    )
  )
  suppressWarnings(generate_models(test_formula, type = 3)) %>%
    expect_equal(expected)
})

test_that("model class is preserved", {
  model <- lm(mpg ~ hp * disp, data = mtcars)
  expected <- list(
    augmented = list(
      `hp`      = supernova:::update(model, mpg ~ hp),
      `disp`    = supernova:::update(model, mpg ~ hp + disp),
      `hp:disp` = supernova:::update(model, mpg ~ hp + disp + hp:disp)
    ),
    compact = list(
      `hp`      = supernova:::update(model, mpg ~ NULL),
      `disp`    = supernova:::update(model, mpg ~ hp),
      `hp:disp` = supernova:::update(model, mpg ~ hp + disp)
    )
  )
  expect_equal(generate_models(model, type = 1), expected)
})
