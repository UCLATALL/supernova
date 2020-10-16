context("Generate models for different SS types")

test_formula <- y ~ a * b * c
test_terms <- c("Full Model", "a", "b", "c", "a:b", "a:c", "b:c", "a:b:c")

expect_attr <- function(obj, attribute, expected) {
  expect_equal(attr(obj, attribute), expected)
}

test_that("model has attribute for type specification", {
  expect_attr(generate_models(test_formula, 1), "type", "I")
  expect_attr(generate_models(test_formula, 2), "type", "II")
  suppressWarnings(
    expect_attr(generate_models(test_formula, 3), "type", "III")
  )
})

test_that("model has attribute for model specification", {
  expect_attr(generate_models(test_formula, 1), "model", y ~ a * b * c)
})

test_that("generating Type 3 models emits a message regarding `anova()`", {
  expect_warning(generate_models(test_formula, 3))
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
  expect_equivalent(generate_models(test_formula, type = 1), expected)
})

test_that("hierarchical models are returned for Type 2", {
  expected <- list(
    `Full Model` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ NULL
    ),
    `a` = list(
      complex = y ~ a + b + c + b:c,
      simple  = y ~ b + c + b:c
    ),
    `b` = list(
      complex = y ~ a + b + c + a:c,
      simple  = y ~ a + c + a:c
    ),
    `c` = list(
      complex = y ~ a + b + c + a:b,
      simple  = y ~ a + b + a:b
    ),
    `a:b` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c,
      simple  = y ~ a + b + c + a:c + b:c
    ),
    `a:c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c,
      simple  = y ~ a + b + c + a:b + b:c
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
  expect_equivalent(generate_models(test_formula, type = 2), expected)
})

test_that("orthogonal models are returned for Type 3", {
  expected <- list(
    `Full Model` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ NULL
    ),
    `a` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple  = y ~ b + c + a:b + a:c + b:c + a:b:c
    ),
    `b` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple  = y ~ a + c + a:b + a:c + b:c + a:b:c
    ),
    `c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple  = y ~ a + b + a:b + a:c + b:c + a:b:c
    ),
    `a:b` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple  = y ~ a + b + c + a:c + b:c + a:b:c
    ),
    `a:c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple  = y ~ a + b + c + a:b + b:c + a:b:c
    ),
    `b:c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple  = y ~ a + b + c + a:b + a:c + a:b:c
    ),
    `a:b:c` = list(
      complex = y ~ a + b + c + a:b + a:c + b:c + a:b:c,
      simple = y ~ a + b + c + a:b + a:c + b:c
    )
  )
  suppressWarnings(generate_models(test_formula, type = 3)) %>%
    expect_equivalent(expected)
})

test_that("model class is preserved", {
  model <- lm(mpg ~ hp * disp, data = mtcars)
  expected <- list(
    `Full Model` = list(
      complex = supernova:::update(model, mpg ~ hp + disp + hp:disp),
      simple = supernova:::update(model, mpg ~ NULL)
    ),
    `hp` = list(
      complex = supernova:::update(model, mpg ~ hp),
      simple = supernova:::update(model, mpg ~ NULL)
    ),
    `disp` = list(
      complex = supernova:::update(model, mpg ~ hp + disp),
      simple = supernova:::update(model, mpg ~ hp)
    ),
    `hp:disp` = list(
      complex = supernova:::update(model, mpg ~ hp + disp + hp:disp),
      simple = supernova:::update(model, mpg ~ hp + disp)
    )
  )
  expect_equivalent(generate_models(model, type = 1), expected)
})

test_that("two-way models work", {
  model <- lm(mpg ~ hp * disp, data = mtcars)
  expected <- list(
    `Full Model` = list(
      complex = supernova:::update(model, mpg ~ hp + disp + hp:disp),
      simple = supernova:::update(model, mpg ~ NULL)
    ),
    `hp` = list(
      complex = supernova:::update(model, mpg ~ hp),
      simple = supernova:::update(model, mpg ~ NULL)
    ),
    `disp` = list(
      complex = supernova:::update(model, mpg ~ hp + disp),
      simple = supernova:::update(model, mpg ~ hp)
    ),
    `hp:disp` = list(
      complex = supernova:::update(model, mpg ~ hp + disp + hp:disp),
      simple = supernova:::update(model, mpg ~ hp + disp)
    )
  )
  expect_equivalent(generate_models(model, type = 1), expected)
})

test_that("one-way models work", {
  model <- lm(mpg ~ hp, data = mtcars)
  expected <- list(
    `Full Model` = list(
      complex = supernova:::update(model, mpg ~ hp),
      simple = supernova:::update(model, mpg ~ NULL)
    ),
    `hp` = list(
      complex = supernova:::update(model, mpg ~ hp),
      simple = supernova:::update(model, mpg ~ NULL)
    )
  )
  expect_equivalent(generate_models(model, type = 1), expected)
})

test_that("null models return NULL", {
  model <- lm(mpg ~ NULL, data = mtcars)
  expect_equivalent(generate_models(model, type = 1), list())
})


# Printing ----------------------------------------------------------------

test_that("model object has explanatory header", {
  printed <- capture.output(generate_models(test_formula, type = 1))
  expect_identical(printed[1:3], c(
    "Comparison Models for Type I SS",
    "Model: y ~ a * b * c",
    ""
  ))

  model <- lm(mpg ~ hp * disp, mtcars)
  printed <- capture.output(generate_models(model, type = 1))
  expect_identical(printed[[2]], "Model: mpg ~ hp * disp")

  model <- lm(mpg ~ NULL, mtcars)
  printed <- capture.output(generate_models(model, type = 1))
  expect_identical(printed[2:4], c(
    "Model: mpg ~ NULL",
    "",
    "No comparisons for empty model."
  ))
})

test_that("model object has human readable printing", {
  short_model <- y ~ a * b
  printed <- capture.output(generate_models(short_model, type = 1))
  expect_identical(printed[4:15], c(
    "Full Model",
    "  complex: y ~ a + b + a:b",
    "   simple: y ~ NULL",
    "a",
    "  complex: y ~ a",
    "   simple: y ~ NULL",
    "b",
    "  complex: y ~ a + b",
    "   simple: y ~ a",
    "a:b",
    "  complex: y ~ a + b + a:b",
    "   simple: y ~ a + b"
  ))
})

test_that("type II models have spaces for removed terms", {
  printed <- capture.output(generate_models(y ~ a * b, type = 2))
  expect_identical(printed[7:15], c(
    "a",
    "  complex: y ~ a + b",
    "   simple: y ~     b",
    "b",
    "  complex: y ~ a + b",
    "   simple: y ~ a",
    "a:b",
    "  complex: y ~ a + b + a:b",
    "   simple: y ~ a + b"
  ))
})

test_that("type III models have spaces for removed terms", {
  printed <- capture.output(suppressWarnings(generate_models(test_formula, type = 3)))
  expect_identical(printed[7:27], c(
    "a",
    "  complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c",
    "   simple: y ~     b + c + a:b + a:c + b:c + a:b:c",
    "b",
    "  complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c",
    "   simple: y ~ a +     c + a:b + a:c + b:c + a:b:c",
    "c",
    "  complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c",
    "   simple: y ~ a + b +     a:b + a:c + b:c + a:b:c",
    "a:b",
    "  complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c",
    "   simple: y ~ a + b + c +       a:c + b:c + a:b:c",
    "a:c",
    "  complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c",
    "   simple: y ~ a + b + c + a:b +       b:c + a:b:c",
    "b:c",
    "  complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c",
    "   simple: y ~ a + b + c + a:b + a:c +       a:b:c",
    "a:b:c",
    "  complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c",
    "   simple: y ~ a + b + c + a:b + a:c + b:c"
  ))
})

test_that("generate_models works with lm objects also", {
  model <- lm(mpg ~ hp * disp, mtcars)
  printed <- capture.output(suppressWarnings(generate_models(model, type = 3)))
  expect_identical(printed[4:15], c(
    "Full Model",
    "  complex: mpg ~ hp + disp + hp:disp",
    "   simple: mpg ~ NULL",
    "hp",
    "  complex: mpg ~ hp + disp + hp:disp",
    "   simple: mpg ~      disp + hp:disp",
    "disp",
    "  complex: mpg ~ hp + disp + hp:disp",
    "   simple: mpg ~ hp +        hp:disp",
    "hp:disp",
    "  complex: mpg ~ hp + disp + hp:disp",
    "   simple: mpg ~ hp + disp"
  ))
})
