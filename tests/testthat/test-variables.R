context("Variable extraction")

test_that("variables returns the variables in a model", {
  expect_identical(
    variables(lm(mpg ~ NULL, mtcars)),
    list(outcome = "mpg", predictor = character(0))
  )
  expect_identical(
    variables(lm(mpg ~ hp, mtcars)),
    list(outcome = "mpg", predictor = "hp")
  )
  expect_identical(
    variables(lm(mpg ~ hp + disp, mtcars)),
    list(outcome = "mpg", predictor = c("hp", "disp"))
  )
  expect_identical(
    variables(lm(mpg ~ hp * disp, mtcars)),
    list(outcome = "mpg", predictor = c("hp", "disp", "hp:disp"))
  )
  expect_identical(
    variables(lm(mpg + hp ~ disp, mtcars)),
    list(outcome = c("mpg", "hp"), predictor = c("disp"))
  )
})

test_that("variables works with bare formulae", {
  expect_identical(
    variables(mpg ~ NULL),
    list(outcome = "mpg", predictor = character(0))
  )
})

test_that("variables works with supernova object", {
  expect_identical(
    variables(supernova(lm(mpg ~ NULL, mtcars))),
    list(outcome = "mpg", predictor = character(0))
  )
})
