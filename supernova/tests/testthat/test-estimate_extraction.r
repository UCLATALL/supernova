context("Estimate extraction")
library(supernova)

test_that("b0 of model is the intercept / first coefficient", {
  regression_model <- lm(mpg ~ hp, data = mtcars)
  estimate <- b0(regression_model)

  expect_is(estimate, "numeric")
  expect_length(estimate, 1)
  expect_equal(estimate, coefficients(regression_model)[[1]])
  expect_equal(b0(formula(regression_model), data = mtcars),
               coefficients(regression_model)[[1]])
})

test_that("b1 of model is the slope / second coefficient", {
  regression_model <- lm(mpg ~ hp, data = mtcars)
  estimate <- b1(regression_model)

  expect_is(estimate, "numeric")
  expect_length(estimate, 1)
  expect_equal(estimate, coefficients(regression_model)[[2]])
  expect_equal(b1(formula(regression_model), data = mtcars),
               coefficients(regression_model)[[2]])
})

test_that("fVal of model is the F value", {
  regression_model <- lm(mpg ~ hp, data = mtcars)
  estimate <- fVal(regression_model)

  expect_is(estimate, "numeric")
  expect_length(estimate, 1)
  expect_equal(estimate, summary(regression_model)$fstatistic[["value"]])
  expect_equal(fVal(formula(regression_model), data = mtcars),
               summary(regression_model)$fstatistic[["value"]])
})

test_that("PRE of model is the proportion of variance explained (r^2)", {
  regression_model <- lm(mpg ~ hp, data = mtcars)
  estimate <- PRE(regression_model)

  expect_is(estimate, "numeric")
  expect_length(estimate, 1)
  expect_equal(estimate, summary(regression_model)$r.squared)
  expect_equal(PRE(formula(regression_model), data = mtcars),
               summary(regression_model)$r.squared)
})
