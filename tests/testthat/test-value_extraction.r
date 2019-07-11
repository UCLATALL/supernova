context("Value extraction")

test_model <- lm(mpg ~ hp, data = mtcars)
estimate_funs <- c(b0, b1, fVal, PRE, SSE, SSM, SSR)

test_that("extracted values are all single element numeric vectors", {
  purrr::map(estimate_funs, function(f) {
    f(test_model) %>%
      expect_length(1) %>%
      expect_is("numeric")
  })
})

test_that("values can be extracted from fitted lm or formula-and-data", {
  purrr::map(estimate_funs, function(f) {
    expect_identical(f(formula(test_model), test_model$model), f(test_model))
  })
})

test_that("extracted values are correct", {
  expect_identical(b0(test_model), test_model$coefficients[[1]])
  expect_identical(b1(test_model), coefficients(test_model)[[2]])
  expect_identical(fVal(test_model), summary(test_model)$fstatistic[["value"]])
  expect_identical(PRE(test_model), summary(test_model)$r.squared)
  expect_identical(SSE(test_model), sum(resid(test_model) ^ 2))

  ssr_expected <- sum((test_model$fitted.values - mean(test_model$model[[1]]))^2)
  expect_identical(SSR(test_model), ssr_expected)
  expect_identical(SSM(test_model), SSR(test_model))
})
