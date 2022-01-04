
test_model <- lm(mpg ~ hp, data = mtcars)
estimate_funs <- c(b0, b1, f, pre, sse, ssm, ssr)

test_that("extracted values are all single element numeric vectors", {
  withr::local_options(lifecycle_verbosity = "quiet")
  purrr::map(estimate_funs, function(f) {
    expect_vector(f(test_model), double(), 1L)
  })
})

test_that("values can be extracted from fitted lm or formula-and-data", {
  withr::local_options(lifecycle_verbosity = "quiet")
  purrr::map(estimate_funs, function(f) {
    expect_identical(f(formula(test_model), test_model$model), f(test_model))
  })
})

test_that("extracted values are correct", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_identical(b0(test_model), test_model$coefficients[[1]])
  expect_identical(b1(test_model), coefficients(test_model)[[2]])
  expect_identical(f(test_model), summary(test_model)$fstatistic[["value"]])
  expect_identical(pre(test_model), summary(test_model)$r.squared)
  expect_identical(sse(test_model), sum(resid(test_model)^2))

  ssr_expected <- sum((test_model$fitted.values - mean(test_model$model[[1]]))^2)
  expect_identical(ssr(test_model), ssr_expected)
  expect_identical(ssm(test_model), ssr(test_model))
})

test_that("the functions are deprecated", {
  estimate_aliases <- c(fVal, PRE, SSE, SSM, SSR)
  purrr::map(c(estimate_funs, estimate_aliases), function(f) {
    expect_snapshot(f(test_model))
  })
})
