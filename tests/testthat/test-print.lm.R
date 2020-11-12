context("Augmenting print.lm")



# Notes -------------------------------------------------------------------


# No notes. Pretty straightforward.



# Tests -------------------------------------------------------------------


test_that("standard output is not altered", {
  model <- lm(mpg ~ cyl, data = mtcars)
  standard <- capture.output(stats:::print.lm(model))
  printed <- capture.output(model)
  expect_identical(printed[1:8], standard)
})


test_that("return value is not altered", {
  model <- lm(mpg ~ cyl, data = mtcars)

  con <- textConnection("errors", open="w")
  sink(con); value <- print.lm(model); sink()

  expect_identical(model, value)
})


test_that("fitted equation is added below standard output", {
  model <- lm(mpg ~ cyl, data = mtcars)
  standard <- capture.output(stats:::print.lm(model))
  printed <- capture.output(model)
  new_part <- printed[(length(standard) + 1):length(printed)]
  expect_identical(new_part, c(
    "Fitted equation:",
    "mpg = 37.88458 + -2.87579*cyl + e",
    ""))
})


test_that("fitted equation works when extracting variables instead of data =", {
  model <- lm(mtcars$mpg ~ mtcars$cyl, data = mtcars)
  standard <- capture.output(stats:::print.lm(model))
  printed <- capture.output(model)
  new_part <- printed[(length(standard) + 1):length(printed)]
  expect_identical(new_part, c(
    "Fitted equation:",
    "mtcars$mpg = 37.88458 + -2.87579*mtcars$cyl + e",
    ""))
})


test_that("fitted equation works with factors", {
  model <- lm(mtcars$mpg ~ factor(cyl), data = mtcars)
  standard <- capture.output(stats:::print.lm(model))
  printed <- capture.output(model)
  new_part <- printed[(length(standard) + 1):length(printed)]
  expect_identical(new_part, c(
    "Fitted equation:",
    "mtcars$mpg = 26.66364 + -6.920779*factor(cyl)6 + -11.56364*factor(cyl)8 + e",
    ""))
})


test_that("fitted equation works with factors when extracting instead of data =", {
  model <- lm(mtcars$mpg ~ factor(mtcars$cyl))
  standard <- capture.output(stats:::print.lm(model))
  printed <- capture.output(model)
  new_part <- printed[(length(standard) + 1):length(printed)]
  expect_equal(new_part, c(
    "Fitted equation:",
    "mtcars$mpg = 26.66364 + -6.920779*factor(mtcars$cyl)6 + -11.56364*factor(mtcars$cyl)8 + e",
    ""))
})
