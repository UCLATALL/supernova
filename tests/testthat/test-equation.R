context("Extracting the fitted equation")



# Notes -------------------------------------------------------------------


# No notes. Pretty straightforward.



# Tests -------------------------------------------------------------------


test_that("fitted equation adds the coefficients and error term to the call formula", {
  model <- lm(mpg ~ cyl, data = mtcars)
  expect_output(equation(model), fixed = TRUE, regexp = paste(
    sep = "\n",
    "Fitted equation:",
    "mpg = 37.88458 + -2.87579*cyl + e",
    ""
  ))
})


test_that("fitted equation works when extracting variables instead of data =", {
  model <- lm(mtcars$mpg ~ mtcars$cyl, data = mtcars)
  expect_output(equation(model), fixed = TRUE, regexp = paste(
    sep = "\n",
    "Fitted equation:",
    "mtcars$mpg = 37.88458 + -2.87579*mtcars$cyl + e",
    ""
  ))
})


test_that("fitted equation works with factors", {
  model <- lm(mtcars$mpg ~ factor(cyl), data = mtcars)
  expect_output(equation(model), fixed = TRUE, regexp = paste(
    sep = "\n",
    "Fitted equation:",
    "mtcars$mpg = 26.66364 + -6.920779*factor(cyl)6 + -11.56364*factor(cyl)8 + e",
    ""
  ))
})


test_that("fitted equation works with factors when extracting instead of data =", {
  model <- lm(mtcars$mpg ~ factor(mtcars$cyl))
  expect_output(equation(model), fixed = TRUE, regexp = paste(
    sep = "\n",
    "Fitted equation:",
    "mtcars$mpg = 26.66364 + -6.920779*factor(mtcars$cyl)6 + -11.56364*factor(mtcars$cyl)8 + e",
    ""
  ))
})
