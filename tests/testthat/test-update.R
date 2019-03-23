context("Custom update function")

get_data_with_missing <- function() {
  df.missing <- mtcars
  df.missing[1,]$hp <- NA_real_
  df.missing[2:3,]$disp <- NA_real_
  return(df.missing)
}

test_that("update() inherits na.action from lm() fit", {
  update <- supernova:::update

  # no missing data
  model <- lm(mpg ~ hp * disp, mtcars)
  updated <- update(model, . ~ NULL)
  expect_length(resid(updated), length(resid(model)))

  # missing data
  df.missing <- get_data_with_missing()

  # na.omit (default)
  model <- lm(mpg ~ hp * disp, df.missing)
  updated <- update(model, . ~ NULL)
  expect_length(resid(updated), length(resid(model)))

  # na.omit (explicit)
  model <- lm(mpg ~ hp * disp, df.missing, na.action = na.omit)
  updated <- update(model, . ~ NULL)
  expect_length(resid(updated), length(resid(model)))

  # na.exclude
  model <- lm(mpg ~ hp * disp, df.missing, na.action = na.exclude)
  updated <- update(model, . ~ NULL)
  expect_length(resid(updated), length(resid(model)))
})
