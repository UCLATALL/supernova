get_data_with_missing <- function() {
  df_missing <- mtcars
  df_missing[1, ]$hp <- NA_real_
  df_missing[2:3, ]$disp <- NA_real_
  df_missing
}

test_that("it does nothing when cases are complete", {
  model <- lm(mpg ~ hp, mtcars)
  expect_identical(listwise_delete(mtcars), mtcars)
})

test_that("it removes cases with missing values from data frames", {
  df_missing <- get_data_with_missing()
  expect_identical(
    suppressMessages(listwise_delete(df_missing)),
    mtcars[4:nrow(mtcars), ]
  )
})

test_that("it leaves a message when rows with missing cases are excluded", {
  df_missing <- get_data_with_missing()
  expect_snapshot(listwise_delete(df_missing))
})

test_that("it refits an lm with missing data with a message about the new call", {
  df_missing <- get_data_with_missing()
  model <- lm(mpg ~ hp * disp, data = df_missing)
  expect_snapshot(listwise_delete(model))
})

test_that("it works in a pipe", {
  skip_if(package_version(R.version) < "4.1")

  get_data_with_missing() |>
    lm(mpg ~ hp * disp, data = _) |>
    listwise_delete() |>
    expect_snapshot()
})

test_that("it works when the call is long and breaks multiple lines when using deparse", {
  mtcars_long_vars <- mtcars
  mtcars_long_vars$mpg_but_with_a_very_long_name <- mtcars$mpg
  mtcars_long_vars$hp_but_with_a_very_long_name <- mtcars$hp
  mtcars_long_vars$hp_but_with_a_very_long_name[1] <- NA
  long_model <- lm(
    formula = mpg_but_with_a_very_long_name ~ hp_but_with_a_very_long_name,
    data = mtcars_long_vars
  )
  expect_warning(listwise_delete(long_model) |> suppressMessages(), NA)
})
