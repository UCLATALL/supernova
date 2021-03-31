test_that("it returns a zero-length vector when called with no arguments", {
  expect_length(number(), 0)
})

test_that("it can be type-checked", {
  expect_true(is_number(number()))
  expect_true(is.double(number()))
})

test_that("it is labeled like a double", {
  expect_output(str(number(1)), "^ dbl")
})

test_that("the displayed number of digits after the decimal is fixed to user input", {
  expect_snapshot(number(1.1, 2))
})

test_that("it can be displayed in scientific notation", {
  expect_snapshot(number(1100, scientific = TRUE))
})

test_that("leading zeroes can be trimmed", {
  expect_snapshot(number(0.05, leading_zero = FALSE))
})

test_that("it can be coerced to a double or character, but not an integer", {
  expect_equal(as.double(number(1)), 1)
  expect_equal(as.character(number(1)), "1.000")
  expect_error(as.integer(number(1)), "Can't convert")
})

test_that("a double or character can be coerced to a number", {
  expect_equal(as_number(1), number(1))
  expect_equal(as_number("1"), number(1))
})

test_that("coercing number to number updates the attributes to the new version", {
  coerced <- vctrs::vec_cast(number(1, 2), number(digits = 3))
  expect_equal(attr(coerced, "digits"), 3)

  coerced <- vctrs::vec_cast(number(1, 2), number(scientific = TRUE))
  expect_equal(attr(coerced, "scientific"), TRUE)

  coerced <- vctrs::vec_cast(number(1, 2), number(leading_zero = FALSE))
  expect_equal(attr(coerced, "leading_zero"), FALSE)
})

test_that("it works like a regular number", {
  expect_equal(-number(1), -1, ignore_attr = TRUE) %>%
    expect_s3_class("supernova_number")
  expect_equal(+number(1), +1, ignore_attr = TRUE) %>%
    expect_s3_class("supernova_number")
  expect_equal(number(1) + number(1), 1 + 1, ignore_attr = TRUE) %>%
    expect_s3_class("supernova_number")
  expect_equal(number(1) - number(1), 1 - 1, ignore_attr = TRUE) %>%
    expect_s3_class("supernova_number")
  expect_equal(number(1) * number(1), 1 * 1, ignore_attr = TRUE) %>%
    expect_s3_class("supernova_number")
  expect_equal(number(1) / number(1), 1 / 1, ignore_attr = TRUE) %>%
    expect_s3_class("supernova_number")
  expect_equal(number(1) %% number(1), 1 %% 1, ignore_attr = TRUE) %>%
    expect_s3_class("supernova_number")
  expect_equal(number(1) ^ number(1), 1 ^ 1, ignore_attr = TRUE) %>%
    expect_s3_class("supernova_number")
})

test_that("the result of arithmetic has the properties of the number (or first number if both)", {
  test_num <- number(1, 4, TRUE, FALSE)
  expect_equal(test_num + 0, test_num)
})

test_that("the underlying value is not rounded", {
  expect_gt(number(5.551, 2), 5.55)
})

test_that("it handles missing values well", {
  expect_true(is.na(number(NA)))
})
