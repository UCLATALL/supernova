context("Padding vectors")

test_that("vectors equal to the given length are unchanged", {
  expect_identical(pad_len(1:2, 2), 1:2)
})

test_that("vector class is not changed by padding it", {
  expect_is(pad_len(1:2, 3), "integer")
  expect_is(pad_len(as.numeric(1:2), 3), "numeric")
  expect_is(pad_len(letters, 27), "character")
})

test_that("vector is padded to a given length", {
  expect_identical(pad_len(numeric(0), 1), NA_real_)
})

test_that("vectors greater than the given length throw a warning", {
  expect_warning(pad_len(1:2, 1))
})

test_that("vector is padded to length of another vector", {
  expect_identical(pad(1:2, 1:3), c(1:2, NA))
})

test_that("the padding can be placed at any index in the vector", {
  expect_identical(pad_len(1:2, 3, after = 2), c(1:2, NA))
  expect_identical(pad_len(1:2, 3, after = 0), c(NA, 1:2))
  expect_identical(pad(1:2, 1:3, after = 0), c(NA, 1:2))
  expect_equal(pad_len(1:2, 3, after = 1), c(1, NA_real_, 2))
})

test_that("the pad value can be specified", {
  expect_identical(pad_len(1:2, 3, pad = "some value"), c(1:2, "some value"))
})
