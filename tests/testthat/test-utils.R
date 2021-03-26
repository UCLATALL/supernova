
# Notes ---------------------------------------------------------------------------------------
# Each test description in this file should start with the name of the function being tested.
# The tests should be arranged alphabetically by function name.


# Tests ---------------------------------------------------------------------------------------

test_that("pad_len returns vectors equal to the given length are unchanged", {
  expect_identical(pad_len(1:2, 2), 1:2)
})

test_that("pad_len does not change the vector class", {
  expect_vector(pad_len(1:2, 3), integer())
  expect_vector(pad_len(as.numeric(1:2), 3), numeric())
  expect_vector(pad_len(letters, 27), character())
})

test_that("pad_len pads a vector to a given length", {
  expect_identical(pad_len(numeric(0), 1), NA_real_)
})

test_that("pad_len throws a warning when given vectors already greater than the desired length", {
  expect_warning(pad_len(1:2, 1))
})

test_that("pad will pad a vector to length of another vector", {
  expect_identical(pad(1:2, 1:3), c(1:2, NA))
})

test_that("pad_len/pad the padding can be placed at any index in the vector", {
  expect_identical(pad_len(1:2, 3, after = 2), c(1:2, NA))
  expect_identical(pad_len(1:2, 3, after = 0), c(NA, 1:2))
  expect_identical(pad(1:2, 1:3, after = 0), c(NA, 1:2))
  expect_equal(pad_len(1:2, 3, after = 1), c(1, NA_real_, 2))
})

test_that("pad_len allows you to choose the pad value", {
  expect_identical(pad_len(1:2, 3, pad = "some value"), c(1:2, "some value"))
})

test_that("paste_line uses a newline character to splice the strings together", {
  expect_identical("a\nb", paste_line("a", "b"))
})

test_that("resolve_type SS types can be specified as numeric, numeral, character", {
  expect_identical(supernova:::resolve_type(1), 1)
  expect_identical(supernova:::resolve_type("1"), 1)
  expect_identical(supernova:::resolve_type("I"), 1)
  expect_identical(supernova:::resolve_type("sequential"), 1)
  expect_identical(supernova:::resolve_type(2), 2)
  expect_identical(supernova:::resolve_type("2"), 2)
  expect_identical(supernova:::resolve_type("II"), 2)
  expect_identical(supernova:::resolve_type("hierarchical"), 2)
  expect_identical(supernova:::resolve_type(3), 3)
  expect_identical(supernova:::resolve_type("3"), 3)
  expect_identical(supernova:::resolve_type("III"), 3)
  expect_identical(supernova:::resolve_type(" iIi "), 3)
  expect_identical(supernova:::resolve_type("orthogonal"), 3)
})
