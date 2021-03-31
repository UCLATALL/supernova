
# Notes ---------------------------------------------------------------------------------------
# The descriptions for these tests should all start with the name of the function being tested.
# Tests for the same function should be adjacent.


# Tests ---------------------------------------------------------------------------------------

test_that("frm_build returns a formula from two strings and an environment", {
  expect_equal(frm_build("y", "a", environment()), y ~ a)
})

test_that("frm_build uses the calling environment if none is given", {
  expect_equal(frm_build("y", "a"), y ~ a)
})

test_that("frm_build returns an empty model when no terms are given", {
  expect_equal(frm_build("y", character(0)), y ~ NULL)
})

test_that("frm_expand expands a formula with implicit terms into an explicit formula", {
  expect_equal(frm_expand(y ~ a * b), y ~ a + b + a:b)
})


test_that("frm_remove_term removes the given term from the formula", {
  expect_equal(frm_remove_term(y ~ a * b, "a"), y ~ b + a:b)
})

test_that("frm_remove_term returns empty model if all right-hand terms are deleted", {
  expect_equal(frm_remove_term(y ~ a, "a"), y ~ NULL)
})

test_that("frm_remove_var is okay with terms that already don't exist", {
  expect_equal(frm_remove_var(y ~ a, "b"), y ~ a)
})

test_that("frm_remove_var removes every term on the right-hand side containing the variable", {
  expect_equal(frm_remove_var(y ~ a + a:b + c, "a"), y ~ c)
})

test_that("frm_remove_var properly handles name overlap", {
  expect_equal(frm_remove_var(y ~ a + aa, "a"), y ~ aa)
})

test_that("frm_remove_ functions return the empty model when all right-hand terms are deleted", {
  expect_equal(frm_remove_term(y ~ a, "a"), y ~ NULL)
  expect_equal(frm_remove_var(y ~ a, "a"), y ~ NULL)
})


test_that("frm_outcome extracts the term from the left-hand side of the formula", {
  expect_equal(frm_outcome(y ~ a * b), "y")
})

test_that("frm_terms extracts the term from the right-hand side of the formula", {
  expect_equal(frm_terms(y ~ a), c("a"))
})

test_that("frm_terms expands the formula before extracting", {
  expect_equal(frm_terms(y ~ a * b), c("a", "b", "a:b"))
})

test_that("frm_interaction_terms extracts the interactive terms from the formula", {
  expect_equal(frm_interaction_terms(y ~ a * b), c("a:b"))
})

test_that("frm_fixed_terms only extracts non-random terms from the formula", {
  expect_equal(frm_fixed_terms(y ~ a + (1 | group)), "a")
})

test_that("frm_random_terms only extracts random terms from the formula", {
  expect_equal(frm_random_terms(y ~ a + (1 | group)), "1 | group")
})

test_that("frm_vars extracts all variables from the right-hand side", {
  expect_equal(frm_vars(y ~ a + a:b + b:c + (1 | group)), c("a", "b", "c", "group"))
})

test_that("frm_fixed_vars extracts only the variables used in fixed terms", {
  expect_equal(frm_fixed_vars(y ~ a + a:b + b:c + (1 | group)), c("a", "b", "c"))
})

test_that("frm_random_vars extracts only the variables used in random terms", {
  expect_equal(frm_random_vars(y ~ a + a:b + b:c + (1 | group)), "group")
})
