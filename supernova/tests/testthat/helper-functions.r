# testthat extensions and other helper functions for the test suites

#' Expectation: is the object similar to a value?
#'
#' Uses the same method as expect_equivalent, but first converts strings
#' to numeric objects.
expect_similar <- function(object, expected, ..., info = NULL, label = NULL, expected.label = NULL) {
  act <- quasi_label(rlang::enquo(object), label)
  exp <- quasi_label(rlang::enquo(expected), expected.label)
  act_comp <- suppressWarnings(as.numeric(act$val))
  exp_comp <- suppressWarnings(as.numeric(exp$val))
  comp <- compare(act_comp, exp_comp, ..., check.attributes = FALSE)
  expect(comp$equal, sprintf("%s not similar to %s.\n%s", act$lab, exp$lab, comp$message), info = info)
  invisible(act$val)
}
