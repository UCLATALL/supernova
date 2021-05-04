library(testthat)
library(supernova)

test_check("supernova")

if (requireNamespace("lintr", quietly = TRUE)) {
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}
