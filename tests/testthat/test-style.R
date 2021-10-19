test_that("Package Style", {
  skip_on_cran()
  skip_if_not_installed("lintr")
  lintr::expect_lint_free()
})
