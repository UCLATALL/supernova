# Notes -------------------------------------------------------------------
# The datasets and expected values used here are taken from the textbook cited in the package
# description (Judd, McClelland, & Ryan). The page numbers and exhibit references correspond to the
# 2nd edition of the text. The datasets can also be found in the JMRData package on the UCLATALL
# GitHub.


# Helper functions --------------------------------------------------------

fit_lmer <- function(formula, data) {
  skip_if_not_installed("lme4")
  lme4::lmer(
    formula,
    data = data,
    na.action = na.omit,
    subset = NULL,
    weights = NULL,
    offset = NULL
  )
}


# Error tests -------------------------------------------------------------

test_that("cannot compute SS types other than Type III for lmerMod", {
  model <- fit_lmer(
    puzzles_completed ~ condition + (1 | subject),
    test_jmr_ex11.9
  )
  expect_error(supernova(model, type = 1))
  expect_error(supernova(model, type = 2))
})

test_that("there is no verbose print for lmerMod (warn and switch off)", {
  model <- fit_lmer(
    puzzles_completed ~ condition + (1 | subject),
    test_jmr_ex11.9
  )
  expect_warning(supernova(model, verbose = TRUE))
})


# Structure tests ---------------------------------------------------------

test_that("supernova object has table, fit, and models", {
  model <- fit_lmer(
    puzzles_completed ~ condition + (1 | subject),
    test_jmr_ex11.9
  )

  obj <- supernova(model, type = 3)

  obj |> expect_s3_class("supernova")

  obj$fit |> expect_identical(model)

  obj$models |> expect_null()

  obj$tbl |> expect_vector(data.frame(
    term = character(),
    SS = double(),
    df = integer(),
    MS = double(),
    `F` = double(),
    PRE = double(),
    p = double(),
    stringsAsFactors = FALSE
  ))
})

test_that("supernova works when lmer() is piped in", {
  fit_lmer(puzzles_completed ~ condition + (1 | subject), test_jmr_ex11.9) |>
    supernova() |>
    expect_s3_class("supernova")
})

test_that("supernova works when data is piped into lmer() is piped in", {
  skip_if(
    package_version(R.version) < "3.5",
    "This is only skipped to make this package compatible with DataCamp Light."
  )

  # Believe it or not, this might not work. Do not remove or refactor test.
  # When stats::update() tries to get the call, the data object is just the
  # placeholder. supernova has to middle-man with supernova::update()
  test_jmr_ex11.9 |>
    fit_lmer(puzzles_completed ~ condition + (1 | subject), data = _) |>
    supernova() |>
    expect_s3_class("supernova")
})


# ANOVA values ------------------------------------------------------------

test_that("supernova can test simple nested designs", {
  model <- fit_lmer(
    value ~ instructions + (1 | group),
    test_jmr_ex11.1
  )

  expect_equal(
    supernova(model)$tbl,
    expected_jmr_ex11.1,
    tolerance = 0.01
  )
})

test_that("supernova can test simple crossed designs", {
  model <- fit_lmer(
    puzzles_completed ~ condition + (1 | subject),
    test_jmr_ex11.9
  )

  expect_equal(
    supernova(model)$tbl,
    expected_jmr_ex11.9,
    tolerance = 0.001
  )
})

test_that("supernova can test multiple crossed designs", {
  model <- fit_lmer(
    recall ~ time * type + (1 | subject) + (1 | time:subject) + (1 | type:subject),
    test_jmr_ex11.17
  )

  expect_equal(
    supernova(model)$tbl,
    expected_jmr_ex11.17,
    tolerance = 0.01
  )
})

test_that("supernova can test mixed designs", {
  model <- fit_lmer(
    rating ~ sex * yearsmarried * children + (1 | couple),
    test_jmr_ex11.22
  )

  expect_equal(
    supernova(model)$tbl,
    expected_jmr_ex11.22,
    tolerance = 0.01
  )
})


# Printing -------------------------------------------------------------------

test_that("nested repeated measures tables are beautifully formatted", {
  model <- fit_lmer(
    value ~ instructions + (1 | group),
    test_jmr_ex11.1
  )

  expect_snapshot(supernova(model))
})

test_that("crossed repeated measures tables are beautifully formatted", {
  skip_if(
    package_version(R.version) < "3.5",
    "The MSE between will be off negligibly on older R versions (~.001)."
  )

  model <- fit_lmer(
    rating ~ sex * yearsmarried * children + (1 | couple),
    test_jmr_ex11.22
  )

  expect_snapshot(supernova(model))
})
