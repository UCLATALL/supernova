context("supernova: Values, crossed designs")

# Notes -------------------------------------------------------------------

# The datasets and expected values used here are taken from the textbook cited
# in the package description (Judd, McClelland, & Ryan). The page numbers and
# exhibit references correspond to the 2nd edition of the text. The datasets
# can also be found in the JMRData package on the UCLATALL GitHub:
#
# devtools::install_github("UCLATALL/JMRData")


# Custom expectations -----------------------------------------------------



# Helper functions --------------------------------------------------------

get_data <- function(name) {
  prefix <- if (interactive()) "./tests/testthat/" else "./"
  readRDS(file.path(prefix, "data", paste0(name, ".Rds")))
}

get_expected <- function(name) {
  prefix <- if (interactive()) "./tests/testthat/" else "./"
  read.csv(file.path(prefix, "expected", paste0(name, ".csv")), stringsAsFactors = FALSE)
}

fit_lmer <- function(formula, data) {
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
    data = get_data("jmr_ex11.9")
  )

  expect_error(supernova(model, type = 1))
  expect_error(supernova(model, type = 2))
})

test_that("there is no verbose print for lmerMod (warn and switch off)", {
  model <- fit_lmer(
    puzzles_completed ~ condition + (1 | subject),
    data = get_data("jmr_ex11.9")
  )

  expect_warning(supernova(model, verbose = TRUE))
})


# Structure tests ---------------------------------------------------------

test_that("supernova object has table, fit, and models", {
  model <- fit_lmer(
    puzzles_completed ~ condition + (1 | subject),
    data = get_data("jmr_ex11.9")
  )

  obj <- supernova(model, type = 3)
  obj %>%
    expect_is("supernova")
  obj$tbl %>%
    expect_is("data.frame")
  obj$fit %>%
    expect_is("lmerMod") %>%
    expect_identical(model)
  obj$models %>%
    # expect_is("comparison_models") %>%
    # expect_identical(suppressWarnings(generate_models(fit, 3)))
    expect_null()
})

test_that("supernova table structure is well-formed", {
  model <- fit_lmer(
    puzzles_completed ~ condition + (1 | subject),
    data = get_data("jmr_ex11.9")
  )

  obj <- supernova(model) %>%
    .[["tbl"]] %>%
    expect_is("data.frame") %>%
    expect_named(c("term", "SS", "df", "MS", "F", "PRE", "p"))

  expect_is(obj[["term"]], "character")
  # expect_is(obj[["description"]], "character")
  expect_is(obj[["SS"]], "numeric")
  expect_is(obj[["df"]], "integer")
  expect_is(obj[["MS"]], "numeric")
  expect_is(obj[["F"]], "numeric")
  expect_is(obj[["PRE"]], "numeric")
  expect_is(obj[["p"]], "numeric")
})

test_that("magrittr can pipe lmer() to supernova", {
  fit_lmer(
    puzzles_completed ~ condition + (1 | subject),
    data = get_data("jmr_ex11.9")
  ) %>%
    supernova() %>%
    expect_is("supernova")
})

test_that("magrittr can pipe data to lm() to supernova", {
  # Believe it or not, this might not work. Do not remove or refactor test.
  # When stats::update() tries to get the call, the data object is just "."
  # supernova has to middle-man with supernova::update() to get this to work
  get_data("jmr_ex11.9") %>%
    fit_lmer(puzzles_completed ~ condition + (1 | subject), data = .) %>%
    supernova() %>%
    expect_is("supernova")
})


# ANOVA values ------------------------------------------------------------

test_that("supernova can test simple nested designs", {
  model <- fit_lmer(
    value ~ instructions + (1 | group),
    data = get_data("jmr_ex11.1")
  )

  expect_equal(
    supernova(model)$tbl,
    get_expected("jmr_ex11.1"),
    tolerance = 0.01
  )
})

test_that("supernova can test simple crossed designs", {
  model <- fit_lmer(
    puzzles_completed ~ condition + (1 | subject),
    data = get_data("jmr_ex11.9")
  )

  expect_equal(
    supernova(model)$tbl,
    get_expected("jmr_ex11.9"),
    tolerance = 0.001
  )
})

test_that("supernova can test multiple crossed designs", {
  model <- fit_lmer(
    recall ~ time * type + (1 | subject) + (1 | time:subject) + (1 | type:subject),
    data = get_data("jmr_ex11.17")
  )

  expect_equal(
    supernova(model)$tbl,
    get_expected("jmr_ex11.17"),
    tolerance = 0.01
  )
})

test_that("supernova can test mixed designs", {
  model <- fit_lmer(
    rating ~ sex * yearsmarried * children + (1 | couple),
    data = get_data("jmr_ex11.22")
  )

  expect_equal(
    supernova(model)$tbl,
    get_expected("jmr_ex11.22"),
    tolerance = 0.01
  )
})
