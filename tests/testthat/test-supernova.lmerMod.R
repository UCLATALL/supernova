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
  readRDS(file.path(prefix, "cache", name))
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
    data = get_data("jmr_ex11.9.Rds")
  )

  expect_error(supernova(model, type = 1))
  expect_error(supernova(model, type = 2))
})

test_that("there is no verbose print for lmerMod (warn and switch off)", {
  model <- fit_lmer(
    puzzles_completed ~ condition + (1 | subject),
    data = get_data("jmr_ex11.9.Rds")
  )

  expect_warning(supernova(model, verbose = TRUE))
})


# Structure tests ---------------------------------------------------------

test_that("supernova object has table, fit, and models", {
  model <- fit_lmer(
    puzzles_completed ~ condition + (1 | subject),
    data = get_data("jmr_ex11.9.Rds")
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
    data = get_data("jmr_ex11.9.Rds")
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
    data = get_data("jmr_ex11.9.Rds")
  ) %>%
    supernova() %>%
    expect_is("supernova")
})

test_that("magrittr can pipe data to lm() to supernova", {
  # Believe it or not, this might not work. Do not remove or refactor test.
  # When stats::update() tries to get the call, the data object is just "."
  # supernova has to middle-man with supernova::update() to get this to work
  get_data("jmr_ex11.9.Rds") %>%
    fit_lmer(puzzles_completed ~ condition + (1 | subject), data = .) %>%
    supernova() %>%
    expect_is("supernova")
})


# ANOVA values ------------------------------------------------------------

test_that("sueprnova can test simple nested designs", {
  model <- fit_lmer(
    value ~ instructions + (1 | group),
    data = get_data("jmr_ex11.1.Rds")
  )

  expect_equal(
    supernova(model)$tbl,
    tibble::tribble(
      ~term,                       ~SS, ~df,   ~MS,    ~F, ~PRE,    ~p,
      "instructions"          ,  12.50,   1, 12.50, 4.686, 0.54, 0.096,
      "Error between subjects",  10.67,   4,  2.67,    NA,   NA,    NA,
      "Total between subjects",  23.17,   5,  4.63,    NA,   NA,    NA,
      "Total within subjects" ,   5.33,  12,  0.44,    NA,   NA,    NA,
      "Total"                 ,  28.50,  17,  1.68,    NA,   NA,    NA,
    ) %>% as.data.frame(),
    tolerance = 0.01
  )
})

test_that("supernova can test simple crossed designs", {
  model <- fit_lmer(
    puzzles_completed ~ condition + (1 | subject),
    data = get_data("jmr_ex11.9.Rds")
  )

  expect_equal(
    supernova(model)$tbl,
    tibble::tribble(
      ~term,                      ~SS, ~df,   ~MS,      ~F,   ~PRE,     ~p,
      "Total between subjects", 18.00,   7,  2.5714,    NA,     NA,     NA,
      "condition",               2.25,   1,  2.2500, 5.727, 0.4500, .04794,
      "Error within subjects",   2.75,   7,  0.3929,    NA,     NA,     NA,
      "Total within subjects",   5.00,   8,  0.6250,    NA,     NA,     NA,
      "Total",                  23.00,  15,  1.5333,    NA,     NA,     NA,
    ) %>% as.data.frame(),
    tolerance = 0.001
  )
})

test_that("supernova can test multiple crossed designs", {
  model <- fit_lmer(
    recall ~ time * type + (1 | subject) + (1 | time:subject) + (1 | type:subject),
    data = get_data("jmr_ex11.17.Rds")
  )

  expect_equal(
    supernova(model)$tbl,
    tibble::tribble(
      ~term,                       ~SS, ~df,   ~MS,    ~F, ~PRE,   ~p,
      "Total between subjects", 131.00,   4, 32.75,    NA,   NA,   NA,
      "time"                  ,  65.87,   2, 32.93, 29.94, 0.88, .000,
      "time error"            ,   8.80,   8,  1.10,    NA,   NA,   NA,
      "type"                  ,  17.63,   1, 17.63, 11.38, 0.74, .028,
      "type error"            ,   6.20,   4,  1.55,    NA,   NA,   NA,
      "time:type"             ,   1.87,   2,  0.93,  9.33, 0.70, .008,
      "time:type error"       ,   0.80,   8,  0.10,    NA,   NA,   NA,
      "Total within subjects" , 101.17,  25,  4.05,    NA,   NA,   NA,
      "Total"                 , 232.17,  29,  8.01,    NA,   NA,   NA,
    ) %>% as.data.frame(),
    tolerance = 0.01
  )
})

test_that("supernova can test mixed designs", {
  model <- fit_lmer(
    rating ~ sex * yearsmarried * children + (1 | couple),
    data = get_data("jmr_ex11.22.Rds")
  )

  expect_equal(
    supernova(model)$tbl,
    tibble::tribble(
      ~term,                          ~SS, ~df,     ~MS,   ~F, ~PRE,   ~p,
      "yearsmarried"             , 10.125,   1, 10.1250, 9.54, 0.44, .009,
      "children"                 ,  0.500,   1,  0.5000, 0.48, 0.04, .506,
      "yearsmarried:children"    , 10.125,   1, 10.1250, 9.54, 0.44, .009,
      "Error between subjects"   , 12.750,  12,  1.0625,   NA,   NA,   NA,
      "Total between subjects"   , 33.500,  15,  2.2333,   NA,   NA,   NA,

      "sex"                      ,  3.125,   1,  3.1250, 8.82, 0.42, .012,
      "sex:yearsmarried"         ,  2.000,   1,  2.0000, 5.66, 0.32, .035,
      "sex:children"             ,  0.125,   1,  0.1250, 0.35, 0.03, .565,
      "sex:yearsmarried:children",  0.505,   1,  0.5000, 1.42, 0.11, .256,
      "Error within subjects"    ,  4.255,  12,  0.3542,   NA,   NA,   NA,
      "Total within subjects"    , 10.000,  16,  0.6250,   NA,   NA,   NA,

      "Total"                    , 43.500,  31,  1.40,   NA,   NA,   NA,
    ) %>% as.data.frame(),
    tolerance = 0.01
  )
})
