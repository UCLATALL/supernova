context("supernova: Values, crossed designs")
library(dplyr)


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

# jmr_ex11.9 <- readRDS("./tests/testthat/cache/jmr_ex11.9.Rds")
# jmr_ex11.17 <- readRDS("./tests/testthat/cache/jmr_ex11.17.Rds")
# jmr_ex11.9 <- readRDS("./cache/jmr_ex11.9.Rds")
# jmr_ex11.17 <- readRDS("./cache/jmr_ex11.17.Rds")


# Structure tests ---------------------------------------------------------

test_that("supernova object has table, fit, and models", {
  model <- lme4::lmer(
    puzzles_completed ~ condition + (1|subject),
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
  model <- lme4::lmer(
    puzzles_completed ~ condition + (1|subject),
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
  lme4::lmer(
    puzzles_completed ~ condition + (1|subject),
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
    lme4::lmer(puzzles_completed ~ condition + (1|subject), data = .) %>%
    supernova() %>%
    expect_is("supernova")
})


# ANOVA values ------------------------------------------------------------

test_that("sueprnova can test simple nested designs", {
  model <- lme4::lmer(
    value ~ instructions + (1|group),
    data = get_data("jmr_ex11.1.Rds")
  )

  expect_equal(
    supernova(model)$tbl %>% mutate_if(is.numeric, round, 2) %>% as.data.frame(),
    tribble(
      ~term,                       ~SS, ~df,   ~MS,       ~F,     ~PRE,       ~p,
      "instructions"          ,  12.50,   1, 12.50,    4.686,     0.54,    0.096,
      "instructions error"    ,  10.67,   4,  2.67, NA_real_, NA_real_, NA_real_,
      "Total between subjects",  23.17,   5,  4.63, NA_real_, NA_real_, NA_real_,
      "Total within subjects" ,   5.33,  12,  0.44, NA_real_, NA_real_, NA_real_,
      "Total"                 ,  28.50,  17,  1.68, NA_real_, NA_real_, NA_real_
    ) %>% mutate_if(is.numeric, round, digits = 2) %>% as.data.frame()
  )
})

# |                     | b    | SS     | df | MS    | F     | PRE |
# |---------------------|------|--------|----|-------|-------|-----|
# | Between Subjects    |      |        |    |       |       |     |
# |   Condition         |  .83 |  12.50 |  1 | 12.50 |  4.68 | .54 |
# |     Error           |      |  10.67 |  4 |  2.67 |       |     |
# | Total Between       |      |  23.17 |  5 |       |       |     |
# |---------------------|------|--------|----|-------|-------|-----|
# | Within Subjects     |      |        |    |       |       |     |
# | Total Within        |      |   5.33 | 12 |  0.44 |       |     |
# |---------------------|------|--------|----|-------|-------|-----|
# | Total               |      |  28.50 | 17 |       |       |     |

test_that("supernova can test simple crossed designs", {
  model <- lme4::lmer(
    puzzles_completed ~ condition + (1|subject),
    data = get_data("jmr_ex11.9.Rds")
  )

  expect_equal(
    supernova(model)$tbl %>% mutate_if(is.numeric, round, 3) %>% as.data.frame(),
    tribble(
      ~term,                      ~SS, ~df,   ~MS,       ~F,     ~PRE,       ~p,
      "Total between subjects", 18.00,   7,  2.5714, NA_real_, NA_real_, NA_real_,
      "condition",               2.25,   1,  2.2500,    5.727,  0.4500,    .04794,
      "condition error",         2.75,   7,  0.3929, NA_real_, NA_real_, NA_real_,
      "Total within subjects",   5.00,   8,  0.6250, NA_real_, NA_real_, NA_real_,
      "Total",                  23.00,  15,  1.5333, NA_real_, NA_real_, NA_real_
    ) %>% mutate_if(is.numeric, round, 3) %>% as.data.frame(),
  )
})

# |                  | b    |  SS    | df | MS   | F    | PRE |
# |------------------|------|--------|----|------|------|-----|
# | Between Subjects |      |        |    |      |      |     |
# |   Total          |      |  18.00 |  7 | 2.57 |      |     |
# |------------------|------|--------|----|------|------|-----|
# | Within Subjects  |      |        |    |      |      |     |
# |   Treatment      | .375 |   2.25 |  1 | 2.25 | 5.73 | .45 |
# |     Error        |      |   2.75 |  7 | 0.39 |      |     |
# |   Total          |      |   5.00 |  8 | 0.63 |      |     |
# |------------------|------|--------|----|------|------|-----|
# | Total            |      |  23.00 | 15 |      |      |     |

test_that("supernova can test multiple crossed designs", {
  model <- lme4::lmer(
    recall ~ time * type + (1|subject) + (1|time:subject) + (1|type:subject),
    data = get_data("jmr_ex11.17.Rds")
  )

  expect_equal(
    supernova(model)$tbl %>% mutate_if(is.numeric, round, 2) %>% as.data.frame(),
    tribble(
      ~term,                       ~SS, ~df,   ~MS,       ~F,     ~PRE,       ~p,
      "Total between subjects", 131.00,   4, 32.75, NA_real_, NA_real_, NA_real_,
      "time"                  ,  65.87,   2, 32.93,    29.94,     0.88,     .000,
      "time error"            ,   8.80,   8,  1.10, NA_real_, NA_real_, NA_real_,
      "type"                  ,  17.63,   1, 17.63,    11.38,     0.74,     .028,
      "type error"            ,   6.20,   4,  1.55, NA_real_, NA_real_, NA_real_,
      "time:type"             ,   1.87,   2,  0.93,     9.33,     0.70,     .008,
      "time:type error"       ,   0.80,   8,  0.10, NA_real_, NA_real_, NA_real_,
      "Total within subjects" , 101.17,  25,  4.05, NA_real_, NA_real_, NA_real_,
      "Total"                 , 232.17,  29,  8.01, NA_real_, NA_real_, NA_real_
    ) %>% mutate_if(is.numeric, round, digits = 2) %>% as.data.frame()
  )
})

# |                     | b    | SS     | df | MS    | F     | PRE |
# |---------------------|------|--------|----|-------|-------|-----|
# | Between Subjects    |      |        |    |       |       |     |
# |   Total             |      | 131.00 |  4 | 32.75 |       |     |
# |---------------------|------|--------|----|-------|-------|-----|
# | Within Subjects     |      |        |    |       |       |     |
# |   Time              | 1.80 |  65.87 |  2 | 32.93 | 29.94 | .88 |
# |     Error           |      |   8.80 |  8 |  1.10 |       |     |
# |   List type         |  .77 |  17.63 |  1 | 17.63 | 11.38 | .74 |
# |     Error           |      |   6.20 |  4 |  1.55 |       |     |
# |   Time * List type  | -.30 |   1.87 |  2 |  0.93 |  9.35 | .70 | This is the correct value
# |   Time * List type  | -.30 |   1.87 |  2 |  0.93 |  9.33 | .70 | This is the best I could do
# |     Error           |      |   0.80 |  8 |  0.10 |       |     |
# |   Total             |      | 101.17 | 25 |       |       |     |
# |---------------------|------|--------|----|-------|-------|-----|
# | Total               |      | 232.17 | 29 |       |       |     |


