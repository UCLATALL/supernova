# Setup code for tests
# This file is sourced by testthat before running the test files. Make sure
# to register teardown code with tools like withr::defer(..., teardown_env())


# Test Data -----------------------------------------------------------------

if (!rlang::is_installed("JMRData")) {
  remotes::install_github("UCLATALL/JMRData")
}

# The test data frames are all named using the format `test_jmr_{exhibit_number}`, where the exhibit
# number corresponds to the exhibit number in the textbook and JMRData package. The data in the
# textbook is formatted in a readable, wide format, so before using it in the tests, it is reshaped
# to a long format.

# simple nested designs
test_jmr_ex11.1 <- JMRData::ex11.1 |>
  tidyr::gather(id, value, dplyr::starts_with("score")) |>
  dplyr::mutate(dplyr::across(c(group, instructions, id), as.factor)) |>
  data.frame()

# crossed designs
test_jmr_ex11.9 <- JMRData::ex11.9 |>
  tidyr::gather(condition, puzzles_completed, -subject) |>
  dplyr::mutate(dplyr::across(c(subject, condition), as.factor)) |>
  data.frame()

test_jmr_ex11.17 <- JMRData::ex11.17 |>
  rlang::set_names(tolower(names(JMRData::ex11.17))) |>
  tidyr::gather(condition, recall, -subject) |>
  tidyr::separate(condition, c("type", "time"), -1) |>
  dplyr::mutate(dplyr::across(c(subject, type, time), as.factor)) |>
  data.frame()

# mixed designs
test_jmr_ex11.22 <- JMRData::ex11.22 |>
  tidyr::gather(sex, rating, Male, Female) |>
  dplyr::mutate(dplyr::across(c(couple, children, sex, yearsmarried), as.factor)) |>
  data.frame()

# Expected Results ----------------------------------------------------------

# The expected results are all named using the format `expected_jmr_{exhibit_number}`, where the
# exhibit number corresponds to the exhibit number in the textbook and JMRData package. These are
# tibbles with the expected results of the an ANOVA where the compact model is the null model.

load_expected <- function(...) {
  path <- rprojroot::find_package_root_file("tests", "data", "expected", ...)
  readr::read_csv(path, col_types = "cdiddd") |> data_frame()
}

expected_jmr_ex11.1 <- load_expected("jmr_ex11.1.csv")
expected_jmr_ex11.9 <- load_expected("jmr_ex11.9.csv")
expected_jmr_ex11.17 <- load_expected("jmr_ex11.17.csv")
expected_jmr_ex11.22 <- load_expected("jmr_ex11.22.csv")
