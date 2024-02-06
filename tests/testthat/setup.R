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
test_jmr_ex11.1 <- JMRData::ex11.1 %>%
  tidyr::gather(id, value, dplyr::starts_with("score")) %>%
  dplyr::mutate(dplyr::across(c(group, instructions, id), as.factor)) %>%
  data.frame()

# crossed designs
test_jmr_ex11.9 <- JMRData::ex11.9 %>%
  tidyr::gather(condition, puzzles_completed, -subject) %>%
  dplyr::mutate(dplyr::across(c(subject, condition), as.factor)) %>%
  data.frame()

test_jmr_ex11.17 <- JMRData::ex11.17 %>%
  rlang::set_names(tolower(names(JMRData::ex11.17))) %>%
  tidyr::gather(condition, recall, -subject) %>%
  tidyr::separate(condition, c("type", "time"), -1) %>%
  dplyr::mutate(dplyr::across(c(subject, type, time), as.factor)) %>%
  data.frame()

# mixed designs
test_jmr_ex11.22 <- JMRData::ex11.22 %>%
  tidyr::gather(sex, rating, Male, Female) %>%
  dplyr::mutate(dplyr::across(c(couple, children, sex, yearsmarried), as.factor)) %>%
  data.frame()

# Expected Results ----------------------------------------------------------

# The expected results are all named using the format `expected_jmr_{exhibit_number}`, where the
# exhibit number corresponds to the exhibit number in the textbook and JMRData package. These are
# tibbles with the expected results of the an ANOVA where the compact model is the null model.

expected_jmr_ex11.1 <- tibble::tribble(
  ~term, ~SS, ~df, ~MS, ~F, ~PRE, ~p,
  "instructions", 12.5, 1, 12.5, 4.686, 0.54, 0.096,
  "Error between subjects", 10.67, 4, 2.67, NA, NA, NA,
  "Total between subjects", 23.17, 5, 4.63, NA, NA, NA,
  "Total within subjects", 5.33, 12, 0.44, NA, NA, NA,
  "Total", 28.5, 17, 1.68, NA, NA, NA
) %>% data.frame()

expected_jmr_ex11.9 <- tibble::tribble(
  ~term, ~SS, ~df, ~MS, ~F, ~PRE, ~p,
  "Total between subjects", 18, 7, 2.5714, NA, NA, NA,
  "condition", 2.25, 1, 2.25, 5.727, 0.45, 0.04794,
  "Error within subjects", 2.75, 7, 0.3929, NA, NA, NA,
  "Total within subjects", 5, 8, 0.625, NA, NA, NA,
  "Total", 23, 15, 1.5333, NA, NA, NA
) %>% data.frame()

expected_jmr_ex11.17 <- tibble::tribble(
  ~term, ~SS, ~df, ~MS, ~F, ~PRE, ~p,
  "Total between subjects", 131, 4, 32.75, NA, NA, NA,
  "time", 65.87, 2, 32.93, 29.94, 0.88, 0,
  "time error", 8.8, 8, 1.1, NA, NA, NA,
  "type", 17.63, 1, 17.63, 11.38, 0.74, 0.028,
  "type error", 6.2, 4, 1.55, NA, NA, NA,
  "time:type", 1.87, 2, 0.93, 9.33, 0.7, 0.008,
  "time:type error", 0.8, 8, 0.1, NA, NA, NA,
  "Total within subjects", 101.17, 25, 4.05, NA, NA, NA,
  "Total", 232.17, 29, 8.01, NA, NA, NA
) %>% data.frame()

expected_jmr_ex11.22 <- tibble::tribble(
  ~term, ~SS, ~df, ~MS, ~F, ~PRE, ~p,
  "yearsmarried", 10.125, 1, 10.125, 9.54, 0.44, 0.009,
  "children", 0.5, 1, 0.5, 0.48, 0.04, 0.506,
  "yearsmarried:children", 10.125, 1, 10.125, 9.54, 0.44, 0.009,
  "Error between subjects", 12.75, 12, 1.0625, NA, NA, NA,
  "Total between subjects", 33.5, 15, 2.2333, NA, NA, NA,
  "sex", 3.125, 1, 3.125, 8.82, 0.42, 0.012,
  "sex:yearsmarried", 2, 1, 2, 5.66, 0.32, 0.035,
  "sex:children", 0.125, 1, 0.125, 0.35, 0.03, 0.565,
  "sex:yearsmarried:children", 0.505, 1, 0.5, 1.42, 0.11, 0.256,
  "Error within subjects", 4.255, 12, 0.3542, NA, NA, NA,
  "Total within subjects", 10, 16, 0.625, NA, NA, NA,
  "Total", 43.5, 31, 1.4, NA, NA, NA
) %>% data.frame()
