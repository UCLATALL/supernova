# Run this to create the JMRData RDS files in ./data

library(magrittr)
remotes::install_github("UCLATALL/JMRData")

data_dir <- "./tests/testthat/data"
if (!dir.exists(data_dir)) dir.create(data_dir)


# Simple nested designs ---------------------------------------------------

JMRData::ex11.1 %>%
  tidyr::gather(id, value, dplyr::starts_with("score")) %>%
  dplyr::mutate(dplyr::across(c(group, instructions, id), as.factor)) %>%
  readr::write_rds(file.path(data_dir, "jmr_ex11.1.Rds"))


# Crossed designs ---------------------------------------------------------

JMRData::ex11.9 %>%
  tidyr::gather(condition, puzzles_completed, -subject) %>%
  dplyr::mutate(dplyr::across(c(subject, condition), as.factor)) %>%
  readr::write_rds(file.path(data_dir, "jmr_ex11.9.Rds"))

JMRData::ex11.17 %>%
  purrr::set_names(tolower(names(.))) %>%
  tidyr::gather(condition, recall, -subject) %>%
  tidyr::separate(condition, c("type", "time"), -1) %>%
  dplyr::mutate(dplyr::across(c(subject, type, time), as.factor)) %>%
  readr::write_rds(file.path(data_dir, "jmr_ex11.17.Rds"))


# Mixed designs -----------------------------------------------------------

JMRData::ex11.22 %>%
  tidyr::gather(sex, rating, Male, Female) %>%
  dplyr::mutate(dplyr::across(c(couple, children, sex, yearsmarried), as.factor)) %>%
  readr::write_rds(file.path(data_dir, "jmr_ex11.22.Rds"))
