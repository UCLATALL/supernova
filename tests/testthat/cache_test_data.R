# Cache the car::Anova data and other data used to test the package  so that we
# don't have to include those packages just for testing

library(magrittr)

cache_dir <- "./tests/testthat/cache"
data_dir <- "./tests/testthat/data"
if (!dir.exists(cache_dir)) dir.create(cache_dir)
if (!dir.exists(data_dir)) dir.create(data_dir)


# Independent designs -----------------------------------------------------

as.character.call <- function(model) {
  Reduce(paste, deparse(model$call))
}

df_missing <- mtcars
df_missing[1, ]$hp <- NA_real_
df_missing[2:3, ]$disp <- NA_real_

models <- list(
  lm(Thumb ~ Weight, supernova::Fingers),
  lm(Thumb ~ RaceEthnic, supernova::Fingers),
  lm(Thumb ~ Weight + Height, supernova::Fingers),
  lm(Thumb ~ RaceEthnic + Weight, supernova::Fingers),
  lm(Thumb ~ RaceEthnic + Sex, supernova::Fingers),
  lm(Thumb ~ RaceEthnic + Weight + Sex, supernova::Fingers),
  lm(Thumb ~ Weight * Height, supernova::Fingers),
  lm(Thumb ~ RaceEthnic * Weight, supernova::Fingers),
  lm(Thumb ~ RaceEthnic * Sex, supernova::Fingers),
  lm(Thumb ~ RaceEthnic + Weight * Sex, supernova::Fingers),
  lm(Thumb ~ RaceEthnic * Weight * Sex, supernova::Fingers),
  lm(mpg ~ hp, df_missing),
  lm(mpg ~ hp * disp, df_missing),
  lm(uptake ~ Treatment, data = CO2[1:80, ]),
  lm(uptake ~ Treatment * Type, data = CO2[1:80, ])
) %>%
  purrr::set_names(purrr::map(., ~ as.character.call(.x)))

models %>%
  purrr::map(anova) %>%
  readr::write_rds(file.path(cache_dir, "model_cache_type_1.Rds"))

models %>%
  purrr::map(car::Anova, type = 2) %>%
  readr::write_rds(file.path(cache_dir, "model_cache_type_2.Rds"))

models %>%
  purrr::map(car::Anova, type = 3) %>%
  readr::write_rds(file.path(cache_dir, "model_cache_type_3.Rds"))


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
