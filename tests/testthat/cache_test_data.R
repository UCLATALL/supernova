# Cache the car::Anova data and other data used to test the package  so that we
# don't have to include those packages just for testing

cache_dir <- "./tests/testthat/cache"
if (!dir.exists(cache_dir)) dir.create(cache_dir)

`%>%` <- magrittr::`%>%`


# Independent designs -----------------------------------------------------

Fingers <- supernova::Fingers

as.character.call <- function(model) {
  Reduce(paste, deparse(model$call))
}

df.missing <- mtcars
df.missing[1, ]$hp <- NA_real_
df.missing[2:3, ]$disp <- NA_real_

models <- list(
  lm(Thumb ~ Weight, Fingers),
  lm(Thumb ~ RaceEthnic, Fingers),
  lm(Thumb ~ Weight + Height, Fingers),
  lm(Thumb ~ RaceEthnic + Weight, Fingers),
  lm(Thumb ~ RaceEthnic + Sex, Fingers),
  lm(Thumb ~ RaceEthnic + Weight + Sex, Fingers),
  lm(Thumb ~ Weight * Height, Fingers),
  lm(Thumb ~ RaceEthnic * Weight, Fingers),
  lm(Thumb ~ RaceEthnic * Sex, Fingers),
  lm(Thumb ~ RaceEthnic + Weight * Sex, Fingers),
  lm(Thumb ~ RaceEthnic * Weight * Sex, Fingers),
  lm(mpg ~ hp, df.missing),
  lm(mpg ~ hp * disp, df.missing),
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
  readr::write_rds(file.path(cache_dir, "jmr_ex11.1.Rds"))


# Crossed designs ---------------------------------------------------------

JMRData::ex11.9 %>%
  tidyr::gather(condition, puzzles_completed, -subject) %>%
  dplyr::mutate(dplyr::across(c(subject, condition), as.factor)) %>%
  readr::write_rds(file.path(cache_dir, "jmr_ex11.9.Rds"))

JMRData::ex11.17 %>%
  purrr::set_names(tolower(names(.))) %>%
  tidyr::gather(condition, recall, -subject) %>%
  tidyr::separate(condition, c("type", "time"), -1) %>%
  dplyr::mutate(dplyr::across(c(subject, type, time), as.factor)) %>%
  readr::write_rds(file.path(cache_dir, "jmr_ex11.17.Rds"))


# Mixed designs -----------------------------------------------------------

JMRData::ex11.22 %>%
  tidyr::gather(sex, rating, Male, Female) %>%
  dplyr::mutate(dplyr::across(c(couple, children, sex, yearsmarried), as.factor)) %>%
  readr::write_rds(file.path(cache_dir, "jmr_ex11.22.Rds"))
