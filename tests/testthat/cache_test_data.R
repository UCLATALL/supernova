# Cache the car:Anova data so that we don't have to include the package just for
# testing

`%>%` <- magrittr::`%>%`
Fingers <- supernova::Fingers

as.character.call <- function(model) {
  Reduce(paste, deparse(model$call))
}

df.missing <- mtcars
df.missing[1,]$hp <- NA_real_
df.missing[2:3,]$disp <- NA_real_

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
  lm(uptake ~ Treatment, data = CO2[1:80,]),
  lm(uptake ~ Treatment * Type, data = CO2[1:80,])
) %>%
  purrr::set_names(purrr::map(., ~as.character.call(.x)))

cache_dir <- "./tests/testthat/cache"
if (!dir.exists(cache_dir)) dir.create(cache_dir)

models %>%
  purrr::map(anova) %>%
  readr::write_rds(file.path(cache_dir, "model_cache_type_1.Rds"))

models %>%
  purrr::map(car::Anova, type = 2) %>%
  readr::write_rds(file.path(cache_dir, "model_cache_type_2.Rds"))

models %>%
  purrr::map(car::Anova, type = 3) %>%
  readr::write_rds(file.path(cache_dir, "model_cache_type_3.Rds"))
