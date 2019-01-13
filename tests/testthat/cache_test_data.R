# Cache the car:Anova data so that we don't have to include the pacakge just for
# testing

Anova <- car::Anova
map <- purrr::map
Fingers <- supernova::Fingers

as.character.call <- function(model) {
  Reduce(paste, deparse(model$call))
}

df.missing <- mtcars
df.missing[1,]$hp <- NA_real_
df.missing[2:3,]$disp <- NA_real_

list(
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
  set_names(map(., ~as.character.call(.x))) %>%
  map(Anova, type = 3) %>%
  write_rds("./tests/testthat/model_cache.Rds")
