## Release summary

* Fix issue where using a model fit with an interactive term that has `factor()`
  in it (e.g. `lm(mpg ~ factor(cyl) * hp, data = mtcars)`) would result in an
  incorrect *df* (and related values) in the ANOVA table.

## Test environments

- Local install on macOS Monterey 12.4 (ARM); R 4.2.1
- GitHub Actions
  * macOS, 11.6.6; R 4.2.1
  * Microsoft Windows Server 2022, 10.0.20348; R 4.2.1
  * Ubuntu, 20.04; R-devel, R 4.2.1


## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded


## Reverse dependencies

No known reverse dependencies (no results from `devtools::revdep('supernova')`)
