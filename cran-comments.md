## Release summary

* Fix issue where models with long calls (i.e. `deparse(model$call)` results in a vector of length greater than 1) would break the functionality of `listwise_delete()`
* Change print method for `generate_models()` to look clean and comprehensible in Jupyter Notebooks.


## Test environments

- Local install on macOS Big Sur 11.5; R 4.1.2
- GitHub Actions
  * Mac OS X, 10.15.7; R 4.1.2
  * Microsoft Windows Server 2019, 10.0.17763; R 4.1.2
  * Ubuntu, 20.04; R-devel, R 4.1.2


## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded


## Reverse dependencies

No known reverse dependencies (no results from `devtools::revdep('supernova')`)
