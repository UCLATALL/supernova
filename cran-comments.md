*Previous reviewer asked for note explaining why package was removed:*
Package was removed from CRAN because it was fixed too late (by a couple of hours) on Oct. 19. The
problem was a recent bug in `lintr` that caused it to fail when running R CMD CHECK during the CRAN 
build process. I implemented a workaround that is documented in the commit fixing the issue, if you 
are interested.

*Previous reviewer asked for additional documentation to `supernova-vctrs`*:
This was not a usable function/class and is just a collection of methods making the package
compatible with the `vctrs` package. I have removed `supernova-vctrs` from the exports.

---

# Original comments on release:

## Release summary

* Fix issues with lintr causing R CMD CHECK to fail
* Change maintainer to [@adamblake](https://github.com/adamblake)
* Change mislabeled factor level in `Fingers$Interest` to "Very Interested"


## Test environments

- Local install on macOS Big Sur 11.5; R 4.1.0
- GitHub Actions
  * Mac OS X, 10.15.7; R 4.1.1
  * Microsoft Windows Server 2019, 10.0.17763; R 4.1.1
  * Ubuntu, 20.04; R-devel, R 4.1.1, 3.4.4


## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded


## Reverse dependencies

No known reverse dependencies (no results from `devtools::revdep('supernova')`)
