## Release summary

- Don't trim the leading digit from p-value or PRE when they are exactly 1
- Fix issue on CRAN check when lme4 (optional) is not installed but is used in tests

## Test environments

- Local install on macOS Monterey 13.1 (ARM); R 4.2.1
- GitHub Actions
  - macOS: 12.6.3; R: 4.2.3
  - Microsoft Windows Server 2022: 10.0.20348; R: 4.2.3, 3.6.3
  - Ubuntu: 18.04.6; R: devel, 4.2.3, 4.1.3
  - `check_rhub()`
  - `check_win_devel()`

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## Reverse dependencies

Dependencies were identified with `devtools::revdep_check()` and tested with `revdepcheck::revdep_check(num_workers = 4)`. No problems were found.

Packages:

- eda4treeR
