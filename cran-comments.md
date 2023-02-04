## Release summary

- Fix print error where `generate_models()` would only print the comparison models for type III SS.

## Test environments

- Local install on macOS Monterey 12.5 (ARM); R 4.2.1
- GitHub Actions
  - macOS, 11.6.8; R 4.2.1
  - Microsoft Windows Server 2022, 10.0.20348; R 4.2.1
  - Ubuntu, 20.04.4; R-devel, R 4.2.1
  - `check_rhub()`
  - `check_win_devel()`

## R CMD check results

0 errors v | 0 warnings v | 0 notes v

R CMD check succeeded

## Reverse dependencies

No known reverse dependencies (no results from `devtools::revdep('supernova')`)
