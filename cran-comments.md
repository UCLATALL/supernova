## Release summary

- Check package version using string comparison to pass _R_CHECK_STOP_ON_INVALID_NUMERIC_VERSION_INPUTS_ check on CRAN
- Match parameter names to docs for two internal functions

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

No known reverse dependencies (no results from `devtools::revdep('supernova')`)
