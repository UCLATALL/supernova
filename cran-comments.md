## Release summary

Patch release to fix deprecation warnings and test failures:

- Fix test failures caused by R reordering interaction term components
- Replace deprecated `geom_errorbarh()` with `geom_errorbar(orientation="y")` for ggplot2 4.0.0+
- Use `reformulas` package instead of deprecated `lme4::findbars()`/`lme4::nobars()`
- Add `reformulas` to Suggests
- Update minimum R version to 4.1

## Test environments

- Local install on macOS Sequoia 15.6.1 (ARM); R 4.5.1
- GitHub Actions
  - macOS: latest; R: release
  - Microsoft Windows Server: latest; R: release
  - Ubuntu: latest; R: devel, release, oldrel

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

Dependencies were identified with `devtools::revdep()` and tested with `revdepcheck::revdep_check(num_workers = 4)`. No problems were found.

Packages:

- coursekata
- eda4treeR
- gvcR
