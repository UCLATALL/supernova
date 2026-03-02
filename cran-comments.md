## Release summary

Patch release for a performance improvement:

- Compute Type III SS from QR decomposition instead of refitting reduced models

## Test environments

- Local install on macOS Sequoia 15.6.1 (ARM); R 4.5.1
- GitHub Actions
  - macOS: latest; R: release
  - Microsoft Windows Server: latest; R: release
  - Ubuntu: latest; R: devel, release
- devtools::check_win_devel()

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

Dependencies were identified with `devtools::revdep()` and tested with `revdepcheck::revdep_check(num_workers = 4)`. No problems were found.

Packages:

- coursekata
- eda4treeR
- gvcR
