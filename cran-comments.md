## Release summary

- Remove deprecated `superanova()`
- Remove `magrittr`, `ggplot2`, and `backports` dependencies

## Test environments

- Local install on macOS Monterey 13.1 (ARM); R 4.3.2
- GitHub Actions
  - macOS: 12.7.2; R: 4.3.2
  - Microsoft Windows Server 2022: 10.0.20348; R: 4.3.2, 3.6.3
  - Ubuntu: 22.04.3; R: devel, 4.3.2, 4.2.3
  - `check_rhub()`
  - `check_win_devel()`

## R CMD check results

There were 4 notes during the checks.

1. I am the maintainer of the package and have updated the email address.

   ```
   * checking CRAN incoming feasibility ... [15s] NOTE
   Maintainer: 'Adam Blake <adam@coursekata.org>'

   New maintainer:
     Adam Blake <adam@coursekata.org>
   Old maintainer(s):
     Adam Blake <adamblake@g.ucla.edu>
   ```

2. On Windows Server 2022, R-devel, 64 bit:

   ```
   * checking for non-standard things in the check directory ... NOTE
   Found the following files/directories:
     ''NULL''
   ```

   As noted in [R-hub issue #560](https://github.com/r-hub/rhub/issues/560), this seems to be an Rhub issue and so can likely be ignored.

3. On Windows Server 2022, R-devel, 64 bit:

   ```
   * checking for detritus in the temp directory ... NOTE
   Found the following files/directories:
     'lastMiKTeXException'
   ```

   As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

4. On Fedora Linux, R-devel, clang, gfortran, and on Ubuntu Linux 20.04.1 LTS, R-release, GCC:

   ```
   * checking HTML version of manual ... NOTE
   Skipping checking HTML validation: no command 'tidy' found
   ```

   As noted in [R-hub issue #548](https://github.com/r-hub/rhub/issues/548), this is a missing formatter/linter on R-Hub and can likely be ignored.

## Reverse dependencies

Dependencies were identified with `devtools::revdep()` and tested with `revdepcheck::revdep_check(num_workers = 4)`. No problems were found.

Packages:

- eda4treeR (meta-package with no code)
