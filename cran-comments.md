Only comment by Uwe Ligges on previous submission:

> Found the following possibly invalid URLs:
>  URL: ISBN:978-1138819832
> From: README.md
> Message: Invalid URI scheme
>
> Please do not use URL markup for ISBN numbers.

To fix, we made sure the ISBN is formatted as plaintext in the generated file.

## Test environments

* local install, R 3.6.1
* win-builder
* r-hub
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  - Ubuntu Linux 16.04 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran


## R CMD check results

0 errors | 0 warnings | 0 notes


## Reverse dependencies

No known reverse dependencies.
