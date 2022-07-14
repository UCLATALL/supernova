## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

.onLoad <- function(libname, pkgname) {
  backports::import(pkgname, "str2lang")
}

# Suppress R CMD check note (these are actually used)
#' @importFrom methods setOldClass
NULL
