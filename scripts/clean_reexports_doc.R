reexports <- readr::read_file("man/reexports.Rd")
purrr::walk2(
  readr::read_lines("scripts/reexports_find.txt"),
  readr::read_lines("scripts/reexports_replace.txt"),
  function(find, replace) {
    reexports <<- stringr::str_replace(reexports, stringr::fixed(find), replace)
  }
)
readr::write_file(reexports, "man/reexports.Rd")
rm(reexports)
