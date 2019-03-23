#' Convert SS type parameter to the corresponding numeric value
#'
#' @param type The value to convert, either string or numeric.
#'
#' @return The numeric value corresponding to the input.
resolve_type <- function(type) {
  clean_type <- trimws(tolower(type))
  if (clean_type %in% c("1", "i", "sequential")) return (1)
  if (clean_type %in% c("2", "ii", "hierarchical")) return (2)
  if (clean_type %in% c("3", "iii", "orthogonal")) return (3)
}
