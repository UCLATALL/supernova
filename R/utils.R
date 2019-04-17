# Paste, Concatenate, add EOL and Print
#
# @param ... Character vectors to paste together.
#
# @return None (invisible \code{NULL}).
cat_line <- function(...) {
  cat(paste0(...), "\n", sep = "")
}

# Pad x to length of y
#
# @param x The vector to pad.
# @param y The vector with target length.
# @param after A subscript, after which the padding is to be appended.
# @param pad The value to pad the vector with.
#
# @return The padded vector.
pad <- function(x, y, after = length(x), pad = NA) {
  pad_len(x, length(y), after, pad)
}

# Pad x to length len
#
# @param x The vector to pad.
# @param len The length to pad the vector to.
# @param after A subscript, after which the padding is to be appended.
# @param pad The value to pad the vector with.
#
# @return The padded vector.
pad_len <- function(x, len, after = length(x), pad = NA) {
  if (length(x) > len) {
    warning("x cannot be padded. It is already longer than the desired length.")
  }
  if (length(x) >= len) return(x)
  append(x, rep(pad, times = len - length(x)), after = after)
}

# Convert SS type parameter to the corresponding numeric value
#
# @param type The value to convert, either string or numeric.
#
# @return The numeric value corresponding to the input.
resolve_type <- function(type) {
  clean_type <- trimws(tolower(type))
  if (clean_type %in% c("1", "i", "sequential")) return (1)
  if (clean_type %in% c("2", "ii", "hierarchical")) return (2)
  if (clean_type %in% c("3", "iii", "orthogonal")) return (3)
}
