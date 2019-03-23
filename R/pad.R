#' Pad x to length of y
#'
#' @param x The vector to pad.
#' @param y The vector with target length.
#' @param after A subscript, after which the padding is to be appended.
#' @param pad The value to pad the vector with.
#'
#' @return The padded vector.
pad <- function(x, y, after = length(x), pad = NA) {
  pad_len(x, length(y), after, pad)
}

#' Pad x to length len
#'
#' @param x The vector to pad.
#' @param len The length to pad the vector to.
#' @param after A subscript, after which the padding is to be appended.
#' @param pad The value to pad the vector with.
#'
#' @return The padded vector.
pad_len <- function(x, len, after = length(x), pad = NA) {
  if (length(x) > len) {
    warning("x cannot be padded. It is already longer than the desired length.")
  }
  if (length(x) >= len) return(x)
  append(x, rep(pad, times = len - length(x)), after = after)
}
