#' ANOVA table with nicer column names.
#'
#' @param model A model fitted by \code{\link{lm}} or \code{\link[lme4]{lmer}}.
#'
#' @return An ANOVA table with standard column names.
#' @keywords internal
anova_tbl <- function(model) {
  tbl <- anova(model)

  if (class(model) == 'lmerMod') {
    names(tbl)[[1]] <- 'Df'
  }

  out <- data.frame(
    term = row.names(tbl),
    SS = tbl[["Sum Sq"]],
    df = tbl[["Df"]],
    MS = tbl[["Mean Sq"]],
    F = tbl[["F value"]],
    stringsAsFactors = FALSE
  )

  if (!is.null(tbl[["Pr(>F)"]])) {
    out[["p"]] <- tbl[["Pr(>F)"]]
  }

  out
}

#' Paste, Concatenate, add EOL and Print
#'
#' @param ... Character vectors to paste together.
#'
#' @return None (invisible \code{NULL}).
#' @keywords internal
cat_line <- function(...) {
  cat(paste0(...), "\n", sep = "")
}

#' Insert a horizontal rule in a table for pretty printing
#'
#' @param df The original data.frame
#' @param insert_at The row in which to insert the dashes.
#'
#' @return The original data.frame with the horizontal rule inserted.
#' @keywords internal
insert_rule <- function(df, insert_at) {
  dashes <- strrep("-", vapply(df, function(x) max(nchar(x)), 0))
  insert_row(df, insert_at, dashes)
}


#' Insert a row of data into a table.
#'
#' @param df The original data.frame.
#' @param insert_at The row in which to insert the data.
#' @param contents The row of contents to insert (should be a vector of length
#'   \code{ncol(df)}).
#'
#' @return The original data.frame with the row of data inserted.
#' @keywords internal
insert_row <- function(df, insert_at, contents) {
  df[seq(insert_at + 1, nrow(df) + 1), ] <- df[seq(insert_at, nrow(df)), ]
  df[insert_at, ] <- contents
  df
}

#' Pad x to length of y
#'
#' @param x The vector to pad.
#' @param y The vector with target length.
#' @param after A subscript, after which the padding is to be appended.
#' @param pad The value to pad the vector with.
#'
#' @return The padded vector.
#' @keywords internal
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
#' @keywords internal
pad_len <- function(x, len, after = length(x), pad = NA) {
  if (length(x) > len) {
    warning("x cannot be padded. It is already longer than the desired length.")
  }
  if (length(x) >= len) return(x)
  append(x, rep(pad, times = len - length(x)), after = after)
}

#' Convert SS type parameter to the corresponding numeric value
#'
#' @param type The value to convert, either string or numeric.
#'
#' @return The numeric value corresponding to the input.
#' @keywords internal
resolve_type <- function(type) {
  clean_type <- trimws(tolower(type))
  if (clean_type %in% c("1", "i", "sequential")) return(1)
  if (clean_type %in% c("2", "ii", "hierarchical")) return(2)
  if (clean_type %in% c("3", "iii", "orthogonal")) return(3)
}
