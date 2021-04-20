
# Notes ---------------------------------------------------------------------------------------
# The functions in this file should be arranged alphabetically.


#' ANOVA table with nicer column names.
#'
#' @param model A model fitted by \code{\link{lm}} or \code{\link[lme4]{lmer}}.
#'
#' @return An ANOVA table with standard column names.
#' @keywords internal
anova_tbl <- function(model) {
  tbl <- anova(model)

  if (class(model) == "lmerMod") {
    names(tbl)[[1]] <- "Df"
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


#' Paste, Concatenate, add End-Of-Line and Print
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


#' Find and return the lower triangle of a matrix
#'
#' Same as [`lower.tri()`] except it returns the values from the matrix (rather than a positional
#' matrix that lets you look up the values).
#'
#' @inheritParams base::lower.tri
#'
#' @return The values in the lower triangluar part of the matrix.
#' @keywords internal
lower_tri <- function(x, diag = FALSE) {
  x[lower.tri(x, diag)]
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


#' Pad x to a given output length
#'
#' @param x The vector to pad.
#' @param output_length The length to pad the vector to.
#' @param after A subscript, after which the padding is to be appended.
#' @param pad The value to pad the vector with.
#'
#' @return The padded vector.
#' @keywords internal
pad_len <- function(x, output_length, after = length(x), pad = NA) {
  if (length(x) > output_length) {
    warning("x cannot be padded. It is already longer than the desired length.")
  }
  if (length(x) >= output_length) {
    return(x)
  }
  append(x, rep(pad, times = output_length - length(x)), after = after)
}


#' Paste together lines of text.
#'
#' The lines are joined together with a newline (`\n`) character.
#'
#' @param ... Vectors of lines of text.
#'
#' @return Check out the [paste] function for more information.
#' @keywords internal
paste_line <- function(...) {
  paste(..., sep = "\n")
}


#' Rename a column in a data frame
#'
#' @param data A data frame to modify.
#' @param col_name A character vector of columns to rename.
#' @param replacement A character vector of replacement column names.
#'
#' @return Returns the renamed data frame.
#' @keywords internal
rename <- function(data, col_name, replacement) {
  names(data)[match(col_name, names(data))] <- replacement
  data
}


#' Convert SS type parameter to the corresponding numeric value
#'
#' @param type The value to convert, either string or numeric.
#'
#' @return The numeric value corresponding to the input.
#' @keywords internal
resolve_type <- function(type) {
  clean_type <- trimws(tolower(type))
  if (clean_type %in% c("1", "i", "sequential")) {
    return(1)
  }
  if (clean_type %in% c("2", "ii", "hierarchical")) {
    return(2)
  }
  if (clean_type %in% c("3", "iii", "orthogonal")) {
    return(3)
  }
}


#' Update a model in the environment the model was created in
#'
#' [`stats::update()`] will perform the update in [`parent.frame()`] by default, but this can cause
#' problems when the update is called by another function (so the parent frame is no longer the
#' environment the user is in).
#'
#' @inheritParams stats::update
#'
#' @return The updated model is returned.
update_in_env <- function(object, formula., ...) {
  code <- stats::update(object, formula., ..., evaluate = FALSE)
  suppressMessages(eval(code, environment(formula(object))))
}
