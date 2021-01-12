#' Print the output of \code{lm()} with the fitted equation.
#'
#' @param x The fitted linear model to print.
#' @param digits The minimal number of significant digits.
#'
#' @return Invisibly return the fitted linear model.
#' @export
equation <- function(x, digits = max(3L, getOption("digits") - 3L)) {
  format_term <- function(value, name, digits) {
    sprintf("%s*%s", format(value, digits), name)
  }

  coefs <- stats::coef(x)
  intercept <- format(coefs[[1]], digits)
  model_terms <- purrr::imap_chr(coefs[2:length(coefs)], ~ format_term(.x, .y, digits))
  model <- paste0(model_terms, collapse = " + ")

  rhs <- paste(intercept, model, "e", sep = " + ")
  equation <- paste0(variables(x)$outcome, " = ", rhs)
  cat("Fitted equation:\n", equation, "\n\n", sep = "")

  invisible(x)
}
