#' Print the output of \code{lm()} with the fitted equation.
#'
#' @param x The fitted linear model to print.
#' @param digits The minimal number of significant digits.
#' @param ... Ignored.
#'
#' @return Invisibly return the fitted linear model.
#' @export
print.lm <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  format_term <- function(value, name, digits) {
    sprintf("%s*%s", format(value, digits), name)
  }

  coefs <- stats::coef(x)
  if (!length(coefs)) return(original_print_lm(x, digits, ...))

  intercept <- format(coefs[[1]], digits)
  model_terms <- purrr::imap_chr(coefs[2:length(coefs)], ~ format_term(.x, .y, digits))
  model <- paste0(model_terms, collapse = " + ")

  rhs <- paste(intercept, model, "e", sep = " + ")
  equation <- paste0(variables(x)$outcome, " = ", rhs)

  original_print_lm(x, digits, ...)
  cat("Fitted equation:\n", equation, "\n\n", sep = "")
  invisible(x)
}


#' @keywords internal
original_print_lm <- function (x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  if (length(stats::coef(x))) {
    cat("Coefficients:\n")
    print.default(format(stats::coef(x), digits = digits), print.gap = 2L, quote = FALSE)
  }
  else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}
