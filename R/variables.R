#' Extract the variables from a model
#'
#' @param object An \code{\link{lm}} or \code{\link{supernova}} object
#'
#' @importFrom stats formula terms
#'
#' @return A list containing the \code{outcome} and \code{predictor} variables
#'   in the model.
#' @export
variables <- function(object) {
  fit <- if ("supernova" %in% class(object)) object$fit else object
  fit_formula <- formula(fit)
  all_vars <- all.vars(fit_formula)
  ivs <- labels(terms(fit_formula))
  list(outcome = all_vars[!(all_vars %in% ivs)], predictor = ivs)
}
