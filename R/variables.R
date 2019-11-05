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
  model <- if ("supernova" %in% class(object)) object$fit else object
  formula_complex <- formula(model)
  formula_simple <- lme4::nobars(formula_complex)

  vars_all <- all.vars(formula_simple)
  vars_pred <- labels(terms(formula_simple))
  vars_outcome <- setdiff(vars_all, vars_pred)

  rand_terms <- gsub("1 ?\\| ?", "", as.character(lme4::findbars(formula_complex)))
  vars_group <- rand_terms[which.min(nchar(rand_terms))]

  if ("lm" %in% class(object)) {
    # can't test within-ss designs using lm (unless you do some heavy lifting)
    vars_within <- character(0)
    vars_between <- vars_pred
  } else if ("lmerMod" %in% class(object)) {
    # need to determine which are within vs. between
    data <- object@frame
    nrow_group <- dplyr::distinct_at(data, dplyr::vars(vars_group)) %>% nrow()
    vars_pred_non_int <- grep("^[^:]+$", vars_pred, value = TRUE)
    is_pred_between_simple <- purrr::map_lgl(vars_pred_non_int, function(var) {
      nrow_var <- dplyr::distinct_at(data, dplyr::vars(vars_group, var)) %>% nrow()
      nrow_var == nrow_group
    })
    vars_within_simple <- vars_pred_non_int[!is_pred_between_simple]
    is_pred_within <- if (length(vars_within_simple) > 0) {
      grepl(paste0(vars_within_simple, collapse = "|"), vars_pred)
    } else {
      FALSE
    }
    vars_within <- vars_pred[is_pred_within]
    vars_between <- vars_pred[!is_pred_within]
  } else {
    # cannot determine which are within or between without more info
    vars_within <- character(0)
    vars_between <- character(0)
  }

  list(
    outcome = vars_outcome,
    predictor = vars_pred,
    group = vars_group,
    within = vars_within,
    between = vars_between
  )
}
