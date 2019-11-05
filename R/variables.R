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
    data <- if (class(object) %in% "lmerMod") object@frame else object$frame
    nrow_group <- dplyr::distinct_at(data, dplyr::vars(vars_group)) %>% nrow()
    vars_pred_non_int <- grep("^[^:]+$", vars_pred, value = TRUE)
    is_pred_between <- purrr::map_lgl(vars_pred_non_int, function(var) {
      nrow_var <- dplyr::distinct_at(data, dplyr::vars(vars_group, var)) %>% nrow()
      nrow_var == nrow_group
    })
    vars_between <- vars_pred[is_pred_between]
    vars_within <- vars_pred[!is_pred_between]
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

# variables.lmer <- function(model) {
#   # get formula with no random terms
#   formula_complex <- formula(model)
#   formula_simple <- lme4::nobars(formula_complex)
#
#   # determine within and between variables
#   bare_terms <- variables.default(formula_simple)
#   outcome_term <- bare_terms$outcome
#   pred_terms <- bare_terms$predictor
  # rand_terms <- gsub(
  #   "1 ?\\| ?", "",
  #   as.character(lme4::findbars(formula_complex))
  # )
#   group_term <- rand_terms[which.min(nchar(rand_terms))]
#   within_terms <- sub_matches(rand_terms, paste0("(.*):", group_term), "\\1")
#   if (length(within_terms) > 0) {
#     # add within interactions
#     within_terms <- grep(paste0(within_terms, collapse = "|"), pred_terms, value = TRUE)
#   }
#   between_terms <- setdiff(pred_terms, within_terms)
# }
