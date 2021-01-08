#' Build a formula from terms
#'
#' @param lhs The outcome term for the left-hand side.
#' @param rhs The terms for the right-hand side.
#'
#' @return The right-hand side terms are joined with \code{+}. Then, the right-hand side is joined
#'   to the left and returned as a \code{\link{formula}}.
#'
#' @rdname formula_building
#' @keywords internal
#' @seealso formula_extraction formula_expansion
frm_build <- function(lhs, rhs) {
  as.formula(paste(lhs, "~", paste(rhs, collapse = "+")))
}


#' Expand a formula
#'
#' @param frm A formula that may have compact terms like \code{a * b}.
#'
#' @return The expanded formula where terms like \code{a * b} are expanded to \code{a + b + a:b}.
#'
#' @rdname formula_expansion
#' @keywords internal
#' @seealso formula_building formula_extraction
frm_expand <- function(frm) {
  frm_build(frm_outcome(frm), frm_terms(frm))
}


#' Extracting from formulae
#'
#' This set of tools all prefixed \code{frm_} is used for extracting parts from formulae. The word
#' \code{term} is used to denote functions that extract full terms from the formula, whereas the
#' word \code{var} is used to denote functions that extract the variables the formula uses. For
#' example, the formula \code{y ~ a * b + (1 | group)} has terms \code{a}, \code{b}, \code{a:b}, and
#' \code{1 | group}. The same formula has vars \code{a}, \code{b}, and \code{group}.
#'
#' These tools are ONLY tested against models and formulae that are explicitly supported. See the
#' README and test cases for more information.
#'
#' @param frm The formula to extract values from
#'
#' @return The name of the function name and signature be clear enough about the return value.
#'
#' @rdname formula_extraction
#' @keywords internal
#' @seealso formula_building formula_expansion
frm_outcome <- function(frm) {
  rlang::expr_deparse(rlang::f_lhs(frm))
}


#' @rdname formula_extraction
#' @keywords internal
frm_terms <- function(frm) {
  labels(stats::terms.formula(frm))
}


#' @rdname formula_extraction
#' @keywords internal
frm_fixed_terms <- function(frm) {
  setdiff(frm_terms(frm), frm_random_terms(frm))
}


#' @rdname formula_extraction
#' @keywords internal
frm_interaction_terms <- function(frm) {
  stringr::str_subset(frm_fixed_terms(frm), stringr::fixed(':'))
}


#' @rdname formula_extraction
#' @keywords internal
frm_fixed_vars <- function(frm) {
  setdiff(frm_fixed_terms(frm), frm_interaction_terms(frm))
}


#' @rdname formula_extraction
#' @keywords internal
frm_random_terms <- function(frm) {
  stringr::str_subset(frm_terms(frm), stringr::fixed('|'))
}


#' @rdname formula_extraction
#' @keywords internal
frm_random_vars <- function(frm) {
  stringr::str_remove(frm_random_terms(frm), stringr::fixed("1 | "))
}
