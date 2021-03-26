#' Get the string representation of the formula.
#'
#' @param frm The formula (or formula-coercible object).
#'
#' @return A character string of the formula.
#' @keywords internal
frm_string <- function(frm) {
  deparse(as.formula(frm))
}


#' Build a formula from terms
#'
#' @param lhs The outcome term for the left-hand side.
#' @param rhs The terms for the right-hand side.
#' @param env The environment to assign to the formula (defaults to calling environment).
#'
#' @return The right-hand side terms are joined with `+`. Then, the right-hand side is joined
#'   to the left and returned as a [formula].
#'
#' @rdname formula_building
#' @keywords internal
#' @seealso formula_extraction formula_expansion
frm_build <- function(lhs, rhs, env = parent.frame()) {
  rhs <- if (length(rhs) == 0) "NULL" else rhs
  string <- paste(lhs, "~", paste(rhs, collapse = "+"))
  as.formula(string, env = env)
}


#' Expand a formula
#'
#' @param frm A formula that may have compact terms like `a * b`.
#'
#' @return The expanded formula where terms like `a * b` are expanded to `a + b + a:b`.
#'
#' @rdname formula_expansion
#' @keywords internal
#' @seealso formula_building formula_extraction
frm_expand <- function(frm) {
  frm_build(frm_outcome(frm), frm_terms(frm), environment(frm))
}


#' Remove a term from the right-hand side of a formula
#'
#' @param frm The formula to modify.
#' @param term The term to drop.
#'
#' @return The formula with the term removed.
#'
#' @keywords internal
#' @seealso formula_building formula_expansion formula_extraction
frm_remove_term <- function(frm, term) {
  rhs <- setdiff(frm_terms(frm), term)
  if (length(rhs) < 1) {
    rhs <- "NULL"
  }
  frm_build(frm_outcome(frm), rhs, environment(frm))
}


#' Extracting from formulae
#'
#' These tools extracting parts from formulae. The only function that extracts from the left-hand
#' side is `frm_outcome`. The rest only extract from the right-hand side. The word `term` is used to
#' denote functions that extract full terms from the formula, whereas `var` denotes functions that
#' extract the variables the formula uses. For example, the formula `y ~ a * b + (1 | group)` has
#' terms `a`, `b`, `a:b`, and `1 | group`. The same formula has variables `a`, `b`, and `group`.
#'
#' These tools are ONLY tested against models and formulae that are explicitly supported. See the
#' README and test cases for more information.
#'
#' @param frm The formula to extract values from
#'
#' @return The function name and parameters should be descriptive enough (see Description above).
#'   The extracted parts are always strings.
#'
#' @rdname formula_extraction
#' @keywords internal
#' @seealso formula_building formula_expansion
frm_outcome <- function(frm) {
  frm <- as.formula(frm)
  rlang::expr_deparse(rlang::f_lhs(frm))
}


#' @rdname formula_extraction
#' @keywords internal
frm_terms <- function(frm) {
  frm <- as.formula(frm)
  labels(stats::terms.formula(frm))
}


#' @rdname formula_extraction
#' @keywords internal
frm_interaction_terms <- function(frm) {
  frm <- as.formula(frm)
  stringr::str_subset(frm_fixed_terms(frm), stringr::fixed(":"))
}


#' @rdname formula_extraction
#' @keywords internal
frm_fixed_terms <- function(frm) {
  frm <- as.formula(frm)
  setdiff(frm_terms(frm), frm_random_terms(frm))
}


#' @rdname formula_extraction
#' @keywords internal
frm_random_terms <- function(frm) {
  frm <- as.formula(frm)
  stringr::str_subset(frm_terms(frm), stringr::fixed("|"))
}


#' @rdname formula_extraction
#' @keywords internal
frm_vars <- function(frm) {
  frm <- as.formula(frm)
  c(frm_fixed_vars(frm), frm_random_vars(frm))
}


#' @rdname formula_extraction
#' @keywords internal
frm_random_vars <- function(frm) {
  frm <- as.formula(frm)
  stringr::str_remove(frm_random_terms(frm), stringr::fixed("1 | "))
}


#' @rdname formula_extraction
#' @keywords internal
frm_fixed_vars <- function(frm) {
  frm <- as.formula(frm)
  frm_fixed_terms(frm) %>%
    stringr::str_split(":") %>%
    purrr::flatten_chr() %>%
    unique()
}
