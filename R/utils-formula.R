#' Get the string representation of the formula.
#'
#' @param frm The formula (or something that can be coerced to a formula).
#'
#' @return A character string of the formula.
#' @keywords internal
frm_string <- function(frm) {
  rlang::expr_deparse(as.formula(frm))
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


#' Remove a term or variable from the right-hand side of a formula
#'
#' @param frm The formula to modify.
#' @param term,var The term or variable to drop.
#'
#' @return The formula with the term removed.
#'
#' @rdname frm_remove
#' @keywords internal
#' @seealso formula_building formula_expansion formula_extraction
frm_remove_term <- function(frm, term) {
  rhs <- setdiff(frm_terms(frm), term)
  if (length(rhs) < 1) {
    rhs <- "NULL"
  }
  frm_build(frm_outcome(frm), rhs, environment(frm))
}


#' @rdname frm_remove
#' @keywords internal
frm_remove_var <- function(frm, var) {
  frm <- as.formula(frm)

  # find terms that var is in
  factors <- attr(terms(frm), "factors")
  if (var %in% rownames(factors)) {
    var_in_term <- factors[var, ] > 0
    to_remove <- colnames(factors)[var_in_term]
  } else {
    return(frm)
  }

  # construct new formula
  remaining_terms <- setdiff(frm_terms(frm), to_remove)
  frm_build(frm_outcome(frm), remaining_terms, environment(frm))
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
  labels(stats::terms(frm))
}


#' @rdname formula_extraction
#' @keywords internal
frm_interaction_terms <- function(frm) {
  frm <- as.formula(frm)
  factors <- attr(stats::terms(frm), "factors")
  multi_var_terms <- colSums(factors) > 1
  colnames(factors)[multi_var_terms]
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

  # there MIGHT be a random term, or it could just be a weird variable name like `a|b`
  # only require lme4 if there is a possibility of a random term
  possible_bars <- stringr::str_detect(frm_terms(frm), stringr::fixed("|"))
  random_terms <- if (any(possible_bars)) lme4::findbars(frm) else list()

  purrr::map_chr(random_terms, rlang::expr_deparse)
}


#' @rdname formula_extraction
#' @keywords internal
frm_vars <- function(frm) {
  frm <- as.formula(frm)
  all.vars(rlang::f_rhs(frm))
}


#' @rdname formula_extraction
#' @keywords internal
frm_random_vars <- function(frm) {
  frm <- as.formula(frm)

  # there MIGHT be a random term, or it could just be a weird variable name like `a|b`
  # only require lme4 if there is a possibility of a random term
  possible_bars <- stringr::str_detect(frm_terms(frm), stringr::fixed("|"))
  random_terms <- if (any(possible_bars)) lme4::findbars(frm) else list()

  purrr::map(random_terms, all.vars) %>%
    purrr::flatten_chr() %>%
    unique()
}


#' @rdname formula_extraction
#' @keywords internal
frm_fixed_vars <- function(frm) {
  frm <- as.formula(frm)

  # there MIGHT be a random term, or it could just be a weird variable name like `a|b`
  # only require lme4 if there is a possibility of a random term
  possible_bars <- stringr::str_detect(frm_terms(frm), stringr::fixed("|"))
  frm <- if (any(possible_bars)) lme4::nobars(frm) else frm

  frm_vars(frm)
}
