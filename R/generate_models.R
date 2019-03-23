#' Generate a List of Models for Computing Different Types of Sums of Squares
#'
#' This function will return a list that contains two lists of models, named
#' \code{augmented} and \code{compact}. In each list, the keys (names) of the
#' values indicate the term that the model relates to. The \code{augmented}
#' models always include the target term, and the \code{compact} models are
#' identical to the \code{augmented} except the target term is removed. Thus,
#' when the models are compared (e.g. using \code{\link{anova}}, except for Type
#' III; see details below), the resulting values will show the effect of adding
#' the target term to the model. There are three generally used approaches to
#' determining what the appropriate comparison models should be, called Type I,
#' II, and III. See the sections below for more information on these types.
#'
#' @section Type I: For Type I SS, or sequential SS, each term is considered in
#'   order after the preceding terms are considered. Consider the example model
#'   \code{Y ~ A + B + A:B}, where ":" indicates an interaction. To determine
#'   the Type I effect of \code{A}, we would compare the model \code{Y ~ A} to
#'   the same model without the term: \code{Y ~ NULL}. For \code{B}, we compare
#'   \code{Y ~ A + B} to \code{Y ~ A}; and for \code{A:B}, we compare \code{Y ~
#'   A + B + A:B} to \code{Y ~ A + B}. Incidentally, the \code{\link{anova}}
#'   function that ships with the base installation of R computes Type I
#'   statistics.
#'
#' @section Type II: For Type II SS, or hierarchical SS, each term is considered
#'   in the presence of all of the terms that do not include it. For example,
#'   consider an example three-way factorial model \code{Y ~ A + B + C + A:B +
#'   A:C + B:C + A:B:C}, where ":" indicates an interaction. The effect of
#'   \code{A} is found by comparing \code{Y ~ B + C + B:C + A} to \code{Y ~ B +
#'   C + B:C} (the only terms included are those that do not include \code{A}).
#'   For \code{B}, the comparison models would be \code{Y ~ A + C + A:C + B} and
#'   \code{Y ~ A + C + A:C}; for \code{A:B}, the models would be \code{Y ~ A + B
#'   + C + A:C + B:C + A:B} and \code{Y ~ A + B + C + A:C + B:C}; and so on.
#'
#' @section Type III: For Type III SS, or orthogonal SS, each term is considered
#'   in the presence of all of the other terms. For example, consider an example
#'   two-way factorial model \code{Y ~ A + B + A:B}, where ":" indicates an
#'   interaction. The effect of \code{A} is found by comparing \code{Y ~ B + A:B
#'   + A} to  \code{Y ~ B + A:B}; for \code{B}, the comparison models would be
#'   \code{Y ~ A + A:B + B} and \code{Y ~ A + A:B}; and for \code{A:B}, the
#'   models would be \code{Y ~ A + B + A:B} and \code{Y ~ A + B}.
#'
#'   Unfortunately, \code{\link{anova}()} cannot be used to compare Type III
#'   models. \code{anova()} does not allow for violation of the principle of
#'   marginality, which is the rule that interactions should only be tested in
#'   the context of their lower order terms. When an interaction term is present
#'   in a model, \code{anova()} will automatically add in the lower-order terms,
#'   making a model like \code{Y ~ A + A:B} unable to be compared: it will add
#'   the lower-order term \code{B},and thus use the model \code{Y ~ A + B + A:B}
#'   instead. To get the appropriate statistics for Type III comparisons, use
#'   \code{\link{drop1}()} with the full scope, i.e. \code{drop1(model_fit,
#'   scope = . ~ .)}.
#'
#' @param model The model to generate the models from, of the type
#'   \code{\link{lm}}, \code{\link{aov}}, or \code{\link{formula}}.
#' @param type The type of sums of squares to calculate:
#'   \itemize{
#'     \item Use \code{1}, \code{I}, and \code{sequential} for Type I.
#'     \item Use \code{2}, \code{II}, and \code{hierarchical} for Type II.
#'     \item Use \code{3}, \code{III}, and \code{orthogonal} for Type III.
#'   }
#'
#' @return A list of the augmented models for each term, where the associated
#'   term is the key for each model in the list.
#'
#' @examples
#' # create all type 2 comparison models
#' mod <- lm(Thumb ~ Height * Sex, data = Fingers)
#' mods_2 <- generate_models(mod, type = 2)
#'
#' # compute the SS for the Height term
#' mod_Height <- anova(mods_2$compact[["Height"]], mods_2$augmented[["Height"]])
#' mod_Height[["Sum of Sq"]][[2]]
#'
#'
#'
#' @export
generate_models <- function(model, type) {
  type <- resolve_type(type)
  augmented <- if (type == 1) {
    type_1_models(formula(model))
  } else if (type == 2) {
    type_2_models(formula(model))
  } else if (type == 3) {
    warning(
      "The Type III models generated above cannot be compared using anova().\n",
      "  For model comparisons, use drop1() instead.\n",
      "  Type ?generate_models() for more details.\n"
    )
    type_3_models(formula(model))
  }
  compact <- comparison_models(augmented)

  if (any(c("lm", "aov") %in% class(model))) {
    augmented <- purrr::map(augmented, ~update(model, .x))
    compact <- purrr::map(compact, ~update(model, .x))
  }

  list(augmented = augmented, compact = compact)
}

comparison_models <- function(models) {
  purrr::imap(models, function(model, term) {
    f_string <- Reduce(paste, deparse(formula(model)))
    parts <- trimws(strsplit(f_string, "~", fixed = TRUE)[[1]])
    terms <- trimws(strsplit(parts[[2]], "+", fixed = TRUE)[[1]])
    rhs <- if (length(terms) > 1) {
      paste0(terms[!terms %in% term], collapse = " + ")
    } else {
      "NULL"
    }
    as.formula(paste0(parts[[1]], " ~ ", rhs))
  })
}

type_1_models <- function(model) {
  vars <- variables(model)
  terms <- vars[["predictor"]]
  models <- purrr::map(seq_along(terms), function(term_index) {
    rhs <- paste0(terms[1:term_index], collapse = " + ")
    as.formula(paste0(vars[["outcome"]], " ~ ", rhs))
  })
  purrr::set_names(models, terms)
}

type_2_models <- function(model) {
  vars <- variables(model)
  terms <- vars[["predictor"]]
  terms_split <- purrr::set_names(strsplit(terms, ":"), terms)
  models <- purrr::map(terms, function(term) {
    term_split <- strsplit(term, ":")[[1]]
    # remove terms that contain the term
    rhs <- purrr::discard(terms_split, function(x) all(term_split %in% x))
    # re-add target term to end of term list
    rhs <- paste0(append(names(rhs), term), collapse = " + ")
    as.formula(paste0(vars[["outcome"]], " ~ ", rhs))
  })
  purrr::set_names(models, terms)
}

type_3_models <- function(model) {
  vars <- variables(model)
  terms <- vars[["predictor"]]
  models <- purrr::map(terms, function(term) {
    # move target term to end of term list
    rhs <- paste0(append(terms[!terms %in% term], term), collapse = " + ")
    as.formula(paste0(vars[["outcome"]], " ~ ", rhs))
  })
  purrr::set_names(models, terms)
}
