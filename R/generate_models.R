#' Generate a List of Models for Computing Different Types of Sums of Squares
#'
#' This function will return a list of lists where the top-level keys (names) of
#' the items indicate the component of the full model (i.e. the term) that the
#' generated models can be used to test. At each of these keys is a list with
#' both the \code{complex} and \code{simple} models that can be compared to test
#' the component. The \code{complex} models always include the target term, and
#' the \code{simple} models are identical to the \code{complex} except the
#' target term is removed. Thus, when the models are compared (e.g. using
#' \code{\link{anova}}, except for Type III; see details below), the resulting
#' values will show the effect of adding the target term to the model. There are
#' three generally used approaches to determining what the appropriate
#' comparison models should be, called Type I, II, and III. See the sections
#' below for more information on these types.
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
#' mod_Height <- anova(mods_2[["Height"]]$simple, mods_2[["Height"]]$complex)
#' mod_Height[["Sum of Sq"]][[2]]
#'
#' @export
generate_models <- function(model, type) {
  type <- resolve_type(type)
  mod_vars <- variables(model)
  outcome <- mod_vars$outcome
  terms <- mod_vars$predictor

  if (type == 3) warning(
    "The Type III models generated cannot be compared using anova().\n",
    "  For model comparisons with Type III, use drop1() instead.\n",
    "  Type ?generate_models() for more details.\n"
  )

  # generate comparison models for individual terms
  models <- purrr::imap(terms, function(term, term_index) {
    if (type == 1) complex <- to_formula(outcome, terms[1:term_index])
    if (type == 2) {
      rhs <- strsplit(mod_vars$predictor, ":") %>%
        magrittr::set_names(mod_vars$predictor) %>%
        # remove terms other than the target term that contain the target term
        purrr::discard(function(x) all(strsplit(term,  ":")[[1]] %in% x)) %>%
        names() %>%
        append(term, after = term_index - 1)
      complex <- to_formula(outcome, rhs)
    }
    if (type == 3) complex <- to_formula(outcome, terms)

    list(complex = complex, simple = remove_term(complex, term))
  })

  # add names to list and prepend full model comparison
  if (!purrr::is_empty(models)) {
    models <- models %>%
      magrittr::set_names(terms) %>%
      append(list(`Full Model` = list(
        complex = to_formula(outcome, terms),
        simple = to_formula(outcome, "NULL")
      )), after = 0L)

    # back-convert to original model fit if needed
    if ("lm" %in% class(model)) {
      models <- purrr::map(models, function(model_pair) {
        purrr::map(model_pair, ~update(model, .x))
      })
    }
  }

  class(models) <- c("comparison_models", class(models))
  attr(models, "type") <- strrep("I", type)
  attr(models, "model") <- model
  models
}

#' @export
print.comparison_models <- function(x, ...) {
  model <- attr(x, "model")
  null_model <- to_formula(variables(model)$outcome, "NULL")
  cat_line("Comparison Models for Type ", attr(x, "type"), " SS")
  cat_line("Model: ", deparse(formula(model)))
  cat_line("")
  if (purrr::is_empty(x)) {
    cat_line("No comparisons for empty model.")
  } else {
    cat_line(names(x)[[1]])
    cat_line("  complex: ", deparse(formula(x[[1]]$complex)))
    cat_line("   simple: ", deparse(null_model))
    purrr::iwalk(x[2:length(x)], function(part, name) {
      cat_line(name)
      cat_line("  complex: ", formula_string(x, part$complex, name))
      cat_line("   simple: ", formula_string(x, part$simple, name))
    })
  }
}


# Helpers -----------------------------------------------------------------

to_formula <- function(lhs, rhs) {
  as.formula(paste(lhs, "~", paste(rhs, collapse = "+")))
}

expand_formula <- function(f) {
  to_formula(variables(f)$outcome, variables(f)$predictor)
}

remove_term <- function(model, term) {
  vars <- variables(model)
  terms <- vars$predictor
  rhs <- if (length(terms) == 1) "NULL" else terms[!terms %in% term]
  to_formula(vars$outcome, rhs)
}

formula_string <- function(obj, part, term) {
  model <- attr(obj, "model")
  type <- resolve_type(attr(obj, "type"))

  if (type == 1) return(deparse(formula(part)))
  if (type == 3 && formula(obj[[term]]$complex) == formula(part)) {
    return(deparse(expand_formula(model)))
  }

  # determine which variables were removed
  rem_terms <- if (type == 2) {
    setdiff(variables(model)$predictor, variables(part)$predictor)
  } else if (type == 3) {
    term
  }

  # escape special regex characters in variables
  rem_pat <- stringr::str_replace_all(rem_terms, "(\\W)", "\\\\\\1")

  # add regex to remove immediately following space and plus
  if (type == 2) {
    rem_pat <- paste0(paste0(" ", rem_pat, collapse = " \\+?|"), "$")
  } else if (type == 3) {
    rem_pat <- paste0(" ", rem_pat, "( \\+|$)")
  }

  # perform replacements
  deparse(expand_formula(model)) %>%
    # replace variables with spaces in full string
    stringr::str_replace_all(rem_pat, function(str) strrep(" ", nchar(str))) %>%
    # trim dangling spaces and plus signs from end
    stringr::str_remove("[ \\+]*$")
}
