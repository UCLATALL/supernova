#' Generate a List of Models for Computing Different Types of Sums of Squares
#'
#' This function will return a list of lists where the top-level keys (names) of the items indicate
#' the component of the full model (i.e. the term) that the generated models can be used to test. At
#' each of these keys is a list with both the `complex` and `simple models` that can be compared to
#' test the component. The `complex` models always include the target term, and the `simple` models
#' are identical to the `complex` except the target term is removed. Thus, when the models are
#' compared (e.g. using [`anova`], except for Type III; see details below), the resulting values
#' will show the effect of adding the target term to the model. There are three generally used
#' approaches to determining what the appropriate comparison models should be, called Type I, II,
#' and III. See the sections below for more information on these types.
#'
#'
#' # Type I
#'
#' For Type I SS, or sequential SS, each term is considered in order after the preceding terms are
#' considered. Consider the example model
#'
#' `Y ~ A + B + A:B`
#'
#' , where ":" indicates an interaction. To determine the Type I effect of `A`, we would compare the
#' model `Y ~ A` to the same model without the term: `Y ~ NULL`. For `B`, we compare `Y ~ A + B` to
#' `Y ~ A`; and for `A:B`, we compare `Y ~ A + B + A:B` to `Y ~ A + B`. Incidentally, the [anova()]
#' function that ships with the base installation of R computes Type I statistics.
#'
#'
#' # Type II
#'
#' For Type II SS, or hierarchical SS, each term is considered in the presence of all of the terms
#' that do not include it. For example, consider an example three-way factorial model
#'
#' `Y ~ A + B + C + A:B + A:C + B:C + A:B:C`
#'
#' , where ":" indicates an interaction. The effect of `A` is found by comparing `Y ~ B + C + B:C +
#' A` to `Y ~ B + C + B:C` (the only terms included are those that do not include `A`). For `B`, the
#' comparison models would be `Y ~ A + C + A:C + B` and `Y ~ A + C + A:C`; for `A:B`, the models
#' would be `Y ~ A + B + C + A:C + B:C + A:B` and `Y ~ A + B + C + A:C + B:C`; and so on.
#'
#'
#' # Type III
#'
#' For Type III SS, or orthogonal SS, each term is considered in the presence of all of the other
#' terms. For example, consider an example two-way factorial model
#'
#' `Y ~ A + B + A:B`
#'
#' , where `:` indicates an interaction between the terms. The effect of `A`, is found by comparing
#' `Y ~ B + A:B + A` to `Y ~ B + A:B`; for `B`, the comparison models would be `Y ~ A + A:B + B` and
#' `Y ~ A + A:B`; and for `A:B`, the models would be `Y ~ A + B + A:B` and `Y ~ A + B`.
#'
#' Unfortunately, [anova()] cannot be used to compare Type III models. `anova()` does not allow for
#' violation of the principle of marginality, which is the rule that interactions should only be
#' tested in the context of their lower order terms. When an interaction term is present in a model,
#' `anova()` will automatically add in the lower-order terms, making a model like `Y ~ A + A:B`
#' unable to be compared: it will add the lower-order term `B`,and thus use the model `Y ~ A + B +
#' A:B` instead. To get the appropriate statistics for Type III comparisons, use [drop1()] with the
#' full scope, i.e. `drop1(model_fit, scope = . ~ .)`.
#'
#' @param model The model to generate the models from, of the type [`lm()`], [`aov()`], or
#'   [`formula()`].
#' @param type The type of sums of squares to calculate: - Use `1`, `I`, and `sequential` for Type
#'   I. - Use `2`, `II`, and `hierarchical` for Type II. - Use `3`, `III`, and `orthogonal` for Type
#'   III.
#'
#'
#' @return A list of the augmented models for each term, where the associated term is the key for
#'   each model in the list.
#'
#' @examples
#' # create all type 2 comparison models
#' models <- generate_models(
#'   lm(mpg ~ hp * factor(am), data = mtcars),
#'   type = 2
#' )
#'
#' # compute the SS for the hp term
#' anova_hp <- anova(models$hp$simple, models$hp$complex)
#' anova_hp[["Sum of Sq"]][[2]]
#' @export
generate_models <- function(model, type = 3) {
  UseMethod("generate_models", model)
}


#' @rdname generate_models
#' @export
generate_models.formula <- function(model, type = 3) {
  type <- resolve_type(type)
  frm <- frm_expand(model)

  # generate comparison models for individual terms
  models <- switch(type,
    build_formulae_type_1(frm),
    build_formulae_type_2(frm),
    build_formulae_type_3(frm)
  )

  if (!vctrs::vec_is_empty(models)) {
    # prepend full model comparison
    models <- append(models, build_formulae_full_model(frm), after = 0L)
  }

  add_class_information(models, model, type)
}

#' @rdname generate_models
#' @export
generate_models.lm <- function(model, type = 3) {
  if (length(model$na.action) > 0) {
    model <- listwise_delete(model)
  }

  if (type == 1 || type == 2) {
    models <- generate_models.formula(formula(model), type = type)
    fits <- lapply(models, function(frm_pair) {
      lapply(frm_pair, function(frm) update_in_env(model, frm))
    })
  }

  if (type == 3) {
    term_labels <- attr(model$terms, "term.labels")

    if (length(term_labels) == 0) {
      return(add_class_information(list(), model, type))
    }

    # Create lightweight model stubs without expensive lm.fit() calls.
    # The actual SS computation is done via QR decomposition in supernova.lm().
    fits <- lapply(term_labels, function(term) {
      stub <- list(call = base::call("drop_term", model$call, term))
      oldClass(stub) <- "lm"
      list(complex = model, simple = stub)
    })
    names(fits) <- term_labels

    # Lightweight null model stub. Table computation uses QR decomposition
    # directly, so we don't need residuals/fitted.values vectors here.
    null_stub <- list(call = base::call("lm"))
    oldClass(null_stub) <- "lm"
    full_model_pair <- list(complex = model, simple = null_stub)
    fits <- c(list("Full Model" = full_model_pair), fits)
  }

  add_class_information(fits, model, type)
}

add_class_information <- function(models, model, type) {
  class(models) <- c("comparison_models", class(models))
  attr(models, "type") <- strrep("I", type)
  attr(models, "model") <- model
  models
}


# Formula builders ----------------------------------------------------------------------------

build_formulae_full_model <- function(frm) {
  null_model <- frm_build(frm_outcome(frm), "NULL", environment(frm))
  models <- list(complex = frm, simple = null_model)
  rlang::set_names(list(models), "Full Model")
}

build_formulae_type_1 <- function(frm) {
  terms <- frm_terms(frm)
  if (length(terms) == 0) {
    return(list())
  }

  models <- purrr::imap(terms, function(term, term_index) {
    complex <- frm_build(frm_outcome(frm), terms[1:term_index], environment(frm))
    list(complex = complex, simple = frm_remove_term(complex, term))
  })

  rlang::set_names(models, terms)
}

build_formulae_type_2 <- function(frm) {
  terms <- frm_terms(frm)
  if (length(terms) == 0) {
    return(list())
  }

  models <- purrr::imap(frm_terms(frm), function(term, term_index) {
    # remove terms other than the target term that contain the target term
    term_parts <- stringr::str_split(term, ":")[[1]]
    remaining_terms <- purrr::discard(frm_terms(frm), function(maybe_delete) {
      # don't remove the term we are testing
      if (maybe_delete != term) {
        maybe_delete_parts <- stringr::str_split(maybe_delete, ":")[[1]]
        all(term_parts %in% maybe_delete_parts)
      } else {
        FALSE
      }
    })

    complex <- frm_build(frm_outcome(frm), remaining_terms, environment(frm))
    list(complex = complex, simple = frm_remove_term(complex, term))
  })

  rlang::set_names(models, terms)
}

build_formulae_type_3 <- function(frm) {
  terms <- frm_terms(frm)
  if (length(terms) == 0) {
    return(list())
  }

  models <- purrr::map(terms, function(term) {
    list(complex = frm, simple = frm_remove_term(frm, term))
  })

  rlang::set_names(models, terms)
}


# Drop 1 --------------------------------------------------------------------------------------

#' Drop a term from the given model
#'
#' This function is needed to re-fit the models for Type III SS. If you have a model with an
#' interactive term (e.g. `y ~ a + b + a:b`), when you try to refit without one of the lower-order
#' terms (e.g. `y ~ a +    a:b`) [`lm()`] will add it back in. This function uses a fitting function
#' that operates underneath `lm()` to circumvent this behavior. (It is very similar to [`drop1()`].)
#'
#' @param fit The model to update.
#' @param term The term to drop from the model.
#'
#' @return An object of the class `lm`.
#' @export
drop_term <- function(fit, term) {
  offset <- stats::model.offset(stats::model.frame(fit))
  y <- fit$residuals + fit$fitted.values
  x <- stats::model.matrix(fit)

  # multiple columns of x may have come from the term we are trying to drop
  # to drop a term we have to drop all of the related columns
  #
  # to find out which term a column came from, check the attribute "assign"
  # this gives us a vector of integers that goes with the columns in x each
  # integer is telling us which term the column came from
  #
  # the integers map onto the term labels from the model fit
  term_labels <- attr(fit$terms, "term.labels")
  term_index_to_drop <- match(term, term_labels)

  column_term_indices <- attr(x, "assign")
  x_reduced <- x[, which(column_term_indices != term_index_to_drop), drop = FALSE]

  reduced_fit <- stats::lm.fit(x_reduced, y, offset = offset)
  reduced_fit$call <- base::call("drop_term", fit$call, term)

  oldClass(reduced_fit) <- "lm"
  reduced_fit
}

#' Fast term-dropping using pre-computed model components
#'
#' Internal helper that avoids redundant model.matrix() and model.frame()
#' calls when dropping multiple terms from the same model.
#'
#' @keywords internal
drop_term_fast <- function(term, x, y, offset, term_labels, col_assignments,
                           original_call) {
  term_index_to_drop <- match(term, term_labels)
  x_reduced <- x[, which(col_assignments != term_index_to_drop), drop = FALSE]

  reduced_fit <- stats::lm.fit(x_reduced, y, offset = offset)
  reduced_fit$call <- base::call("drop_term", original_call, term)
  oldClass(reduced_fit) <- "lm"
  reduced_fit
}

#' Fit a null (intercept-only) model directly via lm.fit
#'
#' Avoids the overhead of update_in_env(model, . ~ NULL) which runs the
#' full lm() pipeline.
#'
#' @keywords internal
fit_null_model <- function(y, offset) {
  x <- matrix(1, nrow = length(y), ncol = 1)
  colnames(x) <- "(Intercept)"

  null_fit <- stats::lm.fit(x, y, offset = offset)
  null_fit$call <- base::call("lm")
  oldClass(null_fit) <- "lm"
  null_fit
}


# Column assignment helper -------------------------------------------------------------------

#' Compute the assign vector mapping model matrix columns to terms
#'
#' Reconstructs the assign attribute (normally from model.matrix()) using
#' the terms structure and model frame, avoiding the expensive model.matrix() call.
#' Falls back to model.matrix() if the reconstruction doesn't match.
#'
#' @keywords internal
get_assign <- function(fit) {
  tt <- fit$terms
  term_labels <- attr(tt, "term.labels")
  if (length(term_labels) == 0) return(0L)

  has_intercept <- attr(tt, "intercept") == 1L
  n_terms <- length(term_labels)

  # Fast path: if n_coefficients == n_terms + intercept, each term has exactly

  # 1 column (all numeric predictors, no factor expansions)
  n_coef <- length(fit$coefficients)
  if (n_coef == n_terms + has_intercept) {
    return(if (has_intercept) 0L:n_terms else seq_len(n_terms))
  }

  # General path: compute from terms structure and model frame
  factors_mat <- attr(tt, "factors")
  mf <- fit$model

  assign <- if (has_intercept) 0L else integer(0)

  for (i in seq_along(term_labels)) {
    vars_in_term <- rownames(factors_mat)[factors_mat[, i] > 0]
    n_cols <- 1L
    for (v in vars_in_term) {
      col_data <- mf[[v]]
      if (is.factor(col_data)) {
        n_cols <- n_cols * (nlevels(col_data) - 1L)
      } else if (is.matrix(col_data)) {
        n_cols <- n_cols * ncol(col_data)
      } else {
        n_cols <- n_cols * 1L
      }
    }
    assign <- c(assign, rep(i, n_cols))
  }

  # Verify against model; fall back to model.matrix() if mismatch
  if (length(assign) != n_coef) {
    assign <- attr(stats::model.matrix(fit), "assign")
  }

  assign
}


# Printer -------------------------------------------------------------------------------------

#' @export
print.comparison_models <- function(x, ...) {
  full_model <- attr(x, "model")
  x <- generate_models.formula(as.formula(full_model), attr(x, "type"))

  if (vctrs::vec_is_empty(x)) {
    cli::cli_alert("No comparisons for empty model.")
  } else {
    cli::cat_line()
    header <- paste("Comparison Models for Type", attr(x, "type"), "SS")
    cli::cat_rule(cli::style_bold(header))
    cli::cat_line()
    purrr::iwalk(x, function(part, term) {
      cli::cat_line(paste0(cli::symbol$line, cli::symbol$line, " ", term))
      cli::cat_line("complex: ", formula_string(x, part$complex, term))
      cli::cat_line("simple:  ", formula_string(x, part$simple, term))
      cli::cat_line()
    })
  }
}

#' Normalize an interaction term by sorting its components alphabetically.
#' This ensures `hp:disp` and `disp:hp` are treated as the same term.
#'
#' This works because R interaction terms are commutative (a:b == b:a),
#' but R's `terms()` function may reorder components alphabetically.
#' @keywords internal
normalize_term <- function(term) {
  parts <- sort(strsplit(term, ":")[[1]])
  paste(parts, collapse = ":")
}

#' We have to insert spaces where terms were removed from the part model.
#' @keywords internal
formula_string <- function(obj, part, term) {
  type <- resolve_type(attr(obj, "type"))
  model_full <- frm_expand(as.formula(attr(obj, "model")))
  model_part <- frm_expand(as.formula(part))

  # For Types II and III the spaces need to be inserted within the formula string.
  # So, determine which variables were removed. We need to normalize interaction
  # terms before comparison because R's terms() can reorder components
  # (e.g., `hp:disp` may become `disp:hp`).
  full_terms <- frm_terms(model_full)
  part_terms <- frm_terms(model_part)
  full_normalized <- vapply(full_terms, normalize_term, character(1))
  part_normalized <- vapply(part_terms, normalize_term, character(1))
  rem_terms <- full_terms[!full_normalized %in% part_normalized]

  # For Type I, the removed terms are all at the end, so no changes.
  # For all types the full model formulae have no deletions.
  # If no terms were removed (Type 3 complex models), just print
  if (type == 1 || term == "Full Model" || length(rem_terms) == 0) {
    return(deparse(model_part))
  }

  if (type == 2) {
    # escape special regex characters in variables
    rem_pat <- stringr::str_replace_all(rem_terms, "(\\W)", "\\\\\\1")
    # add regex to remove immediately following space and plus
    rem_pat <- paste0(paste0(" ", rem_pat, collapse = " \\+?|"), "$")

    # replace variables with spaces in full string
    replacer <- function(str) strrep(" ", nchar(str))
    vars_removed <- stringr::str_replace_all(deparse(model_full), rem_pat, replacer)
    # trim dangling spaces and plus signs from end
    return(stringr::str_remove(vars_removed, "[ \\+]*$"))
  }

  if (type == 3) {
    # escape special regex characters in variables
    rem_pat <- stringr::str_replace_all(rem_terms, "(\\W)", "\\\\\\1")
    # add regex to remove immediately following space and plus
    rem_pat <- paste0(" ", rem_pat, "( \\+|$)")

    # replace variables with spaces in full string
    replacer <- function(str) strrep(" ", nchar(str))
    vars_removed <- stringr::str_replace_all(deparse(model_full), rem_pat, replacer)
    # trim dangling spaces and plus signs from end
    return(stringr::str_remove(vars_removed, "[ \\+]*$"))
  }
}

replace_vars_with_spaces <- function(str, vars) {
  # escape special regex characters in variables
  rem_pat <- stringr::str_replace_all(vars, "(\\W)", "\\\\\\1")
  # add regex to remove immediately following space and plus
  rem_pat <- paste0(paste0(" ", rem_pat, collapse = " \\+?|"), "$")

  # replace variables with spaces in full string
  replacer <- function(str) strrep(" ", nchar(str))
  vars_removed <- stringr::str_replace_all(str, rem_pat, replacer)
  # trim dangling spaces and plus signs from end
  stringr::str_remove(vars_removed, "[ \\+]*$")
}
