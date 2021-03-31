#' Compute all pairwise comparisons between category levels
#'
#' Unlike [`pairwise.t.test`], this function will return a more informative set of tables similar to
#' [`TukeyHSD`]. However, the tables always include the pooled standard error for the estimate and
#' the degrees of freedom for the test.
#'
#' @param fit A model fit by [`lm()`] or [`aov()`] (or similar).
#' @param correction The type of correction (if any) to perform to maintain the family-wise
#'   error-rate specified by `alpha`:
#'   - **Tukey**: computes Tukey's Honestly Significant Differences (see [`TukeyHSD()`])
#'   - **Bonferroni**: computes pairwise *t*-tests and then apply a Bonferroni correction
#'   - **none**: computes pairwise *t*-tests and reports the uncorrected statistics
#' @param term If `NULL`, compute a set of tests for each categorical term in the model. Otherwise,
#'   only compute the tests for the given term.
#' @param alpha The family-wise error-rate to restrict the tests to. If "none" is given for
#'   `correction`, this value is the value for each test (and is used to calculate the family-wise
#'   error-rate for the group of tests).
#'
#' @return A list of tables organized by the terms in the model. For each term (categorical terms
#'   only, as splitting on a continuous variable is generally uninformative), the table describes
#'   all of the pairwise-comparisons possible.
#'
#' @rdname pairwise_comparisons
#' @export
pairwise_comparisons <- function(fit, correction = "Tukey", term = NULL, alpha = .05) {
  rlang::arg_match(correction, c("none", "Bonferroni", "Tukey"))

  switch(
    correction,
    Tukey = pairwise_tukey(fit, term = term, alpha = alpha),
    none = pairwise_t(fit, term = term, alpha = alpha),
    Bonferroni = pairwise_bonferroni(fit, term = term, alpha = alpha)
  )
}


#' @rdname pairwise_comparisons
#' @export
pairwise_t <- function(fit, term = NULL, alpha = .05, correction = "none") {
  rlang::arg_match(correction, c("none", "bonferroni"))
  check_pairwise_args(fit, alpha)
  categorical_fit <- refit_categorical(fit)
  terms <- select_terms(categorical_fit, term)

  mse <- sum(fit$residuals^2) / fit$df.residual

  means_and_counts_by_term <- stats::model.tables(stats::aov(categorical_fit), "means")
  params <- list(
    means = means_and_counts_by_term$tables[terms],
    counts = means_and_counts_by_term$n[terms],
    term = terms
  )

  tests <- purrr::pmap(params, function(means, counts, term) {
    if (term %in% frm_interaction_terms(fit)) {
      simple_terms <- expand.grid(dimnames(means)) %>% purrr::pmap_chr(paste, sep = ":")
      means <- as.vector(means) %>% magrittr::set_names(simple_terms)
      counts <- as.vector(counts) %>% magrittr::set_names(simple_terms)
      pairs <- level_pairs(simple_terms)
    } else {
      pairs <- level_pairs(names(means))
    }

    corr_val <- if (correction == "bonferroni") length(pairs) else 1
    rows <- purrr::map(pairs, function(pair) {
      pair_1 <- pair[[1]]
      pair_2 <- pair[[2]]
      diff <- means[[pair_1]] - means[[pair_2]]
      pooled_se <- sqrt((mse / counts[[pair_1]]) + (mse / counts[[pair_2]]))
      statistic <- diff / pooled_se
      critical <- stats::qt(1 - alpha / corr_val, fit$df.residual, lower.tail = FALSE)
      p <- min(1, 2 * stats::pt(abs(statistic), fit$df.residual, lower.tail = FALSE) * corr_val)
      margin <- abs(critical) * pooled_se

      tbl <- data.frame(
        group_1 = pair_1, group_2 = pair_2,
        diff = number(diff), pooled_se = number(pooled_se),
        statistic = number(statistic),
        lower = number(diff - margin), upper = number(diff + margin),
        p_val = number(p, 4, leading_zero = FALSE)
      )

      if (correction == "bonferroni") {
        tbl$p_adj <- tbl$p_val
        tbl[names(tbl) != "p_val"]
      } else {
        tbl
      }
    })

    fwer <- 1 - (1 - alpha / corr_val) ^ length(rows)
    purrr::reduce(rows, vctrs::vec_c) %>%
      new_pairwise_comparisons_tbl(term, fit, fwer, correction)
  })

  structure(tests, class = "pairwise_comparisons", fit = fit, correction = correction)
}


#' @rdname pairwise_comparisons
#' @export
pairwise_bonferroni <- function(fit, term = NULL, alpha = .05) {
  pairwise_t(fit, term, alpha, correction = "bonferroni")
}


#' @rdname pairwise_comparisons
#' @export
pairwise_tukey <- function(fit, term = NULL, alpha = .05) {
  correction <- "Tukey"
  check_pairwise_args(fit, alpha)
  categorical_fit <- refit_categorical(fit)
  terms <- select_terms(categorical_fit, term)

  mse <- sum(fit$residuals^2) / fit$df.residual

  means_and_counts_by_term <- stats::model.tables(stats::aov(categorical_fit), "means")
  params <- list(
    means = means_and_counts_by_term$tables[terms],
    counts = means_and_counts_by_term$n[terms],
    term = terms
  )

  tests <- purrr::pmap(params, function(means, counts, term) {
    if (term %in% frm_interaction_terms(fit)) {
      simple_terms <- expand.grid(dimnames(means)) %>% purrr::pmap_chr(paste, sep = ":")
      means <- as.vector(means) %>% magrittr::set_names(simple_terms)
      counts <- as.vector(counts) %>% magrittr::set_names(simple_terms)
      pairs <- level_pairs(simple_terms)
    } else {
      pairs <- level_pairs(names(means))
    }

    # honestly everything above this point is the same as in pairwise_t
    rows <- purrr::map(pairs, function(pair) {
      pair_1 <- pair[[1]]
      pair_2 <- pair[[2]]
      diff <- means[[pair_1]] - means[[pair_2]]
      pooled_se <- sqrt((mse / counts[[pair_1]]) + (mse / counts[[pair_2]]))
      pooled_se <- sqrt((mse / 2) * (1 / counts[[pair_1]] + 1 / counts[[pair_2]]))
      critical <- stats::qtukey(1 - alpha, length(means), fit$df.residual)
      margin <- abs(critical) * pooled_se
      statistic <- diff / pooled_se
      p <- stats::ptukey(abs(statistic), length(means), fit$df.residual, lower.tail = FALSE)

      data.frame(
        group_1 = pair_1, group_2 = pair_2,
        diff = number(diff), pooled_se = number(pooled_se),
        statistic = number(statistic),
        lower = number(diff - margin), upper = number(diff + margin),
        p_adj = number(p, 4, leading_zero = FALSE)
      )
    })

    purrr::reduce(rows, vctrs::vec_c) %>%
      new_pairwise_comparisons_tbl(term, fit, alpha, correction)
  })

  structure(tests, class = "pairwise_comparisons", fit = fit, correction = correction)
}


#' Constructor for pairwise comparison tables
#'
#' @param tbl A [`tibble`]-like object.
#' @param term The term the table describes.
#' @param fit The linear model the term comes from.
#' @param fwer The family-wise error-rate for the group of tests in the table.
#' @param correction The type of alpha correction the tests in the table use.
#'
#' @return A tibble subclassed as `pairwise_comparison_tbl`. These have custom printers and retain
#'   their attributes when subsetted.
#' @keywords internal
new_pairwise_comparisons_tbl <- function(tbl, term, fit, fwer, correction) {
  class_name <- "pairwise_comparisons_tbl"
  n_levels <- length(unique(c(tbl$group_1, tbl$group_2)))

  tibble::new_tibble(
    tbl, nrow = nrow(tbl), class = class_name,
    fit = fit, term = term, correction = correction,
    n_levels = n_levels, alpha = fwer
  )
}


#' Check that the arguments are compatible with the rest of the pairwise code.
#'
#' @param fit A linear model (fit by [`lm()`]) to refit.
#' @param alpha A single double value indicating the alpha to use for the tests.
#' @keywords internal
check_pairwise_args <- function(fit, alpha) {
  vctrs::vec_assert(alpha, ptype = double(), size = 1)
  check_aov_compat(fit)
  check_not_empty(fit)
}


#' @describeIn check_pairwise_args Ensure the model can be converted by [`aov()`]
#' @keywords internal
check_aov_compat <- function(fit) {
  if (!("lm" %in% class(fit))) {
    rlang::abort("`fit` must be an object fit by `lm()` or `aov()` (or be compatible with `aov()`.")
  }
}


#' @describeIn check_pairwise_args Check that the model is not the empty model
#' @keywords internal
check_not_empty <- function(fit) {
  models <- generate_models(fit, 1)
  if (vctrs::vec_is_empty(models)) {
    stop("There are no comparisons to make with the empty model.")
  }
}


#' Refit a model, dropping any non-categorical terms.
#'
#' @inheritParams check_pairwise_args
#' @return A linear model that only has categorical predictors.
#' @keywords internal
refit_categorical <- function(fit) {
  categorical_vars <- find_categorical_vars(fit)
  if (vctrs::vec_is_empty(categorical_vars)) {
    stop("There are no categorical variables in this model.")
  }

  to_drop <- setdiff(frm_vars(fit), categorical_vars)
  drop_frm <- purrr::reduce(to_drop, frm_remove_var, .init = fit)
  stats::update(fit, drop_frm)
}


#' Find the categorical variables in a model
#'
#' @inheritParams check_pairwise_args
#' @return A character vector of the categorical variables in the model. Note these are not terms,
#'   they are variables, e.g. interactions are not included here, only the variables they are
#'   comprised of.
#' @keywords internal
find_categorical_vars <- function(fit) {
  is_categorical <- function(x) is.character(x) || is.factor(x)
  purrr::keep(frm_vars(fit), function(term_name) {
    variable <- fit$model[[term_name]]
    is_categorical(variable)
  })
}


#' Select terms based on the user's `term` specification
#'
#' Before returning the selection, ensure that the term we are subsetting on exists.
#'
#' @param fit A linear model after updating to only include categorical terms.
#' @param term The term to check for, if `NULL` return all valid terms.
#'
#' @return A character vector of terms to run analyses on.
#' @keywords internal
select_terms <- function(fit, term = NULL) {
  terms <- frm_terms(fit)
  if (is.null(term)) {
    terms
  } else {
    if (!(term %in% terms)) rlang::abort(sprintf(
      "\n\nYou are trying to select `%s` which is not a valid term.\nValid terms: %s\n",
      term, paste(terms, collapse = ", ")
    ))
    term
  }
}

#' Get all pairs for a given vector
#'
#' The output of this function should match the pairs you get when you run [`TukeyHSD`].
#'
#' @param levels The vector to get pairs for. It is called levels because it was written for the
#'   purpose of comparing levels of a factor to one another with multiple comparisons.
#'
#' @return A tibble with two columns, group 1 and group 2, where each row is a unique pair.
#' @keywords internal
level_pairs <- function(levels) {
  create_row <- function(level) purrr::map(levels, ~list(level, .x)) %>%
    vctrs::vec_rbind() %>%
    suppressMessages()

  purrr::map(levels, create_row) %>%
    purrr::reduce(vctrs::vec_c) %>%
    as.matrix() %>%
    lower_tri()
}


# Formatting ----------------------------------------------------------------------------------

#' @export
print.pairwise_comparisons <- function(x, ..., n_per_table = Inf) {
  fit <- attr(x, "fit")
  dropped_vars <- setdiff(frm_vars(fit), find_categorical_vars(fit))

  title <- switch(
    attr(x, "correction"),
    Tukey = "Tukey's Honestly Significant Differences",
    none = "Pairwise t-tests",
    bonferroni = "Pairwise t-tests with Bonferroni correction"
  )

  cli::cli_h1(title)
  cli::cat_line()
  cli::cli_text("Model: ", deparse(formula(fit)))
  if (length(dropped_vars) > 0) {
    cli::cli_alert_warning(paste0(
      "{length(dropped_vars)} non-categorical variable{?s} dropped: ",
      paste(dropped_vars, collapse = ", ")
    ))
  }

  purrr::walk(x, print, n = n_per_table)
}


#' @export
#' @importFrom pillar tbl_sum
tbl_sum.pairwise_comparisons_tbl <- function(x, setup, ...) {
  cli::cli_h3(cli::style_bold(attr(x, "term")))
  cli::cli_text("{nrow(x)} comparison{?s} of ", attr(x, "n_levels"), " levels")
  cli::cli_text("Family-wise error-rate: ", round(attr(x, "alpha"), 3))
  cli::cat_line()
}
