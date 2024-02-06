#' Compute all pairwise comparisons between category levels
#'
#' This function is useful for generating and testing all pairwise comparisons of categorical terms
#' in a linear model. This can be done in base R using functions like [`pairwise.t.test`] and
#' [`TukeyHSD`], but these functions are inconsistent both in their output format and their general
#' approach to pairwise comparisons. `pairwise()` will return a consistent table format, and will
#' make consistent decisions about how to calculate error terms and confidence intervals. See the
#' **Details** section low for more on how the models are tested (and why your output might not
#' match other functions).
#'
#' @details
#' For simple one-way models where a single categorical variable predicts and outcome, you will get
#' output similar to other methods of computing pairwise comparisons. Essentially, the differences
#' on the outcome between each of the groups defined by the categorical variable are compared with
#' the requested test, and their confidence intervals and p-values are adjusted by the requested
#' `correction`.
#'
#' However, when more than two variables are entered into the model, the outcome will diverge
#' somewhat from other methods of computing pairwise comparisons. For traditional pairwise tests you
#' need to estimate an error term, usually by pooling the standard deviation of the groups being
#' compared. This means that when you have other predictors in the model, their presence is ignored
#' when running these tests. For the functions in this package, we instead compute the pooled
#' standard error by using the mean squared error (MSE) from the full model fit.
#'
#' Let's take a concrete example to explain that. If we are predicting a car's miles-per-gallon
#' (`mpg`) based on whether it has an automatic or manual transmission (`am`), we can create that
#' linear model and get the pairwise comparisons like this:
#'
#' `pairwise(lm(mpg ~ factor(am), data = mtcars))`
#'
#' The output of this code will have one table showing the comparison of manual and automatic
#' transmissions with regard to miles-per-gallon. The pooled standard error is the same as the
#' square root of the MSE from the full model.
#'
#' In these data the `am` variable did not have any other values than *automatic* and *manual*, but
#' we can imagine situations where the predictor has more than two levels. In these cases, the
#' pooled SD would be calculated by taking the MSE of the full model (not of each group) and then
#' weighting it based on the size of the groups in question (divide by *n*).
#'
#' To improve our model, we might add the car's displacement (`disp`) as a quantitative predictor:
#'
#' `pairwise(lm(mpg ~ factor(am) + disp, data = mtcars))`
#'
#' Note that the output still only has a table for `am`. This is because we can't do a pairwise
#' comparison using `disp` because there are no groups to compare. Most functions will drop or not
#' let you use this variable during pairwise comparisons. Instead, `pairwise()` uses the same
#' approach as in the 3+ groups situation: we use the MSE for the full model and then weight it by
#' the size of the groups being compared. Because we are using the MSE for the full model, the
#' effect of `disp` is accounted for in the error term even though we are not explicitly comparing
#' different displacements. **Importantly**, the interpretation of the outcome is different than in
#' other traditional t-tests. Instead of saying, "there is a difference in miles-per-gallon based
#' on the type of transmission," we must add that this difference is found "after accounting for
#' displacement."
#'
#' @param fit A model fit by [`lm()`] or [`aov()`] (or similar).
#' @param correction The type of correction (if any) to perform to maintain the family-wise
#'   error-rate specified by `alpha`:
#'     - **Tukey**: computes Tukey's Honestly Significant Differences (see [`TukeyHSD()`])
#'     - **Bonferroni**: computes pairwise *t*-tests and then apply a Bonferroni correction
#'     - **none**: computes pairwise *t*-tests and reports the uncorrected statistics
#' @param term If `NULL`, use each categorical term in the model. Otherwise, only use the given
#'   term.
#' @param alpha The family-wise error-rate to restrict the tests to. If "none" is given for
#'   `correction`, this value is the value for each test (and is used to calculate the family-wise
#'   error-rate for the group of tests).
#' @param var_equal If `TRUE` (default), treat the variances between each group as being equal,
#'   otherwise the Welch or Satterthwaite method is used to appropriately weight the variances.
#'   **Note:**, currently only `TRUE` is supported. Alternative methods forthcoming.
#' @param plot Setting plot to TRUE will automatically call [`plot`] on the returned object.
#' @return A list of tables organized by the terms in the model. For each term (categorical terms
#'   only, as splitting on a continuous variable is generally uninformative), the table describes
#'   all of the pairwise-comparisons possible.
#'
#' @rdname pairwise
#' @export
pairwise <- function(fit, correction = "Tukey", term = NULL, alpha = .05, var_equal = TRUE,
                     plot = FALSE) {
  rlang::arg_match(correction, c("none", "Bonferroni", "Tukey"))

  tbl <- switch(correction,
    Tukey = pairwise_tukey(fit, term = term, alpha = alpha),
    none = pairwise_t(fit, term = term, alpha = alpha),
    Bonferroni = pairwise_bonferroni(fit, term = term, alpha = alpha)
  )

  if (plot) {
    plot(tbl)
  }

  tbl
}


#' @rdname pairwise
#' @export
pairwise_t <- function(fit, term = NULL, alpha = .05, correction = "none") {
  rlang::arg_match(correction, c("none", "bonferroni"))
  check_pairwise_args(fit, alpha)

  mse <- sum(fit$residuals^2) / fit$df.residual
  params <- means_and_counts(fit, term)
  tests <- purrr::pmap(params, function(means, counts, term) {
    if (term %in% frm_interaction_terms(fit)) {
      simple_terms <- purrr::pmap_chr(expand.grid(dimnames(means)), paste, sep = ":")
      means <- rlang::set_names(as.vector(means), simple_terms)
      counts <- rlang::set_names(as.vector(counts), simple_terms)
      pairs <- level_pairs(simple_terms)
    } else {
      pairs <- level_pairs(names(means))
    }

    corr_val <- if (correction == "bonferroni") length(pairs) else 1
    rows <- purrr::map(pairs, function(pair) {
      pair_1 <- pair[[1]]
      pair_2 <- pair[[2]]
      diff <- means[[pair_1]] - means[[pair_2]]
      pooled_se <- sqrt((mse / 2) * (1 / counts[[pair_1]] + 1 / counts[[pair_2]]))
      statistic <- diff / pooled_se
      critical <- stats::qt(1 - alpha / corr_val, fit$df.residual, lower.tail = FALSE)
      p <- min(1, 2 * stats::pt(abs(statistic), fit$df.residual, lower.tail = FALSE) * corr_val)
      margin <- abs(critical) * pooled_se

      tbl <- data.frame(
        group_1 = pair_1, group_2 = pair_2,
        diff = number(diff), pooled_se = number(pooled_se),
        t = number(statistic),
        df = fit$df.residual,
        lower = number(diff - margin), upper = number(diff + margin),
        p_val = number(p, 4, leading_zero = FALSE),
        stringsAsFactors = FALSE
      )

      if (correction == "bonferroni") {
        tbl$p_adj <- tbl$p_val
        tbl[names(tbl) != "p_val"]
      } else {
        tbl
      }
    })

    fwer <- 1 - (1 - alpha / corr_val)^length(rows)
    new_pairwise_tbl(purrr::reduce(rows, vctrs::vec_c), term, fit, fwer, alpha, correction)
  })

  structure(tests, class = "pairwise", fit = fit, correction = correction)
}


#' @rdname pairwise
#' @export
pairwise_bonferroni <- function(fit, term = NULL, alpha = .05) {
  pairwise_t(fit, term, alpha, correction = "bonferroni")
}


#' @rdname pairwise
#' @export
pairwise_tukey <- function(fit, term = NULL, alpha = .05) {
  correction <- "Tukey"
  check_pairwise_args(fit, alpha)

  mse <- sum(fit$residuals^2) / fit$df.residual
  tests <- purrr::pmap(means_and_counts(fit, term), function(means, counts, term) {
    if (term %in% frm_interaction_terms(fit)) {
      simple_terms <- purrr::pmap_chr(expand.grid(dimnames(means)), paste, sep = ":")
      means <- rlang::set_names(as.vector(means), simple_terms)
      counts <- rlang::set_names(as.vector(counts), simple_terms)
      pairs <- level_pairs(simple_terms)
    } else {
      pairs <- level_pairs(names(means))
    }


    rows <- purrr::map(pairs, function(pair) {
      pair_1 <- pair[[1]]
      pair_2 <- pair[[2]]
      diff <- means[[pair_1]] - means[[pair_2]]
      pooled_se <- sqrt((mse / 2) * (1 / counts[[pair_1]] + 1 / counts[[pair_2]]))
      statistic <- diff / pooled_se
      # honestly everything above this point is the same as in pairwise_t
      critical <- stats::qtukey(1 - alpha, length(means), fit$df.residual)
      margin <- abs(critical) * pooled_se
      p <- stats::ptukey(abs(statistic), length(means), fit$df.residual, lower.tail = FALSE)

      data.frame(
        group_1 = pair_1, group_2 = pair_2,
        diff = number(diff), pooled_se = number(pooled_se),
        q = number(statistic),
        df = fit$df.residual,
        lower = number(diff - margin), upper = number(diff + margin),
        p_adj = number(p, 4, leading_zero = FALSE),
        stringsAsFactors = FALSE
      )
    })

    new_pairwise_tbl(purrr::reduce(rows, vctrs::vec_c), term, fit, alpha, alpha, correction)
  })

  structure(tests, class = "pairwise", fit = fit, correction = correction)
}


#' Constructor for pairwise comparison tables
#'
#' @param tbl A [`tibble`]-like object.
#' @param term The term the table describes.
#' @param fit The linear model the term comes from.
#' @param fwer The family-wise error-rate for the group of tests in the table.
#' @param alpha The alpha to use when computing the family-wise error-rate.
#' @param correction The type of alpha correction the tests in the table use.
#'
#' @return A `tibble` sub-classed as `pairwise_comparison_tbl`. These have custom printers and
#'   retain their attributes when subsetted.
#' @keywords internal
new_pairwise_tbl <- function(tbl, term, fit, fwer, alpha, correction) {
  class_name <- "pairwise_tbl"
  n_levels <- length(unique(c(tbl$group_1, tbl$group_2)))

  tibble::new_tibble(
    tbl,
    nrow = nrow(tbl), class = class_name,
    fit = fit, term = term, correction = correction,
    n_levels = n_levels, alpha = alpha, fwer = fwer
  )
}


#' Check that the arguments are compatible with the rest of the pairwise code.
#'
#' @inheritParams pairwise
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
  if (!inherits(fit, "lm")) {
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

  to_drop <- setdiff(names(fit$model), c(categorical_vars, frm_outcome(fit)))
  drop_frm <- purrr::reduce(to_drop, frm_remove_var, .init = fit)

  update_in_env(fit, drop_frm)
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
  outcome <- frm_outcome(fit)
  explanatory <- purrr::discard(names(fit$model), function(term_name) term_name == outcome)
  purrr::keep(explanatory, function(term_name) is_categorical(fit$model[[term_name]]))
}


#' Get the means and counts for each categorical term in the model
#'
#' @inheritParams pairwise
#' @return A list of the means and counts for each level of each term.
#' @keywords internal
means_and_counts <- function(fit, term) {
  # model.tables only works with categorical vars
  categorical_fit <- refit_categorical(fit)
  terms <- select_terms(categorical_fit, term)

  model_tables <- stats::model.tables(stats::aov(categorical_fit), "means")
  means <- model_tables$tables[terms]
  counts <- purrr::imap(model_tables$n[terms], function(term_counts, index) {
    if (length(term_counts) == 1) {
      term_counts <- rep_len(term_counts, length(means[[index]]))
      names(term_counts) <- names(means[[index]])
    }
    term_counts
  })

  list(means = means, counts = counts, term = terms)
}


#' Select terms based on the user's `term` specification
#'
#' Before returning the selection, ensure that the term we are subsetting on exists.
#'
#' @inheritParams pairwise
#' @return A character vector of terms to run analyses on.
#' @keywords internal
select_terms <- function(fit, term = NULL) {
  terms <- frm_terms(fit)
  if (is.null(term)) {
    terms
  } else {
    if (!(term %in% terms)) {
      rlang::abort(sprintf(
        "\n\nYou are trying to select `%s` which is not a valid term.\nValid terms: %s\n",
        term, paste(terms, collapse = ", ")
      ))
    }
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
#' @return A [`tibble`] with two columns, group 1 and group 2, where each row is a unique pair.
#' @keywords internal
level_pairs <- function(levels) {
  create_row <- function(level) {
    suppressMessages(
      vctrs::vec_rbind(purrr::map(levels, ~ list(level, .x)))
    )
  }

  rows <- purrr::map(levels, create_row)
  mat <- as.matrix(purrr::reduce(rows, vctrs::vec_c))
  lower_tri(mat)
}


# Formatting ----------------------------------------------------------------------------------

#' @export
print.pairwise <- function(x, ..., n_per_table = Inf) {
  fit <- attr(x, "fit")

  title <- switch(attr(x, "correction"),
    Tukey = "Tukey's Honestly Significant Differences",
    none = "Pairwise t-tests",
    bonferroni = "Pairwise t-tests with Bonferroni correction"
  )

  cli::cli_h1(title)
  cli::cli_text("Model: ", deparse(formula(fit)))

  purrr::walk(x, print, n = n_per_table)
}


#' @export
#' @importFrom pillar tbl_sum
tbl_sum.pairwise_tbl <- function(x) {
  cli::cat_line()
  cli::cli_text(cli::style_bold(attr(x, "term")))
  cli::cli_text("Levels: ", attr(x, "n_levels"))
  cli::cli_text("Family-wise error-rate: ", round(attr(x, "fwer"), 3))
  cli::cat_line()
}


# Plotting ------------------------------------------------------------------------------------

scale_type.supernova_number <- function(x) "continuous"

#' Autoplot method for pairwise objects.
#'
#' @param object A [`pairwise`] object.
#' @param ... Additional arguments passed to the plotting geom.
#' @details This function requires an optional dependency: [ggplot2][ggplot2::ggplot2-package]. When
#'  this package is installed, calling `autoplot()` or `plot` on a `pairwise` object will generate a
#'  plot of the pairwise comparisons. The plot will show the differences between the groups, with
#'  error bars representing the confidence intervals. The x-axis will be labeled with the type of
#'  confidence interval used and the values of the differences, and the y-axis will be labeled with
#'  the groups being compared. A dashed line at 0 is included to help visualize the differences.
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' if (require(ggplot2)) {
#'   # generate the plot immediately
#'   pairwise(lm(mpg ~ factor(am) + disp, data = mtcars), plot = TRUE)
#'
#'   # or save the object and plot it later
#'   p <- pairwise(lm(mpg ~ factor(am) + disp, data = mtcars))
#'   plot(p)
#' }
autoplot.pairwise <- function(object, ...) {
  `%+%` <- ggplot2::`%+%`
  x <- object[!(names(object) %in% c("p_adj", "p_val"))]
  p <- purrr::imap(x, function(tbl, term) {
    tbl$term <- term
    tbl$pair <- paste(tbl$group_1, tbl$group_2, sep = " - ")

    correction <- attr(tbl, "correction")
    x_axis_label <- if (correction == "none") {
      conf <- paste0(format((1 - attr(tbl, "alpha")) * 100, digits = 3), "%")
      paste(conf, "CI (per test; uncorrected)")
    } else {
      conf <- paste0(format((1 - attr(tbl, "fwer")) * 100, digits = 3), "%")
      paste(conf, "CI with", stringr::str_to_title(correction), "correction")
    }

    ggplot2::ggplot() %+%
      ggplot2::geom_point(ggplot2::aes(tbl$diff, tbl$pair)) %+%
      ggplot2::geom_errorbarh(ggplot2::aes(
        y = tbl$pair,
        xmin = tbl$lower,
        xmax = tbl$upper
      )) %+%
      ggplot2::geom_vline(ggplot2::aes(xintercept = 0), linetype = "dashed") %+%
      # reverse so that plot order matches table
      ggplot2::scale_y_discrete(limits = rev(tbl$pair)) %+%
      ggplot2::xlab(x_axis_label) %+%
      ggplot2::ylab(NULL)
  })
  invisible(p)
}


#' @rdname autoplot.pairwise
#' @param x A `pairwise` object.
#' @param y Ignored, required for compatibility with the `plot()` generic.
#' @export
plot.pairwise <- function(x, y, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("The ggplot2 package is required to plot pairwise comparisons.")
  }

  purrr::walk(ggplot2::autoplot(x, ...), print)
}
