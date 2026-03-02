#' supernova
#'
#' An alternative set of summary statistics for ANOVA. Sums of squares, degrees
#' of freedom, mean squares, and F value are all computed with Type III sums of
#' squares, but for fully-between subjects designs you can set the type to I or
#' II. This function adds to the output table the proportional reduction in
#' error, an explicit summary of the whole model, separate formatting of p
#' values, and is intended to match the output used in Judd, McClelland, and
#' Ryan (2017).
#'
#' @param fit A model fit by [`lm()`] or `lme4::lmer()`
#' @param type The type of sums of squares to calculate (see [`generate_models()`]). Defaults to the
#'   widely used Type `III` SS.
#' @param verbose If `FALSE`, the `description` column is suppressed.
#'
#' @return An object of the class `supernova`, which has a clean print method for displaying the
#'   ANOVA table in the console as well as a named list:
#'   \item{tbl}{The ANOVA table as a \code{\link{data.frame}}}
#'   \item{fit}{The original \code{\link[stats]{lm}} or \code{lmer}
#'     object being tested}
#'   \item{models}{Models created by \code{\link{generate_models}}}
#'
#' @examples
#' supernova(lm(mpg ~ disp, data = mtcars))
#'
#' change_p_decimals <- supernova(lm(mpg ~ disp, data = mtcars))
#' print(change_p_decimals, pcut = 8)
#' @importFrom stats anova as.formula drop1 pf
#'
#' @references Judd, C. M., McClelland, G. H., & Ryan, C. S. (2017). \emph{Data
#'   Analysis: A Model Comparison Approach to Regression, ANOVA, and Beyond}
#'   (3rd ed.). New York: Routledge. ISBN:879-1138819832
#'
#' @export
supernova <- function(fit, type = 3, verbose = TRUE) {
  UseMethod("supernova", fit)
}


#' @export
#' @rdname supernova
supernova.lm <- function(fit, type = 3, verbose = TRUE) {
  type <- resolve_type(type)
  models <- generate_models(fit, type)
  is_null_model <- length(models) == 0

  if (is_null_model) {
    tbl <- build_null_table(fit)
  } else if (type == 3 && fit$rank == length(fit$coefficients)) {
    # Fast Type III path: compute SS from QR decomposition
    tbl <- build_type3_table_qr(fit)
  } else {
    # Type I/II or rank-deficient Type III: use model comparison
    fit_null <- models[[1]]$simple
    tbl <- build_anova_table(models, fit, fit_null)
  }

  rl <- list(tbl = tbl, fit = fit, models = models)
  class(rl) <- "supernova"
  attr(rl, "type") <- strrep("I", type)
  attr(rl, "verbose") <- verbose
  return(rl)
}

#' A template for a row in an ANOVA table.
#'
#' @param term The name of the term the row describes.
#' @param description An optional, short description of the term (pedagogical).
#' @param ss The sum of squares for the term (defaults to blank)
#' @param df The degrees of freedom the term uses (defaults to blank).
#' @param ms The mean square for the term (defaults to `ss / df`)
#' @param f Fisher's F statistic for the term in the model (defaults to blank).
#' @param pre The proportional reduction of error the term provides (defaults to blank).
#' @param p The p-value of the F (and PRE) for the term in the model (defaults to blank).
#'
#' @returns A [`tibble::tibble_row()`] of length 1 with all of the variables initialized.
#'
#' @keywords internal
row_blank <- function(term = NA_character_, description = NA_character_,
                      ss = NA_real_, df = NA_integer_, ms = ss / df,
                      f = NA_real_, pre = NA_real_, p = NA_real_) {
  vctrs::vec_assert(term, character(), 1, arg = "term")
  vctrs::vec_assert(description, character(), 1, arg = "description")
  vctrs::vec_assert(ss, double(), 1, arg = "ss")
  vctrs::vec_assert(df, integer(), 1, arg = "df")
  vctrs::vec_assert(ms, double(), 1, arg = "ms")
  vctrs::vec_assert(f, double(), 1, arg = "f")
  vctrs::vec_assert(pre, double(), 1, arg = "pre")
  vctrs::vec_assert(p, double(), 1, arg = "p")

  tibble::tibble_row(
    term = term, description = description,
    SS = ss, df = df, MS = ms,
    `F` = f, PRE = pre, p = p
  )
}

#' Compute and construct an ANOVA table row for a term.
#'
#' "Term" is loosely defined here and is probably better understood as "everything in the table that
#' is not an error row.
#'
#' @param term The name of the term the row describes.
#' @param description An optional, short description of the term (pedagogical).
#' @param models The models created by [`generate_models()`] for comparison.
#' @param term The term to compute the row for.
#'
#' @return A [`tibble::tibble_row()`] with the properties initialized. The code has been written to
#'   be as understandable as possible. Please take a look at the source and offer any suggestions
#'   for improvement!
#'
#' @keywords internal
row_term <- function(name, description, models, term) {
  full_model <- models[[1]]$complex
  complex <- models[[term]]$complex
  simple <- models[[term]]$simple
  is_full_model <- term == "Full Model"

  # compute the SS model using the comparison models from generate_models()
  ss <- sum((complex$fitted.values - simple$fitted.values)^2)
  df <- if (is_full_model) {
    full_model$rank - 1L
  } else {
    factor_mat <- as.data.frame(attr(complex$terms, "factors"))
    matched <- rownames(factor_mat)[factor_mat[, term] == 1]
    df_each <- purrr::map_int(matched, function(term) {
      data <- complex$model[[term]]
      degrees <- if (is.numeric(data)) 2L else length(levels(factor(data)))
      degrees - 1L
    })
    as.integer(prod(df_each))
  }

  # always use the more complete SS error from the full model, never the partials
  ss_error <- sum(full_model$residuals^2)
  df_error <- nrow(full_model$model) - length(full_model$coefficients)
  ms_error <- ss_error / df_error

  ms <- ss / df
  f <- ms / ms_error
  pre <- ss / (ss + ss_error)
  p <- pf(f, df, df_error, lower.tail = FALSE)

  row_blank(name, description, ss, df, ms, f, pre, p)
}

#' Compute and construct an ANOVA table row for an error term
#'
#' @param term The name of the term the row describes (e.g. Error or Total).
#' @param description An optional, short description of the term (pedagogical).
#' @param fit The model we are describing error from.
#'
#' @return A [`tibble::tibble_row()`] with the properties initialized. The code has been written to
#'   be as understandable as possible. Please take a look at the source and offer any suggestions
#'   for improvement!
#'
#' @keywords internal
row_error <- function(term, description, fit) {
  ss <- sum(fit$residuals^2)
  df <- nrow(fit$model) - length(fit$coefficients)
  ms <- ss / df

  row_blank(term, description, ss, df, ms)
}


# Fast table builders (lm path) ----------------------------------------------------------------

#' Build the ANOVA table for a null (intercept-only) model
#' @noRd
build_null_table <- function(fit_null) {
  ss_total <- sum(fit_null$residuals^2)
  df_total <- as.integer(length(fit_null$residuals) - length(fit_null$coefficients))
  ms_total <- ss_total / df_total

  make_anova_df(
    term = c("Model", "Error", "Total"),
    description = c("(error reduced)", "(from model)", "(empty model)"),
    ss = c(NA_real_, NA_real_, ss_total),
    df = c(NA_integer_, NA_integer_, df_total),
    ms = c(NA_real_, NA_real_, ms_total),
    f = c(NA_real_, NA_real_, NA_real_),
    pre = c(NA_real_, NA_real_, NA_real_),
    p = c(NA_real_, NA_real_, NA_real_)
  )
}

#' Build Type III ANOVA table directly from QR decomposition
#'
#' Computes all SS values from the fitted model's QR decomposition using the
#' Wald statistic, avoiding expensive model refitting. Only valid for full-rank
#' Type III models.
#' @noRd
build_type3_table_qr <- function(fit) {
  rank <- fit$rank
  n <- length(fit$residuals)
  beta <- fit$coefficients
  term_labels <- attr(fit$terms, "term.labels")
  n_terms <- length(term_labels)

  # SS from stored residuals and QR effects
  ss_error <- sum(fit$residuals^2)
  ss_model <- sum(fit$effects[2:rank]^2)
  ss_total <- ss_model + ss_error

  # Degrees of freedom
  df_model <- rank - 1L
  df_error <- as.integer(n - rank)
  df_total <- as.integer(n - 1L)
  ms_error <- ss_error / df_error

  # Type III SS per term via Wald statistic: SS_j = beta_j' (C_jj)^{-1} beta_j
  col_assign <- get_assign(fit)
  qr_r <- fit$qr$qr[seq_len(rank), , drop = FALSE]
  qr_r[row(qr_r) > col(qr_r)] <- 0
  Rinv <- backsolve(qr_r, diag(rank))
  XtXinv <- tcrossprod(Rinv)

  p_ss <- numeric(n_terms)
  p_df <- integer(n_terms)
  for (i in seq_len(n_terms)) {
    idx <- which(col_assign == i)
    p_df[i] <- length(idx)
    if (length(idx) == 1L) {
      p_ss[i] <- beta[idx]^2 / XtXinv[idx, idx]
    } else {
      C_jj <- XtXinv[idx, idx, drop = FALSE]
      p_ss[i] <- as.numeric(crossprod(beta[idx], solve(C_jj, beta[idx])))
    }
  }

  # Full model stats
  full_ms <- ss_model / df_model
  full_f <- full_ms / ms_error
  full_pre <- ss_model / (ss_model + ss_error)
  full_p <- pf(full_f, df_model, df_error, lower.tail = FALSE)

  # Per-term stats
  p_ms <- p_ss / p_df
  p_f <- p_ms / ms_error
  p_pre <- p_ss / (p_ss + ss_error)
  p_p <- pf(p_f, p_df, df_error, lower.tail = FALSE)

  if (n_terms == 1L) {
    make_anova_df(
      term = c("Model", "Error", "Total"),
      description = c("(error reduced)", "(from model)", "(empty model)"),
      ss = c(ss_model, ss_error, ss_total),
      df = c(df_model, df_error, df_total),
      ms = c(full_ms, ms_error, ss_total / df_total),
      f = c(full_f, NA_real_, NA_real_),
      pre = c(full_pre, NA_real_, NA_real_),
      p = c(full_p, NA_real_, NA_real_)
    )
  } else {
    make_anova_df(
      term = c("Model", term_labels, "Error", "Total"),
      description = c("(error reduced)", rep(NA_character_, n_terms),
        "(from model)", "(empty model)"),
      ss = c(ss_model, p_ss, ss_error, ss_total),
      df = c(df_model, p_df, df_error, df_total),
      ms = c(full_ms, p_ms, ms_error, ss_total / df_total),
      f = c(full_f, p_f, NA_real_, NA_real_),
      pre = c(full_pre, p_pre, NA_real_, NA_real_),
      p = c(full_p, p_p, NA_real_, NA_real_)
    )
  }
}

#' Build the complete ANOVA table for a non-null model
#' @noRd
build_anova_table <- function(models, fit, fit_null) {
  full_model <- models[[1]]$complex
  has_partials <- length(models) > 2

  # pre-compute shared values once
  full_fitted <- full_model$fitted.values
  ss_error <- sum(full_model$residuals^2)
  df_error <- as.integer(nrow(full_model$model) - length(full_model$coefficients))
  ms_error <- ss_error / df_error

  # total row values
  ss_total <- sum(fit_null$residuals^2)
  df_total <- as.integer(length(fit_null$residuals) - length(fit_null$coefficients))

  # full model row: SS = sum((full_fitted - null_fitted)^2)
  null_fitted <- models[["Full Model"]]$simple$fitted.values
  full_ss <- sum((full_fitted - null_fitted)^2)
  full_df <- full_model$rank - 1L
  full_ms <- full_ss / full_df
  full_f <- full_ms / ms_error
  full_pre <- full_ss / (full_ss + ss_error)
  full_p <- pf(full_f, full_df, df_error, lower.tail = FALSE)

  if (!has_partials) {
    make_anova_df(
      term = c("Model", "Error", "Total"),
      description = c("(error reduced)", "(from model)", "(empty model)"),
      ss = c(full_ss, ss_error, ss_total),
      df = c(full_df, df_error, df_total),
      ms = c(full_ms, ms_error, ss_total / df_total),
      f = c(full_f, NA_real_, NA_real_),
      pre = c(full_pre, NA_real_, NA_real_),
      p = c(full_p, NA_real_, NA_real_)
    )
  } else {
    partial_names <- names(models)[-1]
    n_partials <- length(partial_names)

    p_ss <- vapply(partial_names, function(t) {
      sum((models[[t]]$complex$fitted.values - models[[t]]$simple$fitted.values)^2)
    }, numeric(1), USE.NAMES = FALSE)
    p_df <- vapply(partial_names, function(t) {
      compute_term_df(models[[t]]$complex, t)
    }, integer(1), USE.NAMES = FALSE)
    p_ms <- p_ss / p_df
    p_f <- p_ms / ms_error
    p_pre <- p_ss / (p_ss + ss_error)
    p_p <- pf(p_f, p_df, df_error, lower.tail = FALSE)

    make_anova_df(
      term = c("Model", partial_names, "Error", "Total"),
      description = c("(error reduced)", rep(NA_character_, n_partials),
        "(from model)", "(empty model)"),
      ss = c(full_ss, p_ss, ss_error, ss_total),
      df = c(full_df, p_df, df_error, df_total),
      ms = c(full_ms, p_ms, ms_error, ss_total / df_total),
      f = c(full_f, p_f, NA_real_, NA_real_),
      pre = c(full_pre, p_pre, NA_real_, NA_real_),
      p = c(full_p, p_p, NA_real_, NA_real_)
    )
  }
}

#' Construct an ANOVA data.frame directly from vectors, avoiding data.frame() overhead
#' @noRd
make_anova_df <- function(term, description, ss, df, ms, f, pre, p) {
  n <- length(term)
  tbl <- list(
    term = term,
    description = description,
    SS = as.double(ss),
    df = as.integer(df),
    MS = as.double(ms),
    `F` = as.double(f),
    PRE = as.double(pre),
    p = as.double(p)
  )
  attr(tbl, "row.names") <- .set_row_names(n)
  class(tbl) <- "data.frame"
  tbl
}

#' Compute sum of squares from model comparison
#' @noRd
compute_ss <- function(models, term) {
  sum((models[[term]]$complex$fitted.values - models[[term]]$simple$fitted.values)^2)
}

#' Compute degrees of freedom for a term
#' @noRd
compute_term_df <- function(complex, term) {
  factor_mat <- attr(complex$terms, "factors")
  matched <- rownames(factor_mat)[factor_mat[, term] == 1]
  df_each <- vapply(matched, function(var_name) {
    data <- complex$model[[var_name]]
    if (is.factor(data)) nlevels(data) - 1L
    else if (is.numeric(data)) 1L
    else length(unique(data)) - 1L
  }, integer(1), USE.NAMES = FALSE)
  as.integer(prod(df_each))
}


#' @export
#' @rdname supernova
supernova.lmerMod <- function(fit, type = 3, verbose = FALSE) {
  if (resolve_type(type) != 3) {
    stop("Currently only Type III tests can be computed for models with random
         effects (e.g. repeated measures models).")
  }

  if (verbose) {
    warning("There is currently no verbose version of the supernova table for
         lmer() models. Switching to non-verbose.")
    verbose <- FALSE
  }

  model_full <- fit
  model_data <- model_full@frame
  vars_all <- supernova::variables(model_full)
  vars_within_simple <- grep("[:]", vars_all[["within"]], value = TRUE, invert = TRUE)
  vars_between_simple <- grep("[:]", vars_all[["between"]], value = TRUE, invert = TRUE)

  # get formula with no random terms
  formula_complex <- as.formula(model_full)
  formula_simple <- frm_build(
    frm_outcome(formula_complex),
    frm_fixed_terms(formula_complex)
  )

  # TOTAL
  fit_lm <- stats::lm(formula_simple, data = model_data)
  fit_null <- stats::update(fit_lm, . ~ NULL)
  total_row <- row_error("Total", description = NA_character_, fit = fit_null)

  # DF
  df_total_between <- length(unique(model_data[[vars_all[["group"]]]])) - 1
  df_total_within <- total_row[["df"]] - df_total_between

  # TREATMENT ROWS
  anova_lm <- select(anova_tbl(fit_lm), "F", FALSE)
  anova_lmer <- select(anova_tbl(model_full), c("term", "F"))
  partial_rows <- merge_keep_order(anova_lmer, anova_lm, by = "term", order_by = "term")

  # TREATMENT WITHIN
  partial_within <- partial_rows[partial_rows[["term"]] %in% vars_all[["within"]], ]
  treatment_within <- if (length(vars_within_simple) == 0) {
    # No within vars
    data.frame()
  } else if (length(vars_within_simple) == 1) {
    # A single within var, only need one error term
    df_error_within <- df_total_within - sum(partial_within[["df"]])
    partial_within_error <- data.frame(
      term = "Error within subjects",
      df = df_error_within,
      MS = partial_within[["MS"]][[1]] / partial_within[["F"]][[1]],
      stringsAsFactors = FALSE
    )
    partial_within_error[["SS"]] <-
      partial_within_error[["MS"]] * partial_within_error[["df"]]
    partial_within[["PRE"]] <-
      partial_within[["SS"]] / (partial_within_error[["SS"]] + partial_within[["SS"]])
    partial_within[["p"]] <-
      pf(partial_within[["F"]], partial_within[["df"]], df_error_within, lower.tail = FALSE)
    vctrs::vec_c(partial_within, partial_within_error)
  } else {
    # Multiple within vars, need to compute separate error terms
    df_error_within <- partial_within[["df"]] * df_total_between
    partial_within_error <- data.frame(
      match = vars_all[["within"]],
      term = paste(vars_all[["within"]], "error"),
      df = df_error_within,
      MS = partial_within[["MS"]] / partial_within[["F"]],
      stringsAsFactors = FALSE
    )
    partial_within_error[["SS"]] <-
      partial_within_error[["MS"]] * partial_within_error[["df"]]

    purrr::map_dfr(vars_all[["within"]], function(x) {
      part <- vctrs::vec_c(
        partial_within[which(partial_within$term == x), ],
        partial_within_error[which(partial_within_error$match == x), ]
      )

      part <- select(part, "match", keep = FALSE)
      part[, c("PRE", "p")] <- NA_real_
      part[["PRE"]][[1]] <- part[["SS"]][[1]] / sum(part[["SS"]])
      part[["p"]][[1]] <- pf(part[["F"]][[1]], part[["df"]][[1]], part[["df"]][[2]],
        lower.tail = FALSE
      )
      part[c("term", "SS", "df", "MS", "F", "PRE", "p")]
    })
  }

  # TREATMENT BETWEEN
  partial_between <- partial_rows[partial_rows[["term"]] %in% vars_all[["between"]], ]
  treatment_between <- if (length(vars_between_simple) == 0) {
    # No between vars

    data.frame()
  } else {
    # Between vars never need separate error terms
    df_error_between <- df_total_between - sum(partial_between[["df"]])
    partial_between_error <- data.frame(
      term = "Error between subjects",
      df = df_error_between,
      MS = partial_between[["MS"]][[1]] / partial_between[["F"]][[1]],
      stringsAsFactors = FALSE
    )
    partial_between_error[["SS"]] <-
      partial_between_error[["MS"]] * partial_between_error[["df"]]
    partial_between[["PRE"]] <-
      partial_between[["SS"]] / (partial_between_error[["SS"]] + partial_between[["SS"]])
    partial_between[["p"]] <-
      pf(partial_between[["F"]], partial_between[["df"]], df_error_between, lower.tail = FALSE)

    vctrs::vec_c(partial_between, partial_between_error)
  }

  partials <- list(
    within = treatment_within,
    between = treatment_between
  )

  # PART TOTALS
  get_partial_total_ss <- function(partials, type) {
    other_type <- setdiff(c("between", "within"), type)
    if (length(vars_all[[type]]) == 0) {
      # none of this type of variable, have to infer total
      total_row[["SS"]] - sum(partials[[other_type]][["SS"]])
    } else {
      # total is the explicit sum of the partials
      sum(partials[[type]][["SS"]])
    }
  }
  within_total <- data.frame(
    term = "Total within subjects",
    SS = get_partial_total_ss(partials, "within"),
    df = df_total_within,
    stringsAsFactors = FALSE
  )
  between_total <- data.frame(
    term = "Total between subjects",
    SS = get_partial_total_ss(partials, "between"),
    df = df_total_between,
    stringsAsFactors = FALSE
  )

  # FULL TABLE
  tbl <- vctrs::vec_c(
    partials[["between"]], between_total,
    partials[["within"]], within_total,
    total_row
  )[c("term", "SS", "df", "MS", "F", "PRE", "p")]
  tbl[["df"]] <- as.integer(tbl[["df"]])
  tbl[["MS"]] <- tbl[["SS"]] / tbl[["df"]]
  tbl <- tbl[tbl[["df"]] > 0, ]

  rl <- list(tbl = as.data.frame(tbl), fit = fit, models = NULL)
  class(rl) <- "supernova"
  attr(rl, "type") <- strrep("I", type)
  attr(rl, "verbose") <- verbose
  return(rl)
}


# Printing ------------------------------------------------------------------------------------

#' @export
print.supernova <- function(x, pcut = 4, ...) {
  is_verbose <- attr(x, "verbose") == TRUE
  is_lmer_model <- "lmerMod" %in% class(x$fit)
  is_null_model <- length(variables(x$fit)$predictor) == 0

  # setup
  tbl <- x$tbl

  # NUMBER FORMATTING
  # df to integer; SS, MS, F to 3 decimals; PRE to 4 decimals; p to pcut
  tbl[["df"]] <- format(as.integer(tbl[["df"]]))
  tbl[c("SS", "MS", "F")] <- purrr::map(
    c("SS", "MS", "F"),
    function(term) format(round(tbl[[term]], 3), nsmall = 3)
  )
  tbl[["PRE"]] <- format(round(tbl[["PRE"]], 4), nsmall = 4, scientific = FALSE)
  tbl[["p"]] <- format(round(tbl[["p"]], pcut), nsmall = pcut, scientific = FALSE)

  # NAs to blank spots
  if (!is.null(tbl$description)) tbl$description[is.na(tbl$description)] <- ""
  tbl <- lapply(tbl, function(x) gsub("\\s*NA\\s*", "   ", x))
  tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)

  # trim leading 0 from p and PRE
  tbl[, c("p", "PRE")] <- vapply(
    tbl[, c("p", "PRE")],
    function(x) ifelse(x < 1, substring(x, 2), x),
    character(nrow(tbl))
  )

  # TABLE FORMATTING
  # add placeholders for null model
  if (is_null_model) tbl[1:2, 3:8] <- "---"

  # term names and horizontal rules
  if (!is_lmer_model) {
    tbl <- insert_rule(tbl, 1)
    tbl <- insert_rule(tbl, nrow(tbl))
  } else {
    tbl <- insert_row(tbl, 1, c("Between Subjects", rep("", 6)))
    tbl <- insert_row(
      tbl, grep("^Total between subjects", tbl$term) + 1,
      c("Within Subjects", rep("", 6))
    )

    pred_terms <- variables(x$fit)$predictor
    error_terms <- paste(pred_terms, "error")
    tbl[["term"]] <- stringr::str_replace(
      tbl[["term"]], paste0(error_terms, collapse = "|"), "    Error"
    )
    tbl[["term"]] <- stringr::str_replace(
      tbl[["term"]], paste0("(", paste0(pred_terms, collapse = "|"), ")"), "  \\1"
    )
    tbl <- insert_rule(tbl, 1)
    tbl <- insert_rule(tbl, grep("Total between subjects", tbl$term) + 1)
    tbl <- insert_rule(tbl, grep("Total within subjects", tbl$term) + 1)
    tbl[["term"]] <- stringr::str_replace(
      tbl[["term"]], "(Total|Error) (?:between|within) subjects", "\\1"
    )
  }

  # add spaces and a vertical bar to separate the terms & desc from values
  bar_help <- function(x, y) paste0(x, y, " |")
  bar_col <- if (!is_lmer_model && is_verbose) "description" else "term"
  spaces_to_add <- max(nchar(tbl[[bar_col]])) - nchar(tbl[[bar_col]])
  tbl[[bar_col]] <- mapply(bar_help, tbl[[bar_col]], strrep(" ", spaces_to_add))

  # remove unnecessary column names
  names(tbl)[names(tbl) %in% c("term", "description")] <- ""

  # remove unnecessary columns
  if (!is_verbose && !is_lmer_model) tbl[[2]] <- NULL

  # printing
  cli::cat_line(" Analysis of Variance Table (Type ", attr(x, "type"), " SS)")
  cli::cat_line(" Model: ", paste(trimws(deparse(formula(x$fit))), collapse = " "))
  cli::cat_line()

  # use the qualified print method because the generic prints strange in Rmd
  print.data.frame(tbl, row.names = FALSE)
}


# Helpers -------------------------------------------------------------------------------------

select <- function(df, cols, keep = TRUE) {
  if (keep) {
    df[which(names(df) %in% cols)]
  } else {
    df[-which(names(df) %in% cols)]
  }
}


merge_keep_order <- function(left, right, by, order_by) {
  merged <- merge(left, right, by = by)
  merged[match(left[[order_by]], merged[[order_by]]), ]
}
