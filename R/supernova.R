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
#' \code{superanova()} is an alias of \code{supernova()}
#'
#' @param fit A model fit by \code{\link{lm}} or \code{\link[lme4]{lmer}}
#' @param type The type of sums of squares to calculate:
#'   \itemize{
#'     \item \code{1}, \code{I}, and \code{sequential} compute Type I SS.
#'     \item \code{2}, \code{II}, and \code{hierarchical} compute Type II SS.
#'     \item \code{3}, \code{III}, and \code{orthogonal} compute Type III SS.
#'   }
#' @param verbose If \code{FALSE}, the \code{description} column is suppressed.
#'   Defaults to \code{TRUE}.
#'
#' @return An object of the class \code{supernova}, which has a clean print
#'   method for displaying the ANOVA table in the console as well as a  named
#'   list:
#'   \item{tbl}{The ANOVA table as a \code{\link{data.frame}}}
#'   \item{fit}{The original \code{\link[stats]{lm}} or \code{\link[lme4]{lmer}}
#'     object being tested}
#'   \item{models}{Models created by \code{\link{generate_models}}}
#'
#' @examples
#' supernova(lm(Thumb ~ Weight, data = Fingers))
#' format_p <- supernova(lm(Thumb ~ Weight, data = Fingers))
#' print(format_p, pcut = 8)
#'
#' @importFrom stats anova as.formula drop1 pf
#'
#' @references Judd, C. M., McClelland, G. H., & Ryan, C. S. (2017). \emph{Data
#'   Analysis: A Model Comparison Approach to Regression, ANOVA, and Beyond}
#'   (3rd ed.). New York: Routledge. ISBN:879-1138819832
#'
#' @export
supernova <- function(fit, type = 3, verbose = TRUE) {
  UseMethod("supernova")
}


#' @export
#' @rdname supernova
supernova.lm <- function(fit, type = 3, verbose = TRUE) {
  type <- resolve_type(type)
  models <- suppressWarnings(generate_models(fit, type))
  predictors <- variables(fit)$predictor
  fit_null <- update(fit, . ~ NULL)

  # Helpful Table Values
  #
  # n_pred:     the number of predictors in the full model
  # n_rows:     the number of rows in the table
  # model_row:  index for the model row with full model regression statistics
  # model_rows: indices for all rows that have an SS model/regression
  # iv_rows:    indicees for all individual predictor rows
  # error_row:  index for the error row
  n_pred <- length(predictors)
  n_rows <- 3 + ifelse(n_pred < 2, 0, n_pred)
  model_row  <- 1
  model_rows <- 1:(n_rows - 2)
  iv_rows    <- 1 + seq_along(predictors)
  error_row  <- n_rows - 1

  # TABLE SETUP
  term <- c("Model", if (n_pred < 2) NULL else predictors, "Error", "Total")
  desc <- pad(c("(error reduced)", "(from model)", "(empty model)"), term, 1)
  tbl <- data.frame(term = term, description = desc, stringsAsFactors = FALSE)
  tbl$SS = pad(SSE(fit_null), term, 0)
  tbl$df = pad(fit_null$df.residual, term, 0)
  tbl[c("MS", "F", "PRE", "p")] <- NA_real_

  # SS, DF for 1+ PREDICTORS
  if (n_pred > 0) {
    tbl$SS[model_row] <- SSE(fit_null) - SSE(fit)
    tbl$df[model_row] <- length(fit$coefficients) - 1
    tbl$SS[error_row] <- SSE(fit)
    tbl$df[error_row] <- fit$df.residual
  }

  # SS, DF for 2+ PREDICTORS
  if (n_pred > 1) {
    tbl$SS[iv_rows] <- if (type != 3) {
      purrr::map_dbl(models[2:length(models)], function(model) {
        anova(model$simple, model$complex)$`Sum of Sq`[[2]]
      })
    } else if (type == 3) {
      # Type 3 SS cannot be calculated using model comparison with anova()
      # anova() will automatically include lower-order terms when an interaction
      # is present, making it impossible to test for the effect of a term in the
      # presence of its interaction. drop1() fits the model using the low-level
      # matrix representation and bypasses this
      drop1(fit, . ~ .)$`Sum of Sq`[iv_rows]
    }
    tbl$df[iv_rows] <- anova(fit)$Df[iv_rows - 1]
  }

  # MS, F, PRE, p for ALL MODELS
  tbl$MS <- tbl$SS / tbl$df
  tbl$F[model_rows] <- tbl[model_rows, "MS"] / tbl[error_row, "MS"]
  tbl$PRE[model_rows] <- tbl[model_rows, "SS"] / (tbl[model_rows, "SS"] + SSE(fit))
  tbl$p[model_rows] <- pf(tbl$F[model_rows], tbl$df[model_rows], tbl$df[[error_row]], lower.tail = FALSE)

  rl <- list(tbl = tbl, fit = fit, models = models)
  class(rl) <- "supernova"
  attr(rl, "type") <- strrep("I", type)
  attr(rl, "verbose") <- verbose
  return(rl)
}


#' @export
#' @rdname supernova
supernova.lmerMod <- function(fit, type = 3, verbose = FALSE) {
  if (!tolower(type) %in% c(3, "iii", "orthogonal")) {
    stop("Currently only Type III tests can be computed for models with random
         effects (e.g. repeated measures models).")
  }

  model_full <- fit
  model_data <- model_full@frame
  vars_all <- supernova::variables(model_full)
  vars_within_simple <- grep("[:]", vars_all[["within"]], value = TRUE, invert = TRUE)
  vars_between_simple <- grep("[:]", vars_all[["between"]], value = TRUE, invert = TRUE)

  # get formula with no random terms
  formula_complex <- formula(model_full)
  formula_simple <- lme4::nobars(formula_complex)

  # TOTAL
  model_lm <- lm(formula_simple, data = model_data)
  model_empty <- stats::update(model_lm, . ~ NULL)
  total <- anova_tbl(model_empty)
  total[["term"]] <- "Total"

  # DF
  df_total_between <- length(unique(model_data[[vars_all[["group"]]]])) - 1
  df_total_within <- total[["df"]] - df_total_between

  # TREATMENT ROWS
  anova_lm <- anova_tbl(model_lm) %>% dplyr::select(-dplyr::one_of("F"))
  anova_lmer <- anova_tbl(model_full) %>% dplyr::select(dplyr::one_of(c("term", "F")))
  partial_rows <- dplyr::right_join(anova_lm, anova_lmer, by = "term")

  # TREATMENT WITHIN
  partial_within <- partial_rows[partial_rows[["term"]] %in% vars_all[["within"]],]
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
    partial_within[["p"]] <- pf(partial_within[["F"]], partial_within[["df"]],
                                df_error_within, lower.tail = FALSE)

    dplyr::bind_rows(partial_within, partial_within_error)
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
      part <- dplyr::bind_rows(
        dplyr::filter_at(partial_within, dplyr::vars("term"), ~ . == x),
        dplyr::filter_at(partial_within_error, dplyr::vars("match"), ~ . == x)
      ) %>% dplyr::select(-dplyr::one_of("match"))

      part[, c("PRE", "p")] <- NA_real_
      part[["PRE"]][[1]] <- part[["SS"]][[1]] / sum(part[["SS"]])
      part[["p"]][[1]] <- pf(part[["F"]][[1]], part[["df"]][[1]], part[["df"]][[2]],
                             lower.tail = FALSE)
      dplyr::select(part, dplyr::one_of("term", "SS", "df", "MS", "F", "PRE", "p"))
    })
  }

  # TREATMENT BETWEEN
  partial_between <- partial_rows[partial_rows[["term"]] %in% vars_all[["between"]],]
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
    partial_between[["p"]] <- pf(partial_between[["F"]], partial_between[["df"]],
                                 df_error_between, lower.tail = FALSE)

    dplyr::bind_rows(partial_between, partial_between_error)
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
      total[["SS"]] - sum(partials[[other_type]][["SS"]])
    } else {
      # total is the explicit sum of the partials
      sum(partials[[type]][["SS"]])
    }
  }
  within_total <- dplyr::tibble(
    term = "Total within subjects",
    SS = get_partial_total_ss(partials, "within"),
    df = df_total_within
  )
  between_total <- dplyr::tibble(
    term = "Total between subjects",
    SS = get_partial_total_ss(partials, "between"),
    df = df_total_between
  )

  # FULL TABLE
  tbl <- dplyr::bind_rows(
    partials[["between"]], between_total,
    partials[["within"]], within_total,
    total
  ) %>%
    dplyr::select_at(dplyr::vars("term", "SS", "df", "MS", "F", "PRE", "p")) %>%
    dplyr::mutate_at(dplyr::vars("df"), as.integer)
  tbl[["MS"]] <- tbl[["SS"]] / tbl[["df"]]
  tbl <- tbl[tbl[["df"]] > 0, ] %>% as.data.frame()

  rl <- list(tbl = tbl, fit = fit, models = NULL)
  class(rl) <- "supernova"
  attr(rl, "type") <- strrep("I", type)
  attr(rl, "verbose") <- verbose
  return(rl)
}


#' @export
#' @rdname supernova
superanova <- supernova


#' @export
print.supernova <- function(x, pcut = 4, ...) {
  verbose <- attr(x, "verbose")
  is_lmer_model <- "lmerMod" %in% class(x$fit)
  is_null_model <- length(variables(x$fit)$predictor) == 0

  if (is_lmer_model & verbose) {
    warning("There is currently no verbose version of the supernova table for
         lmer() models. Switching to non-verbose.")
    attr(x, "verbose") <- FALSE
  }

  # setup
  tbl <- x$tbl

  # NUMBER FORMATTING
  # df to integer; SS, MS, F to 3 decimals; PRE to 4 decimals; p to pcut
  tbl[["df"]] <- format(as.integer(tbl[["df"]]))
  tbl[c("SS", "MS", "F")] <- purrr::map(c("SS", "MS", "F"),
    function(term) format(round(tbl[[term]], 3), nsmall = 3)
  )
  tbl[["PRE"]] <- format(round(tbl[["PRE"]], 4), nsmall = 4, scientific = FALSE)
  tbl[["p"]] <- format(round(tbl[["p"]], pcut), nsmall = pcut, scientific = FALSE)

  # NAs to blank spots
  if (!is.null(tbl$description)) tbl$description[is.na(tbl$description)] <- ""
  tbl <- lapply(tbl, function(x) gsub("\\s*NA\\s*", "   ", x)) %>%
    as.data.frame(stringsAsFactors = FALSE)

  # trim leading 0 from p
  tbl[["p"]] <- substring(tbl[["p"]], 2)

  # TABLE FORMATTING
  # add placeholders for null model
  if (is_null_model) tbl[1:2, 3:8] <- "---"

  # term names and horizontal rules
  if (!is_lmer_model) {
    tbl <- insert_rule(tbl, 1)
    tbl <- insert_rule(tbl, nrow(tbl))
  } else {
    tbl <- insert_row(tbl, 1, c("Between Subjects", rep("", 6)))
    tbl <- insert_row(tbl, grep("^Total between subjects", tbl$term) + 1,
                      c("Within Subjects", rep("", 6)))

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
    tbl[["term"]] <- tbl[["term"]] %>%
      stringr::str_replace("(Total|Error) (?:between|within) subjects", "\\1")
  }

  # add spaces and a vertical bar to separate the terms & desc from values
  barHelp <- function(x, y) paste0(x, y, " |")
  bar_col <- if (verbose) "description" else "term"
  spaces_to_add <- max(nchar(tbl[[bar_col]])) - nchar(tbl[[bar_col]])
  tbl[[bar_col]] <- mapply(barHelp, tbl[[bar_col]], strrep(" ", spaces_to_add))

  # remove unnecessary column names
  names(tbl)[names(tbl) %in% c("term", "description")] <- ""

  # remove unnecessary columns
  if (!verbose && !is_lmer_model) tbl[[2]] <- NULL

  # printing
  cat_line(" Analysis of Variance Table (Type ", attr(x, "type"), " SS)")
  cat_line(" Model: ", paste(trimws(deparse(formula(x$fit))), collapse = " "))
  cat_line(" ")
  print(tbl, row.names = FALSE)
}
