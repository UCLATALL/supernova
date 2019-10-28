#' supernova
#'
#' An alternative set of summary statistics for ANOVA. Sums of squares, degrees
#' of freedom, mean squares, and F value are all computed with Type III sums of
#' squares. This package adds proportional reduction in error, an explicit
#' summary of the whole model, and separate formatting of p values and is
#' intended to match the output used in Judd, McClelland, and Ryan (2017).
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

  # get formula with no random terms
  formula_complex <- formula(model_full)
  formula_simple <- lme4::nobars(formula_complex)

  # determine within and between variables
  bare_terms <- supernova::variables(formula_simple)
  pred_terms <- bare_terms$predictor
  rand_terms <- gsub(
    "1 ?\\| ?", "",
    as.character(lme4::findbars(formula_complex))
  )
  group_term <- rand_terms[which.min(nchar(rand_terms))]
  within_terms <- sub_matches(rand_terms, paste0("(.*):", group_term), "\\1")
  # add within interactions
  within_terms <- grep(paste0(within_terms, collapse = "|"), pred_terms, value = TRUE)

  # outcome_term <- bare_terms$outcome
  # between_terms <- setdiff(pred_terms, within_terms)

  # total line
  model_lm <- lm(formula_simple, data = model_data)
  anova_lm <- anova_tbl(model_lm)
  model_empty <- stats::update(model_lm, . ~ NULL)
  total <- anova_tbl(model_empty)
  total[["term"]] <- "Total"

  # WITHIN
  within_treatment <- dplyr::slice(anova_lm, -nrow(anova_lm))
  within_treatment[["F"]] <- anova_tbl(model_full)[["F"]]

  if (nrow(within_treatment) == 0) {
    within_error <- data.frame(match = character(0))
  } else {
    df_term_n <- length(unique(model_data[[group_term]])) - 1
    within_error <- data.frame(
      match = within_terms,
      term = paste(within_terms, "error"),
      df = within_treatment[["df"]] * df_term_n,
      MS = within_treatment[["MS"]] / within_treatment[["F"]],
      stringsAsFactors = FALSE
    )
    within_error[["SS"]] <- within_error[["MS"]] * within_error[["df"]]
  }

  within_partials <- purrr::map_dfr(within_terms, function(x) {
    part <- dplyr::bind_rows(
      dplyr::filter_at(within_treatment, dplyr::vars("term"), ~ . == x),
      dplyr::filter_at(within_error, dplyr::vars("match"), ~ . == x)
    ) %>%
      dplyr::select(-dplyr::one_of("match"))

    part[, c("PRE", "p")] <- NA_real_
    part[["PRE"]][[1]] <- part[["SS"]][[1]] / sum(part[["SS"]])
    part[["p"]][[1]] <- pf(part[["F"]][[1]], part[["df"]][[1]], part[["df"]][[2]], lower.tail = FALSE)
    part
  })

  within_total <- data.frame(
    term = "Total within subjects",
    SS = sum(within_partials[["SS"]]),
    df = sum(within_partials[["df"]]),
    stringsAsFactors = FALSE
  )

  # BETWEEN
  between_total <- data.frame(
    term = "Total between subjects",
    SS = total[["SS"]] - within_total[["SS"]],
    df = total[["df"]] - within_total[["df"]],
    stringsAsFactors = FALSE
  )

  # FULL TABLE
  tbl <- dplyr::bind_rows(between_total, within_partials, within_total, total) %>%
    dplyr::select_at(dplyr::vars("term", "SS", "df", "MS", "F", "PRE", "p")) %>%
    dplyr::mutate_at(dplyr::vars("df"), as.integer)
  tbl[["MS"]] <- tbl[["SS"]] / tbl[["df"]]

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

  # df to integer; SS, MS, F to 3 decimals; PRE to 4 decimals; p to pcut
  tbl[["df"]] <- format(as.integer(tbl[["df"]]))
  tbl[c("SS", "MS", "F")] <- purrr::map(c("SS", "MS", "F"),
    function(term) format(round(tbl[[term]], 3), nsmall = 3)
  )
  tbl[["PRE"]] <- format(round(tbl[["PRE"]], 4), nsmall = 4, scientific = FALSE)
  tbl[["p"]] <- format(round(tbl[["p"]], pcut), nsmall = pcut, scientific = FALSE)

  # NAs to blank spots
  if (!is.null(tbl$description)) tbl$description[is.na(tbl$description)] <- ""
  tbl <- data.frame(lapply(tbl, function(x) gsub("\\s*NA\\s*", "   ", x)),
                  stringsAsFactors = FALSE)

  # adjust term names for lmerMod
  if (is_lmer_model) {
    tbl <- insert_row(tbl, 1, rep("", 7))
    tbl <- insert_row(tbl, 3, rep("", 7))
    tbl[1:3, "term"] <- c("Between Subjects", "  Total", "Within Subjects")

    total_wi_row <- nrow(tbl) - 1
    within_rows <- seq(4, total_wi_row)
    tbl[total_wi_row, "term"] <- "Total"
    tbl[within_rows, "term"] <- paste0("  ", tbl[within_rows, "term"])
    tbl[within_rows, "term"] <- stringr::str_replace(
      tbl[within_rows, "term"],
      "(.*) error$", "    Error"
    )
  }

  # trim leading 0 from p
  tbl[["p"]] <- substring(tbl[["p"]], 2)

  # add spaces and a vertical bar to separate the terms & desc from values
  barHelp <- function(x, y) paste0(x, y, " |")
  bar_col <- if (verbose) "description" else "term"
  spaces_to_add <- max(nchar(tbl[[bar_col]])) - nchar(tbl[[bar_col]])
  tbl[[bar_col]] <- mapply(barHelp, tbl[[bar_col]], strrep(" ", spaces_to_add))

  # add placeholders for null model
  if (is_null_model) tbl[1:2, 3:8] <- "---"

  # add horizontal rules at top and between sections
  tbl <- insert_rule(tbl, 1)
  tbl <- insert_rule(tbl, nrow(tbl))
  if (is_lmer_model) tbl <- insert_rule(tbl, 4)

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
