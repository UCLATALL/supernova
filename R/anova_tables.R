#' supernova
#'
#' An alternative set of summary statistics for ANOVA. Sums of squares, degrees
#' of freedom, mean squares, and F value are all computed with type 3 sums of
#' squares. This package adds proportional reduction in error, an explicit
#' summary of the whole model, and separate formatting of p values and is
#' intended to match the output used in Judd, McClelland, and Ryan (2017).
#'
#' \code{superanova()} is an alias of \code{supernova()}
#'
#' @param fit A fitted \code{\link{lm}} object
#'
#' @return An object of the class \code{supernova}, which has a clean print
#'   method for displaying the ANOVA table in the console as well as a  named
#'   list: \item{tbl}{The ANOVA table as a \code{\link{data.frame}}}
#'   \item{fit}{The original \code{\link[stats]{lm}} object being tested}
#'
#' @examples
#' supernova(lm(Thumb ~ Weight, data = Fingers))
#'
#' @importFrom stats resid df.residual predict pf coefficients drop1
#'
#' @references Judd, C. M., McClelland, G. H., & Ryan, C. S. (2017).
#'   \emph{Data Analysis: A Model Comparison Approach to Regression, ANOVA,
#'   and Beyond} (3rd ed.). New York: Routledge. ISBN:879-1138819832
#'
#' @export
supernova <- function(fit) {
  fit_null <- update(fit, . ~ NULL)
  predictors <- variables(fit)$predictor
  n_pred <- length(predictors)
  n_rows <- 3 + ifelse(n_pred < 2, 0, n_pred)  # add a row for each partial
  calc_sse <- function(fit) sum(resid(fit) ^ 2, na.rm = TRUE)

  # 0 PREDICTORS
  na_pad <- rep(NA_real_, n_rows)
  tbl <- data.frame(
    term = c("Model", if (n_pred < 2) NULL else predictors, "Error", "Total"),
    description = c("(error reduced)", rep(NA, n_rows - 3), "(from model)", "(empty model)"),
    SS = c(rep(NA_real_, n_rows - 1), calc_sse(fit_null)),
    df = c(rep(NA_real_, n_rows - 1), df.residual(fit_null)),
    MS = na_pad, F = na_pad, PRE = na_pad, p = na_pad,
    stringsAsFactors = FALSE
  )

  if (n_pred > 0) {  # 1+ PREDICTORS
    # error row SS, df
    tbl[n_rows - 1,]$SS <- calc_sse(fit)
    tbl[n_rows - 1,]$df <- df.residual(fit)

    # model row SS, PRE, df
    tbl[1,]$SS <- calc_sse(fit_null) - calc_sse(fit)
    tbl[1,]$PRE <- summary(fit)$r.squared
    tbl[1,]$df <- length(coefficients(fit)) - 1
  }

  if (n_pred > 1) { # 2+ PREDICTORS
    iv_rows <- 1 + seq_along(predictors)

    # drop1 drops each term, keeping all others in, and then computes SS
    single_term_deletions <- drop1(fit, . ~ .)
    sse.c <- single_term_deletions[iv_rows,]$RSS
    sse.a <- calc_sse(fit)
    tbl[iv_rows,]$PRE <- (sse.c - sse.a) / sse.c
    tbl[iv_rows,]$SS <- single_term_deletions$`Sum of Sq`[iv_rows]
    tbl[iv_rows, ]$df <- single_term_deletions$`Df`[iv_rows]
  }

  # update MS, F, p
  model_rows <- 1:(n_rows - 2)  # indices for rows that have SSMs
  error_row <- n_rows - 1  # index for the error row

  tbl$MS <- tbl$SS / tbl$df
  tbl[model_rows,]$F <- tbl$MS[model_rows] / tbl$MS[[error_row]]
  tbl[model_rows,]$p <- pf(tbl$F[model_rows], tbl$df[model_rows],
                           tbl$df[[error_row]], lower.tail = FALSE)

  rl <- list(tbl = tbl, fit = fit)
  class(rl) <- "supernova"
  return(rl)
}


#' Robust version of \code{\link[stats]{update}}
#'
#' This function solves two problems with \code{\link[stats]{update}}: pipes
#' will break when the data is first piped to the \code{\link[stats]{lm}}
#' object, and data is re-evaluated against the full dataset, even if the model
#' being updated had \code{na.action = "na.omit"}. See details below for more
#' information.
#'
#' \strong{Problem with pipes:} \code{\link[stats]{update}} does not support
#' linear models where the data was piped via \code{\link[magrittr]{\%>\%}} to
#' the model function as in \code{mtcars \%>\% lm(mpg ~ hp, data = .) \%>\%
#' stats::update()}. This is because the \code{\link[stats]{update}} function
#' relies on \code{\link[stats]{getCall}}, which returns a call that has
#' \code{data = .} argument, which is completely uninformative. This function
#' creates a new model by extracting the formula from the old model via
#' \code{\link[stats]{formula}} and the data used in the old model via the
#' \code{model$call} and \code{environment}. Then the new (old) model is updated
#' as it now has all the necessary components.
#'
#' \strong{Problem with \code{na.action}}: When updating models with missing
#' data, the new model is evaluated against the full dataset even if the
#' original model had missing data for some of the values. For example, if there
#' are missing values on X for a model where Y ~ X, when updating to fit the
#' null model Y ~ NULL, the full set of Y is used instead of only the values of
#' Y where there are values of X. This is not inherently a problem, however it
#' yields incorrect degrees of freedom and sum of squares for the total row in
#' an ANOVA because different datasets are being used for the total row and the
#' model and error rows. The present solution runs the \code{na.action} from the
#' \code{old} model on the full dataset before passing it to the \code{new} one.
#'
#' @param old An existing fit from a model function such as \code{\link{lm}},
#'   \code{\link{glm}} and many others.
#' @param new Changes to the formula; see \code{\link{update.formula}} for
#'   details.
#' @param ... Additional arguments to the call (see
#'   \code{\link[stats]{update}}), or arguments with changed values. Use
#'   \code{name = NULL} to remove the argument name.
#' @param na.action	As in \code{\link{lm}}, a function which indicates what
#'   should happen when the data contain NAs. The default is set by the
#'   \code{na.action} setting of \code{\link{options}}, and is
#'   \code{\link{na.fail}} if that is unset. The ‘factory-fresh’ default is
#'   \code{\link{na.omit}}. Another possible value is \code{NULL}, no action.
#'   Value \code{\link{na.exclude}} can be useful.
#'
#' @return If evaluate = TRUE the fitted object, otherwise the updated call.
#' @export
update <- function(old, new, ..., na.action) {
  vars <- all.vars(formula(old))
  data <- eval(old$call$data, environment(formula(old)))

  # listwise delete values from all affected variables
  na_rows <- sort(unlist(
    lapply(data[,vars], function(x) which(is.na(x))),
    use.names = FALSE
  ))
  data[na_rows, vars] <- NA

  # create warning message
  if (length(na_rows) > 0) {
    plural <- if (length(na_rows) > 1) "s" else ""
    message(sprintf(
      "Note: %s case%s removed due to missing value(s). Row number%s: %s",
      length(na_rows), plural, plural, paste(na_rows, collapse = ", ")
    ))
  }

  stats::update(old, new, data = data)
}

#' Extract the variables from a model
#'
#' @param object An \code{\link{lm}} or \code{\link{supernova}} object
#'
#' @importFrom stats formula terms
#'
#' @return A list containing the \code{outcome} and \code{predictor} variables
#'   in the model.
#' @export
variables <- function(object) {
  fit <- if (class(object) == "supernova") object$fit else object
  fit_formula <- formula(fit)
  all_vars <- all.vars(fit_formula)
  ivs <- labels(terms(fit_formula))
  list(outcome = all_vars[!(all_vars %in% ivs)], predictor = ivs)
}

# Calculate the degrees of freedom for a predictor in a model
#
# @param predictor Character string of the name of the predictor
# @param fit An \code{\link{lm}} object with the predictor
#
# @importFrom stats model.frame
#
# @return The df for the predictor.
calc_pred_df <- function(predictor, fit) {
  pred <- model.frame(fit)[[predictor]]
  if (is.factor(pred)) nlevels(pred) - 1 else 1
}

#' @export
#' @rdname supernova
#' @usage NULL
superanova <- supernova


# Printing ----------------------------------------------------------------

#' print.supernova
#'
#' A print method for the supernova class
#'
#' @param x A \code{\link{supernova}} object.
#' @param pcut The integer number of decimal places of p-values to show.
#' @param ... Additional display arguments.
#'
#' @importFrom stats model.frame
#'
#' @export
print.supernova <- function(x, pcut = 4, ...) {
  # setup
  y <- x$tbl

  # df to integer
  y$df <- format(as.integer(y$df))

  # SS, MS, F to 3 decimals
  digits_cols <- names(y) %in% c("SS", "MS", "F")
  y[digits_cols] <- format(round(y[digits_cols], 3), nsmall = 3)

  # PRE to 4 decimals
  y$PRE <- format(round(y$PRE, 4), nsmall = 4, scientific = FALSE)

  # p to pcut
  y$p <- format(round(y$p, pcut), nsmall = pcut, scientific = FALSE)

  # NAs to blank spots
  y$description[is.na(y$description)] <- ""
  y <- data.frame(lapply(y, function(x) {gsub("\\s*NA\\s*", "   ", x)}),
                  stringsAsFactors = FALSE)

  # trim leading 0 from p
  y$p <- substring(y$p, 2)

  # add spaces and a vertical bar to separate the terms & desc from values
  barHelp <- function(x, y) paste0(x, y, " |")
  spaces_to_add <- max(nchar(y$description)) - nchar(y$description)
  y$description <- mapply(barHelp, y$description, strrep(" ", spaces_to_add))

  # remove unnecessary column names
  names(y)[1:2] <- c("", "")

  # add placeholders for null model
  if (length(variables(x$fit)$predictor) == 0) y[1:2, 3:8] <- "---"

  # add horizontal separator under header and before total line
  y <- insert_rule(y, 1)
  y <- insert_rule(y, nrow(y))

  # printing
  cat(" Analysis of Variance Table (Type III SS)", "\n",
      " Model: ", deparse(formula(x$fit)),         "\n",
      " \n", sep = "")
  print(y, row.names = FALSE)
}

# Insert a horizontal rule in table for pretty printing
#
# @param df        Original data.frame
# @param insert_at The row in which to insert the new contents
#
# @return df The original data.frame with the new row inserted.
insert_rule <- function(df, insert_at) {
  df[seq(insert_at + 1, nrow(df) + 1), ] <- df[seq(insert_at, nrow(df)), ]
  df[insert_at, ] <- strrep("-", vapply(df, function(x) max(nchar(x)), 0))
  return(df)
}
