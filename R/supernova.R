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
#' @param fit A fitted \code{\link{lm}} object
#' @param type The type of sums of squares to calculate:
#'   \itemize{
#'     \item \code{1}, \code{I}, and \code{sequential} compute Type I SS.
#'     \item \code{2}, \code{II}, and \code{hierarchical} compute Type II SS.
#'     \item \code{3}, \code{III}, and \code{orthogonal} compute Type III SS.
#'   }
#'
#' @return An object of the class \code{supernova}, which has a clean print
#'   method for displaying the ANOVA table in the console as well as a  named
#'   list:
#'   \item{tbl}{The ANOVA table as a \code{\link{data.frame}}}
#'   \item{fit}{The original \code{\link[stats]{lm}} object being tested}
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
supernova <- function(fit, type = 3) {
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
  return(rl)
}

#' @export
#' @rdname supernova
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
#' @importFrom stats formula
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
  cat(" Analysis of Variance Table (Type ", attr(x, "type"), " SS)", "\n",
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
