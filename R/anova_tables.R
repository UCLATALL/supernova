#' supernova
#'
#' An alternative set of summary statistics for ANOVA. Sums of squares, degrees 
#' of freedom, mean squares, and F value are all equivalent to 
#' \code{\link{anova.lm}}. This package adds proportional reduction in 
#' error, an explicit summary of the whole model, and separate formatting of p 
#' values and is intended to match the output used in Judd, McClelland, and 
#' Ryan (2017).
#' 
#' \code{superanova()} is an alias of \code{supernova()}
#'
#' @param fit A fitted \code{\link{lm}} object
#'
#' @return An object of the class \code{supernova}, which has a clean print method for 
#' displaying the ANOVA table in the console as well as a  named list:
#'   \item{tbl}{The ANOVA table as a \code{\link{data.frame}}}
#'   \item{fit}{The original \code{\link[stats]{lm}} object being tested}
#'
#' @examples
#' supernova(lm(Thumb ~ Weight, data = Fingers))
#'
#' @importFrom stats resid df.residual predict pf
#'
#' @references Judd, C. M., McClelland, G. H., & Ryan, C. S. (2017). Data 
#' Analysis: A Model Comparison Approach to Regression, ANOVA, and Beyond 
#' (3rd edition). New York: Routledge. ISBN:879-1138819832
#'
#' @export
supernova <- function(fit) {
  fit_null <- update(fit, . ~ NULL)
  
  predictors <- variables(fit)$predictor
  n_pred <- length(predictors)
  n_rows <- 3 + ifelse(n_pred < 2, 0, n_pred)  # add a row for each partial
  model_rows <- 1:(n_rows - 2)  # indices for rows that have SSMs
  error_row <- n_rows - 1  # index for the error row
  
  # 0 PREDICTORS
  tbl <- data.frame(
    term = c("Model", if (n_pred < 2) NULL else predictors, "Error", "Total"),
    description = c("(error reduced)", rep(NA, n_rows - 3), "(from model)", "(empty model)"),
    SS = c(rep(NA_real_, n_rows - 1), sum(resid(fit_null) ^ 2)),
    df = c(rep(NA_real_, n_rows - 1), df.residual(fit_null)),
    MS = rep(NA_real_, n_rows),
    F  = rep(NA_real_, n_rows),
    PRE = rep(NA_real_, n_rows),
    p = rep(NA_real_, n_rows),
    stringsAsFactors = FALSE
  )
  
  if (n_pred > 0) {  # 1+ PREDICTORS
    # error row SS, df
    tbl[n_rows - 1,]$SS <- sum(resid(fit) ^ 2)
    tbl[n_rows - 1,]$df <- df.residual(fit)
    
    # model row SS, df, PRE
    tbl[1,]$SS <- sum((predict(fit) - predict(fit_null)) ^ 2)
    tbl[1,]$df <- n_pred
    tbl[1,]$PRE <- calc_pre(fit, fit_null)
  }
  
  if (n_pred > 1) {  # 2+ PREDICTORS
    partial_rows <- 2:(n_pred + 1)
    
    compact_models <- lapply(predictors, function(x) {
      # the update function drops the predictor-to-test
      update(fit, formula(paste0("~ . -", x)))
    })
    
    tbl[partial_rows, ]$SS <- as.numeric(lapply(compact_models, calc_ssr, fit_augmented = fit))
    tbl[partial_rows, ]$df <- rep(1, n_pred)
    tbl[partial_rows, ]$PRE <- as.numeric(lapply(compact_models, function(x) {
      calc_pre(fit, x)
    }))
  }
  
  # update MS, F, p
  tbl$MS <- tbl$SS / tbl$df
  tbl[model_rows,]$F <- tbl$MS[model_rows] / tbl$MS[[error_row]]
  tbl[model_rows,]$p <- pf(tbl$F[model_rows], tbl$df[model_rows], tbl$df[[error_row]], lower.tail = FALSE)

  rl <- list(tbl = tbl, fit = fit)
  class(rl) <- "supernova"
  return(rl)
}


#' Robust version of \code{\link[stats]{update}}
#' 
#' \code{\link[stats]{update}} does not support linear models where the data was 
#' piped via \code{\link[magrittr]{`%>%`}} to the  model function as in 
#' \code{mtcars %>% lm(mpg ~ hp, data = .) %>% stats::update()}. This is because 
#' the \code{\link[stats]{update}} function relies on \code{\link[stats]{getCall}}, 
#' which returns a call that has \code{data = .} argument, which is completely 
#' uninformative. This function creates a new model by extracting the formula 
#' from the old model via \code{\link[stats]{formula}} and the data used in 
#' the old model via the \code{model$call} and \code{environment}. Then the new 
#' (old) model is updated as it now has all the necessary components.
#'
#' @param old An existing fit from a model function such as \code{\link{lm}}, 
#'            \code{\link{glm}} and many others.
#' @param new Changes to the formula; see \code{\link{update.formula}} for details.
#' @param ... Additional arguments to the call, or arguments with changed values. 
#'            Use name = NULL to remove the argument name.
#'
#' @return If evaluate = TRUE the fitted object, otherwise the updated call.
#' @export
update <- function(old, new, ...) {
  data <- eval(old$call$data, environment(formula(old)))
  old_with_data <- lm(formula(old), data = data)
  stats::update(old_with_data, new, ...)
}

#' Extract the variables from a model
#'
#' @param fit An \code{\link{lm}} object of the model with the predictors being tested
#' 
#' @importFrom stats formula terms
#'
#' @return The variables in the model: outcome and predictors
#' @export
variables <- function(fit) {
  fit_formula <- formula(fit)
  all_vars <- all.vars(fit_formula)
  ivs <- labels(terms(fit_formula))
  dvs <- all_vars[!(all_vars %in% ivs)]
  list(outcome = dvs, predictor = ivs) 
}

# Calculate the PRE for a model
#
# @param fit_augmented An \code{\link{lm}} object of the model with the predictors being tested
# @param fit_compact An \code{\link{lm}} object of the model to test against
# 
# @importFrom stats resid
#
# @return The PRE for the model.
calc_pre <- function(fit_augmented, fit_compact) {
  sse.c <- sum(resid(fit_compact) ^ 2)
  sse.a <- sum(resid(fit_augmented) ^ 2)
  (sse.c - sse.a) / sse.c
}

# Calculate the SSR for a model
#
# @param fit An \code{\link{lm}} object. 
# 
# @importFrom stats resid
#
# @return The sum of squares regression/between/explained
calc_ssr <- function(fit, fit_augmented) {
  sum(resid(fit) ^ 2) - sum(resid(fit_augmented) ^2)
}

#' @export
#' @rdname supernova
#' @usage NULL
superanova <- supernova

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
# @param insert_at The row in which to insert the new contents (remaining rows will be pushed down).
#
# @return df The original data.frame with the new row inserted.
insert_rule <- function(df, insert_at) {
  df[seq(insert_at + 1, nrow(df) + 1), ] <- df[seq(insert_at, nrow(df)), ]
  df[insert_at, ] <- strrep("-", sapply(df, function(x) max(nchar(x))))
  return(df)
}
