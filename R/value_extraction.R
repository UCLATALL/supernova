#' Extract Estimates/Statistics From a Model
#'
#' This collection of functions is useful for extracting estimates and
#' statistics from a fitted model. They are particularly useful when estimating
#' many models, like when bootstrapping confidence intervals. Each function can
#' be used with an already fitted model as an \code{\link{lm}} object, or a
#' formula and associated data can be passed to it.
#'
#' \describe{
#'   \item{b0}{The intercept from the full model.}
#'   \item{b1}{The slope b1 from the full model.}
#'   \item{fVal}{The F value from the full model.}
#'   \item{PRE}{The Proportional Reduction in Error for the full model.}
#'   \item{SSE}{The SS Error (SS Residual) from the model.}
#'   \item{SSM}{The SS Model (SS Regression) for the full model.}
#'   \item{SSR}{Alias for SSM.}
#' }
#'
#' @param object A \code{\link{lm}} object, or \code{\link{formula}}.
#' @param data If \code{object} is a formula, the data to fit the formula to as
#'   a \code{\link{data.frame}}.
#' @param ... Passthrough arguments to \code{\link{lm}}.
#'
#' @return The value of the estimate as a single number.
#'
#' @importFrom stats lm
#' @name estimate_extraction

#' @rdname estimate_extraction
#' @export
b0 <- function(object, data = NULL, ...) {
  fit <- convert_lm(object, data, ...)
  fit$coefficients[[1]]
}

#' @rdname estimate_extraction
#' @export
b1 <- function(object, data = NULL, ...) {
  fit <- convert_lm(object, data, ...)
  fit$coefficients[[2]]
}

#' @rdname estimate_extraction
#' @export
fVal <- function(object, data = NULL, ...) {
  fit <- convert_lm(object, data, ...)
  summary(fit)$fstatistic[[1]]
}

#' @rdname estimate_extraction
#' @export
PRE <- function(object, data = NULL, ...) {
  fit <- convert_lm(object, data, ...)
  summary(fit)$r.squared[[1]]
}

#' @rdname estimate_extraction
#' @export
SSE <- function(object, data = NULL, ...) {
  fit <- convert_lm(object, data, ...)
  sum(fit$residuals ^ 2)
}

#' @rdname estimate_extraction
#' @export
SSM <- function(object, data = NULL, ...) {
  fit <- convert_lm(object, data, ...)
  sum((fit$fitted.values - mean(fit$model[[1]])) ^ 2)
}

#' @rdname estimate_extraction
#' @export
SSR <- SSM

convert_lm <- function(object, data, ...) {
  return(if ("lm" %in% class(object)) object else lm(object, data, ...))
}
