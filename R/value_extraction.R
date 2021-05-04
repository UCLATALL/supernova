#' Extract estimates/statistics from a model
#'
#' This collection of functions is useful for extracting estimates and statistics from a fitted
#' model. They are particularly useful when estimating many models, like when bootstrapping
#' confidence intervals. Each function can be used with an already fitted model as an [`lm`] object,
#' or a formula and associated data can be passed to it. **All of these assume the comparison is the
#' empty model.**
#'
#' - **`b0`**: The intercept from the full model.
#' - **`b1`**: The slope b1 from the full model.
#' - **`fVal`**: The F value from the full model.
#' - **`PRE`**: The Proportional Reduction in Error for the full model.
#' - **`SSE`**: The SS Error (SS Residual) from the model.
#' - **`SSM`**: The SS Model (SS Regression) for the full model.
#' - **`SSR`**: Alias for SSM.
#'
#'
#' @param object A [`lm`] object, or [`formula`].
#' @param data If `object` is a formula, the data to fit the formula to as a [`data.frame`].
#' @param ... Additional arguments passed through to [`lm`].
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
f <- function(object, data = NULL, ...) {
  fit <- convert_lm(object, data, ...)
  summary(fit)$fstatistic[[1]]
}

#' @rdname estimate_extraction
#' @export
pre <- function(object, data = NULL, ...) {
  fit <- convert_lm(object, data, ...)
  summary(fit)$r.squared[[1]]
}

#' @rdname estimate_extraction
#' @export
sse <- function(object, data = NULL, ...) {
  fit <- convert_lm(object, data, ...)
  sum(fit$residuals^2)
}

#' @rdname estimate_extraction
#' @export
ssm <- function(object, data = NULL, ...) {
  fit <- convert_lm(object, data, ...)
  sum((fit$fitted.values - mean(fit$model[[1]]))^2)
}

#' @rdname estimate_extraction
#' @export
ssr <- ssm


convert_lm <- function(object, data, ...) {
  return(if ("lm" %in% class(object)) object else lm(object, data, ...))
}


# Deprecated for consistent names -------------------------------------------------------------
# nolint start

#' @rdname estimate_extraction
#' @export
fVal <- function(object, data = NULL, ...) {
  f(object, data, ...)
}

#' @rdname estimate_extraction
#' @export
PRE <- function(object, data = NULL, ...) {
  pre(object, data, ...)
}

#' @rdname estimate_extraction
#' @export
SSE <- function(object, data = NULL, ...) {
  sse(object, data, ...)
}

#' @rdname estimate_extraction
#' @export
SSM <- function(object, data = NULL, ...) {
  sse(object, data, ...)
}

#' @rdname estimate_extraction
#' @export
SSR <- function(object, data = NULL, ...) {
  ssr(object, data, ...)
}

# nolint end
