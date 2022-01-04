#' Extract estimates/statistics from a model
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' **Note:** These functions are deprecated in the `supernova` package and have been moved to the
#' `coursekata` package which can be found at
#' [https://github.com/UCLATALL/coursekata-r](https://github.com/UCLATALL/coursekata-r)
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
#' @keywords internal
#' @export
b0 <- function(object, data = NULL, ...) {
  value_deprecated("b0")
  fit <- convert_lm(object, data, ...)
  fit$coefficients[[1]]
}

#' @rdname estimate_extraction
#' @keywords internal
#' @export
b1 <- function(object, data = NULL, ...) {
  value_deprecated("b1")
  fit <- convert_lm(object, data, ...)
  fit$coefficients[[2]]
}

#' @rdname estimate_extraction
#' @keywords internal
#' @export
f <- function(object, data = NULL, ...) {
  value_deprecated("f")
  fit <- convert_lm(object, data, ...)
  summary(fit)$fstatistic[[1]]
}

#' @rdname estimate_extraction
#' @keywords internal
#' @export
pre <- function(object, data = NULL, ...) {
  value_deprecated("pre")
  fit <- convert_lm(object, data, ...)
  summary(fit)$r.squared[[1]]
}

#' @rdname estimate_extraction
#' @keywords internal
#' @export
sse <- function(object, data = NULL, ...) {
  value_deprecated("sse")
  fit <- convert_lm(object, data, ...)
  sum(fit$residuals^2)
}

#' @rdname estimate_extraction
#' @keywords internal
#' @export
ssm <- function(object, data = NULL, ...) {
  value_deprecated("ssm")
  fit <- convert_lm(object, data, ...)
  sum((fit$fitted.values - mean(fit$model[[1]]))^2)
}

#' @rdname estimate_extraction
#' @keywords internal
#' @export
ssr <- function(object, data = NULL, ...) {
  value_deprecated("ssr")
  ssm(object, data, ...)
}


convert_lm <- function(object, data, ...) {
  return(if ("lm" %in% class(object)) object else lm(object, data, ...))
}


# nolint start

#' @rdname estimate_extraction
#' @keywords internal
#' @export
fVal <- function(object, data = NULL, ...) {
  value_deprecated("fVal")
  withr::local_options(lifecycle_verbosity = "quiet")
  f(object, data, ...)
}

#' @rdname estimate_extraction
#' @keywords internal
#' @export
PRE <- function(object, data = NULL, ...) {
  value_deprecated("PRE")
  withr::local_options(lifecycle_verbosity = "quiet")
  pre(object, data, ...)
}

#' @rdname estimate_extraction
#' @keywords internal
#' @export
SSE <- function(object, data = NULL, ...) {
  value_deprecated("SSE")
  withr::local_options(lifecycle_verbosity = "quiet")
  sse(object, data, ...)
}

#' @rdname estimate_extraction
#' @keywords internal
#' @export
SSM <- function(object, data = NULL, ...) {
  value_deprecated("SSM")
  withr::local_options(lifecycle_verbosity = "quiet")
  ssm(object, data, ...)
}

#' @rdname estimate_extraction
#' @keywords internal
#' @export
SSR <- function(object, data = NULL, ...) {
  value_deprecated("SSR")
  withr::local_options(lifecycle_verbosity = "quiet")
  ssm(object, data, ...)
}

# nolint end

value_deprecated <- function(name) {
  if (!rlang::is_attached('package:coursekata')) {
    lifecycle::deprecate_warn(
      "2.5.0", paste0(name, "()"), paste0("coursekata::", name, "()"),
      details = paste(
        "This function is moving to the coursekata package soon and will no longer be available",
        "in the supernova package. Download the coursekata package at",
        "https://github.com/UCLATALL/coursekata-r"
      )
    )
  }
}
