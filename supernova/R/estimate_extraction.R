#' PRE
#'
#' A function to extract PRE values
#'
#' @param fit A \code{\link{lm}} object.
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}}.
#' @param ... Passthrough arguments to \code{\link{lm}}.
#'
#' @return The PRE value of the relevant model.
#'
#' @importFrom stats lm
#'
#' @rdname PRE
#'
#' @export
PRE <- function(fit, ...) {
  UseMethod("PRE")
}

#' @rdname PRE
#' @export
PRE.default <- function(fit, ...) {
  summary(fit)$r.squared[[1]]
}

#' @rdname PRE
#' @export
PRE.formula <- function(formula, data = list(), ...) {
  PRE.default(lm(formula, data, ...))
}

#' fVal
#'
#' A function to extract F value
#'
#' @param fit A \code{\link{lm}} object.
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}}.
#' @param ... Passthrough arguments to \code{\link{lm}}.
#'
#' @return The F value of the relevant model.
#'
#' @importFrom stats lm
#'
#' @rdname fVal
#'
#' @export
fVal <- function(fit, ...) {
  UseMethod("fVal")
}

#' @rdname fVal
#' @export
fVal.default <- function(fit, ...) {
  summary(fit)$fstatistic[[1]]
}

#' @rdname fVal
#' @export
fVal.formula <- function(formula, data = list(), ...) {
  fVal.default(lm(formula, data, ...))
}

#' b0
#'
#' A function to extract intercept/beta0 value.
#'
#' @param fit A \code{\link{lm}} object.
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}}.
#' @param ... Passthrough arguments to \code{\link{lm}}.
#'
#' @return The intercept of the relevant model.
#'
#' @importFrom stats lm
#'
#' @rdname b0
#'
#' @export
b0 <- function(fit, ...) {
  UseMethod("b0")
}

#' @rdname b0
#' @export
b0.default <- function(fit, ...) {
  fit$coefficients[[1]]
}

#' @rdname b0
#' @export
b0.formula <- function(formula, data = list(), ...) {
  b0.default(lm(formula, data, ...))
}

#' b1
#'
#' A function to extract slope/beta1 value.
#'
#' @param fit A \code{\link{lm}} object.
#' @param formula A \code{\link{formula}}.
#' @param data A \code{\link{data.frame}}.
#' @param ... Passthrough arguments to \code{\link{lm}}.
#'
#' @return The slope of the relevant model.
#'
#' @importFrom stats lm
#'
#' @rdname b1
#'
#' @export
b1 <- function(fit, ...) {
  UseMethod("b1")
}

#' @rdname b1
#' @export
b1.default <- function(fit, ...) {
  fit$coefficients[[2]]
}

#' @rdname b1
#' @export
b1.formula <- function(formula, data = list(), ...) {
  b1.default(lm(formula, data, ...))
}
