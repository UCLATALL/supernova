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
PRE.default <- function(fit) {
  pre <- summary(fit)$r.squared[1]
  names(pre) <- NULL
  pre
}

#' @rdname PRE
#' @export
PRE.formula <- function(formula, data = list(), ...) {
  fit <- lm(formula, data, ...)
  pre <- PRE.default(fit)
  pre
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
fVal.default <- function(fit) {
  fval <- summary(fit)$fstatistic[1]
  names(fval) <- NULL
  fval
}

#' @rdname fVal
#' @export
fVal.formula <- function(formula, data = list(), ...) {
  fit <- lm(formula, data, ...)
  fval <- fVal.default(fit)
  fval
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
b0.default <- function(fit) {
  bz <- fit$coefficients[1]
  names(bz) <- NULL
  bz
}

#' @rdname b0
#' @export
b0.formula <- function(formula, data = list(), ...) {
  fit <- lm(formula, data, ...)
  bz <- b0.default(fit)
  bz
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
b1.default <- function(fit) {
  bo <- fit$coefficients[2]
  names(bo) <- NULL
  bo
}

#' @rdname b1
#' @export
b1.formula <- function(formula, data = list(), ...) {
  fit <- lm(formula, data, ...)
  bo <- b1.default(fit)
  bo
}
