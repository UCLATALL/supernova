#' supernova
#'
#' Creates a \code{supernova} object
#'
#' @param fit \code{\link{lm}} object
#'
#' @return An object of the class \code{\link{supernova}}, a named list with the munged data.frame and the original \code{\link{lm}} object.
#'
#' @details
#' An alternative set of summary statistics for ANOVA. Sums of squares, degrees of freedom, mean squares, and F value are all equivalent to \code{\link{anova.lm}}. This package adds proportional reduction in error, an explicit summary of the whole model, and separate formatting of p values and is intended to match the output used in Judd, McClelland, and Ryan (2017).
#'
#' @examples
#' supernova(lm(Thumb ~ Weight, data = Fingers))
#'
#'
#' @importFrom stats anova model.matrix update pf
#' @importFrom utils head tail
#'
#' @references Judd, C. M., McClelland, G. H., & Ryan, C. S. (2017). Data Analysis: A Model Comparison Approach to Regression, ANOVA, and Beyond (3rd edition). New York: Routledge. ISBN:879-1138819832
#'
#' @export
supernova <- function(fit) {
  if (all(colnames(model.matrix(fit)) == "(Intercept)")) {
    y <- frameSetup(anova(fit))
  } else {
    af <- anova(fit)
    nm <- anova(update(fit, . ~ NULL))
    if (length(af$Df) == 2) {
      y <- frameSetup(nm)
      y[1:2, c(3:6,8)] <- apply(as.data.frame(af[c(2,1,3:5)]), c(1,2), function(x) ifelse(is.na(x), "", x))
      y$PRE[1:2] <- c(as.numeric(y$SS[1]) / as.numeric(y$SS[3]), "")
    } else {
      y <- frameSetup(nm, x = length(af$Df) - 1)
      y$v2[2:(nrow(y) - 2)] <- head(rownames(af), -1)
      y[2:(nrow(y) - 1), c(3:5)] <- as.data.frame(af[c(2,1,3)])
      y[2:(nrow(y) - 1),"F"] <- c(head(af$'F value', -1), "")
      y[2:(nrow(y) - 1),"p"] <- c(head(af$'Pr(>F)', -1), "")
      y$SS[1] <- sum(as.numeric(y$SS[2:(nrow(y) - 2)]))
      y$df[1] <- sum(as.numeric(y$df[2:(nrow(y) - 2)]))
      y$MS[1] <- as.numeric(y$SS[1]) / as.numeric(y$df[1])
      fstat <- summary(fit)$fstatistic
      y$F[1] <- fstat[1]
      y$PRE[1:(nrow(y) - 2)] <- as.numeric(head(y$SS, -2)) / as.numeric(tail(y$SS, 1))
      y$p[1] <- pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    }
  }
  rl <- list(f = y, fit = fit)
  class(rl) <- "supernova"
  return(rl)
}

#' superanova
#'
#' Alias for supernova
#'
#' @param x A \code{\link{lm}} object.
#'
#' @return A \code{\link{supernova}} object.
#'
#' @examples
#' superanova(lm(Thumb ~ Weight, data = Fingers))
#'
#' @export
superanova <- function(x) {
  supernova(x)
}

#' frameSetup
#'
#' Sets up a null supernova data.frame
#'
#' @param anm \code{\link{anova}} object with no predictors.
#' @param x Number of predictiors to be included in the model.
#'
#' @return y An empty data.frame for use by \code{\link{supernova}}.
frameSetup <- function(anm, x = 0) {
  f <- "---"
  y <- data.frame(v1 = c("Model", rep("", x), "Error", "Total"),
                  v2 = c("(error reduced)", rep("", x), "(from model)", "(empty model)"),
                  SS = c(f, rep("", x), f, anm$'Sum Sq'),
                  df = c(f, rep("", x), f, anm$Df),
                  MS = c(f, rep("", x), f, anm$'Mean Sq'),
                  F = c(f, rep("", x), f, ""),
                  PRE = c(f, rep("", x), ifelse(x > 1, "", f), ""),
                  p = c(f, rep("", x), ifelse(x > 1, "", f), ""),
                  stringsAsFactors = FALSE)
  y
}

#' printHelp
#'
#' A function to help with formatting in \code{\link{print.supernova}}
#'
#' @param x An atomic character.
#' @param digits From \code{\link{options}} by default.
#' @param dig.tst As in printCoefMat.
#' @param cs.ind As in printCoefMat.
#' @param tst.ind As in printCoefMat.
#' @param na.print Alternate string to print for NA values.
#' @param ... Additional arguments.
#'
#' @return A character with correct numeric formatting.
printHelp <- function (x, digits = max(3L, getOption("digits") - 2L), dig.tst = max(1L, min(5L, digits - 1L)), cs.ind = 1:k, tst.ind = k + 1, na.print = "", ...) {
  x <- suppressWarnings(as.matrix(ifelse(x %in% c("", "---"), NA, as.numeric(x))))
  if (is.null(d <- dim(x)) || length(d) != 2L)
    stop("'x' must be coefficient matrix/data frame")
  nc <- d[2L]
  xm <- data.matrix(x)
  k <- nc - (if (missing(tst.ind))
    1
    else length(tst.ind))
  Cf <- array("", dim = d, dimnames = dimnames(xm))
  ok <- !(ina <- is.na(xm))
  if (length(cs.ind)) {
    acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
    if (any(ia <- is.finite(acs))) {
      digmin <- 1 + if (length(acs <- acs[ia & acs != 0]))
        floor(log10(range(acs[acs != 0], finite = TRUE)))
      else 0
      Cf[, cs.ind] <- format(round(coef.se, max(1L, digits -
                                                  digmin)), digits = digits)
    }
  }
  if (length(tst.ind))
    Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst),
                            digits = digits)
  if (any(r.ind <- !((1L:nc) %in% c(cs.ind, tst.ind))))
    for (i in which(r.ind)) Cf[, i] <- format(xm[, i], digits = digits)
  ok[, tst.ind] <- FALSE
  okP <- ok
  x1 <- Cf[okP]
  dec <- getOption("OutDec")
  x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
  if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
    Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1L, digits - 1L))
  }
  if (any(ina))
    Cf[ina] <- na.print
  as.vector(Cf)
}

#' barHelp
#'
#' A function to produce vertical bar separators.
#'
#' @param x Original name for row.
#' @param y Number of spaces to add.
#'
#' @return The original row name with a number of spaces and vertical bar added.
barHelp <- function(x, y) {
  paste0(x, y, " |")
}

#' insertRow
#'
#' Function to insert formatting rows in the output data.frame.
#'
#' @param d Original data.frame
#' @param nr Contents of the new row.
#' @param rn The row in which to insert the new contents (remaining rows will be pushed down).
#'
#' @return d The original data.frame with the new row inserted.
insertRow <- function(d, nr, rn = NULL) {
  if (!is.null(rn)) {
    d[seq(rn + 1,nrow(d) + 1), ] <- d[seq(rn, nrow(d)), ]
    d[rn, ] <- nr
  }
  else if (is.null(rn)) {
    d <- rbind(d, nr)
  }
  return(d)
}

#' print.supernova
#'
#' A print method for the supernova class
#'
#' @param x A \code{\link{supernova}} object.
#' @param pcut The integer number of decimal places of p-values to show.
#' @param ... Additional display arguments.
#'
#' @return NULL
#'
#' @importFrom stats model.frame
#'
#' @export
print.supernova <- function(x, pcut = 4, ...) {
  # setup
  y <- x$f
  y[3:(ncol(y) - 1)] <- apply(y[3:(ncol(y) - 1)], 2, printHelp)
  y[x$f == "---"] <- "---"
  y$p <- ifelse(y$p %in% c("", "---"), y$p, ifelse(as.numeric(y$p) < as.numeric(paste0(".", strrep("0", pcut - 1), "1")), paste0(".", strrep(0, pcut)), substring(format(round(as.numeric(y$p), pcut), scientific = FALSE), 2)))
  nc <- abs(nchar(y$v2) - max(nchar(y$v2)))
  y$v2 <- mapply(barHelp, y$v2, strrep(" ", nc), USE.NAMES = FALSE)
  names(y)[1:2] <- c("", "")

  sep <- strrep("-", sapply(y, function(x) max(nchar(x))))
  y <- insertRow(y, sep, 1)
  y <- insertRow(y, sep, nrow(y))

  # printing
  cat("Analysis of Variance Table\nOutcome variable:", colnames(model.frame(x$fit))[1], "\nModel: ")
  print(x$fit$call)
  cat("\n")
  print(y, row.names = FALSE)
}
