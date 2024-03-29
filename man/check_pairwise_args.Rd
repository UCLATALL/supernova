% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairwise.R
\name{check_pairwise_args}
\alias{check_pairwise_args}
\alias{check_aov_compat}
\alias{check_not_empty}
\title{Check that the arguments are compatible with the rest of the pairwise code.}
\usage{
check_pairwise_args(fit, alpha)

check_aov_compat(fit)

check_not_empty(fit)
}
\arguments{
\item{fit}{A model fit by \code{\link[=lm]{lm()}} or \code{\link[=aov]{aov()}} (or similar).}

\item{alpha}{A single double value indicating the alpha to use for the tests.}
}
\description{
Check that the arguments are compatible with the rest of the pairwise code.
}
\section{Functions}{
\itemize{
\item \code{check_aov_compat()}: Ensure the model can be converted by \code{\link[=aov]{aov()}}

\item \code{check_not_empty()}: Check that the model is not the empty model

}}
\keyword{internal}
