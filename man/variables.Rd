% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variables.R
\name{variables}
\alias{variables}
\alias{variables.supernova}
\alias{variables.formula}
\alias{variables.lm}
\alias{variables.lmerMod}
\title{Extract the variables from a model formula}
\usage{
variables(object)

\method{variables}{supernova}(object)

\method{variables}{formula}(object)

\method{variables}{lm}(object)

\method{variables}{lmerMod}(object)
}
\arguments{
\item{object}{A \code{\link{formula}}, \code{\link{lm}} or \code{\link{supernova}} object}
}
\value{
A list containing the \code{outcome} and \code{predictor} variables in the model.
}
\description{
Extract the variables from a model formula
}
