# Robust version of \code{\link[stats]{update}}
#
# This function solves two problems with \code{\link[stats]{update}}: pipes
# will break when the data is first piped to the \code{\link[stats]{lm}}
# object, and data is re-evaluated against the full dataset, even if the model
# being updated had \code{na.action = "na.omit"}. See details below for more
# information.
#
# \strong{Problem with pipes:} \code{\link[stats]{update}} does not support
# linear models where the data was piped via \code{magrittr::\%>\%} to
# the model function as in \code{mtcars \%>\% lm(mpg ~ hp, data = .) \%>\%
# stats::update()}. This is because the \code{\link[stats]{update}} function
# relies on \code{\link{getCall}}, which returns a call that has
# \code{data = .} argument, which is completely uninformative. This function
# creates a new model by extracting the formula from the old model via
# \code{\link[stats]{formula}} and the data used in the old model via the
# \code{model$call} and \code{environment}. Then the new (old) model is updated
# as it now has all the necessary components.
#
# \strong{Problem with \code{na.action}}: When updating models with missing
# data, the new model is evaluated against the full dataset even if the
# original model had missing data for some of the values. For example, if there
# are missing values on X for a model where Y ~ X, when updating to fit the
# null model Y ~ NULL, the full set of Y is used instead of only the values of
# Y where there are values of X. This is not inherently a problem, however it
# yields incorrect degrees of freedom and sum of squares for the total row in
# an ANOVA because different data sets are being used for the total row and the
# model and error rows. The present solution creates \code{NA} values list-wise
# for all variables in the old model so that the new model does not use those
# data.
#
# @param old An existing fit from a model function such as \code{\link{lm}},
#   \code{\link{glm}} and many others.
# @param new Changes to the formula; see \code{\link{update.formula}} for
#   details.
# @param ... Additional arguments to the call (see
#   \code{\link[stats]{update}}), or arguments with changed values. Use
#   \code{name = NULL} to remove the argument name.
#
# @return If evaluate = TRUE the fitted object, otherwise the updated call.
update <- function(old, new, ...) {
  vars <- all.vars(formula(old))
  data <- eval(old$call$data, environment(formula(old)))

  # list-wise delete values from all affected variables
  na_rows <- sort(unlist(
    lapply(data[, vars], function(x) which(is.na(x))),
    use.names = FALSE
  ))
  data[na_rows, vars] <- NA

  # create warning message
  if (length(na_rows) > 0) {
    message(sprintf(
      "Note: %s %s removed due to missing value(s).",
      length(na_rows), ngettext(length(na_rows), "case", "cases")
    ))
  }

  stats::update(old, new, data = data, ...)
}
