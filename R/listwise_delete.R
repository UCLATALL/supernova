#' Remove cases with missing values.
#'
#' @param obj The [`data.frame`] or [`lm`] object to process.
#' @param vars The variables to consider.
#'
#' @return For `data.frame`s, the `vars` are checked for missing values. If one is found on any of
#'   the variables, the entire row is removed (list-wise deletion). For linear models, the model is
#'   refit after the underlying data have been processed.
#' @export
listwise_delete <- function(obj, vars) {
  UseMethod("listwise_delete", obj)
}


#' @rdname listwise_delete
#' @export
listwise_delete.data.frame <- function(obj, vars = names(obj)) {
  missing_rows_by_column <- purrr::map(obj[, vars], ~ which(is.na(.x)))
  na_rows <- unlist(missing_rows_by_column, use.names = FALSE)
  n_missing <- length(unique(na_rows))

  if (n_missing == 0) {
    return(obj)
  }

  rlang::inform(
    message = cli::pluralize("{n_missing} case{?s} removed due to missing value(s)."),
    class = "supernova_missing_values_message"
  )

  vctrs::vec_slice(obj, -na_rows)
}


#' @rdname listwise_delete
#' @export
listwise_delete.lm <- function(obj, vars = all.vars(formula(obj))) {
  n_missing <- vctrs::vec_size(obj$na.action)
  if (n_missing == 0) {
    return(obj)
  }

  new_call <- build_equivalent_call(obj, vars)

  rlang::inform(
    message = c(
      cli::pluralize("Refitting to remove {n_missing} case{?s} with missing value(s)"),
      i = rlang::expr_text(new_call)
    ),
    class = "supernova_missing_values_message"
  )

  rlang::try_fetch(
    eval(new_call, envir = environment(as.formula(obj))),
    supernova_missing_values_message = function(cnd) {
      rlang::cnd_muffle(cnd)
    }
  )
}


build_equivalent_call <- function(linear_model, vars) {
  call_args <- rlang::call_args(linear_model$call)
  if (!is.null(call_args$data)) {
    rlang::call_modify(linear_model$call, data = call("listwise_delete", call_args$data, vars))
  } else {
    rlang::call_modify(linear_model$call, data = listwise_delete(data.frame(vars)))
  }
}
