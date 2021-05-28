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

  call_string <- build_equivalent_call_string(obj, vars)

  rlang::inform(
    message = c(
      cli::pluralize("Refitting to remove {n_missing} case{?s} with missing value(s)"),
      i = paste0(call_string, "\n")
    ),
    class = "supernova_missing_values_message"
  )

  str2lang <- getFromNamespace("str2lang", "backports")
  rlang::with_handlers(
    eval(str2lang(call_string), envir = environment(as.formula(obj))),
    supernova_missing_values_message = rlang::calling(function(cnd) {
      rlang::cnd_muffle(cnd)
    })
  )
}


build_equivalent_call_string <- function(linear_model, vars) {
  call_string <- deparse(linear_model$call)
  var_string <- paste0("c(", paste0("\"", vars, "\"", collapse = ", "), ")")

  if (stringr::str_detect(call_string, " data = .+?[ ,)]")) {
    new_call_string <- stringr::str_replace(
      call_string,
      " data = (.+?)([ ,)])",
      sprintf(" data = listwise_delete(\\1, %s)\\2", var_string)
    )
  } else {
    model <- formula(linear_model)
    variables <- c(frm_outcome(model), frm_vars(model))
    variables_string <- paste(variables, collapse = ", ")
    data_string <- paste0(" data = listwise_delete(data.frame(", variables_string, "))")
    new_call_string <- stringr::str_replace(call_string, "\\)$", paste0(data_string, ")"))
  }

  new_call_string
}
