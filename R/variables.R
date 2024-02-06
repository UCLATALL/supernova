#' Extract the variables from a model formula
#'
#' @param object A [`formula`], [`lm`] or [`supernova`] object
#'
#' @importFrom stats formula terms
#'
#' @return A list containing the `outcome` and `predictor` variables in the model.
#'
#' @rdname variables
#' @export
variables <- function(object) {
  UseMethod("variables", object)
}


#' @rdname variables
#' @export
variables.supernova <- function(object) {
  variables(object[["fit"]])
}


#' @rdname variables
#' @export
variables.formula <- function(object) {
  outcome <- frm_outcome(object)
  predictor <- frm_terms(object)
  list(
    outcome = outcome,
    predictor = predictor,
    group = character(0),
    within = character(0),
    between = character(0)
  )
}


#' @rdname variables
#' @export
variables.lm <- function(object) {
  var_list <- variables.formula(formula(object))
  var_list[["between"]] <- var_list[["predictor"]]
  var_list
}


#' @rdname variables
#' @export
variables.lmerMod <- function(object) {
  frm <- formula(object)
  var_list <- variables.formula(frm) # initialize output list

  # terms = things specified in the model like (1 | group) and mpg:hp
  fixed_terms <- frm_fixed_terms(frm)
  fixed_vars <- frm_fixed_vars(frm)
  random_terms <- frm_random_terms(frm)

  # currently we only support a single grouping variable, logically it must be the random variable
  # with the shortest name --- all other valid random variables will be nested in some way and have
  # (1 | a:b) syntax, where b is the group variable
  group_var <- random_terms[which.min(nchar(random_terms))]
  group_var <- stringr::str_remove(group_var, "^1 [|] ")

  # need to check the data and number of groups to determine within and between below
  data <- object@frame
  nrow_group <- vctrs::vec_unique_count(data[group_var])

  # between vars have different values within the same group
  # the number of unique <between>-<group> pairs should be equal to all <between>-<group> pairs
  between_vars <- fixed_vars[purrr::map_lgl(fixed_vars, function(possible_between) {
    vctrs::vec_unique_count(data[c(group_var, possible_between)]) == nrow_group
  })]

  # within vars have the same values within the same group
  # the number of unique <within>-<group> pairs should be greater than all <within>-<group> pairs
  within_vars <- fixed_vars[purrr::map_lgl(fixed_vars, function(possible_within) {
    vctrs::vec_unique_count(data[c(group_var, possible_within)]) > nrow_group
  })]

  # any interactive term with a within var is within, otherwise it's between
  interaction_terms <- frm_interaction_terms(frm)
  is_within <- purrr::map_lgl(interaction_terms, function(possible_within) {
    vars_in_interaction <- stringr::str_split(possible_within, stringr::fixed(":"))[[1]]
    any(vars_in_interaction %in% within_vars)
  })

  var_list[["predictor"]] <- fixed_terms
  var_list[["group"]] <- group_var
  var_list[["within"]] <- c(within_vars, interaction_terms[is_within])
  var_list[["between"]] <- c(between_vars, interaction_terms[!is_within])
  var_list
}
