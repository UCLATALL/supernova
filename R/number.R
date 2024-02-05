#' @import vctrs
NULL


#' @keywords internal
new_number <- function(x = numeric(),
                       digits = 3L,
                       scientific = FALSE,
                       leading_zero = TRUE) {
  vctrs::vec_assert(x, numeric())
  vctrs::vec_assert(digits, integer(), 1)
  vctrs::vec_assert(scientific, logical(), 1)
  vctrs::vec_assert(leading_zero, logical(), 1)

  vctrs::new_vctr(
    x,
    class = "supernova_number",
    digits = digits,
    scientific = scientific,
    leading_zero = leading_zero
  )
}


methods::setOldClass(c("supernova_number", "vctrs_vctr"))


#' `number` vector
#'
#' This creates a formatted double vector. You can specify the number of digits you want the value
#' to display after the decimal, and the underlying value will not change. Additionally you can
#' explicitly set whether scientific notation should be used and if numbers less than 0 should
#' contain a leading 0.
#'
#' @param x
#'   - For `number()`: A numeric vector
#'   - For `is_number()`: An object to test
#'   - For `as_number()`: An object to coerce to a `number`
#' @param digits The number of digits to display after the decimal point.
#' @param scientific Whether the number should be represented with scientific notation (e.g. 1e2)
#' @param leading_zero Whether a leading zero should be used on numbers less than 0 (e.g. .001)
#'
#' @return An S3 vector of class `supernova_number`. It should behave like a double, but be
#'   formatted consistently.
#'
#' @export
#'
#' @examples
#' number(1:5, digits = 3)
number <- function(x = numeric(), digits = 3L, scientific = FALSE, leading_zero = TRUE) {
  x <- vctrs::vec_cast(x, numeric())
  digits <- vctrs::vec_recycle(vctrs::vec_cast(digits, integer()), 1L)
  new_number(x, digits, scientific, leading_zero)
}

#' @export
#' @rdname number
is_number <- function(x) {
  inherits(x, "supernova_number")
}

#' @export
#' @rdname number
as_number <- function(x) vctrs::vec_cast(x, new_number())


# Formatting ----------------------------------------------------------------------------------

#' @export
format.supernova_number <- function(x, ...) {
  formatter <- function(values) {
    formatted <- values |>
      round(attr(x, "digits")) |>
      format(nsmall = attr(x, "digits"), scientific = attr(x, "scientific"))

    if (!attr(x, "leading_zero")) {
      formatted <- stringr::str_replace(formatted, "^0[.]", ".")
    }

    formatted
  }

  out <- vctrs::vec_data(x)
  out[!is.na(out)] <- formatter(out[!is.na(out)])
  out
}

#' @export
vec_ptype_abbr.supernova_number <- function(x, ...) "dbl"

#' @export
obj_print_header.supernova_number <- function(x, ...) NULL


# Casting and Coercion ------------------------------------------------------------------------

#' @export
vec_ptype2.supernova_number.supernova_number <- function(x, y, ...) y # nolint

#' @export
vec_cast.supernova_number.supernova_number <- function(x, to, ...) { # nolint
  new_number(
    vctrs::vec_data(x),
    digits = attr(to, "digits"),
    scientific = attr(to, "scientific"),
    leading_zero = attr(to, "leading_zero")
  )
}

#' @export
vec_ptype2.supernova_number.double <- function(x, y, ...) double()

#' @export
vec_cast.double.supernova_number <- function(x, to, ...) vctrs::vec_data(x)

#' @export
vec_ptype2.double.supernova_number <- function(x, y, ...) new_number()

#' @export
vec_cast.supernova_number.double <- function(x, to, ...) {
  number(
    x,
    digits = attr(to, "digits"),
    scientific = attr(to, "scientific"),
    leading_zero = attr(to, "leading_zero")
  )
}

#' @export
vec_ptype2.supernova_number.character <- function(x, y, ...) character()

#' @export
vec_cast.character.supernova_number <- function(x, to, ...) format(x)

#' @export
vec_ptype2.character.supernova_number <- function(x, y, ...) new_number()

#' @export
vec_cast.supernova_number.character <- function(x, to, ...) {
  number(
    as.double(x),
    digits = attr(to, "digits"),
    scientific = attr(to, "scientific"),
    leading_zero = attr(to, "leading_zero")
  )
}


# Math ----------------------------------------------------------------------------------------

#' @method vec_arith supernova_number
#' @export
vec_arith.supernova_number <- function(op, x, y, ...) {
  UseMethod("vec_arith.supernova_number", y)
}

#' @method vec_arith.supernova_number default
#' @export
vec_arith.supernova_number.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @method vec_arith.supernova_number supernova_number
#' @export
vec_arith.supernova_number.supernova_number <- function(op, x, y, ...) {
  computed <- vctrs::vec_arith_base(op, x, y)
  vctrs::vec_cast(computed, vctrs::vec_cast(x, y))
}

#' @method vec_arith.supernova_number MISSING
#' @export
vec_arith.supernova_number.MISSING <- function(op, x, y, ...) {
  switch(op,
    `-` = x * -1,
    `+` = x,
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.numeric supernova_number
#' @export
vec_arith.numeric.supernova_number <- function(op, x, y, ...) {
  computed <- vctrs::vec_arith_base(op, x, y)
  vctrs::vec_cast(computed, y)
}

#' @method vec_arith.supernova_number numeric
#' @export
vec_arith.supernova_number.numeric <- function(op, x, y, ...) {
  computed <- vctrs::vec_arith_base(op, x, y)
  vctrs::vec_cast(computed, x)
}
