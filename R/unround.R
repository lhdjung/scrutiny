
# Helper function used in the main function `unround()` via a vectorized version
# right below:
rounding_bounds_scalar <- function(rounding, x_num, d_var, d) {

  # Manage the two rounding procedures that depend on the sign of the input
  # number, rounding with truncation and "anti-truncation":
  if (any(rounding == c("trunc", "anti_trunc"))) {
    rounding_orig <- rounding
    if (x_num > 0) {
      rounding <- "trunc_x_greater"
    } else if (x_num < 0) {
      rounding <- "trunc_x_less"
    } else {
      rounding <- "trunc_x_is_0"
    }

    if (any(rounding_orig == "anti_trunc")) {
      rounding <- paste0("anti_", rounding)
    }

    return(switch(rounding,    #     (1)              (2)               (3)    (4)
        "trunc_x_greater"      = list(x_num,           x_num + (2 * d), "<=",  "<"),
        "trunc_x_less"         = list(x_num - (2 * d), x_num,           "<",  "<="),
        "trunc_x_is_0"         = list(x_num - (2 * d), x_num + (2 * d), "<",   "<"),
        "anti_trunc_x_greater" = list(x_num - (2 * d), x_num,           "<=",  "<"),
        "anti_trunc_x_less"    = list(x_num,           x_num + (2 * d), "<=",  "<"),
        "anti_trunc_x_is_0"    = list(NA,        NA,        NA,   NA)
    ))
  }

  # This switch-statement is evaluated for all other rounding procedures:
  switch(rounding,   #     (1)              (2)               (3)   (4)
        "up_or_down" = list(x_num - d_var,   x_num + d_var,   "<=", "<="),
        "up"         = list(x_num - d_var,   x_num + d_var,   "<=",  "<"),
        "down"       = list(x_num - d_var,   x_num + d_var,   "<",  "<="),
        "even"       = list(x_num - d,       x_num + d,       "<",   "<"),
        "ceiling"    = list(x_num - (2 * d), x_num,           "<",  "<="),
        "floor"      = list(x_num,           x_num + (2 * d), "<=",  "<"),
                       list("error_trigger")
  )
}


# The above function is "scalar" (i.e., single-case only), but `unround()` is
# vectorized: It takes arguments of length > 1. Therefore, `rounding_bounds()`
# is created as a vectorized version of `rounding_bounds_scalar()`:
rounding_bounds <- Vectorize(rounding_bounds_scalar)





#' Reconstruct rounding bounds
#'
#' @description `unround()` takes a rounded number and returns the range of the
#'   original value: lower and upper bounds for the hypothetical earlier number
#'   that was later rounded to the input number. It also displays a range with
#'   inequation signs, showing whether the bounds are inclusive or not.
#'
#'   By default, the presumed rounding method is rounding up (or down) from 5.
#'   See the `Rounding` section for other methods.

#' @details The function is vectorized over `x` and `rounding`. This can be
#'   useful to unround multiple numbers at once, or to check how a single number
#'   is unrounded with different assumed rounding methods.
#'
#'   If both vectors have a length greater than 1, it must be the same
#'   length. However, this will pair numbers with rounding methods, which can be
#'   confusing. It is recommended that at least one of these input vectors has
#'   length 1.
#'
#'   Why does `x` need to be a string if `digits` is not specified? In that
#'   case, `unround()` must count decimal places by itself. If `x` then was
#'   numeric, it wouldn't have any trailing zeros because these get dropped from
#'   numerics.
#'
#'   Trailing zeros are as important for reconstructing boundary values as any
#'   other trailing digits would be. Strings don't drop trailing zeros, so they
#'   are used instead.

#' @section Rounding: Depending on how `x` was rounded, the boundary values can
#'   be inclusive or exclusive. The `incl_lower` and `incl_upper` columns in the
#'   resulting tibble are `TRUE` in the first case and `FALSE` in the second.
#'   The `range` column reflects this with equation and inequation signs.
#'
#'   However, these ranges are based on assumptions about the way `x` was
#'   rounded. Set `rounding` to the rounding method that hypothetically lead to
#'   `x`:
#'
#'   | \strong{Value of `rounding`}           | \strong{Corresponding range} |
#'   | ---                                    | ---                          |
#'   | `"up_or_down"` (default)               | `lower <= x <= upper`        |
#'   | `"up"`                                 | `lower <= x < upper`         |
#'   | `"down"`                               | `lower < x <= upper`         |
#'   | `"even"`                               | (no fix range)               |
#'   | `"ceiling"`                            | `lower < x = upper`          |
#'   | `"floor"`                              | `lower = x < upper`          |
#'   | `"trunc"` (positive `x`)               | `lower = x < upper`          |
#'   | `"trunc"` (negative `x`)               | `lower < x = upper`          |
#'   | `"trunc"` (zero `x`)                   | `lower < x < upper`          |
#'   | `"anti_trunc"` (positive `x`)          | `lower < x = upper`          |
#'   | `"anti_trunc"` (negative `x`)          | `lower = x < upper`          |
#'   | `"anti_trunc"` (zero `x`)              | (undefined; `NA`)            |
#'
#' Base R's own `round()` (R version >= 4.0.0), referenced by `rounding =
#' "even"`, is reconstructed in the same way as `"up_or_down"`, but whether the
#' boundary values are inclusive or not is hard to predict. Therefore,
#' `unround()` checks if they are, and informs you about it.

#' @param x String or numeric. Rounded number. `x` must be a string unless
#'   `digits` is specified (most likely by a function that uses `unround()` as a
#'   helper).
#' @param rounding String. Rounding method presumably used to create `x`.
#'   Default is `"up_or_down"`. For more, see section `Rounding`.
#' @param threshold Integer. Number from which to round up or down. Other
#'   rounding methods are not affected. Default is `5`.
#' @param digits Integer. This argument is meant to make `unround()` more
#'   efficient to use as a helper function so that it doesn't need to
#'   redundantly count decimal places. Don't specify it otherwise. Default is
#'   `NULL`, in which case decimal places really are counted internally and `x`
#'   must be a string.
#'
#' @return A tibble with seven columns: `range`, `rounding`, `lower`,
#'   `incl_lower`, `x`, `incl_upper`, and `upper`. The `range` column is a handy
#'   representation of the information stored in the columns from `lower` to
#'   `upper`, in the same order.
#'
#' @seealso For more about rounding `"up"`, `"down"`, or to `"even"`, see
#'   documentation for `round_up()`.
#'
#'   For more about the less likely `rounding` methods, `"ceiling"`, `"floor"`,
#'   `"trunc"`, and `"anti_trunc"`, see documentation for `round_ceiling()`.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # By default, the function assumes that `x`
#' # was either rounded up or down:
#' unround(x = "2.7")
#'
#' # If `x` was rounded up, run this:
#' unround(x = "2.7", rounding = "up")
#'
#' # Likewise with rounding down...
#' unround(x = "2.7", rounding = "down")
#'
#' # ...and with `base::round()` which, broadly
#' # speaking, rounds to the nearest even number:
#' unround(x = "2.7", rounding = "even")
#'
#' # Multiple input number-strings return
#' # multiple rows in the output data frame:
#' unround(x = c(3.6, "5.20", 5.174))



# # Example values:
# x <- "2.37"
# rounding <- "up_or_down"
# threshold <- 5
# digits <- NULL



unround <- function(x, rounding = "up_or_down", threshold = 5, digits = NULL) {

  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(x, rounding))

  # The number of decimal places might be given from within another function via
  # the `digits` argument. Otherwise -- if `digits` is not specified, and
  # therefore `NULL` -- the `x` argument must be a string so that decimal
  # places can be counted accurately (cf. trailing zeros), which is then done:
  if (is.null(digits)) {
    if (!is.character(x)) {
      cli::cli_abort(c(
        "`x` is {an_a_type(x)}.",
        "x" = "If `digits` is not specified, `x` must be a string."
      ))
    }
    digits <- decimal_places(x)
  }

  # Determine the difference between the rounded number and the boundary values.
  # That difference is variable when rounding up or down, because in that case,
  # it depends on the value of `threshold`:
  p10 <- 10 ^ (digits + 1L)
  d <- 5 / p10
  d_var <- threshold / p10

  # The bound helper function operates on the numeric value of `x`:
  x_num <- as.numeric(x)

  # Calculate the boundary values and determine out whether they are inclusive
  # or not, going by the `rounding` argument. In order to vectorize `rounding`,
  # the helper function at the top of the present file is called:
  bounds <- rounding_bounds(
    rounding = rounding, x_num = x_num, d_var = d_var, d = d
  )

  # Throw error if `rounding` was not specified in a valid way:
  if (list("error_trigger") %in% bounds) {
    cli::cli_abort(c(
      "`rounding` must be one or more of the designated \\
      string values. See documentation for `unround()`, \\
      section `Rounding`.",
      "x" = "It is {wrong_spec_string(rounding)}."
    ))
  }

  # Split the `bounds` list up into its four component vectors:
  lower      <-   as.numeric(bounds[1, ]) # lower bound
  upper      <-   as.numeric(bounds[2, ]) # upper bound
  sign_lower <- as.character(bounds[3, ]) # lower bound inclusive (`"<="`)?
  sign_upper <- as.character(bounds[4, ]) # upper bound inclusive (`"<="`)?

  # Translate the inequation signs into Boolean values for the resulting tibble:
  incl_lower <- dplyr::if_else(sign_lower == "<=", TRUE, FALSE)
  incl_upper <- dplyr::if_else(sign_upper == "<=", TRUE, FALSE)

  # Prepare a string that displays the range with its appropriate signs:
  range <- as.character(glue::glue(
    "{lower} {sign_lower} x({x}) {sign_upper} {upper}"
  ))

  # Finally, return the resulting tibble:
  tibble::tibble(range, rounding, lower, incl_lower, x, incl_upper, upper)
}



