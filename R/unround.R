

# Helper function used in the main function `unround()` via a vectorized version
# right below:
compute_rounding_bounds_scalar <- function(rounding, x_num, d_var, d) {
  dplyr::case_when(           # Component:       (1)              (2)               (3)   (4)
    rounding == "up_or_down"               ~ list(x_num - d_var,   x_num + d_var,   "<=", "<="),
    rounding == "up"                       ~ list(x_num - d_var,   x_num + d_var,   "<=", "<" ),
    rounding == "down"                     ~ list(x_num - d_var,   x_num + d_var,   "<", "<=" ),
    rounding == "even"                     ~ list(x_num - d,       x_num + d,       "<",  "<" ),
    rounding == "ceiling"                  ~ list(x_num - (2 * d), x_num,           "<", "<=" ),
    rounding == "floor"                    ~ list(x_num,           x_num + (2 * d), "<=", "<" ),
    rounding == "trunc"      & x_num > 0   ~ list(x_num,           x_num + (2 * d), "<=", "<" ),
    rounding == "trunc"      & x_num < 0   ~ list(x_num - (2 * d), x_num,           "<", "<=" ),
    rounding == "anti_trunc" & x_num > 0   ~ list(x_num - (2 * d), x_num,           "<", "<=" ),
    rounding == "anti_trunc" & x_num < 0   ~ list(x_num,           x_num + (2 * d), "<=", "<" ),
    rounding == "trunc"      & x_num == 0  ~ list(x_num - (2 * d), x_num + (2 * d), "<",  "<" ),
    # rounding == "anti_trunc" & x_num == 0  ~ list("error_trigger_anti_trunc_zero"),
    rounding == "anti_trunc" & x_num == 0  ~ list(NA,              NA,               NA,   NA),
    TRUE                                   ~ list("error_trigger")
  )
}

# The vectorized version:
compute_rounding_bounds <- Vectorize(compute_rounding_bounds_scalar)



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
#'   If both vectors have a length greater than 1, it needs to be the same
#'   length. However, this will pair numbers with rounding methods, which can be
#'   confusing. It is recommended that at least one of these input vectors has
#'   length 1.
#'
#'   Why does `x` need to be a string if `digits` is not specified? In that
#'   case, `unround()` needs to count decimal places by itself. If `x` then was
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

#' @param x String or numeric. Rounded number. `x` needs to be a string unless
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
#'   needs to be a string.
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
#' @include utils.R subset-superset.R
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


# Title used to be: Reconstruct lower and upper bounds for original value of
# rounded number



unround <- function(x, rounding = "up_or_down", threshold = 5, digits = NULL) {

  # (From the utils.R file:)
  check_lengths_congruent(list(x, rounding, digits))

  # The number of decimal places might be given from within another function via
  # the `digits` argument. Otherwise -- if `digits` is not specified, and
  # therefore `NULL` -- the `x` argument needs to be a string so that decimal
  # places can be counted accurately (cf. trailing zeros), which is then done:
  if (is.null(digits)) {
    if (!is.character(x)) {
      cli::cli_abort(c(
        "`x` is {an_a_type(x)}",
        "i" = "If `digits` is not specified, `x` needs to be a string."
      ))
    }
    digits <- decimal_places(x)
  }

  # Determine the difference between the rounded number and the boundary values.
  # That difference is variable when rounding up or down, because in that case,
  # it depends on the value of `threshold`:
  p10 <- 10 ^ (digits + 1)
  d <- 5 / p10
  d_var <- threshold / p10

  # The bound helper function operates on the numeric value of `x`:
  x_num <- as.numeric(x)

  # Compute the boundary values and figure out whether they are inclusive or
  # not, going by the `rounding` argument. In order to vectorize `rounding`, the
  # helper function at the top of the present file is called:
  bounds <- compute_rounding_bounds(rounding, x_num, d_var, d)

  # Throw error if `rounding` was not specified in a valid way:
  if (list("error_trigger") %in% bounds) {
    cli::cli_abort(c(
      "`rounding` misspecified",
      "x" = "`rounding` was given as {wrong_spec_string(rounding)}.",
      ">" = "Please use one or more of the designated string values instead. \\
      See documentation for `unround()`, section `Rounding`."
    ))
  }

  # # Throw error if a mathematically impossible corner case is present:
  # if ("error_trigger_anti_trunc_zero" %in% bounds) {
  #   cli::cli_abort(c(
  #     "Anti-truncated numbers can't be zero",
  #     "x" = "You specified \"anti_trunc\" for zero, which is mathematically \\
  #     impossible: Rounding with the anti-truncating method always turns away \\
  #     from zero."
  #   ))
  # }

  # Split the `bounds` list up into its four component vectors:
  lower      <-   as.numeric(bounds[1, ])  # lower bound
  upper      <-   as.numeric(bounds[2, ])  # upper bound
  sign_lower <- as.character(bounds[3, ])  # lower bound inclusive or not
  sign_upper <- as.character(bounds[4, ])  # upper bound inclusive or not

  # Translate the inequation signs into Boolean values for the resulting tibble:
  incl_lower <- dplyr::if_else(sign_lower == "<=", TRUE, FALSE)
  incl_upper <- dplyr::if_else(sign_upper == "<=", TRUE, FALSE)

  # If `rounding` is either of `"ceiling"`, `"floor"`, `"trunc"`, and
  # `"anti_trunc"`, one bound is always equal to the rounded value. In that
  # case, change the respective inequation sign to an equation sign:
  sign_lower <- dplyr::if_else(x_num == lower, "==", sign_lower)
  sign_upper <- dplyr::if_else(x_num == upper, "==", sign_upper)

  # Prepare a string that displays the range with its appropriate signs:
  range <- as.character(glue::glue(
    "{lower} {sign_lower} x({x}) {sign_upper} {upper}"
  ))

  # Finally, return the resulting tibble:
  tibble::tibble(range, rounding, lower, incl_lower, x, incl_upper, upper)
}



