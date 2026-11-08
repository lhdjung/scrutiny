# The two exported functions below share their work with `grim_scalar()`:
# `sum_range()` returns the least and the greatest whole-number sum total that
# would have been reported as `x`. Everything else is division and comparison.

grim_sums_scalar <- function(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning,
  rounding,
  threshold,
  symmetric
) {
  if (missing(digits_x)) {
    error_digits_missing(x)
  }
  if (missing(items_averaged)) {
    error_items_missing()
  }
  if (missing(data_type)) {
    error_data_type_missing()
  }
  is_integer <- check_data_type_whole(data_type)

  # The same type checks as in `grim_scalar()`: a string `n` is a mistake, not
  # an undecidable value set, which `is_decidable_n_items()` would make of it.
  is_percentage <- arg_match_in_export(x_meaning, X_MEANINGS) == "percentage"
  if (!is.numeric(n) && !is.na(n)) {
    check_type(n, c("double", "integer"))
  }
  if (!is.numeric(items_averaged) && !is.na(items_averaged)) {
    check_type(items_averaged, c("double", "integer"))
  }

  check_newly_numeric(
    x,
    digits_x,
    scale = rounding_scale(rounding, threshold, symmetric)
  )

  x_num <- x

  if (is_percentage) {
    x_num <- x_num / 100
    digits_x <- digits_x + 2L
  }

  n_items <- as.numeric(n) * items_averaged

  # Undecidable cases as in `grim_scalar()`. An infinite `x` would give an
  # infinitely wide range that `seq()` refuses to walk. Both callers check the
  # sums with `anyNA()`:
  sums <- if (
    !is.finite(x_num) ||
      is.na(digits_x) ||
      !is_integer ||
      !is_decidable_n_items(n, items_averaged)
  ) {
    c(NA_real_, NA_real_)
  } else {
    check_sum_range_exact(
      x_num,
      n_items,
      digits_x,
      scale = rounding_scale(rounding, threshold, symmetric)
    )
    roundwork::sum_range(
      x_num = x_num,
      n_items = n_items,
      digits = digits_x,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )
  }

  list(
    sums = sums,
    n = as.numeric(n),
    n_items = n_items,
    is_integer = is_integer,
    # Values are returned on the scale of `x`, so that they can be read against
    # it. Internally, a percentage is a decimal number:
    scale = if (is_percentage) 100 else 1
  )
}


grim_values_scalar <- function(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  parts <- grim_sums_scalar(
    x,
    n,
    digits_x,
    items_averaged,
    data_type,
    x_meaning,
    rounding,
    threshold,
    symmetric
  )
  sums <- parts$sums

  if (anyNA(sums)) {
    # Inconsistent, not undecidable, if `grim()` says so with only `digits_x`
    # missing; see `digits_least()`:
    is_incon <- is.na(digits_x) &&
      parts$is_integer &&
      isFALSE(grim_scalar(
        x = x,
        n = n,
        digits_x = digits_x,
        items_averaged = items_averaged,
        data_type = "integer",
        x_meaning = x_meaning,
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric
      ))
    return(grim_values_tibble(if (is_incon) numeric(0L) else NA_real_, parts))
  }

  sum1 <- sums[1L]
  sum2 <- sums[2L]

  # An empty range means no whole-number sum total would have been reported as
  # `x`; i.e., GRIM inconsistency:
  if (sum1 > sum2) {
    return(grim_values_tibble(numeric(0L), parts))
  }

  check_values_size(
    sum2 - sum1 + 1,
    "means",
    list(x = x, n = n, items_averaged = items_averaged)
  )

  grim_values_tibble(seq(sum1, sum2), parts)
}


# The means, on the scale of `x`, that whole-number sums give. One row of `NA`
# but for `n` for an undecidable value set, so that it still has a row in the
# output.
grim_values_tibble <- function(sums, parts) {
  tibble::tibble(
    x = sums * parts$scale / parts$n_items,
    n = parts$n,
    sum = sums
  )
}


grim_closest_scalar <- function(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  parts <- grim_sums_scalar(
    x,
    n,
    digits_x,
    items_averaged,
    data_type,
    x_meaning,
    rounding,
    threshold,
    symmetric
  )
  sums <- parts$sums

  if (anyNA(sums)) {
    # With only `digits_x` missing, the answer may not depend on it: an
    # inconsistent `x` has the same neighbors with the fewest decimal places it
    # can have, and one that is consistent with any is achievable itself; see
    # `digits_least()` and `digits_exact_mean()`.
    consistency <- if (is.na(digits_x) && parts$is_integer) {
      grim_scalar(
        x = x,
        n = n,
        digits_x = digits_x,
        items_averaged = items_averaged,
        data_type = "integer",
        x_meaning = x_meaning,
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric
      )
    }
    if (is.null(consistency) || is.na(consistency)) {
      return(grim_values_tibble(NA_real_, parts))
    }
    digits_x <- digits_least(x, digits_x)
    if (consistency) {
      digits_x <- digits_exact_mean(digits_x, parts$n_items)
    }
    # Without these, the call below would land here again, as with `n` missing:
    if (is.na(digits_x)) {
      return(grim_values_tibble(NA_real_, parts))
    }
    return(grim_closest_scalar(
      x = x,
      n = n,
      digits_x = digits_x,
      items_averaged = items_averaged,
      data_type = data_type,
      x_meaning = x_meaning,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    ))
  }

  sum1 <- sums[1L]
  sum2 <- sums[2L]

  sums_candidate <- if (sum1 > sum2) {
    # An empty range is empty by exactly one integer: its endpoints are the two
    # whole-number sum totals that straddle `x`, the lower one first.
    c(sum2, sum1)
  } else {
    # The whole numbers around the unrounded sum total, pulled into the
    # admissible range: a consistent `x` stands for a mean that rounds to it,
    # even where a closer one rounds to something else, as under `"floor"`.
    # One more on either side in case `floor()` lands one integer off through
    # floating-point error in the product:
    sum_floor <- floor(x * parts$n_items / parts$scale)
    sort(unique(pmin(pmax(sum_floor + (-1):2, sum1), sum2)))
  }

  # Distances compared as whole numbers, in units of `x`'s last decimal place
  # times `n_items`, so that two equally close values tie exactly, and the
  # smaller one wins:
  numerator <- round(x * 10^digits_x)
  distance <- abs(
    sums_candidate * parts$scale * 10^digits_x - numerator * parts$n_items
  )
  grim_values_tibble(sums_candidate[which.min(distance)], parts)
}


#' Achievable means behind a reported one
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   These functions return the means (or percentages) that a sample
#'   of `n * items_averaged` integers can have:
#'
#'   - `grim_values()` returns every achievable value that would be reported as
#'   `x`, with the whole-number sum behind it. The result has no rows if `x` is
#'   GRIM-inconsistent with `n`.
#'   - `grim_closest()` returns the single achievable value closest to `x`
#'   among those that would be reported as `x`, or, if there are none, the
#'   closest one overall. Of two equally close values, it returns the smaller.
#'
#'   Both are vectorized like [`grim()`] and return a tibble in which `case`
#'   says which value set a row belongs to. A reported mean may correspond to
#'   one achievable mean, to several, or to none, so `grim_values()` has one
#'   row per achievable mean, and `grim_closest()` one row per value set.
#'   [`grimmer_values()`] and [`grimmer_closest()`] are the counterparts for
#'   GRIMMER.
#'
#' @details The achievable means are the whole numbers in the range that
#'   [`grim_map()`] shows as `sum_lower` and `sum_upper` under `show_rec`, each
#'   divided by `n * items_averaged`. Once `n * items_averaged` exceeds
#'   `10^digits_x`, several of them can be reported as the same `x`, and their
#'   number grows with `n * items_averaged`. GRIM is then no longer informative.
#'   `grim_closest()` stays cheap in that case. `grim_values()` does not, and
#'   more than a million values for one value set is an error.
#'
#'   With `x_meaning = "percentage"`, the values are percentages, like `x`.
#'
#'   Under `rounding = "even"`, or its synonym `"ties_even"`, both bounds of
#'   that range are inclusive (see [`roundwork::unround()`]), so the values
#'   include ties that
#'   [`base::round()`] may round away from `x`:
#'   `grim_values(1.3, 20, 1, 1, "integer", rounding = "even")` returns 1.25 and 1.35
#'   along with 1.3.
#'
#'   With `items_averaged > 1`, the integers are the respondents' totals
#'   across the items, so `sum` is the sum of the totals.
#'
#' @param x Numeric. The reported mean or percentage.
#' @param n Integer. The reported sample size.
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default, since a numeric value does not
#'   keep them (`1.40` is `1.4`).
#' @param items_averaged Integer. As in [`grim()`].
#' @param data_type String. `"integer"` if the values are whole numbers,
#'   as in [`grim()`]. There is no default, so that every call states
#'   this assumption. `"continuous"` is an error: such data can have any
#'   value that would be reported as the one given, so there are none to
#'   list. Under `NA`, a value set is undecidable unless no data can have it.
#' @param x_meaning String. `"mean"` (the default) or `"percentage"`, as in
#'   [`grim()`].
#' @param rounding,threshold,symmetric The same as in [`grim()`].
#'
#' @return
#' - For `grim_values()`, a tibble with one row per achievable mean. `case` is
#'   the index of the value set, i.e., the position in the inputs after they
#'   are recycled to a common length, so it also counts through `n` if `x` is
#'   a single value. `x` is the achievable value, `n` the reported sample size,
#'   and `sum` the whole number behind `x`. An inconsistent value set has no
#'   rows, and one that cannot be decided has one row of `NA` but for `n`, as
#'   in [`grim()`]. So does one with a missing `digits_x` that is consistent
#'   whatever it is: how many values would be reported as `x` still depends on
#'   it.
#' - For `grim_closest()`, a tibble with the same columns and exactly one row
#'   per value set: the closest achievable mean, or a row of `NA` but for `n`
#'   if the value set cannot be decided.
#'
#' @seealso [`grim()`], [`grim_map()`], [`grimmer_values()`], and
#'   [`grim_exp_incon()`].
#'
#' @include grim.R
#'
#' @rdname grim_values
#'
#' @export
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876
#'
#' @examples
#' # A mean of 5.19 is consistent with a sample size of 32,
#' # and there is exactly one way to get there:
#' grim_values(x = 5.19, n = 32, digits_x = 2, items_averaged = 1, data_type = "integer")
#'
#' # With a larger sample, several means round to 5.19:
#' grim_values(x = 5.19, n = 300, digits_x = 2, items_averaged = 1, data_type = "integer")
#'
#' # It is not consistent with a sample size of 28...
#' grim_values(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")
#'
#' # ...but 5.19 is close to a mean that is achievable:
#' grim_closest(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")
#'
#' # Both functions are vectorized. In `grim_values()`, `case` is the index of
#' # the value set, and an inconsistent one has no rows:
#' grim_values(
#'   x = c(5.19, 4.2),
#'   n = c(28, 30),
#'   digits_x = 2,
#'   items_averaged = 1,
#'   data_type = "integer"
#' )
#' grim_closest(
#'   x = c(5.19, 4.2),
#'   n = c(28, 30),
#'   digits_x = 2,
#'   items_averaged = 1,
#'   data_type = "integer"
#' )

# The signatures mirror those of the `*_scalar()` functions; see
# `map_value_sets()`.
grim_values <- function(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  map_value_sets(
    fn = grim_values_scalar,
    along = c("x", "n", "digits_x", "items_averaged", "data_type"),
    type = "list"
  ) |>
    bind_cases(PTYPE_GRIM_VALUES)
}


#' @rdname grim_values
#' @export
grim_closest <- function(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  map_value_sets(
    fn = grim_closest_scalar,
    along = c("x", "n", "digits_x", "items_averaged", "data_type"),
    type = "list"
  ) |>
    bind_cases(PTYPE_GRIM_VALUES)
}
