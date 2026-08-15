#' Possible GRIM inconsistencies
#'
#' @description These functions compute statistics related to GRIM-testing. In
#'   general, `grim_probability()` is the most useful of them, and it is
#'   responsible for the `probability` column in a data frame returned by
#'   [`grim_map()`].
#'
#'   - `grim_probability()` returns the probability that a reported mean or
#'   percentage of integer data that is random except for the number of its
#'   decimal places is inconsistent with the reported sample size. For example,
#'   the mean 1.23 is treated like any other mean with two decimal places.
#'   - `grim_ratio()` is equal to `grim_probability()` unless `grim_ratio()` is
#'   negative, which can occur if the sample size is very large. Strictly
#'   speaking, this is more informative than `grim_probability()`, but it is
#'   harder to interpret.
#'   - `grim_total()` returns the absolute number of GRIM-inconsistencies that
#'   are possible given the mean or percentage's number of decimal places and
#'   the corresponding sample size.
#'
#'   For discussion, see `vignette("grim")`, section *GRIM statistics*.

#' @param x Numeric. Mean or percentage value computed from data with integer
#'   units, e.g., mean scores on a Likert scale or percentage of study
#'   participants in some condition. Only the number of decimal places matters;
#'   see `digits_x`. The argument exists for compatibility with [`grim()`].
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default because it cannot be inferred from a
#'   numeric `x`, which has no trailing zeros: both `1.4` and `1.40` are the
#'   number `1.4`, but only the latter has `digits_x = 2`.
#' @param n Integer. Sample size corresponding to `x`.
#' @param items Integer. Number of items composing the mean or percentage value
#'   in question. Default is `1`.
#' @param percent Logical. Set `percent` to `TRUE` if `x` is expressed as a
#'   proportion of 100 rather than 1. The functions will then account for this
#'   fact through increasing the decimal count by 2. Default is `FALSE`.

#' @seealso [`grim()`] for the GRIM test itself; as well as [`grim_map()`] for
#'   applying it to many cases at once.
#'
#' @return Double. The number of possible GRIM inconsistencies, or their
#'   probability for a random mean or percentage with a given number of decimal
#'   places.
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876
#'
#' @export
#'
#' @name grim-stats
#'
#' @examples
#' # Many value sets are inconsistent here:
#' grim_probability(x = 83.29, n = 21, digits_x = 2)
#' grim_total(x = 83.29, n = 21, digits_x = 2)
#'
#' # No sets are inconsistent in this case...
#' grim_probability(x = 5.14, n = 83, digits_x = 2)
#' grim_total(x = 5.14, n = 83, digits_x = 2)
#'
#' # ... but most would be if `x` was a percentage:
#' grim_probability(x = 5.14, n = 83, digits_x = 2, percent = TRUE)
#' grim_total(x = 5.14, n = 83, digits_x = 2, percent = TRUE)

# Relative ----------------------------------------------------------------

grim_probability <- function(x, n, digits_x, items = 1, percent = FALSE) {
  if (percent) {
    digits_x <- digits_x + 2L
  }
  p10 <- 10^digits_x
  out <- (p10 - n * items) / p10
  out <- dplyr::if_else(out < 0, 0, out)

  # A probability cannot exceed 1, and the formula above returns more than 1
  # whenever `n * items` is negative. That is not a case with a very high
  # probability of inconsistency -- it is a case with nothing to test, which is
  # exactly what `grim()` returns `NA` for. Reporting `1.03` next to a verdict of
  # `NA`, as the `probability` column of `grim_map()` used to, states two
  # incompatible things about one row. The condition is the same one `grim()`
  # itself decides by, so a fractional `n` or `items` is `NA` here too rather
  # than a probability about a data set that cannot exist. `grim_ratio()` is
  # the unclamped one, and it reports the raw formula whatever the inputs:
  dplyr::if_else(is_decidable_n_items(n, items), out, NA_real_)
}


#' @rdname grim-stats
#' @export
grim_ratio <- function(x, n, digits_x, items = 1, percent = FALSE) {
  if (percent) {
    digits_x <- digits_x + 2L
  }
  p10 <- 10^digits_x
  (p10 - n * items) / p10
}


# Absolute ----------------------------------------------------------------

#' @rdname grim-stats
#' @export
grim_total <- function(x, n, digits_x, items = 1, percent = FALSE) {
  if (percent) {
    digits_x <- digits_x + 2L
  }
  p10 <- 10^digits_x

  # Double, always. The count is a whole number, so integer looks like the
  # better representation for it, but it cannot be integer consistently: the
  # count goes past `.Machine$integer.max` from `digits_x = 10` on (or from 8
  # with `percent = TRUE`), where `as.integer()` gives `NA` and a warning.
  # Coercing only while the value fits would make the return type depend on the
  # values -- and, since a vector has one type, on the largest element of the
  # call, so `grim_total(digits_x = c(2, 10))` would return both counts as
  # doubles while `digits_x = 2` alone returned an integer. A double is exact up
  # to 2^53, which is far past anything GRIM can produce, and `10^digits_x` is
  # one already, so the subtraction below is where the type is settled:
  p10 - (n * items)
}
