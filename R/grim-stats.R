
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

#' @param x String (length 1). Mean or percentage value computed from data with
#'   integer units, e.g., mean scores on a Likert scale or percentage of study
#'   participants in some condition. It has to be string to capture any trailing
#'   zeros.
#' @param n Integer. Sample size corresponding to `x`.
#' @param items Integer. Number of items composing the mean or percentage value
#'   in question. Default is `1`.
#' @param percent Logical. Set `percent` to `TRUE` if `x` is expressed as a
#'   proportion of 100 rather than 1. The functions will then account for this
#'   fact through increasing the decimal count by 2. Default is `FALSE`.

#' @seealso [`grim()`] for the GRIM test itself; as well as [`grim_map()`] for
#'   applying it to many cases at once.
#'
#' @return Integer or double. The number of possible GRIM inconsistencies, or
#'   their probability for a random mean or percentage with a given number of
#'   decimal places.
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
#' grim_probability(x = "83.29", n = 21)
#' grim_total(x = "83.29", n = 21)
#'
#' # No sets are inconsistent in this case...
#' grim_probability(x = "5.14", n = 83)
#' grim_total(x = "5.14", n = 83)
#'
#' # ... but most would be if `x` was a percentage:
#' grim_probability(x = "5.14", n = 83, percent = TRUE)
#' grim_total(x = "5.14", n = 83, percent = TRUE)



# Relative ----------------------------------------------------------------

grim_probability <- function(x, n, items = 1, percent = FALSE) {
  # Manual check (instead of calling `check_type()`) for performance; this
  # function will run a great deal:
  if (!is.character(x)) {
    cli::cli_abort(c(
      "!" = "`x` must be of type character.",
      "x" = "It is {an_a_type(x)}."
    ))
  }
  digits <- decimal_places_scalar(x)
  if (percent) digits <- digits + 2L
  p10 <- 10 ^ digits
  out <- (p10 - n * items) / p10
  dplyr::if_else(out < 0, 0, out)
}



#' @rdname grim-stats
#' @export
grim_ratio <- function(x, n, items = 1, percent = FALSE) {
  check_type(x, "character")
  digits <- decimal_places_scalar(x)
  if (percent) digits <- digits + 2L
  p10 <- 10 ^ digits
  (p10 - n * items) / p10
}


# Absolute ----------------------------------------------------------------

#' @rdname grim-stats
#' @export
grim_total <- function(x, n, items = 1, percent = FALSE) {
  check_type(x, "character")
  digits <- decimal_places_scalar(x)
  if (percent) digits <- digits + 2L
  p10 <- 10 ^ digits
  as.integer(p10 - (n * items))
}



#' Upper bound for the GRIM ratio
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#'   `grim_ratio_upper()` is deprecated because it no longer seems very
#'   meaningful. It will be removed in a future version.
#'
#'   See [`grim_probability()`] for a more interesting measure.
#'
#' @inheritParams grim-stats
#'
#' @keywords internal
#'
#' @return Numeric.
#'
#' @export
grim_ratio_upper <- function(x, percent = FALSE) {
  lifecycle::deprecate_warn(
    when = "0.5.0",
    what = "grim_ratio_upper()",
    details = "It will be removed in a future version."
  )
  check_type(x, "character")
  grim_ratio(x = x, n = 1, items = 1, percent = percent)
}

