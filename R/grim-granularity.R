#' Granularity of non-continuous scales
#'
#' @description `grim_granularity()` returns the smallest difference between
#'   two means that a scale with `items_averaged` items and `n` observations can
#'   produce: `1 / (n * items_averaged)`.
#'
#'   `grim_items()` inverts it: given the granularity, it returns the number
#'   of items.
#'
#' @details The granularity of a scale follows from its design, not from the
#'   smallest observed difference between two values. Continuous scales have
#'   no granularity.
#'
#'   Since the number of items is a whole number, a fractional result of
#'   `grim_items()` points to an error upstream, and the function warns about
#'   it.
#'
#' @param n Numeric. Sample size.
#' @param items_averaged Numeric. Number of items averaged into each score.
#'   Default is 1.
#' @param gran Numeric. Granularity.
#' @param tolerance Numeric. Differences from a whole number below `tolerance`
#'   are ignored when checking the result of `grim_items()`. Default is about
#'   `1.5e-8`.
#'
#' @include utils.R
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876
#'
#' @return Numeric. Granularity or number of items.
#'
#' @export
#'
#' @examples
#' grim_granularity(n = 16)
#' grim_granularity(n = 16, items_averaged = 2)
#'
#' grim_items(n = 1, gran = 0.5)
#'
#' # A fractional result draws a warning:
#' grim_items(n = c(10, 15, 20), gran = 0.5)

grim_granularity <- function(n, items_averaged = 1) {
  check_positive(n)
  check_positive(items_averaged)
  1 / (n * items_averaged)
}


#' @rdname grim_granularity
#' @export

grim_items <- function(n, gran, tolerance = .Machine$double.eps^0.5) {
  check_positive(n)
  check_positive(gran)
  out <- 1 / (n * gran)
  out_is_whole <- is_whole_number(out, tolerance)

  # A missing count has no whole-number status to warn about:
  if (all(out_is_whole, na.rm = TRUE)) {
    return(out)
  }

  offenders <- out[out_is_whole %in% FALSE]
  offenders <- round(offenders, 3L)
  offenders <- wrap_in_backticks(offenders)
  if (length(offenders) == 1L) {
    item_items <- "Item count"
    number_numbers <- "isn't a whole number"
  } else {
    item_items <- paste(
      length(offenders),
      "out of",
      length(out),
      "item counts"
    )
    number_numbers <- "aren't whole numbers"
  }

  cli::cli_warn(c(
    "{item_items} {number_numbers}.",
    ">" = "This concerns {offenders}.",
    "!" = "Item counts have a granularity of 1, so they should be whole \\
      numbers. Are you sure about the `n` and `gran` values?"
  ))

  out
}
