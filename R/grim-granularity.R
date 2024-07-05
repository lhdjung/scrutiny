

#' Granularity of non-continuous scales
#'
#' @description `grim_granularity()` computes the minimal difference between two
#'   means or proportions of ordinal or interval data.
#'
#'   `grim_items()` is the reverse: It converts granularity values to the number
#'   of scale items, which might then be used for consistency testing functions
#'   such as `grim()`.
#'
#' @details These two functions differ only in the names of their arguments ---
#'   the underlying formula is the same (and it's very simple). However, for
#'   clarity, they are presented as distinct.
#'
#'   The output of `grim_items()` should be whole numbers, because scale items
#'   have a granularity of 1.
#'
#'   It would be wrong to determine a scale's granularity from the minimal
#'   distance between two values in a given distribution. This would only
#'   signify how those values actually do differ, not how they *can* differ *a
#'   priori* based on scale design. Also, keep in mind that continuous scales
#'   have no granularity at all.
#'
#' @param n Numeric. Sample size.
#' @param items Numeric. Number of items composing the scale. Default is 1,
#'   which will hold for most non-Likert scales.
#' @param gran Numeric. Granularity.
#' @param tolerance Numeric. Any difference between `x` and a truncated version
#'   of `x` less than `tolerance` (in the absolute value) will be ignored. The
#'   default is close to `1 / (10 ^ 8)`. This avoids errors due to spurious
#'   precision in floating-point arithmetic.
#'
#' @include utils.R
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876

#' @return Numeric. Granularity or number of items.

#' @export
#'
#' @examples
#' # If a non-Likert scale ranges from 0 to 3
#' # and measures 16 cases:
#' grim_granularity(n = 16)   # `items = 1` by default
#'
#' # Same but Likert scale with 2 items:
#' grim_granularity(n = 16, items = 2)
#'
#' # If a scale is applied to a single case
#' # and has a granularity of 0.5:
#' grim_items(n = 1, gran = 0.5)
#'
#' # With more cases, a warning appears
#' # because items can only be whole numbers:
#' grim_items(n = c(10, 15, 20), gran = 0.5)


grim_granularity <- function(n, items = 1) {
  1 / (n * items)
}


#' @rdname grim_granularity
#' @export

grim_items <- function(n, gran, tolerance = .Machine$double.eps^0.5) {

  out <- 1 / (n * gran)
  out_is_whole <- is_whole_number(out, tolerance)

  if (all(out_is_whole)) {
    return(out)
  }

  offenders <- out[!out_is_whole]
  offenders <- round(offenders, 3L)
  offenders <- wrap_in_backticks(offenders)
  if (length(offenders) == 1L) {
    item_items <- "Item count"
    number_numbers <- "isn't a whole number"
  } else {
    item_items <- paste(
      length(offenders), "out of", length(out), "item counts"
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


