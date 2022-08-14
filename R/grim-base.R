

#' Count possible GRIM inconsistencies
#'
#' @description Count the GRIM-inconsistencies that are possible given the mean
#'   or percentage's number of decimal places (`D`) and the corresponding sample
#'   size.
#'
#'   `grim_total()` returns the absolute number. `grim_ratio()` returns a
#'   proportion that is normalized by `10^D` and therefore comparable across
#'   mean or percentage values reported to varying `D`.
#'
#' @param x String or numeric. Mean or percentage value computed from data with
#'   integer units (e.g., mean scores on a Likert scale or percentage of study
#'   participants in some condition). *Note*: Numeric inputs don't include
#'   trailing zeros, although these are important for both functions. See
#'   documentation for `decimal_places()`.
#' @param n Integer. Sample size corresponding to `x`.
#' @param items Integer. Number of items composing the mean or percentage value
#'   in question. Default is `1`.
#' @param percent Boolean. Set `percent` to `TRUE` if `x` is expressed as a
#'   proportion of 100 rather than 1. The functions will then account for this
#'   fact through increasing the decimal count by 2. Default is `FALSE`.
#'
#' @seealso `grim()` for the GRIM test itself; as well as `grim_map()` for
#'   applying it to multiple cases at once.
#'
#' @return Integer or numeric. The number or proportion of possible GRIM
#'   inconsistencies.
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876
#'
#' @export
#'
#' @examples
#' # Many value sets are inconsistent here:
#' grim_total(x = "83.29", n = 21)
#' grim_ratio(x = "83.29", n = 21)
#'
#' # No sets are inconsistent in this case...
#' grim_total(x = "5.14", n = 83)
#' grim_ratio(x = "5.14", n = 83)
#'
#' # ... but most would be if `x` was a percentage:
#' grim_total(x = "5.14", n = 83, percent = TRUE)
#' grim_ratio(x = "5.14", n = 83, percent = TRUE)



# Absolute ----------------------------------------------------------------

#' @export

grim_total <- function(x, n, items = 1, percent = FALSE) {

  decimals <- decimal_places(x)
  if (percent) decimals <- decimals + 2
  p10 <- 10 ^ decimals

  return(as.integer(p10 - (n * items)))
}



# Relative ----------------------------------------------------------------

#' @rdname grim_total
#' @export

grim_ratio <- function(x, n, items = 1, percent = FALSE) {

  decimals <- decimal_places(x)
  if (percent) decimals <- decimals + 2
  p10 <- 10 ^ decimals

  return((p10 - (n * items)) / p10)
}



