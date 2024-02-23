
#' Possible GRIM inconsistencies
#'
#' @description Even without GRIM-testing, means / proportions and sample sizes
#'   of granular distributions entail some key data:
#'
#'   - `grim_total()` returns the absolute number of GRIM-inconsistencies that
#'   are possible given the mean or percentage's number of decimal places (`D`)
#'   and the corresponding sample size.
#'   - `grim_ratio()` returns a proportion that is normalized by `10^D`, and
#'   therefore comparable across mean or percentage values reported to varying
#'   `D`.
#'   - `grim_ratio_upper()` returns the upper bound of `grim_ratio()` for a
#'   given `D`.
#'
#'   For discussion, see `vignette("grim")`, section *GRIM statistics*.

#' @param x String or numeric (length 1). Mean or percentage value computed from
#'   data with integer units (e.g., mean scores on a Likert scale or percentage
#'   of study participants in some condition). *Note*: Numeric inputs don't
#'   include trailing zeros, but these are important for GRIM functions. See
#'   documentation for `grim()`.
#' @param n Integer. Sample size corresponding to `x`.
#' @param items Integer. Number of items composing the mean or percentage value
#'   in question. Default is `1`.
#' @param percent Logical. Set `percent` to `TRUE` if `x` is expressed as a
#'   proportion of 100 rather than 1. The functions will then account for this
#'   fact through increasing the decimal count by 2. Default is `FALSE`.

#' @seealso `grim()` for the GRIM test itself; as well as `grim_map()` for
#'   applying it to many cases at once.
#'
#' @return Integer or double. The number or proportion of possible GRIM
#'   inconsistencies.
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

#' @rdname grim-stats
#' @export

grim_total <- function(x, n, items = 1, percent = FALSE) {
  digits <- decimal_places_scalar(x)
  if (percent) digits <- digits + 2L
  p10 <- 10 ^ digits
  as.integer(p10 - (n * items))
}



# Relative ----------------------------------------------------------------

#' @rdname grim-stats
#' @export

grim_ratio <- function(x, n, items = 1, percent = FALSE) {
  digits <- decimal_places_scalar(x)
  if (percent) digits <- digits + 2L
  p10 <- 10 ^ digits
  (p10 - (n * items)) / p10
}


#' @rdname grim-stats
#' @export

grim_ratio_upper <- function(x, percent = FALSE) {
  grim_ratio(x = x, n = 1, items = 1, percent = percent)
}

