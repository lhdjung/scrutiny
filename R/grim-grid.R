#' GRIM-inconsistent combinations of sample size and mean
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `grim_grid()` returns every combination of sample size and fractional
#'   part of a mean that GRIM finds inconsistent, for one number of decimal
#'   places and one rounding method, for studying GRIM's properties directly.
#'   [`grim_plot_grid()`] draws these cells as its background raster.
#'
#' @details The grid covers sample sizes from 1 to `10^digits`, where GRIM stops
#'   ruling out any mean, and fractional parts in steps of `1 / 10^digits`,
#'   without zero. All fractional parts of means are positive, so `symmetric`
#'   makes no difference.
#'
#'   Grids for one or two decimal places are precomputed for every rounding
#'   method that has no `threshold`. All others are computed on demand, which
#'   takes some seconds for three decimal places. Four or more would mean a grid
#'   of 100 million cells, so `digits` is at most 3.
#'
#' @param digits Integer. Number of decimal places, from 1 to 3.
#' @param rounding,threshold The same as in [`grim()`].
#'
#' @return A tibble with the columns `frac` (the fractional part of a mean) and
#'   `n` (the sample size), one row per GRIM-inconsistent combination.
#'
#' @export
#'
#' @examples
#' grim_grid(digits = 1)
#'
#' # Inconsistent cells per sample size:
#' grim_grid(digits = 2, rounding = "up") |>
#'   dplyr::count(n)
grim_grid <- function(digits, rounding = "up_or_down", threshold = 5) {
  check_length(digits, 1L)
  check_digits_whole(digits)

  if (is.na(digits) || digits < 1 || digits > 3) {
    abort_in_export(
      "`digits` must be 1, 2, or 3.",
      "x" = "It is {digits}."
    )
  }

  digits <- as.integer(round(digits))
  check_rounding_args(rounding, threshold)

  # The grids are keyed by the basic `rounding` names. `symmetric = FALSE`
  # because all fractional portions of means are positive.
  rounding <- roundwork::resolve_ties_rounding(rounding, FALSE)$rounding

  # Precomputed in data-raw/data-gen.R, stored in R/sysdata.rda. The `"up_from"`
  # family has no entry because it takes a `threshold`.
  grid <- GRIM_GRIDS[[paste(digits, rounding, sep = "_")]]

  # Not cached: three decimal places, or the `"up_from"` family. The former
  # takes some seconds.
  if (is.null(grid)) {
    generate_grim_grid(digits, rounding, threshold)
  } else {
    grid
  }
}


# The (`n`, `frac`) grid points that are GRIM-*in*consistent.
# `digits` fixes the grid in both directions: `frac` steps by `1 / 10^digits`,
# and `n` runs up to `10^digits`, the sample size at which GRIM stops ruling
# anything out. data-raw/data-gen.R calls this to build `GRIM_GRIDS`.
generate_grim_grid <- function(digits, rounding, threshold = 5) {
  p10 <- 10^digits
  frac <- seq_len(p10 - 1L) / p10

  grid <- tibble::tibble(
    frac = rep(frac, times = p10),
    n = rep(seq_len(p10), each = p10 - 1L)
  )

  # `n` is a positive integer and `frac` is finite throughout, so there is no
  # `NA` in `consistency`.
  consistency <- grim(
    x = grid$frac,
    n = grid$n,
    digits_x = digits,
    items_averaged = 1,
    rounding = rounding,
    threshold = threshold
  )

  grid[!consistency, ]
}
