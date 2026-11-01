#' GRIM verdicts on a grid of sample sizes and means
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `grim_grid()` tests every combination of sample size and fractional part
#'   of a mean with GRIM, for one number of decimal places and one rounding
#'   method. It is meant for studying GRIM's properties directly.
#'   [`grim_plot_grid()`] draws the inconsistent cells as its background
#'   raster.
#'
#' @details The grid covers sample sizes from 1 to `10^digits`, where GRIM stops
#'   ruling out any mean, and fractional parts in steps of `1 / 10^digits`,
#'   without zero. All fractional parts of means are positive, so `symmetric`
#'   makes no difference.
#'
#'   For percentages, use the number of decimal places of the proportion,
#'   which is two more: `grim_grid(2)` for whole percentages, as in
#'   [`grim_map()`] with `percent = TRUE`.
#'
#'   Grids for one or two decimal places are precomputed for every rounding
#'   method that has no `threshold`. All others are computed on demand, which
#'   takes some seconds for three decimal places. Four or more would mean a grid
#'   of 100 million cells, so `digits` is at most 3.
#'
#' @param digits Integer. Number of decimal places, from 1 to 3.
#' @param rounding,threshold The same as in [`grim()`].
#'
#' @return A tibble with one row per cell of the grid and the columns `frac`
#'   (the fractional part of a mean), `n` (the sample size), and `consistency`
#'   (the GRIM verdict).
#'
#' @export
#'
#' @examples
#' grim_grid(digits = 1)
#'
#' # Share of means ruled out per sample size:
#' grim_grid(digits = 2) |>
#'   dplyr::summarise(share_inconsistent = mean(!consistency), .by = n)
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
  inconsistent <- grim_grid_inconsistent(digits, rounding, threshold)

  # The cells run through `frac` within each `n`, so a cell's row is found from
  # its two indices:
  p10 <- 10L^digits
  grid <- grim_grid_cells(digits)
  rows <- (inconsistent$n - 1L) * (p10 - 1L) + round(inconsistent$frac * p10)

  grid$consistency <- TRUE
  grid$consistency[rows] <- FALSE
  grid
}


# The inconsistent cells only, which is all that `grim_plot_grid()` needs and
# all that `GRIM_GRIDS` stores.
grim_grid_inconsistent <- function(digits, rounding, threshold) {
  check_rounding_args(rounding, threshold)

  # The grids are keyed by the basic `rounding` names. `symmetric = FALSE`
  # because all fractional portions of means are positive.
  rounding <- rounding |>
    roundwork::resolve_ties_rounding(symmetric = FALSE) |>
    purrr::pluck("rounding")

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


# Every (`frac`, `n`) cell. `digits` fixes the grid in both directions: `frac`
# steps by `1 / 10^digits`, and `n` runs up to `10^digits`, the sample size at
# which GRIM stops ruling anything out.
grim_grid_cells <- function(digits) {
  p10 <- 10^digits
  tibble::tibble(
    frac = rep(seq_len(p10 - 1L) / p10, times = p10),
    n = rep(seq_len(p10), each = p10 - 1L)
  )
}


# The GRIM-*in*consistent cells. data-raw/data-gen.R calls this to build
# `GRIM_GRIDS`.
generate_grim_grid <- function(digits, rounding, threshold = 5) {
  grid <- grim_grid_cells(digits)

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
