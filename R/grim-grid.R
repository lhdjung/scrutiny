#' GRIM results on a grid of sample sizes and means
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `grim_grid()` tests every combination of sample size and fractional part
#'   of a mean with GRIM, for one number of decimal places and one rounding
#'   method. It is meant for studying GRIM's properties directly.
#'   [`grim_plot_grid()`] draws the inconsistent cells as its background
#'   raster.
#'
#' @details The grid covers sample sizes from 1 to `10^digits_x`, where GRIM stops
#'   ruling out any mean, and fractional parts in steps of `1 / 10^digits_x`,
#'   without zero. All fractional parts of means are positive, so `symmetric`
#'   makes no difference.
#'
#'   For percentages, use the number of decimal places of the proportion,
#'   which is two more: `grim_grid(2)` for whole percentages, as in
#'   [`grim_map()`] with `x_meaning = "percentage"`.
#'
#'   Grids for one or two decimal places are precomputed for every rounding
#'   method that has no `threshold`. All others are computed on demand, which
#'   takes some 20 to 30 seconds for three decimal places. Four or more would mean a
#'   grid of 100 million cells, so `digits_x` is at most 3.
#'
#' @param digits_x Integer. Number of decimal places, from 1 to 3.
#' @param data_type String. `"integer"` if the values are whole numbers, as in
#'   [`grim()`]. Under `"continuous"`, every cell is consistent, and under
#'   `NA`, the inconsistent ones are `NA`.
#' @param rounding,threshold The same as in [`grim()`].
#'
#' @return A tibble with one row per cell of the grid and the columns `frac`
#'   (the fractional part of a mean), `n` (the sample size), and `consistency`
#'   (the GRIM result).
#'
#' @export
#'
#' @examples
#' grim_grid(digits_x = 1, data_type = "integer")
#'
#' # Share of means ruled out per sample size:
#' grim_grid(digits_x = 2, data_type = "integer") |>
#'   dplyr::summarise(share_inconsistent = mean(!consistency), .by = n)
grim_grid <- function(
  digits_x,
  data_type,
  rounding = "up_or_down",
  threshold = 5
) {
  if (missing(digits_x)) {
    abort_in_export(
      "`digits_x` is missing.",
      "i" = "Set it to the number of decimal places: 1, 2, or 3."
    )
  }

  check_length(digits_x, 1L)

  # Near-whole values pass, as elsewhere in the package:
  is_valid <- is.numeric(digits_x) &&
    isTRUE(abs(digits_x - round(digits_x)) < WHOLE_NUMBER_TOLERANCE) &&
    round(digits_x) %in% 1:3

  if (!is_valid) {
    abort_in_export(
      "`digits_x` must be 1, 2, or 3.",
      "x" = "It is {wrap_in_quotes_or_backticks(digits_x)}."
    )
  }

  digits <- as.integer(round(digits_x))

  if (missing(data_type)) {
    error_data_type_missing()
  }
  check_length(data_type, 1L)
  if (is.factor(data_type)) {
    error_data_type_factor()
  }
  if (!is.na(data_type)) {
    data_type <- arg_match_in_export(data_type, DATA_TYPES)
  }

  inconsistent <- grim_grid_inconsistent(digits, rounding, threshold)

  # The cells run through `frac` within each `n`, so a cell's row is found from
  # its two indices:
  p10 <- 10L^digits
  grid <- grim_grid_cells(digits)
  rows <- (inconsistent$n - 1L) * (p10 - 1L) + round(inconsistent$frac * p10)

  # Continuous data can have any mean. Not knowing the type leaves open only
  # the cells that whole numbers can't have, as in `grim()`:
  grid$consistency <- TRUE
  if (identical(data_type, "continuous")) {
    return(grid)
  }
  grid$consistency[rows] <- if (is.na(data_type)) NA else FALSE
  grid
}


# The inconsistent cells only, which is all that `grim_plot_grid()` needs and
# all that `GRIM_GRIDS` stores.
grim_grid_inconsistent <- function(digits, rounding, threshold) {
  # Checked here so that roundwork's message, which names `symmetric`, is not
  # reached:
  check_length(rounding, 1L)
  check_length(threshold, 1L)
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
  # takes some 20 to 30 seconds.
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
    data_type = "integer",
    rounding = rounding,
    threshold = threshold
  )

  grid[!consistency, ]
}
