
#' GRIMMER-test many cases at once
#'
#' @description Call `grimmer_map()` to GRIMMER-test any number of combinations
#'   of mean, standard deviation, sample size, and number of items. Mapping
#'   function for GRIMMER-testing.
#'
#'   For summary statistics, call `audit()` on the results.
#'
#' @param data Data frame with columns `x`, `sd`, `n`, and optionally `items`
#'   (see documentation for `grim()`). Any other columns in `data` will be
#'   returned alongside GRIMMER test results.
#' @param items Integer. If there is no `items` column in `data`, this specifies
#'   the number of items composing the `x` values. Default is 1, the most common
#'   case.
#' @param x,sd,n Optionally specify which columns from `data` contain the means
#'   (`x`), standard deviations (`sd`), and/or sample sizes (`n`). If not
#'   specified here, `data` itself needs to contain columns by those names.
#'   Default is `NULL`.
#' @param show_reason Boolean (length 1). Should there be a `reason` column that
#'   shows the reasons for inconsistencies (and `NA` for consistent values)?
#'   Default is `FALSE`.
#' @param rounding,threshold,symmetric,tolerance Further parameters of
#'   GRIMMER-testing; see documentation for `grimmer()`.

#' @return A tibble with these columns --
#' - `x`, `sd`, `n`: the inputs.
#' - `consistency`: GRIMMER consistency of `x`, `n`, and `items`.
#' - `<extra>`: any columns from `data` other than `x`, `n`, and `items`.
#'
#' The tibble has the `scr_grimmer_map` class, which is recognized by the
#' `audit()` generic.

#' @section Summaries with `audit()`: There is an S3 method for `audit()`, so
#'   you can call `audit()` following `grimmer_map()` to get a summary of
#'   `grimmer_map()`'s results. It is a tibble with a single row and these
#'   columns --
#'
#' 1. `incons_cases`: number of GRIMMER-inconsistent value sets.
#' 2. `all_cases`: total number of value sets.
#' 3. `incons_rate`: proportion of GRIMMER-inconsistent value sets.

#' @include audit.R grimmer.R function-map.R
#'
#' @references
#'
#' @export
#'
#' @examples


grimmer_map <- function_map(
  .fun = grimmer_scalar,
  .reported = c("x", "sd", "n"),
  .name_test = "GRIMMER",
  .name_class = "scr_grim_map",
  .arg_list = list(
    show_reason = TRUE, rounding = "up_or_down", threshold = 5,
    symmetric = FALSE, tolerance = .Machine$double.eps^0.5
  ),
  .col_names = "reason",
  .col_control = "show_reason"
)



# Example data:
pigs5 <- scrutiny::pigs1 %>%
  dplyr::mutate(
    sd = runif(12, 2, 8) %>%
      round_up(2) %>%
      restore_zeros(width = 2),
    .after = 1
  )


