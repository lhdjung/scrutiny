

# NOTE: This file contains some example datasets that are exported as well as
# documentation for them. It also contains the code from which certain internal
# data are generated. For now, these internal data only include the background
# raster vectors for `grim_plot()`.

# Much of the file is normally outcommented. It is brought to life in order to
# generate the data, then outcommented again. I do so because that code is not
# needed otherwise, and some of it would take overly long to load.



# Documenting exported data -----------------------------------------------

#' Means and sample sizes for GRIM-testing
#'
#' A fictional dataset with means and sample sizes of flying pigs. It can be
#' used to demonstrate the functionality of `grim_map()` and functions building
#' up on it.

#' @include utils.R grim.R seq-decimal.R
#'
#' @format A tibble (data frame) with 12 rows and 2 columns. The columns are:
#' \describe{
#'  \item{x}{String. Means.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @seealso `pigs2` for GRIM-testing percentages instead of means, `pigs3` for
#'   DEBIT-testing, and `pigs4` for detecting duplicates.
#'
"pigs1"


#' Percentages and sample sizes for GRIM-testing
#'
#' A fictional dataset with percentages and sample sizes of flying pigs. It can
#' be used to demonstrate the functionality of `grim_map()`, particularly its
#' `percent` argument, and functions building up on it.

#' @include utils.R grim.R seq-decimal.R
#'
#' @format A tibble (data frame) with 6 rows and 2 columns. The columns are:
#' \describe{
#'  \item{x}{String. Percentages.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @seealso `pigs1` for GRIM-testing means instead of percentages, `pigs3` for
#'   DEBIT-testing, and `pigs4` for detecting duplicates.
#'
"pigs2"


#' Binary means and standard deviations for using DEBIT
#'
#' A fictional dataset with means and standard deviations from a binary
#' distribution related to flying pigs. It can be used to demonstrate the
#' functionality of `debit_map()` and functions building up on it.

#' @include utils.R debit-map.R seq-decimal.R
#'
#' @format A tibble (data frame) with 7 rows and 3 columns. The columns are:
#' \describe{
#'  \item{x}{String. Means.}
#'  \item{sd}{String. Standard deviations.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @seealso `pigs1` for GRIM-testing means, `pigs2` for GRIM-testing
#'   percentages, and `pigs4` for detecting duplicates.
#'
"pigs3"


#' Data with duplications
#'
#' A fictional dataset with observations of flying pigs. Two pairs of values are
#' duplicates. The dataset can be used to demonstrate the functionality of
#' `duplicate_detect()` and functions building up on it.

#' @include utils.R duplicate-detect.R
#'
#' @format A tibble (data frame) with 7 rows and 3 columns. The columns are:
#' \describe{
#'  \item{x}{String. Means.}
#'  \item{sd}{String. Standard deviations.}
#'  \item{n}{Numeric. Sample sizes.}
#' }

#' @seealso `pigs1` for GRIM-testing means, `pigs2` for GRIM-testing
#'   percentages, and `pigs3` for using DEBIT.
#'
"pigs4"



# Exported data -----------------------------------------------------------

# # GRIM for means:
# pigs1 <- tibble::tribble(
#   ~x,       ~n,
#   "7.22",    32,
#   "4.74",    25,
#   "5.23",    29,
#   "2.57",    24,
#   "6.77",    27,
#   "2.68",    28,
#   "7.01",    29,
#   "7.38",    26,
#   "3.14",    27,
#   "6.89",    31,
#   "5.00",    25,
#   "0.24",    28,
# )
#
#
# # GRIM for percentages:
# pigs2 <- tibble::tribble(
#   ~x,
#   "67.4",
#   "54.2",
#   "54.0",
#   "69.8",
#   "68.1",
#   "55.4",
# ) %>%
#   dplyr::mutate(n = 150)
#
#
# # DEBIT:
# pigs3 <- tibble::tribble(
#   ~x,         ~sd,
#   "0.53",     "0.50",
#   "0.44",     "0.50",
#   "0.77",     "0.42",
#   "0.19",     "0.35",
#   "0.34",     "0.47",
#   "0.93",     "0.25",
#   "0.12",     "0.33"
# ) %>%
#   dplyr::mutate(n = 1683)
#
#
# pigs4 <- tibble::tribble(
#   ~snout,  ~tail,
#   4.736,   6.887,
#   8.131,   7.331,
#   4.221,   6.095,
#   4.221,   7.574,
#   5.179,   8.131,
# )
#
#
#
#
# # Save data:
#
# usethis::use_data(
#   pigs1,
#   pigs2,
#   pigs3,
#   pigs4,
#
#   overwrite = TRUE
# )
#
#
#
#
# # Internal data -----------------------------------------------------------
#
# # Some nomenclature -- `n`: sample size, `frac`: fractional portion of a mean
# # value.
#
#
#
# Functions for GRIM-testing corresponding to every specific rounding procedure:
#
# grim_up_or_down       <- function(...) grim_scalar(..., rounding = "up_or_down")
# grim_up               <- function(...) grim_scalar(..., rounding = "up")
# grim_down             <- function(...) grim_scalar(..., rounding = "down")
# grim_even             <- function(...) grim_scalar(..., rounding = "even")
# grim_up_from          <- function(...) grim_scalar(..., rounding = "up_from")
# grim_down_from        <- function(...) grim_scalar(..., rounding = "down_from")
# grim_ceiling_or_floor <- function(...) grim_scalar(..., rounding = "ceiling_or_floor")
# grim_ceiling          <- function(...) grim_scalar(..., rounding = "ceiling")
# grim_floor            <- function(...) grim_scalar(..., rounding = "floor")
# grim_trunc            <- function(...) grim_scalar(..., rounding = "up")
# grim_anti_trunc       <- function(...) grim_scalar(..., rounding = "up")
#
#
#
# # This function returns a list of two vectors. The first is `n`, the second is
# # `frac`. Together, these two vectors form the background raster in
# # `grim_plot()`, with `n` on the x-axis and `frac` on the y-axis.
#
# generate_grim_raster <- function(decimals, rounding, n = NULL) {
#
#   # The `decimals` argument is processed in GRIM-typical fashion, such that,
#   # e.g., `decimals = 2` leads to a fractional unit (`frac_unit`) of 0.01:
#   p10 <- 10 ^ decimals
#   frac_unit <- 1 / p10
#
#   # The sample size, `n`, should be equal to `p10` because this is the
#   # right-hand limit of the y-axis, corresponding to the number of decimal
#   # places:
#   if (is.null(n)) n <- p10
#
#   # Produce `n` and `frac` sequences of equal length, corresponding to the x and
#   # y axes later in the plot:
#   frac_sequence <- seq_endpoint(from = frac_unit, to = (1 - frac_unit))
#   n_sequence <- 1:n
#
#   # Assemble the name of the `grim_*()` function that will be used as a filter
#   # below. This goes by `rounding`. For example, if `rounding = even`, then
#   # `grim_filter_func` will be `grim_even` -- a function, not a string:
#   grim_filter_func <- eval(rlang::parse_expr(paste0("grim_", rounding)))
#
#   # The essential workhorse within the present function is `purrr::cross2()`,
#   # which generates the combinations of GRIM-inconsistent values as determined
#   # by the respective `grim_*()` filter function:
#   raster <- purrr::cross2(frac_sequence, n_sequence, .filter = grim_filter_func)
#
#   # Turn the raster from a list into a numeric vector:
#   raster <- as.numeric(unlist(raster))
#
#   # In the `raster` vector, `n` and `frac` values alternate. They are teased
#   # apart here with an internal helper that goes in steps of 2 in both calls,
#   # but starts at different points. In this way, `n` captures all values with
#   # even index numbers, while `frac` captures those with odd index numbers. The
#   # two calls separately parcel out the two underlying vectors:
#   n    <- parcel_nth_elements(raster, n = 2, from = 2)
#   frac <- parcel_nth_elements(raster, n = 2, from = 1)
#
#   # Finally, return both vectors in a list:
#   list(n, frac)
# }
#
#
#
# # Remember:
# # grim_raster_1_up_or_down %>%
# #   parcel_nth_elements(n = 2) %>%
# #   `==`(as.numeric(grim_raster_1_up_or_down_frac))
#
#
#
#
# # Generate the rasters ----------------------------------------------------
#
# grim_raster_1_up_or_down       <- generate_grim_raster(1, "up_or_down")
# grim_raster_1_up               <- generate_grim_raster(1, "up")
# grim_raster_1_down             <- generate_grim_raster(1, "down")
# grim_raster_1_even             <- generate_grim_raster(1, "even")
# grim_raster_1_ceiling_or_floor <- generate_grim_raster(1, "ceiling_or_floor")
# grim_raster_1_ceiling          <- generate_grim_raster(1, "ceiling")
# grim_raster_1_floor            <- generate_grim_raster(1, "floor")
# grim_raster_1_trunc            <- generate_grim_raster(1, "trunc")
# grim_raster_1_anti_trunc       <- generate_grim_raster(1, "anti_trunc")
#
# grim_raster_2_up_or_down       <- generate_grim_raster(2, "up_or_down")
# grim_raster_2_up               <- generate_grim_raster(2, "up")
# grim_raster_2_down             <- generate_grim_raster(2, "down")
# grim_raster_2_even             <- generate_grim_raster(2, "even")
# grim_raster_2_ceiling_or_floor <- generate_grim_raster(2, "ceiling_or_floor")
# grim_raster_2_ceiling          <- generate_grim_raster(2, "ceiling")
# grim_raster_2_floor            <- generate_grim_raster(2, "floor")
# grim_raster_2_trunc            <- generate_grim_raster(2, "trunc")
# grim_raster_2_anti_trunc       <- generate_grim_raster(2, "anti_trunc")
#
#
#
# # Divide the rasters into their `n` and `frac` components -----------------
#
# # For 1 decimal place:
#
# # `n`
# grim_raster_1_up_or_down_n            <- grim_raster_1_up_or_down[[1]]
# grim_raster_1_up_n                    <- grim_raster_1_up[[1]]
# grim_raster_1_down_n                  <- grim_raster_1_down[[1]]
# grim_raster_1_even_n                  <- grim_raster_1_even[[1]]
# grim_raster_1_ceiling_or_floor_n      <- grim_raster_1_ceiling_or_floor[[1]]
# grim_raster_1_ceiling_n               <- grim_raster_1_ceiling[[1]]
# grim_raster_1_floor_n                 <- grim_raster_1_floor[[1]]
# grim_raster_1_trunc_n                 <- grim_raster_1_trunc[[1]]
# grim_raster_1_anti_trunc_n            <- grim_raster_1_anti_trunc[[1]]
#
# # `frac`
# grim_raster_1_up_or_down_frac         <- grim_raster_1_up_or_down[[2]]
# grim_raster_1_up_frac                 <- grim_raster_1_up[[2]]
# grim_raster_1_down_frac               <- grim_raster_1_down[[2]]
# grim_raster_1_even_frac               <- grim_raster_1_even[[2]]
# grim_raster_1_ceiling_or_floor_frac   <- grim_raster_1_ceiling_or_floor[[2]]
# grim_raster_1_ceiling_frac            <- grim_raster_1_ceiling[[2]]
# grim_raster_1_floor_frac              <- grim_raster_1_floor[[2]]
# grim_raster_1_trunc_frac              <- grim_raster_1_trunc[[2]]
# grim_raster_1_anti_trunc_frac         <- grim_raster_1_anti_trunc[[2]]
#
# # For 2 decimal places:
#
# # `n`
# grim_raster_2_up_or_down_n            <- grim_raster_2_up_or_down[[1]]
# grim_raster_2_up_n                    <- grim_raster_2_up[[1]]
# grim_raster_2_down_n                  <- grim_raster_2_down[[1]]
# grim_raster_2_even_n                  <- grim_raster_2_even[[1]]
# grim_raster_2_ceiling_or_floor_n      <- grim_raster_2_ceiling_or_floor[[1]]
# grim_raster_2_ceiling_n               <- grim_raster_2_ceiling[[1]]
# grim_raster_2_floor_n                 <- grim_raster_2_floor[[1]]
# grim_raster_2_trunc_n                 <- grim_raster_2_trunc[[1]]
# grim_raster_2_anti_trunc_n            <- grim_raster_2_anti_trunc[[1]]
#
# # `frac`
# grim_raster_2_up_or_down_frac         <- grim_raster_2_up_or_down[[2]]
# grim_raster_2_up_frac                 <- grim_raster_2_up[[2]]
# grim_raster_2_down_frac               <- grim_raster_2_down[[2]]
# grim_raster_2_even_frac               <- grim_raster_2_even[[2]]
# grim_raster_2_ceiling_or_floor_frac   <- grim_raster_2_ceiling_or_floor[[2]]
# grim_raster_2_ceiling_frac            <- grim_raster_2_ceiling[[2]]
# grim_raster_2_floor_frac              <- grim_raster_2_floor[[2]]
# grim_raster_2_trunc_frac              <- grim_raster_2_trunc[[2]]
# grim_raster_2_anti_trunc_frac         <- grim_raster_2_anti_trunc[[2]]
#
#
#
#
# # Save data:
#
# usethis::use_data(
#
#   grim_raster_1_up_or_down_n,
#   grim_raster_1_up_n,
#   grim_raster_1_down_n,
#   grim_raster_1_even_n,
#   grim_raster_1_ceiling_or_floor_n,
#   grim_raster_1_ceiling_n,
#   grim_raster_1_floor_n,
#   grim_raster_1_trunc_n,
#   grim_raster_1_anti_trunc_n,
#
#   grim_raster_1_up_or_down_frac,
#   grim_raster_1_up_frac,
#   grim_raster_1_down_frac,
#   grim_raster_1_even_frac,
#   grim_raster_1_ceiling_or_floor_frac,
#   grim_raster_1_ceiling_frac,
#   grim_raster_1_floor_frac,
#   grim_raster_1_trunc_frac,
#   grim_raster_1_anti_trunc_frac,
#
#   grim_raster_2_up_or_down_n,
#   grim_raster_2_up_n,
#   grim_raster_2_down_n,
#   grim_raster_2_even_n,
#   grim_raster_2_ceiling_or_floor_n,
#   grim_raster_2_ceiling_n,
#   grim_raster_2_floor_n,
#   grim_raster_2_trunc_n,
#   grim_raster_2_anti_trunc_n,
#
#   grim_raster_2_up_or_down_frac,
#   grim_raster_2_up_frac,
#   grim_raster_2_down_frac,
#   grim_raster_2_even_frac,
#   grim_raster_2_ceiling_or_floor_frac,
#   grim_raster_2_ceiling_frac,
#   grim_raster_2_floor_frac,
#   grim_raster_2_trunc_frac,
#   grim_raster_2_anti_trunc_frac,
#
#
#   internal = TRUE
#
# )




