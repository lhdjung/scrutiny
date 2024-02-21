
# NOTE: This file contains the source code for certain internal data. These are
# only directly available to the developer, not the user. They form the basis
# for the background raster vectors used within `grim_plot()`.

# Code for the example datasets with `pigs` in their names is in the
# R/data-doc.R file.

# Some nomenclature -- `n`: sample size, `frac`: fractional portion of a mean or
# fraction.



# Preparation -------------------------------------------------------------

# The script crucially relies on `purrr::cross2()`, but this function was
# deprecated (see https://github.com/lhdjung/scrutiny/issues/53). In case the
# installed version of purrr is so recent that `cross2()` is no longer part of
# it, install an older version that still includes the function:
cross2_still_in_purrr <- any(ls(asNamespace("purrr")) == "cross2")
if (!cross2_still_in_purrr) {
  # You may enter a more recent version if it still includes `cross2()`:
  purrr_with_cross2 <- "1.0.2"
  purrr_current <- packageVersion("purrr")
  remotes::install_version("purrr", version = purrr_with_cross2)
}

# Functions for GRIM-testing corresponding to every specific rounding procedure:

grim_up_or_down       <- function(...) grim_scalar(..., rounding = "up_or_down")
grim_up               <- function(...) grim_scalar(..., rounding = "up")
grim_down             <- function(...) grim_scalar(..., rounding = "down")
grim_even             <- function(...) grim_scalar(..., rounding = "even")
grim_up_from          <- function(...) grim_scalar(..., rounding = "up_from")
grim_down_from        <- function(...) grim_scalar(..., rounding = "down_from")
grim_ceiling_or_floor <- function(...) grim_scalar(..., rounding = "ceiling_or_floor")
grim_ceiling          <- function(...) grim_scalar(..., rounding = "ceiling")
grim_floor            <- function(...) grim_scalar(..., rounding = "floor")
grim_trunc            <- function(...) grim_scalar(..., rounding = "up")
grim_anti_trunc       <- function(...) grim_scalar(..., rounding = "up")



# This function returns a list of two vectors. The first is `n`, the second is
# `frac`. Together, these two vectors form the background raster in
# `grim_plot()`, with `n` on the x-axis and `frac` on the y-axis.

generate_grim_raster <- function(digits, rounding, n = NULL) {

  # The `digits` argument is processed in GRIM-typical fashion, such that, e.g.,
  # `digits = 2` leads to a fractional unit (`frac_unit`) of 0.01:
  p10 <- 10 ^ digits
  frac_unit <- 1 / p10

  # The sample size, `n`, should be equal to `p10` because this is the
  # right-hand limit of the y-axis, corresponding to the number of decimal
  # places:
  if (is.null(n)) {
    n <- p10
  } else {
    cli::cli_warn(c(
      "Typically, `n` in `generate_grim_raster()` is not \\
      meant to be specified.",
      "!" = "Are you sure you have an uncommon use case that requires it?"
    ))
  }

  # Produce `n` and `frac` sequences of equal length, corresponding to the x and
  # y axes later in the plot:
  frac_sequence <- seq_endpoint(from = frac_unit, to = (1 - frac_unit))
  n_sequence <- seq_len(n)

  # Assemble the name of the `grim_*()` function that will be used as a filter
  # below. This goes by `rounding`. For example, if `rounding = even`, then
  # `grim_filter_func` will be `grim_even` -- a function, not a string:
  grim_filter_func <- eval(rlang::parse_expr(paste0("grim_", rounding)))

  # The essential workhorse within the present function is `purrr::cross2()`,
  # which generates the combinations of GRIM-inconsistent values as determined
  # by the respective `grim_*()` filter function:

  raster <- purrr::cross2(frac_sequence, n_sequence, .filter = grim_filter_func)

  # Turn the raster from a list into a numeric vector:
  raster <- as.numeric(unlist(raster))

  # In the `raster` vector, `n` and `frac` values alternate. They are teased
  # apart here with an internal helper that goes in steps of 2 in both calls,
  # but starts at different points. In this way, `n` captures all values with
  # even index numbers, while `frac` captures those with odd index numbers. The
  # two calls separately parcel out the two underlying vectors:
  n    <- parcel_nth_elements(raster, n = 2, from = 2)
  frac <- parcel_nth_elements(raster, n = 2, from = 1)

  # Finally, return both vectors in a list:
  list(as.integer(n), frac)
}



# Generate the rasters ----------------------------------------------------

grim_raster_1_up_or_down       <- generate_grim_raster(1, "up_or_down")
grim_raster_1_up               <- generate_grim_raster(1, "up")
grim_raster_1_down             <- generate_grim_raster(1, "down")
grim_raster_1_even             <- generate_grim_raster(1, "even")
grim_raster_1_ceiling_or_floor <- generate_grim_raster(1, "ceiling_or_floor")
grim_raster_1_ceiling          <- generate_grim_raster(1, "ceiling")
grim_raster_1_floor            <- generate_grim_raster(1, "floor")
grim_raster_1_trunc            <- generate_grim_raster(1, "trunc")
grim_raster_1_anti_trunc       <- generate_grim_raster(1, "anti_trunc")

grim_raster_2_up_or_down       <- generate_grim_raster(2, "up_or_down")
grim_raster_2_up               <- generate_grim_raster(2, "up")
grim_raster_2_down             <- generate_grim_raster(2, "down")
grim_raster_2_even             <- generate_grim_raster(2, "even")
grim_raster_2_ceiling_or_floor <- generate_grim_raster(2, "ceiling_or_floor")
grim_raster_2_ceiling          <- generate_grim_raster(2, "ceiling")
grim_raster_2_floor            <- generate_grim_raster(2, "floor")
grim_raster_2_trunc            <- generate_grim_raster(2, "trunc")
grim_raster_2_anti_trunc       <- generate_grim_raster(2, "anti_trunc")



# Divide the rasters into their `n` and `frac` components -----------------

# For 1 decimal place:

# `n`
grim_raster_1_up_or_down_n            <- grim_raster_1_up_or_down[[1]]
grim_raster_1_up_n                    <- grim_raster_1_up[[1]]
grim_raster_1_down_n                  <- grim_raster_1_down[[1]]
grim_raster_1_even_n                  <- grim_raster_1_even[[1]]
grim_raster_1_ceiling_or_floor_n      <- grim_raster_1_ceiling_or_floor[[1]]
grim_raster_1_ceiling_n               <- grim_raster_1_ceiling[[1]]
grim_raster_1_floor_n                 <- grim_raster_1_floor[[1]]
grim_raster_1_trunc_n                 <- grim_raster_1_trunc[[1]]
grim_raster_1_anti_trunc_n            <- grim_raster_1_anti_trunc[[1]]

# `frac`
grim_raster_1_up_or_down_frac         <- grim_raster_1_up_or_down[[2]]
grim_raster_1_up_frac                 <- grim_raster_1_up[[2]]
grim_raster_1_down_frac               <- grim_raster_1_down[[2]]
grim_raster_1_even_frac               <- grim_raster_1_even[[2]]
grim_raster_1_ceiling_or_floor_frac   <- grim_raster_1_ceiling_or_floor[[2]]
grim_raster_1_ceiling_frac            <- grim_raster_1_ceiling[[2]]
grim_raster_1_floor_frac              <- grim_raster_1_floor[[2]]
grim_raster_1_trunc_frac              <- grim_raster_1_trunc[[2]]
grim_raster_1_anti_trunc_frac         <- grim_raster_1_anti_trunc[[2]]

# For 2 decimal places:

# `n`
grim_raster_2_up_or_down_n            <- grim_raster_2_up_or_down[[1]]
grim_raster_2_up_n                    <- grim_raster_2_up[[1]]
grim_raster_2_down_n                  <- grim_raster_2_down[[1]]
grim_raster_2_even_n                  <- grim_raster_2_even[[1]]
grim_raster_2_ceiling_or_floor_n      <- grim_raster_2_ceiling_or_floor[[1]]
grim_raster_2_ceiling_n               <- grim_raster_2_ceiling[[1]]
grim_raster_2_floor_n                 <- grim_raster_2_floor[[1]]
grim_raster_2_trunc_n                 <- grim_raster_2_trunc[[1]]
grim_raster_2_anti_trunc_n            <- grim_raster_2_anti_trunc[[1]]

# `frac`
grim_raster_2_up_or_down_frac         <- grim_raster_2_up_or_down[[2]]
grim_raster_2_up_frac                 <- grim_raster_2_up[[2]]
grim_raster_2_down_frac               <- grim_raster_2_down[[2]]
grim_raster_2_even_frac               <- grim_raster_2_even[[2]]
grim_raster_2_ceiling_or_floor_frac   <- grim_raster_2_ceiling_or_floor[[2]]
grim_raster_2_ceiling_frac            <- grim_raster_2_ceiling[[2]]
grim_raster_2_floor_frac              <- grim_raster_2_floor[[2]]
grim_raster_2_trunc_frac              <- grim_raster_2_trunc[[2]]
grim_raster_2_anti_trunc_frac         <- grim_raster_2_anti_trunc[[2]]



# Remember / test:
grim_raster_1_up_or_down[[2]] == scrutiny:::grim_raster_1_up_or_down_frac



# Save data ---------------------------------------------------------------

usethis::use_data(

  grim_raster_1_up_or_down_n,
  grim_raster_1_up_n,
  grim_raster_1_down_n,
  grim_raster_1_even_n,
  grim_raster_1_ceiling_or_floor_n,
  grim_raster_1_ceiling_n,
  grim_raster_1_floor_n,
  grim_raster_1_trunc_n,
  grim_raster_1_anti_trunc_n,

  grim_raster_1_up_or_down_frac,
  grim_raster_1_up_frac,
  grim_raster_1_down_frac,
  grim_raster_1_even_frac,
  grim_raster_1_ceiling_or_floor_frac,
  grim_raster_1_ceiling_frac,
  grim_raster_1_floor_frac,
  grim_raster_1_trunc_frac,
  grim_raster_1_anti_trunc_frac,

  grim_raster_2_up_or_down_n,
  grim_raster_2_up_n,
  grim_raster_2_down_n,
  grim_raster_2_even_n,
  grim_raster_2_ceiling_or_floor_n,
  grim_raster_2_ceiling_n,
  grim_raster_2_floor_n,
  grim_raster_2_trunc_n,
  grim_raster_2_anti_trunc_n,

  grim_raster_2_up_or_down_frac,
  grim_raster_2_up_frac,
  grim_raster_2_down_frac,
  grim_raster_2_even_frac,
  grim_raster_2_ceiling_or_floor_frac,
  grim_raster_2_ceiling_frac,
  grim_raster_2_floor_frac,
  grim_raster_2_trunc_frac,
  grim_raster_2_anti_trunc_frac,


  internal = TRUE,
  overwrite = TRUE

)


# Reminder message after running the script:
if (cross2_still_in_purrr) {
  cli::cli_inform(c(
    "v" = "Installed version of purrr is fine!"
  ))
} else {
  cli::cli_warn(c(
    "!" = "Don't forget to reinstall the current version of purrr!",
    "i" = "Installed before script was run: purrr {purrr_current}",
    "x" = "Installed now: purrr {purrr_with_cross2}"
  ))
}

