# NOTE: This file generates the package's internal data -- the background
# rasters that `grim_plot()` draws behind the value sets. They are precomputed
# here, on the developer's machine, and saved to R/sysdata.rda; the package
# never computes them at run time.
#
# Run this file with the package loaded (`pkgload::load_all()`), and rerun it
# after any change to `grim_scalar()` or to the rounding functions it calls.
#
# Code for the example datasets with `pigs` in their names is in R/data-doc.R.

# Some nomenclature -- `n`: sample size, `frac`: fractional portion of a mean or
# percentage.

# Generate one raster ------------------------------------------------------

# A raster is the set of (`n`, `frac`) grid points that are GRIM-*in*consistent:
# those are the ones drawn as background tiles. `digits` fixes the grid in both
# directions -- `frac` steps by `1 / 10^digits`, and `n` runs up to `10^digits`,
# which is the sample size at which GRIM stops ruling anything out and the
# right-hand limit of the plot's x-axis.
generate_grim_raster <- function(digits, rounding) {
  p10 <- 10^digits
  frac <- seq_len(p10 - 1L) / p10

  grid <- tibble::tibble(
    frac = rep(frac, times = p10),
    n = rep(seq_len(p10), each = p10 - 1L)
  )

  # A single vectorized `grim()` call is sufficient for the grid. `n` is a
  # positive integer and `frac` is finite throughout, so `NA` is not an issue.
  consistency <- grim(
    x = grid$frac,
    n = grid$n,
    digits_x = digits,
    rounding = rounding
  )

  grid[!consistency, ]
}


# Generate all the rasters -------------------------------------------------

# Every rounding method that `grim_plot()` can show a raster for. The
# `"up_from"` family is absent on purpose: those methods take a `threshold`, so
# there is no single raster to precompute. The `"ties_*"` methods are shorthand
# that `resolve_ties_rounding()` turns into these names before the lookup.
rounding_methods <- c(
  "up_or_down",
  "up",
  "down",
  "even",
  "ceiling_or_floor",
  "ceiling",
  "floor",
  "trunc",
  "anti_trunc"
)

keys <- tidyr::expand_grid(
  digits = 1:2,
  rounding = rounding_methods
)

# Identify combinations of number of digits and rounding method by name
rounding_ids <- paste(keys$digits, keys$rounding, sep = "_")

# Keyed by `"{digits}_{rounding}"`, i.e., the key `grim_plot()` assembles from
# the decimal count and the `scrutiny_rounding_*` class of its input. Named like
# a constant because it will actually be saved as one by `use_data()` below.
GRIM_RASTERS <- keys |>
  purrr::pmap(generate_grim_raster) |>
  purrr::set_names(rounding_ids)


# Save data ----------------------------------------------------------------

usethis::use_data(GRIM_RASTERS, internal = TRUE, overwrite = TRUE)

# Cleanup. Also remove the memory version of `GRIM_RASTERS` here; the object is
# no longer needed because the data were saved in R/sysdata.rda
rm(generate_grim_raster, rounding_methods, keys, rounding_ids, GRIM_RASTERS)
