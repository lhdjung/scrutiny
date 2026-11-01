# NOTE: This file generates the package's internal data -- the background
# rasters that `grim_plot()` draws behind the value sets. They are precomputed
# here, on the developer's machine, and saved to R/sysdata.rda.
# `grim_raster()` computes the others on demand with the same
# `generate_grim_raster()`.
#
# Run this file with the package loaded (`pkgload::load_all()`), and rerun it
# after any change to `grim_scalar()` or to the rounding functions it calls.
#
# Code for the example datasets with `pigs` in their names is in data-raw/pigs.R.

# Some nomenclature -- `n`: sample size, `frac`: fractional portion of a mean or
# percentage.

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
rm(rounding_methods, keys, rounding_ids, GRIM_RASTERS)
