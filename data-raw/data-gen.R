# NOTE: This file generates the package's internal data -- the GRIM grids that
# `grim_grid()` returns and `grim_plot_grid()` draws as its background raster.
# The ones for 1 or 2 decimal places and every rounding method without a
# `threshold` are precomputed here, on the developer's machine, and saved to
# R/sysdata.rda. `grim_grid()` computes the others on demand with the same
# `generate_grim_grid()`.
#
# Run this file with the package loaded (`pkgload::load_all()`), and rerun it
# after any change to `grim_scalar()` or to the rounding functions it calls.
#
# Code for the example datasets with `pigs` in their names is in data-raw/pigs.R.

# Some nomenclature -- `n`: sample size, `frac`: fractional portion of a mean or
# percentage.

# Generate all the grids ---------------------------------------------------

# Every rounding method without a `threshold`. The `"up_from"` family is absent:
# those methods take a `threshold`, so there is no single grid to precompute.
# The `"ties_*"` methods are shorthand that `resolve_ties_rounding()` turns into
# these names before the lookup.
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

# Keyed by `"{digits}_{rounding}"`, the key that `grim_grid()` assembles from
# its arguments. Named like a constant because `use_data()` below saves it as
# one.
GRIM_GRIDS <- keys |>
  purrr::pmap(generate_grim_grid) |>
  purrr::set_names(rounding_ids)


# Save data ----------------------------------------------------------------

usethis::use_data(GRIM_GRIDS, internal = TRUE, overwrite = TRUE)

# Cleanup. Also remove the memory version of `GRIM_GRIDS` here; the object is
# no longer needed because the data were saved in R/sysdata.rda
rm(rounding_methods, keys, rounding_ids, GRIM_GRIDS)
