# Package-level constants, in SCREAMING_SNAKE_CASE so that a name elsewhere is
# recognizable as one on sight. The file is named `aaa.R` because roxygen2 sorts
# unconstrained files alphabetically into `Collate`, which puts this one first.
#
# They are evaluated at install time, not load time. For the tolerance that is
# deliberate: `.Machine$double.eps` is `2.220446e-16` on every platform R
# runs on, and a verdict must not depend on the machine that reached it.

# The tolerance below which a difference from the nearest whole number counts as
# none, used by `is_whole_number()` and by the check that `digits_x` and
# `digits_sd` are whole.
#
# A constant rather than a default argument `.Machine$double.eps^0.5`, which
# would be evaluated on every call, costing more than the arithmetic it is a
# tolerance for.

WHOLE_NUMBER_TOLERANCE <- .Machine$double.eps^0.5


# The arguments of the tests that are whole numbers, `percentage_of()`'s
# among them. `round_whole()` rounds them where the tolerance accepts them, so
# that `100 * 0.29` is tested as 29, the number it is shown as, not as
# 28.999999999999996 (see `is_whole_number()`). Mappers return these columns as
# integer.

ARGS_WHOLE <- c(
  "n",
  "items_averaged",
  "digits_x",
  "digits_sd",
  "absolute",
  "digits_percentage"
)


# The values of `sd_meaning` in GRIMMER and DEBIT: what a value reported as an
# SD really is. The sample SD divides by `n - 1`, the population SD by `n`,
# and `"sd_sample_or_pop"` accepts a value set that passes under one or
# the other. `"se"` is the standard error of the mean, the sample SD divided by
# `sqrt(n)`, for an SE reported as if it were an SD. Each value is one premise,
# so a new one takes a row in `sd_denominator()`. The values of DEBIT's
# `formula`. And the values of GRIM's `x_meaning`: what `x` is, a mean or a
# percentage, which is tested as a proportion with two more decimal places.
#
# Every function with one of these arguments spells the values out as its
# default, so that they show in its usage. The `*_scalar()` functions check
# against the constants, which is cheaper than reading their own formals once
# per row. A test in test-utils.R requires every default to equal its constant.
SD_MEANINGS <- c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
DEBIT_FORMULAS <- c("exact", "mean_n")
X_MEANINGS <- c("mean", "percentage")

# The values of GRIM's and GRIMMER's `data_type`. Unlike the ones above, the
# argument has no default and takes one value per row, like `items_averaged`,
# so the functions don't spell these out; see `test_data_type()`.
DATA_TYPES <- c("integer", "continuous")


# The strings that `as.numeric()` reads as an infinity or `NaN`, once their
# letters are lowercased. `decimal_places()` and `decimal_places_scalar()` give
# them `NA` decimal places, via `is_non_finite_token()`.
NON_FINITE_TOKENS <- c(outer(
  c("", "+", "-"),
  c("inf", "infinity", "nan"),
  paste0
))


# Placeholder data for `grim_plot_grid()`'s tile layer when there is no
# raster, as with `digits > 2`:
TIBBLE_FRAC_N_ZERO <- tibble::new_tibble(list(frac = 0, n = 0L), nrow = 1L)

# The most rows that `grim_values()` and `grimmer_values()` return for one
# value set; see `check_values_size()`:
VALUES_MAX <- 1e6

# The most values that `grim_plot_line()` and `grimmer_plot_line()` test,
# one test each; see `comb_values()`:
COMB_TEETH_MAX <- 2e4

# The values on either side of the reported one that the zoomed plots show at
# least; see `comb_window()`:
COMB_ZOOM_STEPS <- 10L

# The values of `zoom` in `grim_plot_line()` and `grimmer_plot_line()`:
COMB_ZOOMS <- c("close", "medium", "wide")

# The most digits in the integer or decimal part that `restore_zeros()` writes
# out a number with an exponent to, such as `"1e-400"`, and the largest `width`
# it pads to; see `write_out_decimal()`:
WRITTEN_OUT_MAX <- 100000L

# The columns of `grim_values()` and `grimmer_values()` output, for the
# zero-row case; see `bind_cases()`:
PTYPE_GRIM_VALUES <- tibble::tibble(
  case = integer(),
  x = double(),
  n = double(),
  sum = double()
)
PTYPE_GRIMMER_VALUES <- tibble::tibble(
  case = integer(),
  x = double(),
  sd = double(),
  n = double(),
  sum = double(),
  sum_squares = double()
)


# The `diff_*` distances in `audit_seq()`, passed to `dplyr::across()` as one
# list. `x` is the number of dispersion steps between a reported value and its
# consistent neighbors, as in `grim_map_seq()`.
LIST_MIN_DISTANCE_FUNCTIONS <- list(
  # Absolute distance:
  function(x) {
    vapply(
      x,
      function(x) {
        if (all(is.numeric(x))) {
          min(abs(x), na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      numeric(1L),
      USE.NAMES = FALSE
    )
  },

  # Positive distance:
  function(x) {
    vapply(
      x,
      function(x) {
        if (all(is.numeric(x))) {
          min(x[x > 0L], na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      numeric(1L),
      USE.NAMES = FALSE
    )
  },

  # Negative distance:
  function(x) {
    vapply(
      x,
      function(x) {
        if (all(is.numeric(x))) {
          max(x[x < 0L], na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      numeric(1L),
      USE.NAMES = FALSE
    )
  }
)


# A number such as `"+.5"`, `"1.50E+2"`, or `"-1e-3"`: sign, integer digits,
# decimal digits, and an optional exponent.
PATTERN_DECIMAL_SPELLING <-
  "^([+-]?)([0-9]*)\\.?([0-9]*)(?:[eE]([+-]?[0-9]+))?$"

# All valid values of `rounding`, as roundwork accepts them. roundwork's own
# error for a wrong value only points to a vignette, so `check_rounding_args()`
# names all of them instead. A test in test-utils.R requires roundwork to accept
# each one.

# fmt: skip
ROUNDING_METHODS <- c(
  "up_or_down", "up_from_or_down_from", "ceiling_or_floor",
  "up", "down", "even", "up_from", "down_from",
  "ceiling", "floor", "trunc", "anti_trunc",
  "ties_up", "ties_down", "ties_away", "ties_zero", "ties_even"
)

# The colors of consistent and inconsistent value sets in `grim_plot_grid()`
# and `debit_plot()`, and of values that are attainable but not reported, like
# the consistent cells of `grim_plot_grid()`'s raster. The functions with color
# arguments spell these out as defaults, so that they show in their usage.
COLORS_CONSISTENCY <- c("TRUE" = "#0047ff", "FALSE" = "#ff2a00")
COLOR_ATTAINABLE <- "grey75"
