test_that("`grim_total()` returns correct values", {
  5.30 |> grim_total(20:30, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "up") |> expect_equal(80:70)
})


test_that("`grim_total()` works with percentage conversion", {
  87.50 |> grim_total(45:55, digits_x = 2, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |> expect_equal(9955:9945)
})


exp2 <- c(
  0.1,
  0.09,
  0.08,
  0.07,
  0.06,
  0.05,
  0.04,
  0.03,
  0.02,
  0.01,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0
)

test_that("`grim_exp_incon()` returns correct values", {
  5.30 |> grim_exp_incon(90:110, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "up") |> expect_equal(exp2)
})


# Errors ------------------------------------------------------------------

test_that("all functions error if `digits_x` is missing", {
  5.30 |> grim_exp_incon(20, items_averaged = 1, data_type = "integer") |> expect_error()
  5.30 |> grim_total(20:30, items_averaged = 1, data_type = "integer")    |> expect_error()
})


test_that("`grim_total()` doesn't overflow the integer range", {
  # `as.integer(1e10)` is `NA` with a warning, so the count of possible
  # inconsistencies used to come out as no count at all.
  5.30 |> grim_total(40, digits_x = 10, items_averaged = 1, data_type = "integer") |> expect_equal(1e10 - 40)
  5.30 |> grim_total(40, digits_x = 10, items_averaged = 1, data_type = "integer") |> expect_no_warning()
})


test_that("`grim_total()` returns a double whatever the count", {
  # The type must not depend on how large the count happens to be, or on which
  # element of a vectorized call is the largest.
  5.30 |> grim_total(40, digits_x = 2, items_averaged = 1, data_type = "integer")                 |> expect_type("double")
  5.30 |> grim_total(40, digits_x = 10, items_averaged = 1, data_type = "integer")                |> expect_type("double")
  5.30 |> grim_total(40, digits_x = c(2, 10), items_averaged = 1, data_type = "integer")          |> expect_type("double")
  5.30 |> grim_total(40, digits_x = 8, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |> expect_type("double")
})


test_that("`grim_exp_incon()` is a probability", {
  # A non-positive `n` leaves nothing to test, which `grim()` reports as `NA`.
  # The formula returned 1 or more there, so the `exp_incon` column of
  # `grim_map()` used to state a probability of 1.03 next to a verdict of `NA`.
  5.19 |> grim_exp_incon(n = 0, digits_x = 2, items_averaged = 1, data_type = "integer")  |> expect_na()
  5.19 |> grim_exp_incon(n = -3, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_na()
  tibble::tibble(x = 5.19, n = -3L) |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("exp_incon") |>
    expect_na()
  # Ordinary cases are untouched, and never above 1 or below 0:
  5.19 |> grim_exp_incon(n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")  |> expect_equal(0.72)
  5.19 |> grim_exp_incon(n = 200, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_equal(0)
})


test_that("`grim_total()` counts the cells of every GRIM grid", {
  # `GRIM_GRIDS` holds every GRIM-inconsistent (`n`, `frac`) cell of the
  # grids with one and two decimal places, found by `grim()` itself in
  # data-raw/data-gen.R. Counting the cells per `n` is therefore `grim_total()`
  # done by brute force, for every rounding method and every `n` up to the one
  # where GRIM stops ruling anything out.
  for (key in names(GRIM_GRIDS)) {
    digits <- as.integer(sub("_.*", "", key))
    rounding <- sub("^\\d+_", "", key)
    n <- seq_len(10^digits)
    expected <- GRIM_GRIDS[[key]]$n |> tabulate(nbins = length(n))
    1 |> grim_total(n, digits, rounding = rounding, items_averaged = 1, data_type = "integer") |> expect_equal(expected)
  }
})


test_that("`grim_map()`'s `exp_incon` is the share its `consistency` rules out", {
  # The live counterpart to the grid test above: every mean with one decimal
  # place at every `n` up to 10, and every mean with no decimal places as a
  # percentage at a few `n`, under every rounding method -- including those
  # that take a `threshold`, which have no cached grid -- with and without
  # `items`.
  one_decimal <- tidyr::expand_grid(n = 1:10, x = (0:9) / 10)
  percentage <- tidyr::expand_grid(n = c(1, 7, 8, 40, 64, 99), x = 0:99)
  roundings <- c(
    "up_or_down", "up", "down", "even", "ceiling_or_floor", "ceiling",
    "floor", "trunc", "anti_trunc", "up_from", "down_from",
    "up_from_or_down_from"
  )
  for (rounding in roundings) {
    for (items in c(1, 3)) {
      out <- one_decimal |> grim_map(digits_x = 1, items_averaged = items, data_type = "integer", rounding = rounding, threshold = 3)
      out$exp_incon |> expect_equal(ave(!out$consistency, out$n))
    }
    out <- percentage |> grim_map(digits_x = 0, items_averaged = 1, data_type = "integer", x_meaning = "percentage", rounding = rounding, threshold = 3)
    out$exp_incon |> expect_equal(ave(!out$consistency, out$n))
  }
})


test_that("`grim_exp_incon()` and `grim_total()` are `NA` where `grim()` is", {
  # Never negative, and `NA` wherever `grim()` is:
  5    |> grim_total(200, 2, items_averaged = 1, data_type = "integer") |> expect_equal(0)
  5.14 |> grim_total(83, 2, items_averaged = 0.5, data_type = "integer") |> expect_na()
  tibble::tibble(x = c(Inf, NA, 5.19), n = c(20L, 20L, 28L)) |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("exp_incon") |>
    expect_equal(c(NA, NA, 0.72))
})


test_that("`grim_total()` rejects a string `x` but not a missing one", {
  "5.19"      |> grim_total(20, 2, items_averaged = 1, data_type = "integer")       |> expect_error("must be numeric as of scrutiny 1.0.0")
  "5.19"      |> grim_exp_incon(20, 2, items_averaged = 1, data_type = "integer") |> expect_error("must be numeric as of scrutiny 1.0.0")
  # Exports new since 0.6.1 never took strings, so they say so plainly:
  "5.19"      |> grim_values(20, 2, items_averaged = 1, data_type = "integer")    |> expect_error("`x` must be numeric.\n")
  NA          |> grim_exp_incon(20, 2, items_averaged = 1, data_type = "integer") |> expect_na()
  c(NA, 5.19) |> grim_total(20, 2, items_averaged = 1, data_type = "integer")       |> expect_equal(c(NA, 80))
})


test_that("`grim_total()` rejects a fractional `digits_x`, as `grim()` does", {
  5 |> grim_total(20, digits_x = 2.5, items_averaged = 1, data_type = "integer") |> expect_error("whole numbers")
})


test_that("`digits_x` must be from 0 to 15, as in `grim()`", {
  5.19 |> grim_exp_incon(28, digits_x = -1, items_averaged = 1, data_type = "integer") |> expect_error("0 to 15")
  5.19 |> grim_total(28, digits_x = 16, items_averaged = 1, data_type = "integer")       |> expect_error("0 to 15")
})


test_that("`grim_exp_incon()` recycles like `grim()`", {
  c(5, 6) |> grim_exp_incon(c(21, 40, 50), digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("Can't recycle")
  c(5, 6) |> grim_exp_incon(c(21, 40), digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_length(2L)
  5 |> grim_exp_incon(numeric(0), digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_length(0L)
})


test_that("`grim_probability()` is deprecated", {
  lifecycle::expect_deprecated(grim_probability(5.19, 28, digits_x = 2, items_averaged = 1, data_type = "integer"))
})


test_that("near-whole arguments are counted as the whole numbers they are", {
  # The common divisor behind the count of ties needs whole numbers; `100 *
  # 0.56` gave 0.44 rather than 0.4, and `1 - 1e-9` was undecidable:
  1.5 |> grim_exp_incon(100 * 0.56, digits_x = 2, items_averaged = 1, data_type = "integer")     |> expect_equal(grim_exp_incon(1.5, 56, digits_x = 2, items_averaged = 1, data_type = "integer"))
  1.5 |> grim_exp_incon(1 - 1e-9, digits_x = 2, items_averaged = 1, data_type = "integer")       |> expect_equal(grim_exp_incon(1.5, 1, digits_x = 2, items_averaged = 1, data_type = "integer"))
  1.5 |> grim_exp_incon(40, digits_x = 2 - 1e-9, items_averaged = 1, data_type = "integer")      |> expect_equal(grim_exp_incon(1.5, 40, digits_x = 2, items_averaged = 1, data_type = "integer"))
  1.5 |> grim_total(40, digits_x = 2, items_averaged = 2 - 1e-9, data_type = "integer")          |> expect_equal(grim_total(1.5, 40, digits_x = 2, items_averaged = 2, data_type = "integer"))
  1.5 |> grim_exp_incon(40, digits_x = 1 - 1e-9, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |> expect_equal(grim_exp_incon(1.5, 40, digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = "percentage"))
})


test_that("type mistakes are errors, as in `grim()`", {
  1 |> grim_total(n = "10", digits_x = 2, items_averaged = 1, data_type = "integer")         |> expect_error("`n` must be one of these types")
  1 |> grim_total(n = 7, digits_x = 2, items_averaged = "1", data_type = "integer")          |> expect_error("`items_averaged` must be one of these types")
  # Missing values are undecidable, whatever their type. This was an error:
  1 |> grim_total(n = 7, digits_x = 2, items_averaged = NA_character_, data_type = "integer") |> expect_na()
  1 |> grim_exp_incon(10, digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = NA)                       |> expect_error("`x_meaning`")
  1 |> grim_exp_incon(10, digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = c("mean", "mean"))       |> expect_error("`x_meaning`")
  1 |> grim_total(10, digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = "percent")                   |> expect_error("percentage")
})


test_that("counts beyond 2^53 come without a precision warning", {
  # `10^17 %% 7` lost all accuracy; the true count is `10^17 - 7`:
  1 |> grim_total(7, digits_x = 15, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |> expect_no_warning()
  1 |> grim_total(7, digits_x = 15, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |> expect_equal(1e17 - 7)
  # Below 2^53, the count is exact:
  1 |> grim_total(7, digits_x = 13, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |> expect_identical(1e15 - 7)
  # 2^14 means, half of them on a tie that is consistent both ways:
  1 |> grim_total(2^14, digits_x = 13, items_averaged = 1, data_type = "integer")              |> expect_identical(1e13 - 2^14 - 2^13)
})

test_that("`grim_total()` counts exactly where the steps pass 2^53", {
  # With `x_meaning = "percentage"`, the offset times `n` passed 2^53 and the count was
  # one too few or too many; the true counts are `1e16 - n` and the other:
  1 |> grim_total(1862956302252229, 14, 1, x_meaning = "percentage", rounding = "floor", data_type = "integer")                       |> expect_equal(1e16 - 1862956302252229)
  1 |> grim_total(2126481517639647, 14, 1, x_meaning = "percentage", rounding = "down_from", threshold = 1.25, data_type = "integer") |> expect_equal(1e16 - 2126481517639647)
})

test_that("`grim_total()` refuses sizes where its remainders lose accuracy", {
  # This returned 0 with a base R warning about the modulus:
  1.5 |> grim_total(2^53 - 1, 0, 1, data_type = "integer") |> expect_error("too large for an exact count")
  1.5 |> grim_total(2^51, 0, 2, data_type = "integer")     |> expect_error("too large for an exact count")
})

test_that("`grim_total()` and `grim_exp_incon()` check `rounding` themselves", {
  # roundwork's message named its own `symmetric` and `unround()`:
  1.5 |> grim_total(20, 1, 1, rounding = "bogus", data_type = "integer")     |> expect_error("must be one of the rounding methods")
  1.5 |> grim_exp_incon(20, 1, 1, rounding = "bogus", data_type = "integer") |> expect_error("must be one of the rounding methods")
})


test_that("`data_type` is required, and continuous data give 0", {
  5.19 |> grim_exp_incon(28, 2, 1) |> expect_error("Need to specify `data_type`")
  5.19 |> grim_total(28, 2, 1)     |> expect_error("Need to specify `data_type`")
  5.19 |> grim_total(28, 2, 1, c("integer", "continuous", NA))  |> expect_equal(c(72, 0, NA))
  5.19 |> grim_total(500, 2, 1, c("integer", "continuous", NA)) |> expect_equal(c(0, 0, 0))
  # `NA` where `grim()` is, as for no data at all:
  c(5.2, 5.2, Inf) |> grim_total(c(0, NA, 10), 1, 1, "continuous") |> expect_equal(c(NA, 0, NA))
  # Like `grim()`, which is `TRUE` there; it used to divide 0 by `10^NA`:
  5.19 |> grim_exp_incon(28, NA, 1, c("integer", "continuous")) |> expect_equal(c(NA, 0))
  5.19 |> grim_total(28, 2, 1, NULL)              |> expect_error("must not be `NULL`")
  5.19 |> grim_total(28, 2, NULL, "integer")      |> expect_error("must not be `NULL`")
  5.19 |> grim_total(28, 2, 1, factor("integer")) |> expect_error("must be a character vector")
})


test_that("a missing `x` or `n` gets the package's own error", {
  # It was R's "argument "x" is missing", naming an internal `get()` call:
  grim_total(n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")      |> expect_error("Need to specify `x`")
  grim_total(x = 1.5, digits_x = 2, items_averaged = 1, data_type = "integer")     |> expect_error("Need to specify `n`")
  grim_exp_incon(n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")  |> expect_error("Need to specify `x`")
  grim_exp_incon(x = 1.5, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("Need to specify `n`")
})
