test_that("`grim_total()` returns correct values", {
  5.30 |> grim_total(20:30, digits_x = 2, rounding = "up") |> expect_equal(80:70)
})


test_that("`grim_total()` works with percentage conversion", {
  87.50 |> grim_total(45:55, digits_x = 2, percent = TRUE) |> expect_equal(9955:9945)
})


exp1 <- seq(from = 0.8, to = 0.7, by = -0.01)
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
exp3 <- seq(from = 0.9955, to = 0.9945, by = -0.0001)

test_that("`grim_ratio()` returns correct values", {
  5.30 |> grim_ratio(20:30, digits_x = 2) |> expect_equal(exp1)
})

test_that("`grim_probability()` returns correct values", {
  5.30 |> grim_probability(90:110, digits_x = 2, rounding = "up") |> expect_equal(exp2)
})


test_that("`grim_ratio()` works with percentage conversion", {
  87.50 |> grim_ratio(45:55, digits_x = 2, percent = TRUE) |> expect_equal(exp3)
})


# Errors ------------------------------------------------------------------

test_that("all functions error if `digits_x` is missing", {
  5.30 |> grim_probability(20) |> expect_error()
  5.30 |> grim_ratio(20)       |> expect_error()
  5.30 |> grim_total(20:30)    |> expect_error()
})


test_that("`grim_total()` doesn't overflow the integer range", {
  # `as.integer(1e10)` is `NA` with a warning, so the count of possible
  # inconsistencies used to come out as no count at all.
  5.30 |> grim_total(40, digits_x = 10) |> expect_equal(1e10 - 40)
  5.30 |> grim_total(40, digits_x = 10) |> expect_no_warning()
})


test_that("`grim_total()` returns a double whatever the count", {
  # The type must not depend on how large the count happens to be, or on which
  # element of a vectorized call is the largest.
  5.30 |> grim_total(40, digits_x = 2)                 |> expect_type("double")
  5.30 |> grim_total(40, digits_x = 10)                |> expect_type("double")
  5.30 |> grim_total(40, digits_x = c(2, 10))          |> expect_type("double")
  5.30 |> grim_total(40, digits_x = 8, percent = TRUE) |> expect_type("double")
})


test_that("`grim_probability()` is a probability", {
  # A non-positive `n` leaves nothing to test, which `grim()` reports as `NA`.
  # The formula returned 1 or more there, so the `probability` column of
  # `grim_map()` used to state a probability of 1.03 next to a verdict of `NA`.
  5.19 |> grim_probability(n = 0, digits_x = 2)  |> expect_na()
  5.19 |> grim_probability(n = -3, digits_x = 2) |> expect_na()
  tibble::tibble(x = 5.19, n = -3L) |>
    grim_map(digits_x = 2) |>
    purrr::pluck("probability") |>
    expect_na()
  # Ordinary cases are untouched, and never above 1 or below 0:
  5.19 |> grim_probability(n = 28, digits_x = 2)  |> expect_equal(0.72)
  5.19 |> grim_probability(n = 200, digits_x = 2) |> expect_equal(0)
  # `grim_ratio()` is the unclamped one and keeps saying so:
  5.19 |> grim_ratio(n = 200, digits_x = 2) |> expect_equal(-1)
})


test_that("`grim_total()` counts the cells of every GRIM raster", {
  # `GRIM_RASTERS` holds every GRIM-inconsistent (`n`, `frac`) cell of the
  # grids with one and two decimal places, found by `grim()` itself in
  # data-raw/data-gen.R. Counting the cells per `n` is therefore `grim_total()`
  # done by brute force, for every rounding method and every `n` up to the one
  # where GRIM stops ruling anything out.
  for (key in names(GRIM_RASTERS)) {
    digits <- as.integer(sub("_.*", "", key))
    rounding <- sub("^\\d+_", "", key)
    n <- seq_len(10^digits)
    expected <- GRIM_RASTERS[[key]]$n |> tabulate(nbins = length(n))
    1 |> grim_total(n, digits, rounding = rounding) |> expect_equal(expected)
  }
})


test_that("`grim_map()`'s `probability` is the share its `consistency` rules out", {
  # The live counterpart to the raster test above: every mean with one decimal
  # place at every `n` up to 10, and every mean with no decimal places as a
  # percentage at a few `n`, under every rounding method -- including those
  # that take a `threshold`, which have no raster -- with and without `items`.
  one_decimal <- tidyr::expand_grid(n = 1:10, x = (0:9) / 10)
  percentage <- tidyr::expand_grid(n = c(1, 7, 8, 40, 64, 99), x = 0:99)
  roundings <- c(
    "up_or_down", "up", "down", "even", "ceiling_or_floor", "ceiling",
    "floor", "trunc", "anti_trunc", "up_from", "down_from",
    "up_from_or_down_from"
  )
  for (rounding in roundings) {
    for (items in c(1, 3)) {
      out <- one_decimal |> grim_map(digits_x = 1, items = items, rounding = rounding, threshold = 3)
      out$probability |> expect_equal(ave(!out$consistency, out$n))
    }
    out <- percentage |> grim_map(digits_x = 0, percent = TRUE, rounding = rounding, threshold = 3)
    out$probability |> expect_equal(ave(!out$consistency, out$n))
  }
})


test_that("`grim_probability()` and `grim_total()` are `NA` where `grim()` is", {
  # Never negative, and `NA` wherever `grim()` is:
  5    |> grim_total(200, 2)              |> expect_equal(0)
  5.14 |> grim_total(83, 2, items = 0.5) |> expect_na()
  tibble::tibble(x = c(Inf, NA, 5.19), n = c(20L, 20L, 28L)) |>
    grim_map(digits_x = 2) |>
    purrr::pluck("probability") |>
    expect_equal(c(NA, NA, 0.72))
})


test_that("`grim_total()` rejects a string `x` but not a missing one", {
  "5.19"  |> grim_total(20, 2)       |> expect_error("must be one of these types")
  "5.19"  |> grim_probability(20, 2) |> expect_error("must be one of these types")
  NA      |> grim_probability(20, 2) |> expect_na()
  c(NA, 5.19) |> grim_total(20, 2)   |> expect_equal(c(NA, 80))
})


test_that("`grim_total()` rejects a fractional `digits_x`, as `grim()` does", {
  5 |> grim_total(20, digits_x = 2.5) |> expect_error("whole numbers")
})
