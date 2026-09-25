test_that("`grim_total()` returns correct values", {
  grim_total(5.30, 20:30, digits_x = 2, rounding = "up") |> expect_equal(80:70)
})


test_that("`grim_total()` works with percentage conversion", {
  grim_total(87.50, 45:55, digits_x = 2, percent = TRUE) |> expect_equal(9955:9945)
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
  grim_ratio(5.30, 20:30, digits_x = 2) |> expect_equal(exp1)
})

test_that("`grim_probability()` returns correct values", {
  grim_probability(5.30, 90:110, digits_x = 2, rounding = "up") |>
    expect_equal(exp2)
})


test_that("`grim_ratio()` works with percentage conversion", {
  grim_ratio(87.50, 45:55, digits_x = 2, percent = TRUE) |> expect_equal(exp3)
})


# Errors ------------------------------------------------------------------

test_that("all functions error if `digits_x` is missing", {
  grim_probability(5.30, 20) |> expect_error()
  grim_ratio(5.30, 20)       |> expect_error()
  grim_total(5.30, 20:30)    |> expect_error()
})


test_that("`grim_total()` doesn't overflow the integer range", {
  # `as.integer(1e10)` is `NA` with a warning, so the count of possible
  # inconsistencies used to come out as no count at all.
  grim_total(5.30, 40, digits_x = 10) |> expect_equal(1e10 - 40)
  grim_total(5.30, 40, digits_x = 10) |> expect_no_warning()
})


test_that("`grim_total()` returns a double whatever the count", {
  # The type must not depend on how large the count happens to be, or on which
  # element of a vectorized call is the largest.
  grim_total(5.30, 40, digits_x = 2)  |> expect_type("double")
  grim_total(5.30, 40, digits_x = 10) |> expect_type("double")
  grim_total(5.30, 40, digits_x = c(2, 10)) |> expect_type("double")
  grim_total(5.30, 40, digits_x = 8, percent = TRUE) |> expect_type("double")
})


test_that("`grim_probability()` is a probability", {
  # A non-positive `n` leaves nothing to test, which `grim()` reports as `NA`.
  # The formula returned 1 or more there, so the `probability` column of
  # `grim_map()` used to state a probability of 1.03 next to a verdict of `NA`.
  grim_probability(5.19, n = 0, digits_x = 2) |> expect_na()
  grim_probability(5.19, n = -3, digits_x = 2) |> expect_na()
  grim_map(tibble::tibble(x = 5.19, n = -3L), digits_x = 2)$probability |>
    expect_na()
  # Ordinary cases are untouched, and never above 1 or below 0:
  grim_probability(5.19, n = 28, digits_x = 2) |> expect_equal(0.72)
  grim_probability(5.19, n = 200, digits_x = 2) |> expect_equal(0)
  # `grim_ratio()` is the unclamped one and keeps saying so:
  grim_ratio(5.19, n = 200, digits_x = 2) |> expect_equal(-1)
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
    grim_total(1, n, digits, rounding = rounding) |>
      expect_equal(tabulate(GRIM_RASTERS[[key]]$n, nbins = length(n)))
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
      out <- grim_map(
        one_decimal, 1, items = items, rounding = rounding, threshold = 3
      )
      out$probability |> expect_equal(ave(!out$consistency, out$n))
    }
    out <- grim_map(
      percentage, 0, percent = TRUE, rounding = rounding, threshold = 3
    )
    out$probability |> expect_equal(ave(!out$consistency, out$n))
  }
})


test_that("`grim_probability()` and `grim_total()` are `NA` where `grim()` is", {
  # Never negative, and `NA` wherever `grim()` is:
  grim_total(5, 200, 2) |> expect_equal(0)
  grim_total(5.14, 83, 2, items = 0.5) |> expect_na()
  grim_map(
    tibble::tibble(x = c(Inf, NA, 5.19), n = c(20L, 20L, 28L)),
    digits_x = 2
  )$probability |>
    expect_equal(c(NA, NA, 0.72))
})
