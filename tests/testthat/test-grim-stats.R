test_that("`grim_total()` returns correct values", {
  grim_total(5.30, 20:30, digits_x = 2) |> expect_equal(80:70)
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
  grim_probability(5.30, 90:110, digits_x = 2) |> expect_equal(exp2)
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
