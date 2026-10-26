# Exact oracle: with `P` the percentage times `10^digits` as an integer, the
# share `100 * absolute / n` rounds to it iff `2 * (absolute * 100 * 10^digits -
# P * n)` lies within `n` of 0 -- ties included on both sides for "up_or_down",
# only below for "up".
oracle <- function(absolute, percentage, n, digits, rounding) {
  scale <- 10^digits
  gap <- 2 * (absolute * 100 * scale - round(percentage * scale) * n)
  if (rounding == "up_or_down") abs(gap) <= n else -n <= gap & gap < n
}

test_that("`is_percentage_of()` agrees with an exact oracle", {
  grid <- expand.grid(n = 1:60, absolute = 0:60, digits = 0:2)
  grid <- grid[grid$absolute <= grid$n, ]
  for (rounding in c("up_or_down", "up")) {
    # Each share as rounded, then one step off:
    for (digits in 0:2) {
      rows <- grid$digits == digits
      reported <- round(grid$absolute[rows] / grid$n[rows] * 100, digits)
      for (offset in c(0, 10^-digits)) {
        p <- reported + offset
        actual   <- grid$absolute[rows] |> is_percentage_of(p, grid$n[rows], digits, rounding = rounding)
        expected <- grid$absolute[rows] |> oracle(p, grid$n[rows], digits, rounding)
        actual |> expect_equal(expected)
      }
    }
  }
})

test_that("a percentage is GRIM-consistent iff some count produces it", {
  n <- 37
  percentage <- seq(0, 100, by = 0.1)
  some_count <- percentage |> vapply(\(p) any(is_percentage_of(0:n, p, n, 1)), logical(1L))
  percentage |> grim(n, digits_x = 1, items_averaged = 1, percent = TRUE) |> expect_equal(some_count)
})

test_that("`is_percentage_of()` is `NA` for undecidable cases", {
  c(NA, 1.5, -1, 1) |> is_percentage_of(50, c(2, 2, 2, 0), 0) |> expect_equal(rep(NA, 4))
  1 |> is_percentage_of(NA, 2, 0) |> expect_equal(NA)
  1 |> is_percentage_of(50, 2, NA) |> expect_equal(NA)
})

test_that("`is_percentage_of()` requires `digits_percentage`", {
  40 |> is_percentage_of(33, 120) |> expect_error("digits_percentage")
  "40" |> is_percentage_of(33, 120, 0) |> expect_error("absolute")
})

test_that("`is_percentage_of_map()` tests each row", {
  df <- tibble::tibble(absolute = c(40, 37, 25), percentage = c(33, 31, 20), n = 120)
  out <- df |> is_percentage_of_map(digits_percentage = 0)
  out$consistency |> expect_equal(c(TRUE, TRUE, FALSE))
  out |> expect_s3_class("scrutiny_is_percentage_of_map")
  out |> audit() |> getElement("incon_cases") |> expect_equal(1L)
  df |> is_percentage_of_map() |> expect_error("digits_percentage")
})

test_that("`is_percentage_of_map_seq()` disperses all three columns", {
  df <- tibble::tibble(absolute = 25, percentage = 20, n = 120)
  out <- df |> is_percentage_of_map_seq(digits_percentage = 0)
  out$var |> unique() |> expect_equal(c("absolute", "percentage", "n"))
  # 24 out of 120 is exactly 20%:
  out[out$var == "absolute" & out$consistency, ]$absolute |> expect_equal(24)
  out |> audit_seq() |> getElement("hits_absolute") |> expect_equal(1L)
})
