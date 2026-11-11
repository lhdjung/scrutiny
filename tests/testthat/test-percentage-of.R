# Exact oracle: with `P` the percentage times `10^digits` as an integer, the
# share `100 * absolute / n` rounds to it iff `2 * (absolute * 100 * 10^digits -
# P * n)` lies within `n` of 0 -- ties included on both sides for "up_or_down",
# only below for "up".
oracle <- function(absolute, percentage, n, digits, rounding) {
  scale <- 10^digits
  gap <- 2 * (absolute * 100 * scale - round(percentage * scale) * n)
  if (rounding == "up_or_down") abs(gap) <= n else -n <= gap & gap < n
}

test_that("`percentage_of()` agrees with an exact oracle", {
  grid <- expand.grid(n = 1:60, absolute = 0:60, digits = 0:2)
  grid <- grid[grid$absolute <= grid$n, ]
  for (rounding in c("up_or_down", "up")) {
    # Each share as rounded, then one step off:
    for (digits in 0:2) {
      rows <- grid$digits == digits
      reported <- round(grid$absolute[rows] / grid$n[rows] * 100, digits)
      for (offset in c(0, 10^-digits)) {
        p <- reported + offset
        actual   <- grid$absolute[rows] |> percentage_of(p, grid$n[rows], digits, rounding = rounding)
        expected <- grid$absolute[rows] |> oracle(p, grid$n[rows], digits, rounding)
        actual |> expect_equal(expected)
      }
    }
  }
})

test_that("a percentage is GRIM-consistent iff some count produces it", {
  n <- 37
  percentage <- seq(0, 100, by = 0.1)
  some_count <- percentage |> vapply(\(p) any(percentage_of(0:n, p, n, 1)), logical(1L))
  percentage |> grim(n, digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |> expect_equal(some_count)
})

test_that("`percentage_of()` is `NA` for undecidable cases", {
  c(NA, 1.5, -1, 1) |> percentage_of(50, c(2, 2, 2, 0), 0) |> expect_equal(rep(NA, 4))
  1 |> percentage_of(NA, 2, 0) |> expect_equal(NA)
  1 |> percentage_of(33, 3, NA) |> expect_equal(NA)
})

test_that("a missing `absolute` is `FALSE` if no count has the percentage", {
  # This was `NA`, although GRIM rules out 33.4% of 120:
  NA |> percentage_of(33.4, 120, 1)  |> expect_false()
  NA |> percentage_of(101.0, 120, 1) |> expect_false()
  NA |> percentage_of(33.3, 120, 1)  |> expect_equal(NA)
  # `NA` exactly where GRIM finds some count, over every percentage with one
  # decimal place:
  percentage <- seq(0, 100, by = 0.1)
  some_count <- percentage |> grim(37, digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = "percentage")
  NA |> percentage_of(percentage, 37, 1) |> is.na() |> expect_equal(some_count)
})

test_that("`percentage_of()` requires `digits_percentage`", {
  40 |> percentage_of(33, 120) |> expect_error("digits_percentage")
  "40" |> percentage_of(33, 120, 0) |> expect_error("absolute")
})

test_that("`percentage_of_map()` tests each row", {
  df <- tibble::tibble(absolute = c(40, 37, 25), percentage = c(33, 31, 20), n = 120)
  out <- df |> percentage_of_map(digits_percentage = 0)
  out$consistency |> expect_equal(c(TRUE, TRUE, FALSE))
  out |> expect_s3_class("scrutiny_percentage_of_map")
  out |> audit() |> getElement("incon_cases") |> expect_equal(1L)
  df |> percentage_of_map() |> expect_error("digits_percentage")
})

test_that("`percentage_of_map_seq()` disperses all three columns", {
  df <- tibble::tibble(absolute = 25, percentage = 20, n = 120)
  out <- df |> percentage_of_map_seq(digits_percentage = 0)
  out$var |> unique() |> expect_equal(c("absolute", "percentage", "n"))
  # 24 out of 120 is exactly 20%:
  out[out$var == "absolute" & out$consistency, ]$absolute |> expect_equal(24)
  out |> audit() |> getElement("hits_absolute") |> expect_equal(1L)
})

test_that("an abbreviated key argument is an error, not a partial match", {
  df <- tibble::tibble(absolute = 40, percentage = 33, n = 120)
  df |> percentage_of_map(digits_percentage = 0, percent = TRUE) |> expect_error("`percent` is not an argument")
  df |> percentage_of_map(digits_percentage = 0, percentage = "percentage") |> expect_no_error()
})


test_that("a near-whole count or decimal count is tested as the whole number", {
  # `absolute` passed the whole-number check but was compared to the range of
  # counts as is, so `100 * 0.29` was not 29. A near-whole `digits_percentage`
  # was used as is, too:
  (100 * 0.29) |> percentage_of(72.5, 40, digits_percentage = 1)        |> expect_true()
  10           |> percentage_of(31.3, 32, digits_percentage = 1 - 1e-9) |> expect_true()
  tibble::tibble(absolute = 100 * 0.29, percentage = 72.5, n = 40) |> percentage_of_map(digits_percentage = 1) |> _$consistency |> expect_true()
})

test_that("a count too large for exact arithmetic is an error", {
  # 100 * 6622 / 12791 is 51.770776327104996..., which rounds to ...710:
  6622 |> percentage_of(51.77077632711, 12791, digits_percentage = 11) |> expect_error("too large for an exact test")
  6622 |> percentage_of(51.77,          12791, digits_percentage = 2)  |> expect_true()
})

test_that("a count above its total is tested as any other", {
  # It was `NA`, so `audit_seq()` counted no hits near `absolute == n`:
  c(121, 121) |> percentage_of(c(100.8, 100.9), 120, 1) |> expect_equal(c(TRUE, FALSE))
  130         |> percentage_of(108.3, 120, 1)           |> expect_true()
  120         |> percentage_of(100, 120, 1)             |> expect_true()
  tibble::tibble(absolute = 118, percentage = 97, n = 120) |> percentage_of_map_seq(digits_percentage = 0) |> audit() |> _$hits_total |> expect_equal(4L)
})


test_that("a missing `digits_percentage` or `n` that can't change the verdict doesn't make it `NA`", {
  # As in `grim()`: inconsistent with the decimal places shown, or consistent
  # with any. These were `NA`:
  c(5, 30) |> percentage_of(71.3, 43, NA) |> expect_equal(c(FALSE, FALSE))
  NA       |> percentage_of(71.3, 43, NA) |> expect_false()
  1        |> percentage_of(50, 2, NA)    |> expect_true()
  # No `n` makes 5 a share of 71.3%, and 0 is a share of 0% out of any `n`:
  5        |> percentage_of(71.3, NA, 1)  |> expect_false()
  # The guard against products past 2^53 was too strict by a factor of the
  # upper bound, so these were `NA` although no `n` gives the percentage:
  20000    |> percentage_of(64.877, NA, 3)  |> expect_false()
  139      |> percentage_of(64.877, NA, 4)  |> expect_false()
  1328     |> percentage_of(64.877, NA, 3, rounding = "up_from", threshold = 2.5) |> expect_false()
  1e15     |> percentage_of(64.877, NA, 3)  |> expect_equal(NA)
  0        |> percentage_of(0, NA, 1)     |> expect_true()
  0        |> percentage_of(1, NA, 1)     |> expect_false()
  0        |> percentage_of(0, NA, NA)    |> expect_true()
  # Some `n` passes, others fail:
  40       |> percentage_of(33.3, NA, 1)  |> expect_equal(NA)
  NA       |> percentage_of(33.3, NA, 1)  |> expect_equal(NA)
  # No count gives a negative share, out of any `n`; this was `NA`:
  NA       |> percentage_of(-5, NA, 0)    |> expect_false()
  NA       |> percentage_of(0, NA, 0)     |> expect_equal(NA)

  # Every `n` that the verdict rules out fails:
  set.seed(sweep_seed(1107L))
  for (i in 1:200) {
    absolute <- sample(0:30, 1)
    digits <- sample(0:2, 1)
    percentage <- round(runif(1, 0, 100), digits)
    verdict <- percentage_of(absolute, percentage, NA, digits)
    if (!is.na(verdict)) {
      absolute |> percentage_of(percentage, 1:3000, digits) |> expect_equal(rep(verdict, 3000))
    }
  }
})
