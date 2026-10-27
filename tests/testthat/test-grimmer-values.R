test_that("`grimmer_values()` returns the achievable pairs", {
  out <- 5.23 |> grimmer_values(2.55, 31, digits_x = 2, digits_sd = 2, items_averaged = 1)
  out |> names() |> expect_equal(c("case", "x", "sd", "sum", "sum_squares"))
  out |> nrow() |> expect_equal(1L)
  out$case |> expect_equal(1L)
  out$x  |> expect_equal(out$sum / 31)
  out$sd |> expect_equal(sqrt((out$sum_squares - out$sum^2 / 31) / 30))
  out$x  |> round(2) |> expect_equal(5.23)
  out$sd |> round(2) |> expect_equal(2.55)
  # No achievable pair at all if the value set is inconsistent:
  out <- 5.23 |> grimmer_values(2.55, 35, digits_x = 2, digits_sd = 2, items_averaged = 1)
  out |> nrow()  |> expect_equal(0L)
  out |> names() |> expect_equal(c("case", "x", "sd", "sum", "sum_squares"))
})


test_that("`grimmer_values()` agrees with a brute force", {
  # Every (sum, sum of squares) pair that `n` whole numbers can have, for means
  # from 1 to 5 and SDs up to 2.5; see `grimmer_reachable_pairs()`:
  pairs <- grimmer_reachable_pairs()
  eps <- 1e-9

  expected <- function(x, sd, n, items, sd_meaning) {
    s <- pairs[[n]]$s
    q <- pairs[[n]]$q
    sd_exact <- sqrt((q - s^2 / n) / sd_denominator(n, sd_meaning)) / items
    keep <- abs(s / (n * items) - x) <= 0.05 + eps & abs(sd_exact - sd) <= 0.05 + eps
    tibble::tibble(sum = s[keep], sum_squares = q[keep]) |> dplyr::arrange(sum, sum_squares)
  }

  n_checked <- 0L
  n_consistent <- 0L

  for (sd_meaning in c("sd_sample", "sd_pop", "se")) {
    for (items in 1:2) {
      # Means of totals from 1 to 5 and their SDs up to 2.5, which the pairs
      # cover completely, at one decimal place:
      grid <- expand.grid(
        x = if (items == 1) seq(1, 5, by = 0.3) else seq(0.5, 2.5, by = 0.2),
        sd = if (items == 1) seq(0, 2.4, by = 0.3) else seq(0, 1.2, by = 0.2),
        n = 2:8
      )
      for (i in seq_len(nrow(grid))) {
        x <- grid$x[i]
        sd <- grid$sd[i]
        n <- grid$n[i]
        info <- paste0("x = ", x, ", sd = ", sd, ", n = ", n, ", items = ", items, ", ", sd_meaning)
        truth <- expected(x, sd, n, items, sd_meaning)
        out <- x |>
          grimmer_values(sd, n, digits_x = 1, digits_sd = 1, items_averaged = items, sd_meaning = sd_meaning)
        out |> dplyr::select(sum, sum_squares) |> expect_equal(truth, info = info)
        out$x  |> expect_equal(out$sum / (n * items), info = info)
        out$sd |> expect_equal(sqrt((out$sum_squares - out$sum^2 / n) / sd_denominator(n, sd_meaning)) / items, info = info)
        n_checked <- n_checked + 1L
        n_consistent <- n_consistent + (nrow(out) > 0L)
      }
    }
  }

  # Guard against the loops silently collapsing to nothing:
  n_checked |> expect_gt(4000L)
  n_consistent |> expect_gt(1000L)
})


test_that("`grimmer_values()` has a pair exactly where `grimmer()` is `TRUE`", {
  # Not `"even"`: its bounds admit ties that `base::round()` sends the other
  # way, as in `grim_values()`.
  for (rounding in c("up_or_down", "up", "down", "ceiling", "floor", "trunc")) {
    for (n in c(12, 30, 200)) {
      for (sd in c(0.5, 0.7, 2.55)) {
        info <- paste0("sd = ", sd, ", n = ", n, ", rounding = ", rounding)
        out <- 5.3 |> grimmer_values(sd, n, digits_x = 1, digits_sd = 2, items_averaged = 1, rounding = rounding)
        consistency <- 5.3 |> grimmer(sd, n, digits_x = 1, digits_sd = 2, items_averaged = 1, rounding = rounding)
        out |> nrow() |> call_on(\(x) x > 0L) |> expect_equal(consistency, info = info)
        # Every pair would be reported as `x` and `sd`:
        for (value in out$x) {
          value |> roundwork::reround(digits = 1, rounding = rounding) |> round(10L) |> expect_contains(5.3)
        }
        for (value in out$sd) {
          value |> roundwork::reround(digits = 2, rounding = rounding) |> round(10L) |> expect_contains(sd)
        }
      }
    }
  }
})


test_that("the Guéguen (2012) value sets have the pairs found by brute force", {
  # From `special-scripts/grimmest.md`: 7 pairs for 5.3 (0.7) and 4 for
  # 5.9 (0.5), 15 combinations for 5.1 (0.6) and 5.6 (0.5), `n = 30` each:
  values <- function(x, sd) {
    x |> grimmer_values(sd, 30, digits_x = 1, digits_sd = 1, items_averaged = 1) |> nrow()
  }
  values(5.3, 0.7) |> expect_equal(7L)
  values(5.9, 0.5) |> expect_equal(4L)
  (values(5.1, 0.6) * values(5.6, 0.5)) |> expect_equal(15L)
})


test_that("`grimmer_values()` is vectorized like `grimmer()`", {
  out <- c(5.23, 5.23) |> grimmer_values(2.55, c(31, 35), digits_x = 2, digits_sd = 2, items_averaged = 1)
  # One row for the first value set, none for the inconsistent second one:
  out |> tibble::is_tibble() |> expect_true()
  out$case |> expect_equal(1L)

  numeric(0) |> grimmer_values(numeric(0), 31, digits_x = 2, digits_sd = 2, items_averaged = 1) |> expect_equal(tibble::tibble(case = integer(), x = double(), sd = double(), sum = double(), sum_squares = double()))
  5.23 |> grimmer_values(2.55, 31, digits_x = 2, digits_sd = 2, items_averaged = 1, rounding = c("up", "floor")) |> expect_error("must be length 1")
  c(5.23, 5.1) |> grimmer_values(2.55, c(31, 35, 40), digits_x = 2, digits_sd = 2, items_averaged = 1) |> expect_error()
})


test_that("`sd_meaning = \"sd_sample_or_pop\"` is an error", {
  5.23 |>
    grimmer_values(2.55, 31, digits_x = 2, digits_sd = 2, items_averaged = 1, sd_meaning = "sd_sample_or_pop") |>
    expect_error("sd_sample_or_pop")
  5.23 |>
    grimmer_values(2.55, 31, digits_x = 2, digits_sd = 2, items_averaged = 1, sd_meaning = "foo") |>
    expect_error()
})


test_that("a single value is decidable under `sd_meaning = \"sd_pop\"` only", {
  out <- 3 |> grimmer_values(0, 1, digits_x = 0, digits_sd = 0, items_averaged = 1, sd_meaning = "sd_pop")
  out$sum         |> expect_equal(3)
  out$sum_squares |> expect_equal(9)
  3 |> grimmer_values(0, 1, digits_x = 0, digits_sd = 0, items_averaged = 1) |> expect_undecidable()
})


test_that("undecidable cases give one row of `NA`", {
  NA   |> grimmer_values(2.55, 31,   digits_x = 2, digits_sd = 2, items_averaged = 1)   |> expect_undecidable()
  5.23 |> grimmer_values(NA,   31,   digits_x = 2, digits_sd = 2, items_averaged = 1)   |> expect_undecidable()
  5.23 |> grimmer_values(2.55, 31.5, digits_x = 2, digits_sd = 2, items_averaged = 1)   |> expect_undecidable()
  5.23 |> grimmer_values(2.55, 31,   digits_x = 2, digits_sd = 2, items_averaged = 1.5) |> expect_undecidable()
  5.23 |> grimmer_values(2.55, 31,   digits_x = 2, digits_sd = NA, items_averaged = 1)  |> expect_undecidable()
})


test_that("`digits_*` and `items_averaged` are required, with the bespoke errors", {
  5.23 |> grimmer_values(2.55, 31, digits_sd = 2, items_averaged = 1) |> suppressMessages() |> expect_error("digits_x")
  5.23 |> grimmer_values(2.55, 31, digits_x = 2, items_averaged = 1)  |> suppressMessages() |> expect_error("digits_sd")
  5.23 |> grimmer_values(2.55, 31, digits_x = 2, digits_sd = 2)       |> suppressMessages() |> expect_error("items_averaged")
})


test_that("too many pairs is an error, not a huge allocation", {
  # Would be 5e8 rows; `check_enumeration_size()` only caps the sums:
  50 |> grimmer_values(20, 5000, digits_x = 0, digits_sd = 0, items_averaged = 1) |> expect_error("Too many achievable pairs")
  # Below the limit, the same shape of value set works. Counted independently in
  # whole numbers: 100 values have a sum `s` from 4950 to 5050 for a mean of 50
  # (ties either way), and a sum of squares `q` of the parity of `s` for an SD
  # of 20 if `100 * q - s^2` is from `99 * 100 * 19.5^2` to `99 * 100 * 20.5^2`.
  # That keeps `q` far above the least sum of squares, which does not bind:
  count <- 4950:5050 |>
    vapply(\(s) {
      q <- ceiling((s^2 + 99 * 100 * 19.5^2) / 100):floor((s^2 + 99 * 100 * 20.5^2) / 100)
      sum(q %% 2 == s %% 2)
    }, numeric(1L)) |>
    sum()
  50 |> grimmer_values(20, 100, digits_x = 0, digits_sd = 0, items_averaged = 1) |> nrow() |> expect_equal(count)

  # Two numbers have few sums of squares in a wide range, which only an upper
  # bound of 4e7 would count as too many. Two totals with the sum `s` and the
  # difference `d` have the sum of squares `(s^2 + d^2) / 2`, so every pair of
  # `s` and `d` of the same parity within the bounds is one achievable pair:
  d <- 0:3000
  d <- d[d^2 >= 2 * 19.5^2 * 100^2 & d^2 <= 2 * 20.5^2 * 100^2]
  expected <- tidyr::expand_grid(sum = 9900:10100, d = d) |>
    dplyr::filter(d %% 2 == sum %% 2) |>
    dplyr::mutate(sum_squares = (sum^2 + d^2) / 2) |>
    dplyr::select(-d) |>
    dplyr::arrange(sum, sum_squares)
  50 |>
    grimmer_values(20, 2, digits_x = 0, digits_sd = 0, items_averaged = 100) |>
    dplyr::select(sum, sum_squares) |>
    expect_equal(expected)
})


test_that("`sum_squares_attainable_all()` agrees with `sum_squares_attainable()`", {
  # Stepping through the range one attainable sum of squares at a time, with
  # chunks small enough that many of them are crossed:
  for (n in 2:4) {
    for (s in c(0, 1, 2, 7, 30)) {
      first <- s |> sum_squares_attainable(n, sum_squares_min(s, n), Inf)
      upper <- first + 2000
      expected <- first
      repeat {
        q <- s |> sum_squares_attainable(n, expected[length(expected)] + 2, upper)
        if (is.na(q)) break
        expected <- c(expected, q)
      }
      info <- paste0("n = ", n, ", s = ", s)
      s |> sum_squares_attainable_all(n, first, upper, count_max = Inf, chunk = 7) |> expect_equal(expected, info = info)
      s |> sum_squares_attainable_all(n, first, upper, count_max = 9,   chunk = 7) |> expect_equal(expected[1:10], info = info)
    }
  }
})


test_that("`loeschian_between()` agrees with `is_loeschian()`", {
  expected <- (0:3000)[vapply(0:3000, is_loeschian, logical(1L))]
  0    |> loeschian_between(3000) |> expect_equal(expected)
  1000 |> loeschian_between(1200) |> expect_equal(expected[expected >= 1000 & expected <= 1200])
  # Large values, where `sqrt()` is least exact near the ends of each run:
  m <- 1e10 + 0:200
  m[m |> vapply(is_loeschian, logical(1L))] |> expect_equal(loeschian_between(1e10, 1e10 + 200))
})


test_that("`sum_squares_attainable_all()` stops one past `count_max`", {
  # Four numbers summing to 1 have every odd sum of squares from 1 on:
  1 |> sum_squares_attainable_all(4, first = 1, upper = 1e9, count_max = 5) |> expect_equal(c(1, 3, 5, 7, 9, 11))
  1 |> sum_squares_attainable_all(4, first = 1, upper = 7,   count_max = 5) |> expect_equal(c(1, 3, 5, 7))
})
