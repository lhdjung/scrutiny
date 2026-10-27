test_that("`grimmer_values()` returns the achievable pairs", {
  out <- 5.23 |> grimmer_values(2.55, 31, digits_x = 2, digits_sd = 2, items_averaged = 1) |> purrr::pluck(1L)
  out |> names() |> expect_equal(c("x", "sd", "sum", "sum_squares"))
  out |> nrow() |> expect_equal(1L)
  out$x  |> expect_equal(out$sum / 31)
  out$sd |> expect_equal(sqrt((out$sum_squares - out$sum^2 / 31) / 30))
  out$x  |> round(2) |> expect_equal(5.23)
  out$sd |> round(2) |> expect_equal(2.55)
  # No achievable pair at all if the value set is inconsistent:
  out <- 5.23 |> grimmer_values(2.55, 35, digits_x = 2, digits_sd = 2, items_averaged = 1) |> purrr::pluck(1L)
  out |> nrow()  |> expect_equal(0L)
  out |> names() |> expect_equal(c("x", "sd", "sum", "sum_squares"))
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
          grimmer_values(sd, n, digits_x = 1, digits_sd = 1, items_averaged = items, sd_meaning = sd_meaning) |>
          purrr::pluck(1L)
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
        out[[1L]] |> nrow() |> call_on(\(x) x > 0L) |> expect_equal(consistency, info = info)
        # Every pair would be reported as `x` and `sd`:
        for (value in out[[1L]]$x) {
          value |> roundwork::reround(digits = 1, rounding = rounding) |> round(10L) |> expect_contains(5.3)
        }
        for (value in out[[1L]]$sd) {
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
    x |> grimmer_values(sd, 30, digits_x = 1, digits_sd = 1, items_averaged = 1) |> purrr::pluck(1L) |> nrow()
  }
  values(5.3, 0.7) |> expect_equal(7L)
  values(5.9, 0.5) |> expect_equal(4L)
  (values(5.1, 0.6) * values(5.6, 0.5)) |> expect_equal(15L)
})


test_that("`grimmer_values()` is vectorized like `grimmer()`", {
  out <- c(5.23, 5.23) |> grimmer_values(2.55, c(31, 35), digits_x = 2, digits_sd = 2, items_averaged = 1)
  out |> expect_type("list")
  out |> expect_length(2L)
  out |> purrr::map_int(nrow) |> expect_equal(c(1L, 0L))

  numeric(0) |> grimmer_values(numeric(0), 31, digits_x = 2, digits_sd = 2, items_averaged = 1) |> expect_identical(list())
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
  out <- 3 |> grimmer_values(0, 1, digits_x = 0, digits_sd = 0, items_averaged = 1, sd_meaning = "sd_pop") |> purrr::pluck(1L)
  out$sum         |> expect_equal(3)
  out$sum_squares |> expect_equal(9)
  3 |> grimmer_values(0, 1, digits_x = 0, digits_sd = 0, items_averaged = 1) |> expect_equal(list(NA))
})


test_that("undecidable cases give `NA`", {
  NA   |> grimmer_values(2.55, 31,   digits_x = 2, digits_sd = 2, items_averaged = 1)   |> expect_equal(list(NA))
  5.23 |> grimmer_values(NA,   31,   digits_x = 2, digits_sd = 2, items_averaged = 1)   |> expect_equal(list(NA))
  5.23 |> grimmer_values(2.55, 31.5, digits_x = 2, digits_sd = 2, items_averaged = 1)   |> expect_equal(list(NA))
  5.23 |> grimmer_values(2.55, 31,   digits_x = 2, digits_sd = 2, items_averaged = 1.5) |> expect_equal(list(NA))
  5.23 |> grimmer_values(2.55, 31,   digits_x = 2, digits_sd = NA, items_averaged = 1)  |> expect_equal(list(NA))
})


test_that("`digits_*` and `items_averaged` are required, with the bespoke errors", {
  5.23 |> grimmer_values(2.55, 31, digits_sd = 2, items_averaged = 1) |> suppressMessages() |> expect_error("digits_x")
  5.23 |> grimmer_values(2.55, 31, digits_x = 2, items_averaged = 1)  |> suppressMessages() |> expect_error("digits_sd")
  5.23 |> grimmer_values(2.55, 31, digits_x = 2, digits_sd = 2)       |> suppressMessages() |> expect_error("items_averaged")
})
