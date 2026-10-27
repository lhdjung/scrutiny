# The reference values below are what `rsprite2::GRIM_test(return_values =
# TRUE)` returns for the same inputs. rsprite2 finds them by walking outward
# from the rounded sum total one integer at a time, with a fudge factor;
# `sum_range()` derives the same set in closed form and in exact integer
# arithmetic.

test_that("`grim_values()` returns the achievable means and their sums", {
  out <- 5.19 |> grim_values(32, digits_x = 2, items_averaged = 1) |> purrr::pluck(1L)
  out |> expect_equal(tibble::tibble(x = 5.1875, sum = 166))
  out <- 5.19 |> grim_values(300, digits_x = 2, items_averaged = 1) |> purrr::pluck(1L)
  out |> expect_equal(tibble::tibble(x = c(1556, 1557, 1558) / 300, sum = c(1556, 1557, 1558)))
  # No achievable mean at all if the value set is GRIM-inconsistent:
  out <- 5.19 |> grim_values(28, digits_x = 2, items_averaged = 1) |> purrr::pluck(1L)
  out |> nrow()  |> expect_equal(0L)
  out |> names() |> expect_equal(c("x", "sum"))
})


test_that("`grim_closest()` returns the achievable mean nearest to `x`", {
  # The two sums straddling an inconsistent value set are 145 and 146; 145/28
  # is the closer of the two, and it is what rsprite2 reports (as 5.18):
  5.19 |> grim_closest(28, digits_x = 2, items_averaged = 1) |> expect_equal(145 / 28)
  5.19 |> grim_closest(28, digits_x = 2, items_averaged = 1) |> round(2) |> expect_equal(5.18)
  # For a consistent value set, it is one of the `grim_values()`:
  5.19 |> grim_closest(32, digits_x = 2, items_averaged = 1) |> expect_equal(5.1875)
  5.19 |> grim_closest(300, digits_x = 2, items_averaged = 1) |> expect_equal(5.19)
})


test_that("both functions are vectorized like `grim()`", {
  out <- c(5.19, 5.19, 5.19) |> grim_values(c(32, 300, 28), digits_x = 2, items_averaged = 1)
  out |> expect_type("list")
  out |> expect_length(3L)
  out |> purrr::map_int(nrow) |> expect_equal(c(1L, 3L, 0L))

  c(5.19, 5.19) |>
    grim_closest(c(32, 28), digits_x = 2, items_averaged = 1) |>
    expect_equal(c(5.1875, 145 / 28))
  5.19 |> grim_closest(25:30, digits_x = 2, items_averaged = 1) |> expect_length(6L)

  numeric(0) |> grim_values(numeric(0), digits_x = 2, items_averaged = 1)  |> expect_identical(list())
  numeric(0) |> grim_closest(numeric(0), digits_x = 2, items_averaged = 1) |> expect_identical(numeric(0))
  5.19 |> grim_closest(28, digits_x = 2, items_averaged = 1, rounding = c("up", "floor")) |> expect_error("must be length 1")
  5.19 |> grim_values(28, digits_x = 2, items_averaged = 1, percent = c(TRUE, FALSE))     |> expect_error("must be length 1")
  c(1.1, 1.2, 1.3) |> grim_closest(c(10, 20), digits_x = 1, items_averaged = 1) |> expect_error()
  5.19 |> grim_closest(28, items_averaged = 1) |> expect_error("digits_x")
})

test_that("`grim_closest()` keeps a consistent `x` to the means reported as `x`", {
  # 56 / 11 is closer to 5.1 than 57 / 11, but it is floored to 5.0:
  5.1 |> grim_closest(11, digits_x = 1, items_averaged = 1, rounding = "floor") |> expect_equal(57 / 11)
})


test_that("`grim_values()` agrees with `grim()` itself", {
  # Every achievable mean must be reported as `x` when rounded the same way,
  # and there must be as many of them as the value set is consistent:
  for (rounding in c("up_or_down", "up", "down", "ceiling", "floor", "trunc")) {
    for (n in c(28, 32, 300)) {
      for (x in c(5.19, 4.2, 0.24)) {
        values <- x |> grim_values(n, digits_x = 2, items_averaged = 1, rounding = rounding) |> purrr::pluck(1L)
        info <- paste0("x = ", x, ", n = ", n, ", rounding = ", rounding)
        consistency <- grim(x, n, digits_x = 2, items_averaged = 1, rounding = rounding)
        values |> nrow() |> call_on(\(x) x > 0L) |> expect_equal(consistency, info = info)
        for (value in values$x) {
          value |>
            roundwork::reround(digits = 2, rounding = rounding) |>
            purrr::pluck(1L) |>
            round(10L) |>
            expect_equal(x, info = info)
        }
      }
    }
  }
})


test_that("`grim_values()` matches the `sum_*` columns of `grim_map()`", {
  out <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, show_rec = TRUE)
  values <- grim_values(pigs1$x, pigs1$n, digits_x = 2, items_averaged = 1)
  counts <- pmax(0L, out$sum_upper - out$sum_lower + 1L)
  values |> purrr::map_int(nrow) |> expect_equal(as.integer(counts))
  for (i in which(out$consistency)) {
    values[[i]]$sum |> expect_equal(seq(out$sum_lower[i], out$sum_upper[i]))
    # `grim_closest()` is one of them wherever there are any:
    pigs1$x[i] |>
      grim_closest(pigs1$n[i], digits_x = 2, items_averaged = 1) |>
      expect_in(values[[i]]$x)
  }
})


test_that("`percent` returns values on the scale of `x`", {
  # Not on the decimal scale that GRIM works with internally, so that the
  # values can be read against `x`:
  closest <- grim_closest(71, 43, digits_x = 0, items_averaged = 1, percent = TRUE)
  closest |> expect_gt(70)
  closest |> expect_lt(74)
  # 40% of 5 is 2 people, so this is consistent and there is one way to get
  # there. The value comes back as 40, not as 0.4:
  40 |> grim_values(5, digits_x = 0, items_averaged = 1, percent = TRUE) |> purrr::pluck(1L) |> expect_equal(tibble::tibble(x = 40, sum = 2))
  40 |> grim_closest(5, digits_x = 0, items_averaged = 1, percent = TRUE) |> expect_equal(40)
})


test_that("`items_averaged` multiplies into the sample size", {
  2.84 |>
    grim_values(16, digits_x = 2, items_averaged = 2) |>
    purrr::pluck(1L) |>
    expect_equal(grim_values(2.84, 32, digits_x = 2, items_averaged = 1)[[1L]])
})


test_that("undecidable cases give one row of `NA`", {
  # A missing mean has no bounds to derive, as in `grim()`. (This used to be
  # tested with `rounding = "anti_trunc"` at a mean of zero, which had no
  # defined bounds either until `anti_trunc()` stopped sending zero away from
  # zero.)
  NA |> grim_values(40, digits_x = 2, items_averaged = 1) |> purrr::pluck(1L) |> expect_undecidable()
  NA |> grim_closest(40, digits_x = 2, items_averaged = 1) |> expect_equal(NA_real_)
})


test_that("`rounding = \"anti_trunc\"` at a mean of zero is decidable", {
  # Every non-zero value is taken away from zero, so a mean reported as 0.00
  # pins the sum to exactly 0 -- attainable only by all-zero data:
  0 |>
    grim_values(40, digits_x = 2, items_averaged = 1, rounding = "anti_trunc") |>
    purrr::pluck(1L) |>
    expect_equal(tibble::tibble(x = 0, sum = 0))
  0 |> grim_closest(40, digits_x = 2, items_averaged = 1, rounding = "anti_trunc") |> expect_equal(0)
})


test_that("`digits_x` is required, with the bespoke error message", {
  # `suppressMessages()` mutes the changelog hint that `error_digits_missing()`
  # prints via `on.exit()` as it unwinds:
  5.19 |> grim_values(28, items_averaged = 1)  |> suppressMessages() |> expect_error("digits_x")
  5.19 |> grim_closest(28, items_averaged = 1) |> suppressMessages() |> expect_error("digits_x")
})


test_that("`grim_values()` and `grim_closest()` are undecidable where `grim()` is", {
  5.19 |> grim(20.5, 2, items_averaged = 1)         |> is.na() |> expect_true()
  5.19 |> grim_values(20.5, 2, items_averaged = 1)  |> purrr::pluck(1L) |> expect_undecidable()
  5.19 |> grim_closest(20.5, 2, items_averaged = 1) |> expect_equal(NA_real_)
  5.19 |> grim_values(28, 2, items_averaged = 1.5)  |> purrr::pluck(1L) |> expect_undecidable()
})
