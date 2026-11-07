# The reference values below are what `rsprite2::GRIM_test(return_values =
# TRUE)` returns for the same inputs. rsprite2 finds them by walking outward
# from the rounded sum total one integer at a time, with a fudge factor;
# `sum_range()` derives the same set in closed form and in exact integer
# arithmetic.

test_that("`grim_values()` returns the achievable means and their sums", {
  5.19 |> grim_values(32, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_equal(tibble::tibble(case = 1L, x = 5.1875, n = 32, sum = 166))
  5.19 |>
    grim_values(300, digits_x = 2, items_averaged = 1, data_type = "integer") |>
    expect_equal(tibble::tibble(case = 1L, x = c(1556, 1557, 1558) / 300, n = 300, sum = c(1556, 1557, 1558)))
  # No achievable mean at all if the value set is GRIM-inconsistent:
  out <- 5.19 |> grim_values(28, digits_x = 2, items_averaged = 1, data_type = "integer")
  out |> nrow()  |> expect_equal(0L)
  out |> names() |> expect_equal(c("case", "x", "n", "sum"))
})


test_that("`grim_closest()` returns the achievable mean nearest to `x`", {
  # The two sums straddling an inconsistent value set are 145 and 146; 145/28
  # is the closer of the two, and it is what rsprite2 reports (as 5.18):
  5.19 |> grim_closest(28, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_equal(145 / 28)
  5.19 |> grim_closest(28, digits_x = 2, items_averaged = 1, data_type = "integer") |> round(2) |> expect_equal(5.18)
  # For a consistent value set, it is one of the `grim_values()`:
  5.19 |> grim_closest(32, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_equal(5.1875)
  5.19 |> grim_closest(300, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_equal(5.19)
  # Consistent whatever the missing values are, but without a nearest mean:
  5 |> grim_closest(NA, digits_x = NA, items_averaged = 1, data_type = "integer") |> expect_na()
  5 |> grim_closest(10, digits_x = NA, items_averaged = NA, data_type = "integer") |> expect_na()
})


test_that("both functions are vectorized like `grim()`", {
  out <- c(5.19, 5.19, 5.19) |> grim_values(c(32, 300, 28), digits_x = 2, items_averaged = 1, data_type = "integer")
  # One row per achievable mean, none for the inconsistent third value set:
  out |> tibble::is_tibble() |> expect_true()
  out$case |> expect_equal(c(1L, 2L, 2L, 2L))
  out$n    |> expect_equal(c(32, 300, 300, 300))
  # `case` counts through `n` if `x` is a single value:
  out <- 5.19 |> grim_values(25:35, digits_x = 2, items_averaged = 1, data_type = "integer")
  out$case |> expect_equal(c(2L, 3L, 7L, 8L))
  out$n    |> expect_equal(c(26, 27, 31, 32))

  c(5.19, 5.19) |>
    grim_closest(c(32, 28), digits_x = 2, items_averaged = 1, data_type = "integer") |>
    expect_equal(c(5.1875, 145 / 28))
  5.19 |> grim_closest(25:30, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_length(6L)

  numeric(0) |> grim_values(numeric(0), digits_x = 2, items_averaged = 1, data_type = "integer")  |> expect_equal(tibble::tibble(case = integer(), x = double(), n = double(), sum = double()))
  numeric(0) |> grim_closest(numeric(0), digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_identical(numeric(0))
  5.19 |> grim_closest(28, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = c("up", "floor")) |> expect_error("must be length 1")
  5.19 |> grim_values(28, digits_x = 2, items_averaged = 1, data_type = "integer", x_meaning = c("percentage", "mean")) |> expect_error("must be length 1")
  c(1.1, 1.2, 1.3) |> grim_closest(c(10, 20), digits_x = 1, items_averaged = 1, data_type = "integer") |> expect_error()
  5.19 |> grim_closest(28, items_averaged = 1, data_type = "integer") |> expect_error("digits_x")
})

test_that("`grim_closest()` keeps a consistent `x` to the means reported as `x`", {
  # 56 / 11 is closer to 5.1 than 57 / 11, but it is floored to 5.0:
  5.1 |> grim_closest(11, digits_x = 1, items_averaged = 1, data_type = "integer", rounding = "floor") |> expect_equal(57 / 11)
})


test_that("too many means is an error, not a huge allocation", {
  5 |> grim_values(1e7, digits_x = 0, items_averaged = 1, data_type = "integer")  |> expect_error("Too many achievable means")
  # The error names the value set, with `x` as given:
  1.2345 |> grim_values(2e10, digits_x = 4, items_averaged = 1, data_type = "integer") |> expect_error("`x = 1.2345`, `n = 20000000000`")
  5 |> grim_closest(1e7, digits_x = 0, items_averaged = 1, data_type = "integer") |> expect_equal(5)
})


test_that("`grim_values()` agrees with `grim()` itself", {
  # Every achievable mean must be reported as `x` when rounded the same way,
  # and there must be as many of them as the value set is consistent:
  for (rounding in c("up_or_down", "up", "down", "ceiling", "floor", "trunc")) {
    for (n in c(28, 32, 300)) {
      for (x in c(5.19, 4.2, 0.24)) {
        values <- x |> grim_values(n, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = rounding)
        info <- paste0("x = ", x, ", n = ", n, ", rounding = ", rounding)
        consistency <- grim(x, n, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = rounding)
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
  out <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", show_rec = TRUE)
  values <- grim_values(pigs1$x, pigs1$n, digits_x = 2, items_averaged = 1, data_type = "integer")
  counts <- pmax(0L, out$sum_upper - out$sum_lower + 1L)
  values$case |> tabulate(nrow(out)) |> expect_equal(as.integer(counts))
  for (i in which(out$consistency)) {
    values$sum[values$case == i] |> expect_equal(seq(out$sum_lower[i], out$sum_upper[i]))
    # `grim_closest()` is one of them wherever there are any:
    pigs1$x[i] |>
      grim_closest(pigs1$n[i], digits_x = 2, items_averaged = 1, data_type = "integer") |>
      expect_in(values$x[values$case == i])
  }
})


test_that("`x_meaning = \"percentage\"` returns values on the scale of `x`", {
  # Not on the decimal scale that GRIM works with internally, so that the
  # values can be read against `x`:
  closest <- grim_closest(71, 43, digits_x = 0, items_averaged = 1, data_type = "integer", x_meaning = "percentage")
  closest |> expect_gt(70)
  closest |> expect_lt(74)
  # 40% of 5 is 2 people, so this is consistent and there is one way to get
  # there. The value comes back as 40, not as 0.4:
  40 |> grim_values(5, digits_x = 0, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |> expect_equal(tibble::tibble(case = 1L, x = 40, n = 5, sum = 2))
  40 |> grim_closest(5, digits_x = 0, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |> expect_equal(40)
})


test_that("`items_averaged` multiplies into the sample size", {
  2.84 |>
    grim_values(16, digits_x = 2, items_averaged = 2, data_type = "integer") |>
    dplyr::select(-n) |>
    expect_equal(grim_values(2.84, 32, digits_x = 2, items_averaged = 1, data_type = "integer") |> dplyr::select(-n))
})


test_that("undecidable cases give one row of `NA`", {
  # A missing mean has no bounds to derive, as in `grim()`. (This used to be
  # tested with `rounding = "anti_trunc"` at a mean of zero, which had no
  # defined bounds either until `anti_trunc()` stopped sending zero away from
  # zero.)
  NA |> grim_values(40, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_undecidable()
  NA |> grim_closest(40, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_equal(NA_real_)
})


test_that("`rounding = \"anti_trunc\"` at a mean of zero is decidable", {
  # Every non-zero value is taken away from zero, so a mean reported as 0.00
  # pins the sum to exactly 0 -- attainable only by all-zero data:
  0 |>
    grim_values(40, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "anti_trunc") |>
    expect_equal(tibble::tibble(case = 1L, x = 0, n = 40, sum = 0))
  0 |> grim_closest(40, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "anti_trunc") |> expect_equal(0)
})


test_that("`digits_x` is required, with the bespoke error message", {
  # `suppressMessages()` mutes the changelog hint that `error_digits_missing()`
  # prints via `on.exit()` as it unwinds:
  5.19 |> grim_values(28, items_averaged = 1, data_type = "integer")  |> suppressMessages() |> expect_error("digits_x")
  5.19 |> grim_closest(28, items_averaged = 1, data_type = "integer") |> suppressMessages() |> expect_error("digits_x")
})


test_that("`grim_values()` and `grim_closest()` are undecidable where `grim()` is", {
  5.19 |> grim(20.5, 2, items_averaged = 1, data_type = "integer")         |> is.na() |> expect_true()
  5.19 |> grim_values(20.5, 2, items_averaged = 1, data_type = "integer")  |> expect_undecidable()
  5.19 |> grim_closest(20.5, 2, items_averaged = 1, data_type = "integer") |> expect_equal(NA_real_)
  5.19 |> grim_values(28, 2, items_averaged = 1.5, data_type = "integer")  |> expect_undecidable()
})


test_that("type mistakes are errors, as in `grim()`, not undecidable value sets", {
  5.19 |> grim_values(n = "20", digits_x = 2, items_averaged = 1, data_type = "integer")   |> expect_error("`n` must be one of these types")
  5.19 |> grim_closest(n = "20", digits_x = 2, items_averaged = 1, data_type = "integer")  |> expect_error("`n` must be one of these types")
  5.19 |> grim_values(n = 20, digits_x = 2, items_averaged = "1", data_type = "integer")   |> expect_error("`items_averaged` must be one of these types")
  5.19 |> grim_values(n = 20, digits_x = 2, items_averaged = 1, data_type = "integer", x_meaning = NA)    |> expect_error("`x_meaning`")
  5.19 |> grim_closest(n = 20, digits_x = 2, items_averaged = 1, data_type = "integer", x_meaning = "yes") |> expect_error("`x_meaning`")
})


test_that("`data_type` is required, and continuous data are an error", {
  5.19 |> grim_values(28, digits_x = 2, items_averaged = 1)                            |> expect_error("`grim_values()` only applies", fixed = TRUE)
  5.19 |> grim(28, digits_x = 2, items_averaged = 1)                                   |> expect_error("no data at all can have", fixed = TRUE)
  5.19 |> grim_closest(28, digits_x = 2, items_averaged = 1)                           |> expect_error("Need to specify `data_type`")
  5.19 |> grim_values(28, digits_x = 2, items_averaged = 1, data_type = "continuous")  |> expect_error("`grim_values()` only applies", fixed = TRUE)
  5.19 |> grim_closest(28, digits_x = 2, items_averaged = 1, data_type = "continuous") |> expect_error("`grim_closest()` only applies", fixed = TRUE)
  5.19 |> grim_values(28, digits_x = 2, items_averaged = 1, data_type = factor("integer")) |> expect_error("not a factor")
  # Undecidable under `NA`, whether or not whole numbers can have the mean:
  5.19 |> grim_values(c(28, 32), digits_x = 2, items_averaged = 1, data_type = NA)  |> expect_equal(tibble::tibble(case = 1:2, x = NA_real_, n = c(28, 32), sum = NA_real_))
  5.19 |> grim_closest(c(28, 32), digits_x = 2, items_averaged = 1, data_type = NA) |> expect_equal(c(NA_real_, NA_real_))
  5.19 |> grim_closest(28, digits_x = NA, items_averaged = 1, data_type = NA)       |> expect_na()
})
