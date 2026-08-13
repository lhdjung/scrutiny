# The reference values below are what `rsprite2::GRIM_test(return_values =
# TRUE)` returns for the same inputs. rsprite2 finds them by walking outward
# from the rounded sum total one integer at a time, with a fudge factor;
# `sum_range()` derives the same set in closed form and in exact integer
# arithmetic.

test_that("`grim_values()` returns the achievable means", {
  grim_values(5.19, 32, digits_x = 2)[[1L]] |> expect_equal(5.1875)
  grim_values(5.19, 300, digits_x = 2)[[1L]] |>
    expect_equal(c(1556, 1557, 1558) / 300)
  # No achievable mean at all if the value set is GRIM-inconsistent:
  grim_values(5.19, 28, digits_x = 2)[[1L]] |> expect_equal(numeric(0L))
})


test_that("`grim_closest()` returns the achievable mean nearest to `x`", {
  # The two sums straddling an inconsistent value set are 145 and 146; 145/28
  # is the closer of the two, and it is what rsprite2 reports (as 5.18):
  grim_closest(5.19, 28, digits_x = 2) |> expect_equal(145 / 28)
  round(grim_closest(5.19, 28, digits_x = 2), 2) |> expect_equal(5.18)
  # For a consistent value set, it is one of the `grim_values()`:
  grim_closest(5.19, 32, digits_x = 2) |> expect_equal(5.1875)
  grim_closest(5.19, 300, digits_x = 2) |> expect_equal(5.19)
})


test_that("both functions are vectorized like `grim()`", {
  out <- grim_values(c(5.19, 5.19, 5.19), c(32, 300, 28), digits_x = 2)
  out |> expect_type("list")
  out |> expect_length(3L)
  lengths(out) |> expect_equal(c(1L, 3L, 0L))

  grim_closest(c(5.19, 5.19), c(32, 28), digits_x = 2) |>
    expect_equal(c(5.1875, 145 / 28))
  grim_closest(5.19, 25:30, digits_x = 2) |> expect_length(6L)
})


test_that("`grim_values()` agrees with `grim()` itself", {
  # Every achievable mean must be reported as `x` when rounded the same way,
  # and there must be as many of them as the value set is consistent:
  for (rounding in c("up_or_down", "up", "down", "ceiling", "floor", "trunc")) {
    for (n in c(28, 32, 300)) {
      for (x in c(5.19, 4.2, 0.24)) {
        values <- grim_values(
          x, n, digits_x = 2, rounding = rounding
        )[[1L]]
        info <- paste0("x = ", x, ", n = ", n, ", rounding = ", rounding)
        consistency <- grim(x, n, digits_x = 2, rounding = rounding)
        expect_equal(length(values) > 0L, consistency, info = info)
        for (value in values) {
          expect_equal(
            reround(value, digits = 2, rounding = rounding)[1L] |>
              round(10L),
            x,
            info = info
          )
        }
      }
    }
  }
})


test_that("`grim_values()` matches the `sum_*` columns of `grim_map()`", {
  out <- grim_map(pigs1, digits_x = 2, show_rec = TRUE)
  values <- grim_values(pigs1$x, pigs1$n, digits_x = 2)
  counts <- pmax(0L, out$sum_upper - out$sum_lower + 1L)
  lengths(values) |> expect_equal(as.integer(counts))
  # `grim_closest()` is one of them wherever there are any:
  for (i in which(out$consistency)) {
    grim_closest(pigs1$x[i], pigs1$n[i], digits_x = 2) |>
      expect_in(values[[i]])
  }
})


test_that("`percent` returns values on the scale of `x`", {
  # Not on the decimal scale that GRIM works with internally, so that the
  # values can be read against `x`:
  closest <- grim_closest(71, 43, digits_x = 0, percent = TRUE)
  closest |> expect_gt(70)
  closest |> expect_lt(74)
  # 40% of 5 is 2 people, so this is consistent and there is one way to get
  # there. The value comes back as 40, not as 0.4:
  grim_values(40, 5, digits_x = 0, percent = TRUE)[[1L]] |> expect_equal(40)
  grim_closest(40, 5, digits_x = 0, percent = TRUE) |> expect_equal(40)
})


test_that("`items` multiplies into the sample size", {
  grim_values(2.84, 16, digits_x = 2, items = 2)[[1L]] |>
    expect_equal(grim_values(2.84, 32, digits_x = 2)[[1L]])
})


test_that("undecidable cases give `NA`", {
  # A missing mean has no bounds to derive, as in `grim()`. (This used to be
  # tested with `rounding = "anti_trunc"` at a mean of zero, which had no
  # defined bounds either until `anti_trunc()` stopped sending zero away from
  # zero.)
  grim_values(NA, 40, digits_x = 2)[[1L]] |>
    expect_equal(NA_real_)
  grim_closest(NA, 40, digits_x = 2) |>
    expect_equal(NA_real_)
})


test_that("`rounding = \"anti_trunc\"` at a mean of zero is decidable", {
  # Every non-zero value is taken away from zero, so a mean reported as 0.00
  # pins the sum to exactly 0 -- attainable only by all-zero data:
  grim_values(0, 40, digits_x = 2, rounding = "anti_trunc")[[1L]] |>
    expect_equal(0)
  grim_closest(0, 40, digits_x = 2, rounding = "anti_trunc") |>
    expect_equal(0)
})


test_that("`digits_x` is required, with the bespoke error message", {
  # `suppressMessages()` mutes the changelog hint that `error_digits_missing()`
  # prints via `on.exit()` as it unwinds:
  suppressMessages(grim_values(5.19, 28)) |> expect_error("digits_x")
  suppressMessages(grim_closest(5.19, 28)) |> expect_error("digits_x")
})
