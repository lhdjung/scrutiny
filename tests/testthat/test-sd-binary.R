test_that("the sample SD is `NA` for fewer than two values", {
  # The formula used to return `Inf` or `NaN` with the warning suppressed:
  sd_binary_groups(1, 0)          |> expect_na()
  sd_binary_0_n(1, 1)             |> expect_na()
  sd_binary_1_n(0, 1)             |> expect_na()
  sd_binary_mean_n(0.5, 1)        |> expect_na()
  sd_binary_mean_n(0.5, c(1, 2))  |> expect_equal(c(NA, sqrt(0.5)))
  sd_binary_groups(127, 153)      |> expect_equal(sqrt((280 / 279) * (127 * 153) / 280^2))
})


test_that("impossible counts and means are errors, not `NaN`", {
  -1  |> sd_binary_groups(5) |> expect_error("must not be negative")
  3   |> sd_binary_0_n(2)    |> expect_error("from 0 to `n`")
  -1  |> sd_binary_1_n(2)    |> expect_error("from 0 to `n`")
  1.5 |> sd_binary_mean_n(4) |> expect_error("between 0 and 1")
})


test_that("a near-whole `n` is the whole number it is", {
  1           |> sd_binary_0_n(2 - 1e-9)    |> expect_equal(sd_binary_0_n(1, 2))
  1           |> sd_binary_1_n(2 - 1e-9)    |> expect_equal(sd_binary_1_n(1, 2))
  0.5         |> sd_binary_mean_n(2 - 1e-9) |> expect_equal(sd_binary_mean_n(0.5, 2))
  (1 - 1e-9)  |> sd_binary_groups(1)        |> expect_equal(sd_binary_groups(1, 1))
})


test_that("input of the wrong type or length is an error", {
  # These failed in base R or recycled with only a base warning:
  "1"     |> sd_binary_1_n(3)       |> expect_error("`group_1` must be one of these types")
  "0.5"   |> sd_binary_mean_n(3)    |> expect_error("`mean` must be one of these types")
  c(1, 2, 3) |> sd_binary_0_n(c(5, 6)) |> expect_error("Can't recycle")
  # A negative `n` is named as the culprit, not the count:
  1       |> sd_binary_1_n(-3)      |> expect_error("`n` must not be negative")
  NA      |> sd_binary_1_n(5)       |> expect_na()
  # A character `NA` used to reach the arithmetic:
  NA_character_ |> sd_binary_mean_n(5) |> expect_error("`mean` must be one of these types")
  NA_character_ |> sd_binary_0_n(5)    |> expect_error("`group_0` must be one of these types")
})


test_that("a fractional count or `n` is `NA`, as it describes no data", {
  0.5 |> sd_binary_mean_n(10.5) |> expect_na()
  1.5 |> sd_binary_groups(2)    |> expect_na()
  1.5 |> sd_binary_0_n(10)      |> expect_na()
  c(127, 1.5) |> sd_binary_groups(153) |> is.na() |> expect_equal(c(FALSE, TRUE))
})


test_that("integer counts don't overflow", {
  # `group_0 * group_1` was an integer product, `NA` from 46,341 each:
  50000L |> sd_binary_groups(50000L) |> expect_equal(sd_binary_groups(50000, 50000))
  50000L |> sd_binary_0_n(100000L)   |> expect_equal(sd_binary_0_n(50000, 100000))
  50000L |> sd_binary_1_n(100000L)   |> expect_equal(sd_binary_1_n(50000, 100000))
})


test_that("huge counts don't overflow", {
  # `n^2` is `Inf` from about 1e154, which made the SD 0:
  1e200 |> sd_binary_groups(3e200) |> expect_equal(sqrt(0.25 * 0.75))
  1e200 |> sd_binary_0_n(4e200)    |> expect_equal(sqrt(0.25 * 0.75))
  3e200 |> sd_binary_1_n(4e200)    |> expect_equal(sqrt(0.25 * 0.75))
})


test_that("an infinite or `NaN` count, `n`, or mean is `NA`, not `NaN`", {
  Inf |> sd_binary_groups(10)  |> is.nan() |> expect_false()
  5   |> sd_binary_0_n(Inf)    |> is.nan() |> expect_false()
  0.5 |> sd_binary_mean_n(Inf) |> is.nan() |> expect_false()
  NaN |> sd_binary_mean_n(10)  |> is.nan() |> expect_false()
  NaN |> sd_binary_0_n(10)     |> is.nan() |> expect_false()
  c(Inf, 5) |> sd_binary_groups(10) |> is.na() |> expect_equal(c(TRUE, FALSE))
})
