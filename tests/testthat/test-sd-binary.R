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
