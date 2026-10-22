test_that("`grim_binomial()` leaves undecidable cases out", {
  skip_if_not_installed("poibin")
  # A row with an `NA` verdict has an `NA` probability, and one such row used
  # to make the p-value `NA` for the whole table:
  df <- tibble::tibble(x = c(5.19, 5.19, 5.20), n = c(28L, NA, 30L))
  out_all       <- df        |> grim_map(digits_x = 2, items_averaged = 1) |> grim_binomial()
  out_decidable <- df[-2L, ] |> grim_map(digits_x = 2, items_averaged = 1) |> grim_binomial()
  out_all |> expect_equal(out_decidable)
  out_all$parameter |> expect_equal(2L)
})


# `poibin::ppoibin()` crashes the whole R session when given no probabilities,
# which is what is left once every row is undecidable.
test_that("the binomial functions reject empty input instead of crashing", {
  skip_if_not_installed("poibin")
  tibble::tibble(x = c(1.23, 1.24), n = c(NA, NA)) |>
    grim_map(digits_x = 2, items_averaged = 1) |>
    grim_binomial() |>
    expect_error("No value sets to test")
  0.5 |>
    grim_binomial_power(0.9, k = 0) |>
    expect_error("at least one value set")
})


test_that("`grim_binomial()` returns the exact interval, one-sided like the test", {
  skip_if_not_installed("poibin")
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1)
  k <- sum(data$consistency)
  n <- nrow(data)
  for (alternative in c("greater", "less", "two.sided")) {
    out      <- data |> grim_binomial(alternative = alternative, conf.level = 0.9)
    expected <- k    |> stats::binom.test(n, alternative = alternative, conf.level = 0.9)
    c(out$conf.low, out$conf.high) |> expect_equal(as.numeric(expected$conf.int))
  }
  data |> grim_binomial()                          |> getElement("conf.high") |> expect_equal(1)
  data |> grim_binomial(alternative = "less")      |> getElement("conf.low")  |> expect_equal(0)
  data |> grim_binomial(conf.level = 1)            |> expect_error("between 0 and 1")
  data |> grim_binomial(conf.level = c(0.9, 0.95)) |> expect_error("between 0 and 1")
})


test_that("`grim_binomial_power()` checks its probabilities and `k`", {
  skip_if_not_installed("poibin")
  # These used to give `NaN`, an error from poibin, or a power of 1:
  0.5 |> grim_binomial_power(1.5, k = 3)            |> expect_error("`p1` must be between 0 and 1")
  1.5 |> grim_binomial_power(0.9, k = 3)            |> expect_error("`p0` must be between 0 and 1")
  0.5 |> grim_binomial_power(0.9, k = 3, alpha = 5) |> expect_error("`alpha` must be between 0 and 1")
  0.5 |> grim_binomial_power(0.9, k = 2.5)          |> expect_error("whole number")
})
