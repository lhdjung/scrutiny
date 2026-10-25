test_that("`grim_binomial()` leaves undecidable cases out", {
  # A row with an `NA` verdict has an `NA` probability, and one such row used
  # to make the p-value `NA` for the whole table:
  df <- tibble::tibble(x = c(5.19, 5.19, 5.20), n = c(28L, NA, 30L))
  out_all       <- df        |> grim_map(digits_x = 2, items_averaged = 1) |> grim_binomial()
  out_decidable <- df[-2L, ] |> grim_map(digits_x = 2, items_averaged = 1) |> grim_binomial()
  out_all |> expect_equal(out_decidable)
  out_all$parameter |> expect_equal(2L)
})


# No probabilities are left once every row is undecidable.
test_that("the binomial functions reject empty input instead of crashing", {
  tibble::tibble(x = c(1.23, 1.24), n = c(NA, NA)) |>
    grim_map(digits_x = 2, items_averaged = 1) |>
    grim_binomial() |>
    expect_error("No value sets to test")
  0.5 |>
    grim_binomial_power(0.9, k = 0) |>
    expect_error("at least 1")
  numeric(0) |>
    grim_binomial_power(0.9) |>
    expect_error("at least one value set")
})


test_that("`grim_binomial()` returns the exact interval, one-sided like the test", {
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
  # These used to give `NaN`, an error, or a power of 1:
  0.5 |> grim_binomial_power(1.5, k = 3)            |> expect_error("`p1` must be between 0 and 1")
  1.5 |> grim_binomial_power(0.9, k = 3)            |> expect_error("`p0` must be between 0 and 1")
  0.5 |> grim_binomial_power(0.9, k = 3, alpha = 5) |> expect_error("`alpha` must be between 0 and 1")
  0.5 |> grim_binomial_power(0.9, k = 2.5)          |> expect_error("whole number")
  0.5 |> grim_binomial_power(0.9, k = "10")         |> expect_error("whole number")
  0.5 |> grim_binomial_power(0.9, k = Inf)          |> expect_error("whole number")
  0.5 |> grim_binomial_power(0.9, k = c(3, 4))      |> expect_error("single whole number")
  # The error names the function that was called, not an internal helper:
  1.5 |> grim_binomial_power(0.9, k = 3)            |> expect_error(class = "rlang_error") |> conditionCall() |> deparse() |> expect_match("^grim_binomial_power")
})


test_that("p-values agree with the exact binomial and keep their precision", {
  # Every row consistent with a null probability of 0.5, so `P(X >= k) = 0.5^k`:
  for (k in c(10L, 60L, 100L)) {
    tibble::tibble(x = rep(5.18, k), n = 50) |> grim_map(digits_x = 2, items_averaged = 1) |> grim_binomial() |> _$p.value |> expect_equal(0.5^k)
  }
  # Unequal probabilities, against every one of the 2^10 outcomes:
  set.seed(42)
  p <- runif(10L, 0.1, 0.9)
  outcomes <- rep(list(0:1), 10L) |> expand.grid() |> as.matrix()
  prob_outcome <- outcomes |> apply(1L, function(o) prod(ifelse(o == 1L, p, 1 - p)))
  pmf_brute <- outcomes |> rowSums() |> factor(levels = 0:10) |> split(x = prob_outcome) |> vapply(sum, numeric(1L))
  p |> poibin_p_values("less")    |> expect_equal(unname(cumsum(pmf_brute)))
  p |> poibin_p_values("greater") |> expect_equal(unname(rev(cumsum(rev(pmf_brute)))))
  rep(0.3, 20L) |> poibin_p_values("less")    |> expect_equal(pbinom(0:20, 20, 0.3))
  rep(0.3, 20L) |> poibin_p_values("greater") |> expect_equal(pbinom(-1:19, 20, 0.3, lower.tail = FALSE))
})

test_that("a p-value equal to `alpha` rejects in `grim_binomial_power()`", {
  for (k in c(2L, 5L, 6L, 8L, 9L)) {
    0.5 |> grim_binomial_power(1, k = k, alpha = 0.5^k) |> expect_equal(1)
  }
})


test_that("`grim_binomial()` rejects sequence and total-n output", {
  # Their rows are hypothetical value sets, inconsistent by construction, so a
  # p-value over them used to be returned without a word:
  pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1) |> grim_binomial() |> expect_error("hypothetical")
  tibble::tibble(x1 = 4.71, x2 = 5.3, n = 40) |>
    grim_map_total_n(digits_x = c(2, 1), items_averaged = 1) |>
    grim_binomial() |>
    expect_error("hypothetical")
  pigs1 |> grim_binomial() |> expect_error("output of `grim_map()`", fixed = TRUE)
})
