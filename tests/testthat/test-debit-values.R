test_that("`debit_values()` returns the achievable means and SDs", {
  0.35 |> debit_values(0.49, 20, 2, 2, "integer") |> expect_equal(tibble::tibble(case = 1L, x = 0.35, sd = sqrt(0.35 * 0.65 * 20 / 19), n = 20, sum = 7))
  0.35 |> debit_values(0.41, 20, 2, 2, "integer") |> nrow() |> expect_equal(0L)
  out <- 0.5 |> debit_values(0.5, 300, 1, 1, "integer")
  out$sum |> expect_equal(135:165)
  out |> names() |> expect_equal(c("case", "x", "sd", "n", "sum"))
  # The population SD of 7 ones in 20 is 0.477, which is not reported as 0.49:
  0.35 |> debit_values(0.49, 20, 2, 2, "integer", sd_meaning = "sd_pop") |> nrow() |> expect_equal(0L)
  0.35 |> debit_values(0.11, 20, 2, 2, "integer", sd_meaning = "se")     |> _$sd   |> expect_equal(sqrt(0.35 * 0.65 / 19))
})


test_that("`debit_values()` lists exactly the samples that `debit()` admits", {
  set.seed(sweep_seed(711L))
  for (i in 1:300) {
    n <- sample(2:60, 1)
    x <- round(runif(1), 2)
    sd <- round(runif(1, 0, 0.6), 2)
    sd_meaning <- sample(c("sd_sample", "sd_pop", "se"), 1)
    if (sd_meaning == "se") {
      sd <- round(sd / sqrt(n), 3)
    }
    digits_sd <- if (sd_meaning == "se") 3 else 2
    ones <- 0:n
    # Every number of ones, checked by hand:
    admitted <- ones[vapply(
      ones,
      function(k) {
        # Under `"up_or_down"`, a value is reported as any number within half a
        # unit of its last decimal place, ties included:
        sd_k <- sd_binary_1_n(k, n, sd_meaning = sd_meaning)
        abs(k / n - x) <= 0.005 + 1e-12 && abs(sd_k - sd) <= 0.5 * 10^-digits_sd + 1e-12
      },
      logical(1L)
    )]
    out <- x |> debit_values(sd, n, 2, digits_sd, "integer", sd_meaning = sd_meaning)
    out$sum |> expect_equal(as.numeric(admitted), info = paste(x, sd, n, sd_meaning))
    (nrow(out) > 0) |> expect_equal(debit(x, sd, n, 2, digits_sd, "integer", sd_meaning = sd_meaning))
  }
})


test_that("`debit_values()` handles missing values and bad input", {
  out <- c(0.35, NA) |> debit_values(0.49, 20, 2, 2, "integer")
  out$case |> expect_equal(1:2)
  out$x[2] |> expect_na()
  0.35 |> debit_values(0.49, 20, 2, 2, NA) |> _$x |> expect_na()
  0.35 |> debit_values(0.49, 20, 2, 2)               |> expect_error("`debit_values()` only applies", fixed = TRUE)
  0.35 |> debit_values(0.49, 20, 2, 2, "continuous") |> expect_error("`debit_values()` only applies", fixed = TRUE)
  0.35 |> debit_values(0.49, 20, 2)                  |> expect_error("digits_sd")
  0.35 |> debit_values(0.49, 20, 2, 2, "integer", sd_meaning = "sd_sample_or_pop") |> expect_error("debit_values")
  0.35 |> debit_values(0.49, 20, 2, 2, "integer", formula = "mean_n")             |> expect_error("can't be \"mean_n\"", fixed = TRUE)
  0.35 |> debit_values(0.49, 20, 2, 2, "integer", formula = "exactly")            |> expect_error("formula")
  # The numbers of ones are counted before the SD narrows them down:
  0.5  |> debit_values(0.4999, 1e7, 1, 4, "integer")                              |> expect_error("before `sd` narrows them down", fixed = TRUE)
  0.35 |> debit_values(0.49, 20, 2, 2, "integer", formula = "exact")              |> expect_equal(debit_values(0.35, 0.49, 20, 2, 2, "integer"))
  1.35 |> debit_values(0.49, 20, 2, 2, "integer")    |> expect_error()
  numeric(0) |> debit_values(numeric(0), numeric(0), 2, 2, "integer") |> expect_equal(PTYPE_DEBIT_VALUES)
})


test_that("`debit_closest()` returns the pair whose SD is closest to `sd`", {
  # Consistent: the only pair there is:
  0.35 |> debit_closest(0.49, 20, 2, 2, "integer") |> expect_equal(debit_values(0.35, 0.49, 20, 2, 2, "integer"))
  # Inconsistent: 7 ones in 20 is the only mean reported as 0.35:
  0.35 |> debit_closest(0.41, 20, 2, 2, "integer") |> _$sum |> expect_equal(7)
  # Of the 31 means reported as 0.5, those with the least SD are farthest from
  # 0.5; of these, 135 ones has the smaller number of ones:
  0.5  |> debit_closest(0.3, 300, 1, 1, "integer") |> _$sum |> expect_equal(135)
  # No mean reported as 0.33 with 4 values, so the closest mean decides:
  0.33 |> debit_closest(0.5, 4, 2, 2, "integer") |> _$sum |> expect_equal(1)
  out <- c(0.35, NA) |> debit_closest(0.41, 20, 2, 2, "integer")
  out$case |> expect_equal(1:2)
  out$sum[2] |> expect_na()
  0.35 |> debit_closest(0.41, 20, 2, 2, "integer", formula = "mean_n") |> expect_error("can't be \"mean_n\"", fixed = TRUE)
  0.35 |> debit_closest(0.41, 20, 2, 2, "continuous")                  |> expect_error("`debit_closest()` only applies", fixed = TRUE)
  numeric(0) |> debit_closest(numeric(0), numeric(0), 2, 2, "integer") |> expect_equal(PTYPE_DEBIT_VALUES)
})


test_that("`debit_closest()` agrees with a search over every number of ones", {
  set.seed(sweep_seed(712L))
  for (i in 1:300) {
    n <- sample(2:60, 1)
    x <- round(runif(1), 2)
    sd <- round(runif(1, 0, 0.6), 2)
    sd_meaning <- sample(c("sd_sample", "sd_pop"), 1)
    ones <- 0:n
    sd_ones <- sd_binary_1_n(ones, n, sd_meaning = sd_meaning)
    # Under `"up_or_down"`, ties included:
    fits_x <- abs(ones / n - x) <= 0.005 + 1e-12
    fits_sd <- abs(sd_ones - sd) <= 0.005 + 1e-12
    candidates <- if (any(fits_x & fits_sd)) {
      ones[fits_x & fits_sd]
    } else if (any(fits_x)) {
      ones[fits_x]
    } else {
      # The mean that `grim_closest()` returns, the smaller one in a tie:
      distance_x <- abs(ones / n - x)
      min(ones[distance_x - min(distance_x) < 1e-12])
    }
    distance <- abs(sd_binary_1_n(candidates, n, sd_meaning = sd_meaning) - sd)
    candidates <- candidates[distance - min(distance) < 1e-12]
    distance_x <- abs(candidates / n - x)
    expected <- min(candidates[distance_x - min(distance_x) < 1e-12])
    x |> debit_closest(sd, n, 2, 2, "integer", sd_meaning = sd_meaning) |> _$sum |> expect_equal(expected, info = paste(x, sd, n, sd_meaning))
  }
})
