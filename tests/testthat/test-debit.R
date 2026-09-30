# Set this to `TRUE` to run the exhaustive DEBIT oracle further below. It takes
# a few minutes.
run_full_debit_oracle <- FALSE


# x <- rnorm(1000, 0.5, 0.08) |> censor(0, 1) |> as.character()
# sd <- runif(1000, 0.1, 0.4) |> as.character()
# n <- rnorm(1000, 1000, 200) |> censor(100, 1900)
#
#
# x <- seq_endpoint("0.01", 1) |> rep(10)
# sd <- rnorm(100, 0.15, 0.05) |> round(2) |> restore_zeros(width = 2)
#
# out <- purrr::pmap_lgl(list(x, sd, n), debit)

out <- pigs3 |> purrr::pmap_lgl(debit, digits_x = 2, digits_sd = 2)
out_expected <- c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)

test_that("bla", {
  out |> expect_equal(out_expected)
})


# The rounding methods that `grim()` and `grimmer()` know. DEBIT used to know
# only eight of them because its bounds came from the pre-1.0.0 `unround()`,
# which had its own, shorter list -- even though `debit_map()`'s documentation
# said that `rounding` is passed on to `debit()` just like everywhere else.
rounding_methods <- c(
  "up_or_down",
  "up",
  "down",
  "even",
  "ceiling",
  "floor",
  "ceiling_or_floor",
  "trunc",
  "anti_trunc",
  "up_from",
  "down_from",
  "up_from_or_down_from"
)


test_that("`debit()` accepts every rounding method that `grim()` accepts", {
  for (r in rounding_methods) {
    0.53 |>
      debit(
        sd = 0.50,
        n = 1683,
        digits_x = 2,
        digits_sd = 2,
        rounding = r,
        threshold = 6
      ) |>
      expect_type("logical")
  }
})


test_that("`threshold` only affects the `*_from` rounding methods", {
  # `round_up()` and `round_down()` round from a fixed 5, so the bounds that
  # `debit()` reconstructs for them must not move with `threshold`. Before DEBIT
  # went through the shared bounds machinery, they did.
  for (r in c("up_or_down", "up", "down")) {
    out_5 <- pigs3 |> purrr::pmap_lgl(debit, digits_x = 2, digits_sd = 2, rounding = r, threshold = 5)
    out_9 <- pigs3 |> purrr::pmap_lgl(debit, digits_x = 2, digits_sd = 2, rounding = r, threshold = 9)
    out_5 |> expect_equal(out_9)
  }
})


test_that("`symmetric` is taken into account", {
  # `debit_scalar()` reconstructs the bounds under the same assumption that it
  # re-rounds with. It used to unround asymmetrically and re-round
  # symmetrically, because the old `unround()` had no `symmetric` argument.
  0.53 |>
    debit(
      sd = 0.50,
      n = 1683,
      digits_x = 2,
      digits_sd = 2,
      rounding = "up",
      symmetric = TRUE
    ) |>
    expect_type("logical")
})


test_that("`show_rec` returns the reconstructed values", {
  out_rec <- debit_scalar(
    x = 0.53,
    sd = 0.50,
    n = 1683,
    digits_x = 2,
    digits_sd = 2,
    show_rec = TRUE
  )

  out_rec |> expect_type("list")
  out_rec |> expect_length(10L)
  out_rec[[1L]] |> expect_true()
  out_rec[[2L]] |> expect_equal("up_or_down")
  # `sd_lower`, `sd_upper`, `x_lower`, and `x_upper`:
  out_rec[[3L]] |> expect_equal(0.495)
  out_rec[[5L]] |> expect_equal(0.505)
  out_rec[[7L]] |> expect_equal(0.525)
  out_rec[[8L]] |> expect_equal(0.535)
  # `sum_lower` and `sum_upper`, the numbers of ones whose mean is reported as
  # 0.53: 884 / 1683 is 0.5252525, and 900 / 1683 is 0.5347594.
  out_rec[[9L]] |> expect_equal(884)
  out_rec[[10L]] |> expect_equal(900)
})


test_that("`debit()` returns `NA` where the bounds are undefined", {
  # A missing value has no bounds to derive. (This used to be tested with
  # `rounding = "anti_trunc"` at a mean of zero, which had no bounds either
  # until `anti_trunc()` stopped sending zero away from zero.)
  NA |> debit(sd = 0.50, n = 1683, digits_x = 2, digits_sd = 2) |> expect_na()

  0.30 |> debit(sd = NA, n = 1683, digits_x = 2, digits_sd = 2) |> expect_na()
})


test_that("`debit()` still checks the range of its inputs", {
  1.5 |> debit(sd = 0.5, n = 100, digits_x = 2, digits_sd = 2) |> expect_error()
  0.5 |> debit(sd = 1.5, n = 100, digits_x = 2, digits_sd = 2) |> expect_error()
})


# Boundary means ----------------------------------------------------------

# A mean of binary data cannot lie outside of 0 and 1, but its rounding bounds
# can. `sd_binary_mean_n()` returns `NaN` for such a bound, which used to make
# the verdict undecidable for a mean reported as 0.00 or 1.00 -- even though
# both are perfectly consistent: every value is 0, or every value is 1, and the
# SD is 0 either way.

test_that("DEBIT decides means of exactly 0 and 1", {
  0 |> debit(sd = 0, n = 50, digits_x = 2, digits_sd = 2) |> expect_true()
  1 |> debit(sd = 0, n = 50, digits_x = 2, digits_sd = 2) |> expect_true()
  0 |> debit(sd = 0, n = 5, digits_x = 3, digits_sd = 3)  |> expect_true()
  1 |> debit(sd = 0, n = 5, digits_x = 3, digits_sd = 3)  |> expect_true()
})


test_that("DEBIT still rejects impossible SDs at those means", {
  0 |> debit(sd = 0.5, n = 50, digits_x = 2, digits_sd = 2) |> expect_false()
  1 |> debit(sd = 0.5, n = 50, digits_x = 2, digits_sd = 2) |> expect_false()
})


test_that("`debit_map()` reports no negative SD bound and no mean out of range", {
  out <- tibble::tibble(x = c(0, 1), sd = c(0, 0), n = c(50L, 50L)) |> debit_map(digits_x = 2, digits_sd = 2)
  out$sd_lower |> call_on(\(x) x >= 0) |> all() |> expect_true()
  out$x_lower  |> call_on(\(x) x >= 0) |> all() |> expect_true()
  out$x_upper  |> call_on(\(x) x <= 1) |> all() |> expect_true()
})


# A brute-force oracle. Every split of `n` observations into zeros and ones is a
# real binary sample, so the mean and SD it produces -- rounded the way a paper
# would report them -- must be accepted by DEBIT. This is the direction that can
# be checked exhaustively: DEBIT's conditions are necessary, not sufficient, so
# a `TRUE` verdict does not imply that a sample exists, but a `FALSE` one for a
# sample that does exist is an outright error. It is what caught the means of
# exactly 0 and 1, where the SD is undefined at the rounding bounds.

test_that("DEBIT never rejects a real binary sample", {
  n_checked <- 0L

  for (n in c(5L, 8L, 20L, 37L)) {
    for (digits in 2:3) {
      # `k` ones and `n - k` zeros, for every `k`:
      k <- 0:n
      means <- (k / n) |> reround(digits, "up_or_down")
      sds   <- (n - k) |> sd_binary_0_n(n = n) |> reround(digits, "up_or_down")

      # `reround()` with a compound method returns both variants per input,
      # interleaved, and either is a way the value could have been reported:
      for (i in seq_along(k)) {
        pair <- c(2L * i - 1L, 2L * i)
        for (x in unique(means[pair])) {
          for (sd in unique(sds[pair])) {
            if (is.na(sd)) {
              next
            }
            n_checked <- n_checked + 1L
            x |>
              debit(sd = sd, n = n, digits_x = digits, digits_sd = digits) |>
              isTRUE() |>
              expect_true(
                label = paste0(
                  "x = ", x, ", sd = ", sd, ", n = ", n, ", digits = ", digits,
                  " comes from ", k[i], " ones and ", n - k[i], " zeros, so DEBIT"
                )
              )
          }
        }
      }
    }
  }

  # Guard against the loops silently collapsing to nothing (152 as written):
  n_checked |> expect_gt(100L)
})


# The reconstructed SD is not monotonic in the mean: `sd_binary_mean_n()` is a
# downward parabola peaking at a mean of 0.5. DEBIT used to evaluate it only at
# the two bounds of the mean's rounding interval and reason from there to every
# mean in between, which is an intermediate-value argument that monotonicity
# would be needed for. An interval containing 0.5 reaches SDs above both of its
# endpoints, and every one of them was invisible to the test.

test_that("DEBIT accepts real binary data with a mean reported as 0.50", {
  # 50 ones and 50 zeros: mean 0.5, SD 0.5025189, i.e. 0.503 at three decimal
  # places. There is nothing wrong with this data set.
  0.50 |> debit(sd = 0.503, n = 100, digits_x = 2, digits_sd = 3) |> expect_true()
})

test_that("DEBIT accepts every real binary data set reported as a mean of 0.50", {
  # Enumerate the actual data sets: `k` ones out of `n`, keeping those whose
  # mean would have been reported as 0.50 at two decimal places.
  cases <- 10:250 |>
    purrr::map(function(n) {
      k <- 0:n
      k <- k[abs((k / n) - 0.5) <= 0.005]
      sd_true <- sd_binary_mean_n(k / n, n)
      sd_rep <- c(round_up(sd_true, 3L), round_down(sd_true, 3L))
      tibble::tibble(n = n, sd_rep = sd_rep)
    }) |>
    purrr::list_rbind() |>
    dplyr::distinct()

  out <- list(cases$sd_rep, cases$n) |> purrr::pmap_lgl(function(sd_rep, n) {
    debit(x = 0.50, sd = sd_rep, n = n, digits_x = 2, digits_sd = 3)
  })

  out |> all() |> expect_true()
})

# `"exact"` only considers means of the form `k / n`. The test above shows that
# it passes every real binary sample; this one shows the converse, that it
# passes nothing else, by listing every reportable pair of mean and SD.

test_that("`formula = \"exact\"` passes exactly the real binary samples", {
  grid <- tidyr::expand_grid(x = 0:100 / 100, sd = 0:55 / 100)

  for (rounding in c("up_or_down", "up", "even", "ceiling")) {
    for (n in c(7L, 20L)) {
      real <- 0:n |>
        purrr::map(function(k) {
          tidyr::expand_grid(
            x = (k / n) |> reround(2, rounding),
            sd = (k / n) |> sd_binary_mean_n(n) |> reround(2, rounding)
          )
        }) |>
        purrr::list_rbind()
      real <- paste(round(real$x * 100), round(real$sd * 100))
      expected <- paste(round(grid$x * 100), round(grid$sd * 100)) %in% real

      exact  <- grid$x |> debit(grid$sd, n, 2, 2, rounding = rounding)
      mean_n <- grid$x |> debit(grid$sd, n, 2, 2, "mean_n", rounding)

      exact |> expect_equal(expected)
      # The difference only ever turns `TRUE` into `FALSE`:
      (exact & !mean_n) |> any() |> expect_false()
    }
  }
})


# The full oracle: the test above for every rounding method, with and without
# `symmetric`, for more sample sizes and decimal places. `"even"` needs care in
# the oracle itself. `base::round()` decides a tie by the binary representation
# of the number, so `round(0.95, 1)` is 0.9, but scrutiny takes both neighbors
# of an exact tie to be possible (see `rounding_offsets()`). The oracle must do
# the same, so it detects exact ties in whole-number arithmetic. `twice` is
# twice the value in units of the last decimal place: an odd whole number
# exactly at a tie.

if (run_full_debit_oracle) {
  oracle_round <- function(value, twice, is_tie, digits, rounding, symmetric) {
    if (is_tie && rounding == "even") {
      c(twice - 1, twice + 1) / 2 / 10^digits
    } else {
      reround(value, digits, rounding, threshold = 4, symmetric = symmetric)
    }
  }

  # Every value that the mean of `k` ones among `n` values could be reported as:
  oracle_means <- function(k, n, digits, rounding, symmetric) {
    twice_times_n <- 2 * k * 10^digits
    twice <- twice_times_n %/% n
    is_tie <- twice_times_n %% n == 0 && twice %% 2 == 1
    oracle_round(k / n, twice, is_tie, digits, rounding, symmetric)
  }

  # ...and the same for their SD, whose square is `k * (n - k) / (n * (n - 1))`:
  oracle_sds <- function(k, n, digits, rounding, symmetric) {
    sd <- sd_binary_1_n(k, n)
    twice <- round(2 * sd * 10^digits)
    is_tie <- twice %% 2 == 1 &&
      twice^2 * n * (n - 1) == 4 * k * (n - k) * 10^(2 * digits)
    oracle_round(sd, twice, is_tie, digits, rounding, symmetric)
  }

  specs <- tibble::tibble(
    rounding = c(
      rounding_methods,
      "ties_up",
      "ties_down",
      "ties_away",
      "ties_zero",
      "up_or_down",
      "up",
      "down",
      "up_from",
      "down_from",
      "up_from_or_down_from"
    ),
    symmetric = rep(c(FALSE, TRUE), c(length(rounding_methods) + 4L, 6L))
  )

  test_that("`formula = \"exact\"` passes exactly the real binary samples (full)", {
    for (i in seq_len(nrow(specs))) {
      rounding <- specs$rounding[i]
      symmetric <- specs$symmetric[i]

      for (n in c(2L, 3L, 9L, 13L, 40L, 101L, 333L)) {
        for (digits_x in 1:2) {
          for (digits_sd in 1:2) {
            real <- 0:n |>
              purrr::map(function(k) {
                tidyr::expand_grid(
                  x  = oracle_means(k, n, digits_x,  rounding, symmetric),
                  sd = oracle_sds(  k, n, digits_sd, rounding, symmetric)
                )
              }) |>
              purrr::list_rbind()
            real <- paste(round(real$x * 10^digits_x), round(real$sd * 10^digits_sd))

            grid <- tidyr::expand_grid(
              x  = 0:10^digits_x / 10^digits_x,
              sd = 0:(0.75 * 10^digits_sd) / 10^digits_sd
            )
            expected <- paste(round(grid$x * 10^digits_x), round(grid$sd * 10^digits_sd)) %in% real

            exact <- grid$x |>
              debit(grid$sd, n, digits_x, digits_sd, "exact", rounding, 4, symmetric)

            mean_n <- grid$x |>
              debit(grid$sd, n, digits_x, digits_sd, "mean_n", rounding, 4, symmetric)

            label <- paste0(
              "rounding = ", rounding, ", symmetric = ", symmetric, ", n = ", n,
              ", digits_x = ", digits_x, ", digits_sd = ", digits_sd
            )
            exact |> expect_equal(expected, label = label)
            # The difference only ever turns `TRUE` into `FALSE`:
            (exact & !mean_n) |> any() |> expect_false(label = label)
          }
        }
      }
    }
  })
}

# Two branches of `binary_sd_attainable()` that the grid above doesn't reach.
# The full oracle does, but it is off by default.

test_that("`formula = \"exact\"` folds a range of `k` around `n / 2` fully", {
  # 22 ones in 40: a mean of 0.55, reported as 0.5 when rounding up from 6, and
  # an SD of 0.50383. Folded onto the lower half, `k = 22` is `j = 18`, which is
  # below the least `k`, 19, so the range of `j` has to start at `n - 22`:
  0.5 |> debit(0.504, 40, 1, 3, rounding = "up_from", threshold = 6) |> expect_true()
})

test_that("`formula = \"exact\"` excludes an SD exactly on an exclusive bound", {
  # Three ones in nine have an SD of exactly 0.5, which `"floor"` reports as
  # 0.5, not 0.4 -- the upper bound of 0.4 is exclusive:
  0.33 |> debit(0.4, 9, 2, 1, rounding = "floor") |> expect_false()
  0.33 |> debit(0.5, 9, 2, 1, rounding = "floor") |> expect_true()
})

test_that("`formula = \"mean_n\"` accepts SDs no binary sample has", {
  0.05 |> debit(0.21, 20, 2, 2)                     |> expect_false()
  0.05 |> debit(0.21, 20, 2, 2, formula = "mean_n") |> expect_true()
  0.05 |> debit(0.22, 20, 2, 2)                     |> expect_true()
})

test_that("`formula` other than \"exact\" or \"mean_n\" is an error", {
  0.35 |>
    debit(0.48, 100, digits_x = 2, digits_sd = 2, formula = "0_n") |>
    expect_error("must be \"exact\" or \"mean_n\"")
})
