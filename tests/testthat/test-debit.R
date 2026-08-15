# x <- rnorm(1000, 0.5, 0.08) |> censor(0, 1) |> as.character()
# sd <- runif(1000, 0.1, 0.4) |> as.character()
# n <- rnorm(1000, 1000, 200) |> censor(100, 1900)
#
#
# x <- seq_endpoint("0.01", 1) |> rep(10)
# sd <- rnorm(100, 0.15, 0.05) |> round(2) |> restore_zeros(width = 2)
#
# out <- purrr::pmap_lgl(list(x, sd, n), debit)

out <- purrr::pmap_lgl(pigs3, debit, digits_x = 2, digits_sd = 2)
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
    debit(
      x = 0.53,
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
    out_5 <- purrr::pmap_lgl(
      pigs3,
      debit,
      digits_x = 2,
      digits_sd = 2,
      rounding = r,
      threshold = 5
    )
    out_9 <- purrr::pmap_lgl(
      pigs3,
      debit,
      digits_x = 2,
      digits_sd = 2,
      rounding = r,
      threshold = 9
    )
    out_5 |> expect_equal(out_9)
  }
})


test_that("`symmetric` is taken into account", {
  # `debit_scalar()` reconstructs the bounds under the same assumption that it
  # re-rounds with. It used to unround asymmetrically and re-round
  # symmetrically, because the old `unround()` had no `symmetric` argument.
  debit(
    x = 0.53,
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
  out_rec |> expect_length(8L)
  out_rec[[1L]] |> expect_true()
  out_rec[[2L]] |> expect_equal("up_or_down")
  # `sd_lower`, `sd_upper`, `x_lower`, and `x_upper`:
  out_rec[[3L]] |> expect_equal(0.495)
  out_rec[[5L]] |> expect_equal(0.505)
  out_rec[[7L]] |> expect_equal(0.525)
  out_rec[[8L]] |> expect_equal(0.535)
})


test_that("`debit()` returns `NA` where the bounds are undefined", {
  # A missing value has no bounds to derive. (This used to be tested with
  # `rounding = "anti_trunc"` at a mean of zero, which had no bounds either
  # until `anti_trunc()` stopped sending zero away from zero.)
  debit(
    x = NA,
    sd = 0.50,
    n = 1683,
    digits_x = 2,
    digits_sd = 2
  ) |>
    expect_na()

  debit(
    x = 0.30,
    sd = NA,
    n = 1683,
    digits_x = 2,
    digits_sd = 2
  ) |>
    expect_na()
})


test_that("`debit()` still checks the range of its inputs", {
  debit(x = 1.5, sd = 0.5, n = 100, digits_x = 2, digits_sd = 2) |>
    expect_error()
  debit(x = 0.5, sd = 1.5, n = 100, digits_x = 2, digits_sd = 2) |>
    expect_error()
})


# Boundary means ----------------------------------------------------------

# A mean of binary data cannot lie outside of 0 and 1, but its rounding bounds
# can. `sd_binary_mean_n()` returns `NaN` for such a bound, which used to make
# the verdict undecidable for a mean reported as 0.00 or 1.00 -- even though
# both are perfectly consistent: every value is 0, or every value is 1, and the
# SD is 0 either way.

test_that("DEBIT decides means of exactly 0 and 1", {
  debit(x = 0, sd = 0, n = 50, digits_x = 2, digits_sd = 2) |> expect_true()
  debit(x = 1, sd = 0, n = 50, digits_x = 2, digits_sd = 2) |> expect_true()
  debit(x = 0, sd = 0, n = 5, digits_x = 3, digits_sd = 3) |> expect_true()
  debit(x = 1, sd = 0, n = 5, digits_x = 3, digits_sd = 3) |> expect_true()
})


test_that("DEBIT still rejects impossible SDs at those means", {
  debit(x = 0, sd = 0.5, n = 50, digits_x = 2, digits_sd = 2) |> expect_false()
  debit(x = 1, sd = 0.5, n = 50, digits_x = 2, digits_sd = 2) |> expect_false()
})


test_that("`debit_map()` reports no negative SD bound and no mean out of range", {
  out <- debit_map(
    tibble::tibble(x = c(0, 1), sd = c(0, 0), n = c(50L, 50L)),
    digits_x = 2,
    digits_sd = 2
  )
  (out$sd_lower >= 0)  |> all() |> expect_true()
  (out$x_lower  >= 0)  |> all() |> expect_true()
  (out$x_upper  <= 1)  |> all() |> expect_true()
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
      means <- reround(k / n, digits, "up_or_down")
      sds <- reround(
        sd_binary_0_n(group_0 = n - k, n = n), digits, "up_or_down"
      )

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
            expect_true(
              isTRUE(debit(
                x = x, sd = sd, n = n,
                digits_x = digits, digits_sd = digits
              )),
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
  expect_gt(n_checked, 100L)
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
  debit(x = 0.50, sd = 0.503, n = 100, digits_x = 2, digits_sd = 3) |>
    expect_true()
})

test_that("DEBIT accepts every real binary data set reported as a mean of 0.50", {
  # Enumerate the actual data sets: `k` ones out of `n`, keeping those whose
  # mean would have been reported as 0.50 at two decimal places.
  cases <- purrr::map(10:250, function(n) {
    k <- 0:n
    k <- k[abs((k / n) - 0.5) <= 0.005]
    sd_true <- sd_binary_mean_n(k / n, n)
    tibble::tibble(
      n = n,
      sd_rep = c(round_up(sd_true, 3L), round_down(sd_true, 3L))
    )
  })
  cases <- purrr::list_rbind(cases)
  cases <- dplyr::distinct(cases)

  out <- purrr::pmap_lgl(
    list(cases$sd_rep, cases$n),
    function(sd_rep, n) {
      debit(x = 0.50, sd = sd_rep, n = n, digits_x = 2, digits_sd = 3)
    }
  )

  out |> all() |> expect_true()
})

test_that("`formula` other than \"mean_n\" is an error, not a missing argument", {
  debit(0.35, 0.48, 100, digits_x = 2, digits_sd = 2, formula = "0_n") |>
    expect_error("must be \"mean_n\"")
  debit(0.35, 0.48, 100, digits_x = 2, digits_sd = 2, formula = "groups") |>
    expect_error("must be \"mean_n\"")
})
