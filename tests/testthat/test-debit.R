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
  # `anti_trunc` is the one rounding method with no bounds at zero:
  debit(
    x = 0,
    sd = 0.50,
    n = 1683,
    digits_x = 2,
    digits_sd = 2,
    rounding = "anti_trunc"
  ) |>
    expect_na()
})


test_that("`debit()` still checks the range of its inputs", {
  debit(x = 1.5, sd = 0.5, n = 100, digits_x = 2, digits_sd = 2) |>
    expect_error()
  debit(x = 0.5, sd = 1.5, n = 100, digits_x = 2, digits_sd = 2) |>
    expect_error()
})
