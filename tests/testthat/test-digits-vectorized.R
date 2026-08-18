# A `digits_*` argument may be a single number for the whole column, or one
# number per row. Before, it had to be a single number, so a data frame whose
# values were reported with different numbers of decimal places could not be
# tested correctly at all: any single `digits_x` was wrong for some of the rows.

# `0.1` at one decimal place is GRIM-consistent with n = 28, but the same value
# read as "0.10" is not. `4.71` needs two decimal places either way.
df_grim <- tibble::tibble(x = c(4.71, 0.1), n = c(28, 28))
df_grimmer <- tibble::tibble(
  x = c(7.3, 5.23),
  sd = c(2.51, 2.55),
  n = c(12, 35)
)
df_debit <- tibble::tibble(
  x = c(0.35, 0.53),
  sd = c(0.18, 0.5),
  n = c(20, 1683)
)


test_that("basic mappers take one `digits_*` value per row", {
  # Testing each row on its own must give the same verdicts as testing them
  # together with a per-row `digits_x`:
  per_row <- grim_map(df_grim, digits_x = c(2, 1))$consistency
  separate <- c(
    grim_map(df_grim[1, ], digits_x = 2)$consistency,
    grim_map(df_grim[2, ], digits_x = 1)$consistency
  )
  expect_equal(per_row, separate)

  # `0.1` at one decimal place is not the same test as `0.10` at two:
  expect_equal(per_row, c(TRUE, TRUE))
  grim_map(df_grim, digits_x = 2)$consistency |> expect_equal(c(TRUE, FALSE))

  grimmer_map(df_grimmer, digits_x = c(1, 2), digits_sd = 2)$consistency |>
    expect_equal(c(
      grimmer_map(df_grimmer[1, ], digits_x = 1, digits_sd = 2)$consistency,
      grimmer_map(df_grimmer[2, ], digits_x = 2, digits_sd = 2)$consistency
    ))

  debit_map(df_debit, digits_x = 2, digits_sd = c(2, 1))$consistency |>
    expect_equal(c(
      debit_map(df_debit[1, ], digits_x = 2, digits_sd = 2)$consistency,
      debit_map(df_debit[2, ], digits_x = 2, digits_sd = 1)$consistency
    ))
})


test_that("a single `digits_*` value still applies to the whole column", {
  grim_map(df_grim, digits_x = 2)$consistency |>
    expect_equal(grim_map(df_grim, digits_x = c(2, 2))$consistency)
})


test_that("a `digits_*` vector of the wrong length is an error", {
  df_grim |>
    grim_map(digits_x = c(2, 1, 2)) |>
    expect_error("length 1 or the number of rows")
  df_grimmer |>
    grimmer_map(digits_x = c(1, 2, 1), digits_sd = 2) |>
    expect_error("length 1 or the number of rows")
})


# In a total-n mapper, a length-2 `digits_*` states the decimal places of the
# two groups. This is what makes the Bauer and Francis (2021) case study in
# `vignette("grim")` reproducible: it reports means of 4.71 and 5.3, which have
# different numbers of decimal places.
test_that("total-n mappers take one `digits_*` value per group", {
  df <- tibble::tibble(x1 = 4.71, x2 = 5.3, n = 40)
  out <- df |> grim_map_total_n(digits_x = c(2, 1)) |> audit_total_n()

  # 17/23 with the original pairing, plus 19/21 and 16/24 with it reversed:
  expect_equal(out$hits_forth, 1L)
  expect_equal(out$hits_back, 2L)
  expect_equal(out$hits_total, 3L)

  # Testing 5.3 as "5.30" is stricter and finds only what Bauer and Francis did:
  out_strict <- df |> grim_map_total_n(digits_x = 2) |> audit_total_n()
  expect_equal(out_strict$hits_total, 1L)
})


test_that("total-n mappers reject a `digits_*` vector that isn't length 2", {
  df <- tibble::tibble(x1 = 4.71, x2 = 5.3, n = 40)
  df |>
    grim_map_total_n(digits_x = c(2, 1, 2)) |>
    expect_error("must have length 1 or 2")
})


test_that("sequence mappers reject a `digits_*` vector", {
  df_grim |>
    grim_map_seq(digits_x = c(2, 1), dispersion = 1:2) |>
    expect_error("must be a single number here")
})


# The dispersion step size has to come from `digits_*`, not from the stored
# value. A mean reported as 5.30 is stored as `5.3`, so going by the value gave
# it steps of 0.1 while a neighbouring 4.71 got steps of 0.01 -- in the same
# call, with the same `digits_x`.
test_that("`*_map_seq()` steps on the decimal level given by `digits_*`", {
  out <- tibble::tibble(x = c(5.3, 4.71), n = c(40, 40)) |>
    grim_map_seq(digits_x = 2, dispersion = 1:3, include_consistent = TRUE)
  out_x <- out[out$var == "x", ]

  # Every dispersed value is one hundredth away from its neighbour:
  out_x$x |> decimal_places() |> max() |> expect_equal(2L)
  expect_true(all(round(abs(out_x$x - rep(c(5.3, 4.71), each = 6)), 10) <= 0.03))

  # `n` has no `digits_n`, so it keeps stepping by whole numbers:
  expect_true(all(out$n[out$var == "n"] %% 1 == 0))
})
