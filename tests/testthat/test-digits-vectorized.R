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
  per_row <- df_grim |> grim_map(digits_x = c(2, 1), items_averaged = 1, data_type = "integer") |> purrr::pluck("consistency")
  separate <- c(
    df_grim[1, ] |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> purrr::pluck("consistency"),
    df_grim[2, ] |> grim_map(digits_x = 1, items_averaged = 1, data_type = "integer") |> purrr::pluck("consistency")
  )
  per_row |> expect_equal(separate)

  # `0.1` at one decimal place is not the same test as `0.10` at two:
  per_row |> expect_equal(c(TRUE, TRUE))
  df_grim |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("consistency") |>
    expect_equal(c(TRUE, FALSE))

  separate <- c(
    df_grimmer[1, ] |> grimmer_map(digits_x = 1, digits_sd = 2, items_averaged = 1, data_type = "integer") |> purrr::pluck("consistency"),
    df_grimmer[2, ] |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> purrr::pluck("consistency")
  )
  df_grimmer |>
    grimmer_map(digits_x = c(1, 2), digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("consistency") |>
    expect_equal(separate)

  separate <- c(
    df_debit[1, ] |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |> purrr::pluck("consistency"),
    df_debit[2, ] |> debit_map(digits_x = 2, digits_sd = 1, data_type = "integer") |> purrr::pluck("consistency")
  )
  df_debit |>
    debit_map(digits_x = 2, digits_sd = c(2, 1), data_type = "integer") |>
    purrr::pluck("consistency") |>
    expect_equal(separate)
})


test_that("a single `digits_*` value still applies to the whole column", {
  per_row <- df_grim |> grim_map(digits_x = c(2, 2), items_averaged = 1, data_type = "integer") |> purrr::pluck("consistency")
  df_grim |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("consistency") |>
    expect_equal(per_row)
})


test_that("a `digits_*` vector of the wrong length is an error", {
  df_grim |>
    grim_map(digits_x = c(2, 1, 2), items_averaged = 1, data_type = "integer") |>
    expect_error("length 1 or the number of rows")
  df_grimmer |>
    grimmer_map(digits_x = c(1, 2, 1), digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    expect_error("length 1 or the number of rows")
})


# In a total-n mapper, a length-2 `digits_*` states the decimal places of the
# two groups. This is what makes the Bauer and Francis (2021) case study in
# `vignette("grim")` reproducible: it reports means of 4.71 and 5.3, which have
# different numbers of decimal places.
test_that("total-n mappers take one `digits_*` value per group", {
  df <- tibble::tibble(x1 = 4.71, x2 = 5.3, n = 40)
  out <- df |> grim_map_total_n(digits_x = c(2, 1), items_averaged = 1, data_type = "integer") |> audit()

  # 17/23 with the original pairing, plus 19/21 and 16/24 with it reversed:
  out$hits_forth |> expect_equal(1L)
  out$hits_back  |> expect_equal(2L)
  out$hits_total |> expect_equal(3L)

  # Testing 5.3 as "5.30" is stricter and finds only what Bauer and Francis did:
  out_strict <- df |> grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer") |> audit()
  out_strict$hits_total |> expect_equal(1L)
})


test_that("total-n mappers reject a `digits_*` vector that isn't length 2", {
  df <- tibble::tibble(x1 = 4.71, x2 = 5.3, n = 40)
  df |>
    grim_map_total_n(digits_x = c(2, 1, 2), items_averaged = 1, data_type = "integer") |>
    expect_error("must have length 1 or 2")
})


test_that("sequence mappers disperse each case on its own decimal level", {
  out <- df_grim |> grim_map_seq(digits_x = c(2, 1), items_averaged = 1, data_type = "integer", dispersion = 1:2, include_consistent = TRUE)
  out |> dplyr::filter(case == 1, var == "x") |> _$x |> expect_equal(c(4.69, 4.70, 4.72, 4.73))
  out |> dplyr::filter(case == 2, var == "x") |> _$x |> expect_equal(c(-0.1, 0.0, 0.2, 0.3))
  out |> dplyr::distinct(case, digits_x) |> _$digits_x |> expect_equal(c(2L, 1L))
  out |> with(grim(x, n, digits_x, items_averaged, data_type)) |> expect_equal(out$consistency)
  # The same from a column:
  df_grim |> dplyr::mutate(digits_x = c(2, 1)) |> grim_map_seq(items_averaged = 1, data_type = "integer", dispersion = 1:2, include_consistent = TRUE) |> unclass_scr() |> expect_equal(unclass_scr(out))
  # A case to disperse needs its decimal level:
  df_grim |> dplyr::mutate(x = c(5.19, 0.1)) |> grim_map_seq(digits_x = c(NA, 1), items_averaged = 1, data_type = "integer") |> expect_error("missing in a case to disperse")
})


test_that("mappers take `digits_*` from columns of `data`", {
  df_grim  |> dplyr::mutate(digits_x = c(2, 1)) |> grim_map(items_averaged = 1, data_type = "integer") |> unclass_scr() |>
    expect_equal(df_grim |> grim_map(digits_x = c(2, 1), items_averaged = 1, data_type = "integer") |> unclass_scr())
  df_grimmer |> dplyr::mutate(digits_x = 1:2, digits_sd = 2) |> grimmer_map(items_averaged = 1, data_type = "integer") |> _$consistency |>
    expect_equal(df_grimmer |> grimmer_map(digits_x = 1:2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> _$consistency)
  # In a total-n mapper, a row's value applies to both groups:
  df <- tibble::tibble(x1 = c(4.71, 5.3), x2 = c(5.38, 4.9), n = c(40, 40))
  out <- df |> dplyr::mutate(digits_x = c(2, 1)) |> grim_map_total_n(items_averaged = 1, data_type = "integer")
  out |> dplyr::distinct(case, digits_x) |> _$digits_x |> expect_equal(c(2L, 1L))
  out |> with(grim(x, n, digits_x, items_averaged, data_type)) |> expect_equal(out$consistency)
  # Without either, the error says that a column would do:
  df_grim |> grim_map(items_averaged = 1, data_type = "integer") |> expect_error("`digits_x` column")
})


# The dispersion step size has to come from `digits_*`, not from the stored
# value. A mean reported as 5.30 is stored as `5.3`, so going by the value gave
# it steps of 0.1 while a neighbouring 4.71 got steps of 0.01 -- in the same
# call, with the same `digits_x`.
test_that("`*_map_seq()` steps on the decimal level given by `digits_*`", {
  out <- tibble::tibble(x = c(5.3, 4.71), n = c(40, 40)) |>
    grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = 1:3, include_consistent = TRUE)
  out_x <- out[out$var == "x", ]

  # Every dispersed value is one hundredth away from its neighbour:
  out_x$x |> decimal_places() |> max() |> expect_equal(2L)

  c(5.3, 4.71) |>
    rep(each = 6) |>
    call_on(\(v) out_x$x - v) |>
    abs() |>
    round(10) |>
    call_on(\(x) x <= 0.03) |>
    all() |>
    expect_true()

  # `n` has no `digits_n`, so it keeps stepping by whole numbers:
  out$n[out$var == "n"] |>
    call_on(\(x) x %% 1 == 0) |>
    all() |>
    expect_true()
})


test_that("an all-`NA` `digits_*` column of any type is an integer `NA`", {
  df <- tibble::tibble(x = c(5.19, 4.2), n = c(28, 25))
  for (na in list(NA, NA_character_, NA_real_)) {
    out <- df |> dplyr::mutate(digits_x = na) |> grim_map(items_averaged = 1, data_type = "integer")
    out$digits_x |> expect_identical(c(NA_integer_, NA_integer_))
    out$consistency |> expect_identical(grim(df$x, df$n, NA, items_averaged = 1, data_type = "integer"))
  }
  out <- df |> grim_map(digits_x = NA, items_averaged = 1, data_type = "integer")
  out$digits_x |> expect_identical(c(NA_integer_, NA_integer_))
})


test_that("a `digits_*` column or argument of strings is an error that names it", {
  df <- tibble::tibble(x = c(5.19, 4.2), n = c(28, 25))
  df |> dplyr::mutate(digits_x = "2") |> grim_map(items_averaged = 1, data_type = "integer") |> expect_error("`digits_x` column of `data` is a character vector")
  df |> grim_map(digits_x = "2", items_averaged = 1, data_type = "integer") |> expect_error("`digits_x` must be numeric")
  df |> dplyr::mutate(digits_x = factor("2")) |> grim_map(items_averaged = 1, data_type = "integer") |> expect_error("not a factor")
  # One bad value among numbers is still blamed on its row:
  df |> grim_map(digits_x = c(2, 1.5), items_averaged = 1, data_type = "integer") |> expect_error("In row 2")
})
