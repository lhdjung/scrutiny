basic1_exp <- c(
  "20",
  "21",
  "22",
  "23",
  "24",
  "25",
  "26",
  "27",
  "28",
  "29",
  "30"
)

basic1_df_exp <- tibble::tibble(
  x = c("20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30")
)

basic2_exp <- c(
  "4.51",
  "4.52",
  "4.53",
  "4.54",
  "4.55",
  "4.56",
  "4.57",
  "4.58",
  "4.59",
  "4.60",
  "4.61"
)

basic2_df_exp <- tibble::tibble(
  x = c(
    "4.51",
    "4.52",
    "4.53",
    "4.54",
    "4.55",
    "4.56",
    "4.57",
    "4.58",
    "4.59",
    "4.60",
    "4.61"
  ),
)

with_out_max_exp <- list(
  c("70", "71", "72", "73", "74", "76", "77"),
  c(-5L, -4L, -3L, -2L, -1L, 1L, 2L)
)

with_out_max_df_exp <- tibble::tibble(
  x = c("70", "71", "72", "73", "74", "76", "77"),
  diff_var = c(-5L, -4L, -3L, -2L, -1L, 1L, 2L),
)

# The `-5` step lands on `0.1`, which is exactly `out_min` (the default,
# `"auto"`, resolves to one decimal unit), so it belongs in the sequence. It
# used to be dropped because `0.6 - 5 * 0.1` is `0.09999999999999998` in
# floating-point arithmetic, hence seemingly below `out_min`.
with_track_diff_var <- list(
  c("0.1", "0.3", "0.9", "1.1", "1.2"),
  c(-5, -3, 3, 5, 6)
)


# Testing -----------------------------------------------------------------

test_that("it works with the defaults", {
  seq_disperse(25)        |> expect_equal(basic1_exp)
  seq_disperse_df(25)     |> expect_equal(basic1_df_exp)
  seq_disperse("4.56")    |> expect_equal(basic2_exp)
  seq_disperse_df("4.56") |> expect_equal(basic2_df_exp)
})

test_that("it works when overriding some of the defaults", {
  seq_disperse(
    75, out_max = 77,
    include_reported = FALSE, track_diff_var = TRUE
  ) |>
    expect_equal(with_out_max_exp)
  seq_disperse_df(
    75, .out_max = 77,
    .include_reported = FALSE, .track_diff_var = TRUE
  ) |>
    expect_equal(with_out_max_df_exp)
  seq_disperse(
    from = 0.6, dispersion = c(3, 5, 6),
    track_diff_var = TRUE, include_reported = FALSE
  ) |>
    expect_equal(with_track_diff_var)
})

# The sequence must stay on the decimal level given by `by` (or, if `by` is not
# specified, by `from`), no matter how far `dispersion` reaches. Floating-point
# arithmetic used to break this: `3.14 - (305 * 0.01)` is `0.0899999999999999`
# rather than `0.09`. See issue #83.
test_that("long dispersion sequences stay on their decimal level", {
  seq_disperse(
    from = 3.14, dispersion = 1:305,
    string_output = FALSE, include_reported = FALSE
  ) |>
    decimal_places() |>
    max() |>
    expect_equal(2L)

  seq_disperse(
    from = 3.14, dispersion = 305,
    string_output = FALSE, include_reported = FALSE
  ) |>
    expect_equal(c(0.09, 6.19))

  # Same for a manually specified `by` with fewer decimal places than `from`:
  seq_disperse(
    from = 3.14, by = 0.1, dispersion = 31, out_min = NULL,
    string_output = FALSE, include_reported = FALSE
  ) |>
    expect_equal(c(0.04, 6.24))
})


test_that("a zero step doesn't repeat the value it disperses from", {
  # Each value in `dispersion` is a number of steps taken both up and down, so a
  # step of 0 used to add `from` twice on top of `include_reported`.
  seq_disperse(from = 4.02, dispersion = 0) |> expect_equal("4.02")
  seq_disperse(from = 4.02, dispersion = 0, include_reported = FALSE) |>
    expect_equal(character(0))
  seq_disperse(from = 4.02, dispersion = c(0, 1)) |>
    expect_equal(c("4.01", "4.02", "4.03"))
})
