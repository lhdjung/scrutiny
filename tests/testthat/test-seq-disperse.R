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
  25     |> seq_disperse()    |> expect_equal(basic1_exp)
  25     |> seq_disperse_df() |> expect_equal(basic1_df_exp)
  "4.56" |> seq_disperse()    |> expect_equal(basic2_exp)
  "4.56" |> seq_disperse_df() |> expect_equal(basic2_df_exp)
})

test_that("it works when overriding some of the defaults", {
  75 |>
    seq_disperse(
      out_max = 77,
      include_reported = FALSE, track_diff_var = TRUE
    ) |>
    expect_equal(with_out_max_exp)
  75 |>
    seq_disperse_df(
      .out_max = 77,
      .include_reported = FALSE, .track_diff_var = TRUE
    ) |>
    expect_equal(with_out_max_df_exp)
  0.6 |>
    seq_disperse(
      dispersion = c(3, 5, 6),
      track_diff_var = TRUE, include_reported = FALSE
    ) |>
    expect_equal(with_track_diff_var)
})

# The dots used to be captured as expressions and evaluated inside
# `seq_disperse_df()`, where a variable local to the caller does not exist:
test_that("`seq_disperse_df()` evaluates the dots where they were written", {
  add_n <- function() {
    my_n <- 45
    seq_disperse_df(.from = 4.02, n = my_n, .dispersion = 1)
  }
  add_n() |> purrr::pluck("n") |> expect_equal(c(45, 45, 45))
})

# The sequence must stay on the decimal level given by `by` (or, if `by` is not
# specified, by `from`), no matter how far `dispersion` reaches. Floating-point
# arithmetic used to break this: `3.14 - (305 * 0.01)` is `0.0899999999999999`
# rather than `0.09`. See issue #83.
test_that("long dispersion sequences stay on their decimal level", {
  3.14 |>
    seq_disperse(
      dispersion = 1:305,
      string_output = FALSE, include_reported = FALSE
    ) |>
    decimal_places() |>
    max() |>
    expect_equal(2L)

  3.14 |>
    seq_disperse(
      dispersion = 305,
      string_output = FALSE, include_reported = FALSE
    ) |>
    expect_equal(c(0.09, 6.19))

  # Same for a manually specified `by` with fewer decimal places than `from`:
  3.14 |>
    seq_disperse(
      by = 0.1, dispersion = 31, out_min = NULL,
      string_output = FALSE, include_reported = FALSE
    ) |>
    expect_equal(c(0.04, 6.24))
})


test_that("a zero step doesn't repeat the value it disperses from", {
  # Each value in `dispersion` is a number of steps taken both up and down, so a
  # step of 0 used to add `from` twice on top of `include_reported`.
  4.02 |> seq_disperse(dispersion = 0) |> expect_equal("4.02")
  4.02 |>
    seq_disperse(dispersion = 0, include_reported = FALSE) |>
    expect_equal(character(0))
  4.02 |>
    seq_disperse(dispersion = c(0, 1)) |>
    expect_equal(c("4.01", "4.02", "4.03"))
})


# Each limit used to be checked against one side of the sequence only: the steps
# down against `out_min`, the steps up against `out_max`. So with `from = 30`
# and `out_max = 25`, the steps down landed on `29`, `28`, ..., all above the
# maximum, and all kept.
test_that("both limits apply to both sides of the sequence", {
  30 |> seq_disperse(out_max = 25, include_reported = FALSE) |> expect_equal("25")
  30 |> seq_disperse(out_max = 24, include_reported = FALSE) |> expect_equal(character(0))
  30 |> seq_disperse(out_max = 27, include_reported = FALSE) |> expect_equal(c("25", "26", "27"))
  30 |> seq_disperse(out_min = 33, include_reported = FALSE) |> expect_equal(c("33", "34", "35"))
  # The limits apply after the offset, since that is what moves the values:
  30 |>
    seq_disperse(offset_from = 10, out_max = 42, include_reported = FALSE) |>
    expect_equal(c("35", "36", "37", "38", "39", "41", "42"))
})


test_that("the limits apply to `from` itself", {
  30 |> seq_disperse(out_max = 25)                    |> expect_equal("25")
  30 |> seq_disperse(out_max = 30)                    |> expect_equal(as.character(25:30))
  0  |> seq_disperse(dispersion = 1:2)                |> expect_equal(c("1", "2"))
  0  |> seq_disperse(dispersion = 1:2, out_min = NULL) |> expect_equal(as.character(-2:2))
  30 |> seq_disperse(out_max = 25, track_diff_var = TRUE) |> expect_equal(list("25", -5L))
})


test_that("`seq_disperse()` checks `dispersion` and the limits", {
  # A fractional step used to be taken and then padded off the decimal level:
  4 |> seq_disperse(dispersion = 1.5) |> expect_error("whole numbers")
  # `from` with more decimal places than `by` failed in `restore_zeros()`:
  0.35 |>
    seq_disperse(by = 0.1, dispersion = 1:2) |>
    expect_equal(c("0.15", "0.25", "0.35", "0.45", "0.55"))
  # A string limit was compared as a string, so `"9" > "10"`:
  8 |>
    seq_disperse(dispersion = 1:3, out_max = "10") |>
    expect_equal(c("5", "6", "7", "8", "9", "10"))
  4 |> seq_disperse(out_min = NA) |> expect_error("single number")
})


# An integer `from` was coerced back to integer after dispersion, truncating
# every fractional step: `4L` gave `3 3 3 3 3 4 4 4 4 4 4`.
test_that("an integer `from` keeps fractional steps", {
  4L |> seq_disperse(by = 0.1, dispersion = 1:2, string_output = FALSE) |> expect_equal(c(3.8, 3.9, 4, 4.1, 4.2))
  4L |> seq_disperse(dispersion = 1:2, string_output = FALSE) |> expect_identical(2:6)
})


test_that("`from` must be a finite number", {
  NA_real_ |> seq_disperse()          |> expect_error("finite number")
  NA_real_ |> seq_disperse(by = 0.1)  |> expect_error("finite number")
  Inf      |> seq_disperse()          |> expect_error("finite number")
  "abc"    |> seq_disperse()          |> expect_error("finite number")
})
