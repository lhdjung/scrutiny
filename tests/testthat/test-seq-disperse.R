basic1_exp <- 20:30 + 0

basic2_exp <- seq(4.51, 4.61, by = 0.01) |> round(2)

with_out_max_exp <- list(
  c(70, 71, 72, 73, 74, 76, 77),
  c(-5L, -4L, -3L, -2L, -1L, 1L, 2L)
)

with_out_max_df_exp <- tibble::tibble(
  x = c(70, 71, 72, 73, 74, 76, 77),
  diff_var = c(-5L, -4L, -3L, -2L, -1L, 1L, 2L),
)

# The `-5` step lands on `0.1`, which is exactly `out_min` (the default,
# `"auto"`, resolves to one decimal unit), so it belongs in the sequence. It
# used to be dropped because `0.6 - 5 * 0.1` is `0.09999999999999998` in
# floating-point arithmetic, hence seemingly below `out_min`.
with_track_diff_var <- list(
  c(0.1, 0.3, 0.9, 1.1, 1.2),
  c(-5, -3, 3, 5, 6)
)


# Testing -----------------------------------------------------------------

test_that("it works with the defaults", {
  25     |> seq_disperse()    |> expect_equal(basic1_exp)
  25     |> seq_disperse_df() |> expect_equal(tibble::tibble(x = basic1_exp))
  4.56   |> seq_disperse()    |> expect_equal(basic2_exp)
  "4.56" |> seq_disperse_df() |> expect_equal(tibble::tibble(x = basic2_exp))
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

# Handed on to `tibble()`, a variable named like an output column was taken for
# that column:
test_that("`seq_disperse_df()` finds a caller's variable named `x` or `diff_var`", {
  add_n <- function(x, diff_var) {
    seq_disperse_df(.from = 4.02, n = x, group = diff_var, .dispersion = 1, .track_diff_var = TRUE)
  }
  out <- add_n(x = 45, diff_var = "a")
  out$n        |> expect_equal(c(45, 45, 45))
  out$group    |> expect_equal(c("a", "a", "a"))
  out$diff_var |> expect_equal(-1:1)
})

# The sequence must stay on the decimal level given by `by` (or, if `by` is not
# specified, by `from`), no matter how far `dispersion` reaches. Floating-point
# arithmetic used to break this: `3.14 - (305 * 0.01)` is `0.0899999999999999`
# rather than `0.09`. See issue #83.
test_that("long dispersion sequences stay on their decimal level", {
  3.14 |>
    seq_disperse(
      dispersion = 1:305,
      include_reported = FALSE
    ) |>
    decimal_places() |>
    max() |>
    expect_equal(2L)

  3.14 |>
    seq_disperse(
      dispersion = 305,
      include_reported = FALSE
    ) |>
    expect_equal(c(0.09, 6.19))

  # Same for a manually specified `by` with fewer decimal places than `from`:
  3.14 |>
    seq_disperse(
      by = 0.1, dispersion = 31, out_min = NULL,
      include_reported = FALSE
    ) |>
    expect_equal(c(0.04, 6.24))
})


test_that("a zero step doesn't repeat the value it disperses from", {
  # Each value in `dispersion` is a number of steps taken both up and down, so a
  # step of 0 used to add `from` twice on top of `include_reported`.
  4.02 |> seq_disperse(dispersion = 0) |> expect_equal(4.02)
  4.02 |>
    seq_disperse(dispersion = 0, include_reported = FALSE) |>
    expect_equal(numeric(0))
  4.02 |>
    seq_disperse(dispersion = c(0, 1)) |>
    expect_equal(c(4.01, 4.02, 4.03))
  # So does a near-whole step of 0, which used to repeat it:
  4.02 |>
    seq_disperse(dispersion = c((0.1 + 0.2) * 10 - 3, 1)) |>
    expect_equal(c(4.01, 4.02, 4.03))
})


# Each limit used to be checked against one side of the sequence only: the steps
# down against `out_min`, the steps up against `out_max`. So with `from = 30`
# and `out_max = 25`, the steps down landed on `29`, `28`, ..., all above the
# maximum, and all kept.
test_that("both limits apply to both sides of the sequence", {
  30 |> seq_disperse(out_max = 25, include_reported = FALSE) |> expect_equal(25)
  30 |> seq_disperse(out_max = 24, include_reported = FALSE) |> expect_equal(numeric(0))
  30 |> seq_disperse(out_max = 27, include_reported = FALSE) |> expect_equal(c(25, 26, 27))
  30 |> seq_disperse(out_min = 33, include_reported = FALSE) |> expect_equal(c(33, 34, 35))
  # The limits apply after the offset, since that is what moves the values:
  30 |>
    seq_disperse(offset_from = 10, out_max = 42, include_reported = FALSE) |>
    expect_equal(c(35, 36, 37, 38, 39, 41, 42))
})


test_that("the limits apply to `from` itself", {
  30 |> seq_disperse(out_max = 25)                    |> expect_equal(25)
  30 |> seq_disperse(out_max = 30)                    |> expect_equal(25:30 + 0)
  0  |> seq_disperse(dispersion = 1:2)                |> expect_equal(c(1, 2))
  0  |> seq_disperse(dispersion = 1:2, out_min = NULL) |> expect_equal(-2:2 + 0)
  30 |> seq_disperse(out_max = 25, track_diff_var = TRUE) |> expect_equal(list(25, -5L))
})


test_that("`seq_disperse()` checks `dispersion` and the limits", {
  # A fractional step used to be taken and then padded off the decimal level:
  4 |> seq_disperse(dispersion = 1.5) |> expect_error("whole numbers")
  4 |> seq_disperse(dispersion = "a") |> expect_error("must be one of these types")
  # `from` with more decimal places than `by` failed in `restore_zeros()`:
  0.35 |>
    seq_disperse(by = 0.1, dispersion = 1:2) |>
    expect_equal(c(0.15, 0.25, 0.35, 0.45, 0.55))
  # A string limit was compared as a string, so `"9" > "10"`:
  8 |>
    seq_disperse(dispersion = 1:3, out_max = "10") |>
    expect_equal(5:10 + 0)
  4 |> seq_disperse(out_min = NA) |> expect_error("single number")
})


# An integer `from` was coerced back to integer after dispersion, truncating
# every fractional step: `4L` gave `3 3 3 3 3 4 4 4 4 4 4`.
test_that("an integer `from` keeps fractional steps", {
  4L |> seq_disperse(by = 0.1, dispersion = 1:2) |> expect_equal(c(3.8, 3.9, 4, 4.1, 4.2))
  4L |> seq_disperse(dispersion = 1:2)           |> expect_identical(2:6)
})


test_that("`from` must be a finite number", {
  NA_real_ |> seq_disperse()          |> expect_error("finite number")
  NA_real_ |> seq_disperse(by = 0.1)  |> expect_error("finite number")
  Inf      |> seq_disperse()          |> expect_error("finite number")
  "abc"    |> seq_disperse()          |> expect_error("finite number")
})


test_that("a factor `from` is dispersed as its label, not its code", {
  factor("4.02") |> seq_disperse(dispersion = 1) |> expect_equal(c(4.01, 4.02, 4.03))
})

test_that("a string `from` keeps its trailing zeros as the step size", {
  "5.30" |> seq_disperse(dispersion = 1) |> expect_equal(c(5.29, 5.30, 5.31))
  5.30   |> seq_disperse(dispersion = 1) |> expect_equal(c(5.2, 5.3, 5.4))
})

test_that("`by`, `offset_from`, and the flags are checked", {
  4.02 |> seq_disperse(by = 0)                |> expect_error("positive, finite number")
  4.02 |> seq_disperse(by = -0.01)            |> expect_error("positive, finite number")
  4.02 |> seq_disperse(by = Inf)              |> expect_error("positive, finite number")
  4.02 |> seq_disperse(offset_from = 1.5)     |> expect_error("whole number")
  4.02 |> seq_disperse(offset_from = NA_real_) |> expect_error("whole number")
  4.02 |> seq_disperse(offset_from = Inf)      |> expect_error("whole number")
  4.02 |> seq_disperse(include_reported = NA) |> expect_error("include_reported")
})

test_that("the steps are integers even if `dispersion` is double", {
  4.02 |> seq_disperse(dispersion = c(1, 2), track_diff_var = TRUE) |> purrr::pluck(2) |> expect_identical(c(-2L, -1L, 0L, 1L, 2L))
})

test_that("an infinite `dispersion` is an error", {
  seq_disperse(4.02, dispersion = Inf)       |> expect_error("missing values or infinities")
  seq_disperse(4.02, dispersion = c(1, Inf)) |> expect_error("missing values or infinities")
})

test_that("a repeated step counts once", {
  seq_disperse(4, dispersion = c(1, 1)) |> expect_equal(c(3, 4, 5))
})

test_that("a repeated step doesn't keep sequence output from being audited", {
  out <- tibble::tibble(x = 5.19, n = 28) |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = c(1, 1, 2))
  out |> audit()           |> nrow() |> expect_equal(1L)
  out |> reverse_map_seq() |> nrow() |> expect_equal(1L)
})

test_that("the flags must be `TRUE` or `FALSE`", {
  4.02 |> seq_disperse(track_diff_var = NA)        |> expect_error("`track_diff_var` must be `TRUE` or `FALSE`")
  4.02 |> seq_disperse_df(.include_reported = NA)  |> expect_error("`.include_reported` must be `TRUE` or `FALSE`")
  4.02 |> seq_disperse_df(.track_diff_var = "yes") |> expect_error("`.track_diff_var` must be `TRUE` or `FALSE`")
})

test_that("the error about fractional steps counts them as given", {
  4.02 |> seq_disperse(dispersion = c(1.5, 1.5)) |> expect_error("2 values that are not")
})


test_that("`from` must be a number small enough to step from", {
  1e300              |> seq_disperse(dispersion = 1) |> expect_error("too large to disperse exactly")
  123456789012345678 |> seq_disperse(dispersion = 1) |> expect_error("too large to disperse exactly")
  # A double's spacing passes 0.01 there, so values repeated and left the
  # decimal places:
  "80000000000000.01" |> seq_disperse(dispersion = 1:3) |> expect_error("beyond 2\\^52")
  "4000000000000.01"  |> seq_disperse(dispersion = 1:3) |> unique() |> length() |> expect_equal(7)
  # `TRUE` was dispersed as 1:
  TRUE               |> seq_disperse()               |> expect_error("`from` must be one of these types")
  list(1)            |> seq_disperse()               |> expect_error("`from` must be one of these types")
})

test_that("`from` with more decimal places than a double scales to is an error", {
  # Its step underflowed to 0, and a base R error followed:
  "1e-400"          |> seq_disperse() |> expect_error("It has 400, but")
  "1.5e-2147483648" |> seq_disperse() |> expect_error("It has billions, but")
})


test_that("floating-point noise in `from` or a limit doesn't drop a value", {
  # `from` was compared and returned as given, so `0.1 + 0.2` was above an
  # `out_max` of 0.3:
  seq_disperse(0.1 + 0.2, out_max = 0.3)   |> expect_equal(c(0.1, 0.2, 0.3))
  seq_disperse(3 * 1.1, out_max = 3.3)     |> expect_equal(seq_disperse("3.3", out_max = 3.3))
  seq_disperse(100 * 0.57, out_min = 57)   |> expect_equal(57:62)
  seq_disperse(0.1 + 0.2)[3]               |> expect_identical(0.3)
  seq_disperse(0.5, out_min = 0.1 + 0.2, dispersion = 1:3) |> expect_equal(c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8))
})


test_that("limits and `from` are read as the numbers they show", {
  # A factor limit was read by its level code, and `TRUE` as 1:
  seq_disperse(5, out_max = factor("10")) |> expect_equal(1:10)
  seq_disperse(5, out_min = factor("4"))  |> expect_equal(4:10)
  seq_disperse(5, out_max = TRUE)         |> expect_error("must be a single number")
  # A non-breaking space was an error here, though not elsewhere:
  seq_disperse("5.30 ")              |> expect_equal(seq_disperse("5.30"))
  seq_disperse(5, out_max = "7 ")    |> expect_equal(1:7)
  # A hexadecimal `from` was dispersed on the level of its own digits:
  seq_disperse("0x1p-2")                  |> expect_error("must be a decimal number")
  # A `by` with too many decimal places was blamed on `from`:
  seq_disperse(5, by = 1 / 3)             |> expect_error("`by` has too many decimal places")
})
