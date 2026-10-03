x <- rnorm(25000, 500, 30)

# fmt: skip
test_reround <- function(x, digits) {
  all(
    x |> reround(digits, "up")         |> dplyr::near(round_up(x, digits)),
    x |> reround(digits, "down")       |> dplyr::near(round_down(x, digits)),
    x |> reround(digits, "even")       |> dplyr::near(round(x, digits)),
    x |> reround(digits, "ceiling")    |> dplyr::near(round_ceiling(x, digits)),
    x |> reround(digits, "floor")      |> dplyr::near(round_floor(x, digits)),
    x |> reround(digits, "trunc")      |> dplyr::near(round_trunc(x, digits)),
    x |> reround(digits, "anti_trunc") |> dplyr::near(round_anti_trunc(x, digits))
  )
}


test_that("`reround()` works like each of the specific rounding functions", {
  # `digits` is recycled to the length of `x` explicitly. `reround()` now holds
  # it to the tidyverse rule -- equal length, or length 1 -- so the `1:250` that
  # used to be passed here, and that R recycled a hundred times over because
  # 25000 happens to be a multiple of 250, is an error. The test means one
  # decimal level per value either way.
  x |> test_reround(rep_len(1:250, length(x))) |> expect_true()
})


# Checks on `digits` --------------------------------------------------------

# `digits` is the one argument that really is vectorized along with `x`: one
# number of decimal places per value. `rounding`, `threshold`, and `symmetric`
# describe a single procedure and are checked separately.

test_that("`digits` is checked against the length of `x`", {
  # R's own recycling rules passed a shorter `digits` without a word and
  # rounded the extra values at the wrong decimal level. `unround()` was fixed
  # for this in scrutiny 1.0.0; `reround()` never got the matching check, so it
  # returned `c(1.3, 2, 3.5, 5)` here rather than saying anything.
  c(1.25, 2.35, 3.45, 4.55) |> reround(digits = c(1, 0)) |> expect_error()
  # A non-multiple used to surface R's bare "longer object length is not a
  # multiple of shorter object length" warning:
  c(1.25, 2.35, 3.45) |> reround(digits = c(1, 0)) |> expect_error()
  # A length-1 `digits` is still recycled, as is one of matching length:
  c(1.25, 2.35) |> reround(digits = 1) |> expect_no_error()
  c(1.25, 2.35) |> reround(digits = c(1, 2)) |> expect_no_error()
})


test_that("a fractional `digits` is rejected", {
  # It is not a decimal level: `10^1.5` is about 31.6, so the value came back
  # sitting on no decimal grid at all. `reround(1.25, digits = 1.5, rounding =
  # "up")` was 1.264911, while the same call with `rounding = "even"` returned
  # 1.25, because `base::round()` rounds `digits` to a whole number first --
  # scrutiny's own methods disagreeing with each other on one input.
  1.25 |> reround(digits = 1.5, rounding = "up") |> expect_error()
  1.25 |> reround(digits = 1.5, rounding = "even") |> expect_error()
  1.25 |> reround(digits = c(1, 2.5), rounding = "up") |> expect_error()
  1.25 |> reround(digits = Inf) |> expect_error()
  1.25 |> reround(digits = "2") |> expect_error()
  # Whole numbers pass whatever their storage mode, and negative ones round to
  # powers of ten:
  1.25 |> reround(digits = 2) |> expect_no_error()
  1.25 |> reround(digits = 2L) |> expect_no_error()
  1250 |> reround(digits = -2, rounding = "up") |> expect_equal(1300)
  # A missing `digits` propagates to a missing result, as a missing `x` does:
  1.25 |> reround(digits = NA_real_, rounding = "up") |> expect_na()
})


# Checks on the rounding procedure ------------------------------------------

test_that("an invalid `rounding` string names `reround()`", {
  # Not the internal helper that throws the error:
  cnd <- 2.5 |> reround(0, rounding = "bogus") |> expect_error("designated string values")
  cnd$call |> rlang::format_error_call() |> expect_equal("`reround()`")
})


test_that("`rounding` must be a string, and `symmetric` `TRUE` or `FALSE`", {
  # `resolve_ties_rounding()` indexes a list by `rounding`, and a list indexed
  # by a number or a `TRUE` returns an element by position: `rounding = 2` was
  # silently `"ties_down"`, and `rounding = TRUE` was `"ties_up"`, in every
  # function that takes the argument.
  2.5   |> reround(0, rounding = 2)             |> expect_error("must be a string")
  2.5   |> reround(0, rounding = TRUE)          |> expect_error("must be a string")
  2.5   |> reround(0, rounding = NA_character_) |> expect_error("must be a string")
  "2.5" |> unround(rounding = 2)                |> expect_error("must be a string")
  5.19  |> grim(28, digits_x = 2, rounding = 2) |> expect_error("must be a string")

  # A `symmetric` of `NA` failed in an `if ()` with base R's message:
  -2.5 |> reround(0, "up", symmetric = NA)    |> expect_error("`TRUE` or `FALSE`")
  -2.5 |> reround(0, "up", symmetric = "yes") |> expect_error("`TRUE` or `FALSE`")
  -5.19 |>
    grim(28, digits_x = 2, rounding = "up", symmetric = NA) |>
    expect_error("`TRUE` or `FALSE`")
})


test_that("a missing `digits` propagates, and an overflowing one is rejected", {
  1.25 |> reround(NA, "up")       |> expect_equal(NA_real_)
  1.25 |> reround(NA_real_, "up") |> expect_equal(NA_real_)
  # `10^400` is `Inf`, and `10^-400` is `0`, so both used to give `NaN`:
  1.25 |> reround(400, "up")  |> expect_error("between -308 and 308")
  1.25 |> reround(-400, "up") |> expect_error("between -308 and 308")
  # A fractional value is reported as such, and a whole one is not among them:
  1.25 |> reround(c(1.5, 400), "up") |> expect_error("not: 1.5\\.")
})


test_that("`reround()` never returns a negative zero", {
  0 |> reround(2, "down") |> sprintf(fmt = "%.2f") |> expect_equal("0.00")
})
