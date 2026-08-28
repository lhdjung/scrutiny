x <- rnorm(25000, 500, 30)

test_reround <- function(x, digits) {
  all(
    all(dplyr::near(reround(x, digits, "up"), round_up(x, digits))),
    all(dplyr::near(reround(x, digits, "down"), round_down(x, digits))),
    all(dplyr::near(reround(x, digits, "even"), round(x, digits))),
    all(dplyr::near(reround(x, digits, "ceiling"), round_ceiling(x, digits))),
    all(dplyr::near(reround(x, digits, "floor"), round_floor(x, digits))),
    all(dplyr::near(reround(x, digits, "trunc"), round_trunc(x, digits))),
    all(dplyr::near(
      reround(x, digits, "anti_trunc"),
      round_anti_trunc(x, digits)
    ))
  )
}


test_that("`reround()` works like each of the specific rounding functions", {
  # `digits` is recycled to the length of `x` explicitly. `reround()` now holds
  # it to the tidyverse rule -- equal length, or length 1 -- so the `1:250` that
  # used to be passed here, and that R recycled a hundred times over because
  # 25000 happens to be a multiple of 250, is an error. The test means one
  # decimal level per value either way.
  test_reround(x, rep_len(1:250, length(x))) |> expect_true()
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
