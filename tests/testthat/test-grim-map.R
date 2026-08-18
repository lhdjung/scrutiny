df1 <- pigs1

df1_grim_up_or_down <- grim_map(df1, digits_x = 2, rounding = "up_or_down")
df1_grim_up <- grim_map(df1, digits_x = 2, rounding = "up")
df1_grim_down <- grim_map(df1, digits_x = 2, rounding = "down")
df1_grim_even <- grim_map(df1, digits_x = 2, rounding = "even")
df1_grim_ceiling_or_floor <- grim_map(
  df1,
  digits_x = 2,
  rounding = "ceiling_or_floor"
)
df1_grim_ceiling <- grim_map(df1, digits_x = 2, rounding = "ceiling")
df1_grim_floor <- grim_map(df1, digits_x = 2, rounding = "floor")
df1_grim_trunc <- grim_map(df1, digits_x = 2, rounding = "trunc")
df1_grim_anti_trunc <- grim_map(df1, digits_x = 2, rounding = "anti_trunc")


df1_grim <- grim_map(df1, digits_x = 2)


test_that("A tibble is returned", {
  expect_s3_class(df1_grim, c("tbl_df", "tbl", "data.frame"))
})


test_that("It has the correct function-general class", {
  expect_s3_class(df1_grim, "scrutiny_grim_map")
})

test_that("It has the correct rounding-specific class", {
  df1_grim_up_or_down       |> expect_s3_class("scrutiny_rounding_up_or_down")
  df1_grim_up               |> expect_s3_class("scrutiny_rounding_up")
  df1_grim_down             |> expect_s3_class("scrutiny_rounding_down")
  df1_grim_ceiling_or_floor |> expect_s3_class("scrutiny_rounding_ceiling_or_floor")
  df1_grim_ceiling          |> expect_s3_class("scrutiny_rounding_ceiling")
  df1_grim_floor            |> expect_s3_class("scrutiny_rounding_floor")
  df1_grim_trunc            |> expect_s3_class("scrutiny_rounding_trunc")
  df1_grim_anti_trunc       |> expect_s3_class("scrutiny_rounding_anti_trunc")
})


t <- TRUE
f <- FALSE

consistency_exp <- c(t, f, f, f, f, t, f, t, f, f, t, f)

test_that("`consistency` has the correct values", {
  df1_grim$consistency |> expect_equal(consistency_exp)
})


df2 <- df1 |>
  dplyr::mutate(n = n * 100)

df2_grim <- grim_map(df2, digits_x = 2)

# Comparison with what `grim_ratio()` would return -- it can be negative:
test_that("`probability` is zero if `ratio` would be negative", {
  (df2_grim$probability == 0) |> all() |> expect_true()
})


x <- rnorm(500, 65, 15) |>
  censor(30, 90) |>
  round()

n <- rnorm(500, 50, 20) |>
  censor(20, 90) |>
  round()

df3 <- tibble::tibble(x, n)

df3_percent_true <- grim_map(
  df3,
  digits_x = 0,
  percent = TRUE,
  show_rec = TRUE
) |>
  suppressMessages()

df3_percent_false <- grim_map(df3, digits_x = 0, show_rec = TRUE)

percent_probabilities_greater <-
  df3_percent_true$probability > df3_percent_false$probability

test_that(
  "The probability of GRIM inconsistency is always greater
  with `percent = TRUE` than without it", {
    percent_probabilities_greater |> all() |> expect_true()
})


test_that("`percent = TRUE` leaves the granules on the scale of `x`", {
  # Up to scrutiny 1.0.0, these were the decimal numbers that GRIM converts `x`
  # to internally, so they could not be read against the `x` column they are
  # documented as bracketing:
  (df3_percent_true$rec_x_lower <= df3_percent_true$x) |> all() |> expect_true()
  (df3_percent_true$rec_x_upper >= df3_percent_true$x) |> all() |> expect_true()

  # `grim_values()` returns its values on that same scale, so a consistent case
  # must be bracketed by the two granules there as well:
  out <- grim_map(
    tibble::tibble(x = c(71, 84), n = c(25L, 25L)),
    digits_x = 0,
    percent = TRUE,
    show_rec = TRUE
  )
  out$rec_x_lower |> expect_equal(c(68, 84))
  out$rec_x_upper |> expect_equal(c(72, 84))
  grim_values(84, 25, digits_x = 0, percent = TRUE)[[1L]] |> expect_equal(84)

  # The sums are not converted: they count the underlying data either way.
  out$sum_lower |> expect_equal(c(18, 21))
  out$sum_upper |> expect_equal(c(17, 21))
})


# The stated consistency must accord with what can be reconstructed from the
# numbers presented -- for every row and every rounding method.
#
# The version of this test up to scrutiny 1.0.0 was weaker than it looked. It
# only exercised the default rounding, and its `any()` collapsed the whole
# column to one scalar instead of testing row by row. It could not have caught
# what it was written to catch: with `rounding = "anti_trunc"`, the `rec_`
# columns of the day displayed a reconstruction implying inconsistency while
# `consistency` was `TRUE`. Those columns were re-rounded granules, a second
# derivation of the verdict that had drifted away from the verdict itself. They
# are now the sum range that decides it, so the two cannot come apart.

rounding_methods_predictable <- c(
  "up_or_down",
  "up",
  "down",
  "ceiling_or_floor",
  "ceiling",
  "floor",
  "trunc",
  "anti_trunc"
)

# `"even"` is not predictable in this sense; see the comment further below.
rounding_methods <- c(rounding_methods_predictable, "even")


test_that("`consistency` accords with the displayed sum range, row by row", {
  for (rounding in rounding_methods) {
    out <- df3 |>
      grim_map(digits_x = 0, show_rec = TRUE, rounding = rounding) |>
      suppressMessages()
    expect_equal(
      out$consistency,
      out$sum_lower <= out$sum_upper,
      info = paste0("rounding = ", rounding)
    )
  }
})


# Does a given sum total reconstruct the reported `x`? For the "_or_" methods,
# `reround()` returns one value per rounding variant, and either of them may hit
# `x`, hence `any()`:
sum_rounds_back <- function(sum_total, x, n, digits, rounding) {
  granules <- suppressWarnings(reround(
    sum_total / n,
    digits = digits,
    rounding = rounding
  ))
  any(abs(granules - x) < 1e-11, na.rm = TRUE)
}


test_that("the displayed sum range is exactly the range of admissible sums", {
  for (rounding in rounding_methods_predictable) {
    out <- grim_map(df1, digits_x = 2, show_rec = TRUE, rounding = rounding)
    for (i in seq_len(nrow(out))) {
      info <- paste0("rounding = ", rounding, ", row = ", i)
      # Every sum inside the range reconstructs `x`, so the range is not too
      # wide. The range is empty for an inconsistent row, and `seq()` would
      # then count downwards, so it is only walked if it has any members:
      if (out$sum_lower[i] <= out$sum_upper[i]) {
        for (sum_total in seq(out$sum_lower[i], out$sum_upper[i])) {
          sum_total |>
            sum_rounds_back(out$x[i], out$n[i], 2, rounding) |>
            expect_true(info = info)
        }
      }
      # Neither sum just outside the range does, so it is not too narrow. For an
      # empty range these are the two integers that straddle it:
      for (sum_total in c(out$sum_lower[i] - 1L, out$sum_upper[i] + 1L)) {
        sum_total |>
          sum_rounds_back(out$x[i], out$n[i], 2, rounding) |>
          expect_false(info = info)
      }
    }
  }
})


df4 <- df1 |>
  grim_map(digits_x = 2, items = 2)

df4_cons_true <- df4$consistency[df4$consistency]

test_that("", {
  df4_cons_true |> expect_length(6)
})


df5 <- df1 |>
  grim_map(digits_x = 2, show_rec = TRUE)


test_that("`show_rec` increases the number of columns correctly", {
  # The same five columns for every rounding method, unlike up to scrutiny
  # 1.0.0, where `"up_or_down"` and the other "_or_" methods added seven:
  df5 |> ncol() |> expect_equal(10)
  df1 |>
    grim_map(digits_x = 2, show_rec = TRUE, rounding = "ceiling") |>
    ncol() |>
    expect_equal(10)
})


df6 <- df1 |>
  dplyr::rename(Mean = x, Sample_Size = n)

df6_grim <- df6 |>
  grim_map(digits_x = 2, x = Mean, n = Sample_Size) |>
  dplyr::mutate(Mean = NULL, Sample_Size = NULL)


test_that("`x` and `n` make the specified columns take on these roles", {
  df6_grim |> expect_equal(df1_grim)
})


# `df7` was omitted

test_that("a `probability` column is naturally present", {
  df1_grim |> colnames() |> expect_contains("probability")
})


df8 <- tibble::tibble(
  x = df1$x,
  n40 = 40,
  n80 = 80
)

df8_n40_grim_up_or_down <- grim_map(
  df8,
  digits_x = 2,
  n = n40,
  rounding = "up_or_down"
)
df8_n40_grim_up <- grim_map(df8, digits_x = 2, n = n40, rounding = "up")
df8_n40_grim_down <- grim_map(df8, digits_x = 2, n = n40, rounding = "down")
df8_n40_grim_even <- grim_map(df8, digits_x = 2, n = n40, rounding = "even")
df8_n40_grim_ceiling_or_floor <- grim_map(
  df8,
  digits_x = 2,
  n = n40,
  rounding = "ceiling_or_floor"
)
df8_n40_grim_ceiling <- grim_map(
  df8,
  digits_x = 2,
  n = n40,
  rounding = "ceiling"
)
df8_n40_grim_floor <- grim_map(df8, digits_x = 2, n = n40, rounding = "floor")
df8_n40_grim_trunc <- grim_map(df8, digits_x = 2, n = n40, rounding = "trunc")
df8_n40_grim_anti_trunc <- grim_map(
  df8,
  digits_x = 2,
  n = n40,
  rounding = "anti_trunc"
)

df8_n80_grim_up_or_down <- grim_map(
  df8,
  digits_x = 2,
  n = n80,
  rounding = "up_or_down"
)
df8_n80_grim_up <- grim_map(df8, digits_x = 2, n = n80, rounding = "up")
df8_n80_grim_down <- grim_map(df8, digits_x = 2, n = n80, rounding = "down")
df8_n80_grim_even <- grim_map(df8, digits_x = 2, n = n80, rounding = "even")
df8_n80_grim_ceiling_or_floor <- grim_map(
  df8,
  digits_x = 2,
  n = n80,
  rounding = "ceiling_or_floor"
)
df8_n80_grim_ceiling <- grim_map(
  df8,
  digits_x = 2,
  n = n80,
  rounding = "ceiling"
)
df8_n80_grim_floor <- grim_map(df8, digits_x = 2, n = n80, rounding = "floor")
df8_n80_grim_trunc <- grim_map(df8, digits_x = 2, n = n80, rounding = "trunc")
df8_n80_grim_anti_trunc <- grim_map(
  df8,
  digits_x = 2,
  n = n80,
  rounding = "anti_trunc"
)


t <- TRUE
f <- FALSE


df8_n40_grim_up_or_down_exp <- c(t, f, t, t, t, t, f, t, f, f, t, f)
df8_n40_grim_up_exp <- c(f, f, t, f, f, t, f, t, f, f, t, f)
df8_n40_grim_down_exp <- c(t, f, f, t, t, f, f, f, f, f, t, f)
df8_n40_grim_even_exp <- c(t, f, t, t, t, t, f, t, f, f, t, f)
df8_n40_grim_ceiling_or_floor_exp <- c(t, f, t, t, t, t, f, t, f, f, t, f)
df8_n40_grim_ceiling_exp <- c(f, f, t, f, f, t, f, t, f, f, t, f)
df8_n40_grim_floor_exp <- c(t, f, f, t, t, f, f, f, f, f, t, f)
df8_n40_grim_trunc_exp <- c(t, f, f, t, t, f, f, f, f, f, t, f)
# Every `x` in `pigs1` is positive, and `round_anti_trunc()` is
# `round_ceiling()` above zero, so the two must agree here. (Until
# `anti_trunc()` stopped sending a value that is already on the rounding
# grid one step further out, they came apart.)
df8_n40_grim_anti_trunc_exp <- df8_n40_grim_ceiling_exp


test_that("rounding specifications lead to the expected consistency
          results in the corner case of n = 40", {
  df8_n40_grim_up_or_down       $consistency |> expect_equal(df8_n40_grim_up_or_down_exp       )
  df8_n40_grim_up               $consistency |> expect_equal(df8_n40_grim_up_exp               )
  df8_n40_grim_down             $consistency |> expect_equal(df8_n40_grim_down_exp             )
  df8_n40_grim_even             $consistency |> expect_equal(df8_n40_grim_even_exp             )
  df8_n40_grim_ceiling_or_floor $consistency |> expect_equal(df8_n40_grim_ceiling_or_floor_exp )
  df8_n40_grim_ceiling          $consistency |> expect_equal(df8_n40_grim_ceiling_exp          )
  df8_n40_grim_floor            $consistency |> expect_equal(df8_n40_grim_floor_exp            )
  df8_n40_grim_trunc            $consistency |> expect_equal(df8_n40_grim_trunc_exp            )
  df8_n40_grim_anti_trunc       $consistency |> expect_equal(df8_n40_grim_anti_trunc_exp       )
})


df8_n80_grim_up_or_down_exp <- c(t, t, t, t, t, t, t, t, t, t, t, t)
df8_n80_grim_up_exp <- c(f, t, t, f, f, t, t, t, t, t, t, t)
df8_n80_grim_down_exp <- c(t, t, f, t, t, f, t, f, t, t, t, t)
df8_n80_grim_even_exp <- c(t, t, t, t, t, t, t, t, t, t, t, t)
df8_n80_grim_ceiling_or_floor_exp <- c(t, t, t, t, t, t, t, t, t, t, t, t)
df8_n80_grim_ceiling_exp <- c(t, t, t, t, t, t, f, t, t, t, t, t)
df8_n80_grim_floor_exp <- c(t, f, t, t, t, t, t, t, f, f, t, f)
df8_n80_grim_trunc_exp <- c(t, f, t, t, t, t, t, t, f, f, t, f)
# Every `x` in `pigs1` is positive, and `round_anti_trunc()` is
# `round_ceiling()` above zero, so the two must agree here. (Until
# `anti_trunc()` stopped sending a value that is already on the rounding
# grid one step further out, they came apart.)
df8_n80_grim_anti_trunc_exp <- df8_n80_grim_ceiling_exp


test_that("rounding specifications lead to the expected consistency
          results in the corner case of n = 80", {
  df8_n80_grim_up_or_down       $consistency |> expect_equal(df8_n80_grim_up_or_down_exp       )
  df8_n80_grim_up               $consistency |> expect_equal(df8_n80_grim_up_exp               )
  df8_n80_grim_down             $consistency |> expect_equal(df8_n80_grim_down_exp             )
  df8_n80_grim_even             $consistency |> expect_equal(df8_n80_grim_even_exp             )
  df8_n80_grim_ceiling_or_floor $consistency |> expect_equal(df8_n80_grim_ceiling_or_floor_exp )
  df8_n80_grim_ceiling          $consistency |> expect_equal(df8_n80_grim_ceiling_exp          )
  df8_n80_grim_floor            $consistency |> expect_equal(df8_n80_grim_floor_exp            )
  df8_n80_grim_trunc            $consistency |> expect_equal(df8_n80_grim_trunc_exp            )
  df8_n80_grim_anti_trunc       $consistency |> expect_equal(df8_n80_grim_anti_trunc_exp       )
})


# The expectations above only record what GRIM returns. This one derives the
# truth independently: it enumerates the integer sums around `x * n` and asks
# the rounding functions themselves whether the resulting granule rounds back to
# `x`. GRIM must agree for every rounding method whose bounds are predictable.
#
# `"even"` is not among them. `base::round()` breaks midpoint ties by the parity
# of the preceding digit, and whether a tie occurs at all depends on the binary
# representation of the value, so GRIM treats both of its bounds as inclusive.
# That can only make GRIM too permissive, never too strict, which is the safe
# direction for an error-detection tool -- hence the one-sided expectation.

grim_rounds_back <- function(x, n, digits, rounding, threshold = 5) {
  # Wide enough to cover the whole rounding interval in sum space, plus slack:
  width <- ceiling(n * 10^-digits) + 3
  sums <- seq(floor(x * n) - width, ceiling(x * n) + width)
  granules_rounded <- suppressWarnings(reround(
    sums / n,
    digits = digits,
    rounding = rounding,
    threshold = threshold
  ))
  any(abs(granules_rounded - x) < 1e-11, na.rm = TRUE)
}

test_that("GRIM agrees with the rounding functions themselves", {
  predictable <- c(
    "up_or_down", "up", "down", "ceiling_or_floor",
    "ceiling", "floor", "trunc", "anti_trunc"
  )

  for (n in c(40, 80)) {
    for (rounding in predictable) {
      consistency <- grim_map(
        tibble::tibble(x = df1$x, n = n),
        digits_x = 2,
        rounding = rounding
      )$consistency
      expect_equal(
        consistency,
        vapply(df1$x, grim_rounds_back, logical(1), n, 2, rounding),
        info = paste0("n = ", n, ", rounding = ", rounding)
      )
    }

    # `"even"` may only err on the permissive side:
    consistency_even <- grim_map(
      tibble::tibble(x = df1$x, n = n),
      digits_x = 2,
      rounding = "even"
    )$consistency
    rounds_back_even <- df1$x |>
      vapply(grim_rounds_back, logical(1), n, 2, "even")
    consistency_even[rounds_back_even] |> all() |> expect_true()
  }
})


test_that("GRIM agrees with the rounding functions for `\"*_from\"` methods", {
  for (n in c(40, 80)) {
    for (rounding in c("up_from", "down_from", "up_from_or_down_from")) {
      for (threshold in c(1, 3, 7, 9)) {
        consistency <- grim_map(
          tibble::tibble(x = df1$x, n = n),
          digits_x = 2,
          rounding = rounding,
          threshold = threshold
        )$consistency
        expect_equal(
          consistency,
          vapply(
            df1$x, grim_rounds_back, logical(1), n, 2, rounding, threshold
          ),
          info = paste0(
            "n = ", n, ", rounding = ", rounding, ", threshold = ", threshold
          )
        )
      }
    }
  }
})


df9_up_1 <- grim_map(df1, digits_x = 2, rounding = "up_from", threshold = 1)
df9_up_9 <- grim_map(df1, digits_x = 2, rounding = "up_from", threshold = 9)

df9_up_1_exp <- c(t, f, f, f, f, t, f, f, f, f, t, f)
df9_up_9_exp <- c(f, f, f, f, t, f, f, t, t, f, t, f)

test_that("the minimum of `threshold` yields expected results", {
  df9_up_1$consistency |> expect_equal(df9_up_1_exp)
  df9_up_9$consistency |> expect_equal(df9_up_9_exp)
})


df9_down_1 <- grim_map(df1, digits_x = 2, rounding = "down_from", threshold = 1)
df9_down_9 <- grim_map(df1, digits_x = 2, rounding = "down_from", threshold = 9)

df9_down_1_exp <- c(f, f, f, f, t, f, f, t, t, f, t, f)
df9_down_9_exp <- c(t, f, f, f, f, t, f, f, f, f, t, f)

test_that("the maximum of `threshold` yields expected results", {
  df9_down_1$consistency |> expect_equal(df9_down_1_exp)
  df9_down_9$consistency |> expect_equal(df9_down_9_exp)
})


# Errors ------------------------------------------------------------------

df10 <- df1 |>
  dplyr::mutate(items = 2)

df11 <- df1 |>
  dplyr::rename(Snout = x)

df11_exp <- grim_map(df1, digits_x = 2)

df12 <- df1 |>
  dplyr::rename(Sample_Size = n)

df12_exp <- grim_map(df1, digits_x = 2)


test_that("expectations related to various individual
          error messages hold", {
  df1  |> grim_map(digits_x = 2, items = 1:3) |> expect_error()
  df10 |> grim_map(digits_x = 2, items = 3) |> expect_error()
  df11 |> grim_map(digits_x = 2, x = Snout) |> expect_equal(df11_exp)
  df11 |> grim_map(digits_x = 2, x = Mouth) |> expect_error()
  df12 |> grim_map(digits_x = 2, n = Sample_Size) |> expect_equal(df12_exp)
  df12 |> grim_map(digits_x = 2, n = Count_Pigs) |> expect_error()
})


df13 <- df1 |>
  dplyr::mutate(girth = 30, mirth = 50, birth = 70)

test_that("other columns of `data` come along, to the right of the results", {
  out <- df13 |> grim_map(digits_x = 2)
  out |> colnames() |> expect_equal(c(
    "x", "n", "digits_x", "consistency", "probability",
    "girth", "mirth", "birth"
  ))
  out$girth |> expect_equal(df13$girth)
  out$mirth |> expect_equal(df13$mirth)
  out$birth |> expect_equal(df13$birth)
})


test_that("`extra` is gone; use `dplyr::select()` on the output instead", {
  df13 |> grim_map(digits_x = 2, extra = 0) |> expect_error()
})
