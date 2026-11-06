# A missing value in a key column makes a case undecidable, not erroneous. Every
# test returns `NA` for it, and everything downstream treats `NA` as neither
# consistent nor inconsistent. Up to scrutiny 1.0.0, the mappers aborted here
# from inside `check_newly_numeric()` with "missing value where TRUE/FALSE
# needed".

df_grim <- tibble::tibble(
  x = c(5.19, NA, 4.20),
  n = c(28L, 28L, NA)
)

df_grimmer <- tibble::tibble(
  x = c(1.03, NA, 1.03),
  sd = c(0.41, 0.41, NA),
  n = c(40L, 40L, 40L)
)

df_debit <- tibble::tibble(
  x = c(0.53, NA, 0.53),
  sd = c(0.50, 0.50, NA),
  n = c(1683L, 1683L, 1683L)
)


test_that("the single-case functions return `NA` for a missing value", {
  # Not aligning pipes here because the lengths are too different
  NA |> grim(28, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_na()
  5.19 |> grim(NA, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_na()
  NA |> grimmer(0.41, 40, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_na()
  1.03 |> grimmer(NA, 40, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_na()
  1.03 |> grimmer(0.41, NA, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_na()
  NA |> debit(0.50, 1683, digits_x = 2, digits_sd = 2) |> expect_na()
  0.53 |> debit(NA, 1683, digits_x = 2, digits_sd = 2) |> expect_na()
  0.53 |> debit(0.50, NA, digits_x = 2, digits_sd = 2) |> expect_na()
})


test_that("a missing value is `NA` under every rounding method", {
  # The bounds of `"trunc"` and `"anti_trunc"` depend on the sign of `x`, and
  # `symmetric` mirrors the methods it applies to -- and a missing value has no
  # sign. Every one of these used to abort with "missing value where TRUE/FALSE
  # needed", the very error this behavior was meant to replace, and a mapper
  # then failed for the whole data frame over a single missing value.

  # fmt: skip
  roundings <- c(
    "up_or_down", "up", "down", "even", "ceiling", "floor",
    "ceiling_or_floor", "trunc", "anti_trunc"
  )
  for (rounding in roundings) {
    for (symmetric in c(FALSE, TRUE)) {
      info <- paste0("rounding = ", rounding, ", symmetric = ", symmetric)
      NA   |> grim(28, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = rounding, symmetric = symmetric) |> expect_na()
      5.19 |> grim(NA, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = rounding, symmetric = symmetric) |> expect_na()
      verdict_first <- 5.19 |> grim(28, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = rounding, symmetric = symmetric)
      df_grim |>
        grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", rounding = rounding, symmetric = symmetric) |>
        purrr::pluck("consistency") |>
        expect_equal(c(verdict_first, NA, NA), info = info)
      NA |>
        grim_values(28, digits_x = 2, items_averaged = 1, rounding = rounding, symmetric = symmetric) |>
        expect_undecidable()
      NA |>
        grim_closest(28, digits_x = 2, items_averaged = 1, rounding = rounding, symmetric = symmetric) |>
        expect_na()
      NA_real_ |>
        roundwork::unround(digits = 2, rounding = rounding, symmetric = symmetric) |>
        purrr::pluck("lower") |>
        expect_na()
    }
  }
})


test_that("a missing value does not excuse an unknown `rounding`", {
  # An undecidable case is still no reason to accept a rounding method that does
  # not exist -- that is an input error whatever `x` is:
  NA |> grim(28, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "nonsense") |> expect_error()
  NA_real_ |> roundwork::unround(digits = 2, rounding = "nonsense") |> expect_error()
})


test_that("they are still vectorized over the other values", {
  c(5.19, NA) |>
    grim(c(28, 28), digits_x = 2, items_averaged = 1, data_type = "integer") |>
    expect_equal(c(FALSE, NA))
  c(1.03, NA) |>
    grimmer(c(0.41, 0.41), c(40, 40), digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    expect_equal(c(FALSE, NA))
  c(0.53, NA) |>
    debit(c(0.50, 0.50), c(1683, 1683), digits_x = 2, digits_sd = 2) |>
    expect_equal(c(TRUE, NA))
})


test_that("the mappers return `NA` instead of aborting", {
  df_grim |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("consistency") |>
    expect_equal(c(FALSE, NA, NA))
  df_grimmer |>
    grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("consistency") |>
    expect_equal(c(FALSE, NA, NA))
  df_debit |>
    debit_map(digits_x = 2, digits_sd = 2) |>
    purrr::pluck("consistency") |>
    expect_equal(c(TRUE, NA, NA))
})


test_that("the `show_*` columns keep their shape and their types", {
  out <- df_grim |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", show_rec = TRUE)
  out |> expect_named(c(
    "x", "n", "items_averaged", "data_type", "digits_x", "consistency", "exp_incon",
    "rec_sum",
    "sum_lower", "sum_upper", "rec_x_upper", "rec_x_lower"
  ))
  for (col in c(
    "rec_sum",
    "sum_lower",
    "sum_upper",
    "rec_x_upper",
    "rec_x_lower"
  )) {
    out[[col]] |> expect_type("double")
    out[[col]][2:3] |> expect_equal(c(NA_real_, NA_real_))
  }

  # GRIMMER states the reason for a missing verdict the way it states the
  # reason for an inconsistent one:
  df_grimmer |>
    grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("reason") |>
    expect_equal(c(
      "GRIMMER inconsistent (test 1)",
      "Missing value",
      "Missing value"
    ))

  # The bounds of a known value are returned next to a missing one:
  out_debit <- df_debit |> debit_map(digits_x = 2, digits_sd = 2, show_rec = TRUE)
  out_debit$sd_lower[2:3] |> expect_equal(c(0.495, NA_real_))
  out_debit$x_lower[2:3]  |> expect_equal(c(NA_real_, 0.525))
  out_debit$sd_lower |> expect_type("double")
})


test_that("`roundwork::unround()`, `grim_values()`, and `grim_closest()` propagate `NA`", {
  bounds <- c(0.53, NA) |> roundwork::unround(digits = 2)
  bounds$lower |> expect_equal(c(0.525, NA))
  bounds$upper |> expect_equal(c(0.535, NA))

  values <- c(5.19, NA) |> grim_values(c(32, 32), digits_x = 2, items_averaged = 1)
  values$case |> expect_equal(1:2)
  values$x    |> expect_equal(c(5.1875, NA))
  values$sum  |> expect_equal(c(166, NA))

  c(5.19, NA) |>
    grim_closest(c(28, 28), digits_x = 2, items_averaged = 1) |>
    expect_equal(c(5.178571, NA), tolerance = 1e-6)
})


test_that("`audit()` counts an undecidable case as neither", {
  # One `FALSE`, one `NA`, one `TRUE`. Indexing rows by `NA` used to add a
  # phantom row, so `incon_cases` was 2 out of 3 here:
  out <- tibble::tibble(x = c(5.19, NA, 4.20), n = c(28L, 28L, 30L)) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  out$consistency |> expect_equal(c(FALSE, NA, TRUE))

  out_audit <- out |> audit()
  out_audit$incon_cases |> expect_equal(1L)
  out_audit$all_cases |> expect_equal(3L)
  out_audit$emp_incon |> expect_equal(1 / 3)
})


test_that("the sequence mappers drop undecidable cases", {
  # An undecidable case has no values to disperse around, so it is dropped like
  # a consistent one. Only the genuinely inconsistent 5.19 / 28 is varied:
  out <- tibble::tibble(x = c(5.19, NA, 4.20), n = c(28L, 28L, 30L)) |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer", dispersion = 1:2)
  out$case |> unique() |> expect_equal(1L)
  out$x |> anyNA() |> expect_false()
  out$n |> anyNA() |> expect_false()

  out |> audit_seq() |> purrr::pluck("hits_total") |> expect_equal(4L)
})


test_that("a missing `digits_*` argument still errors, and so do bad values", {
  # `suppressMessages()` mutes the changelog hint that `error_digits_missing()`
  # prints via `on.exit()` as it unwinds:
  df_grim |> grim_map(data_type = "integer") |> suppressMessages() |> expect_error()
  NA |> grim(28, items_averaged = 1, data_type = "integer") |> suppressMessages() |> expect_error()
  # `NA` is undecidable, but a mismatched `digits_x` is still an input error:
  5.19 |> grim(28, digits_x = 1, items_averaged = 1, data_type = "integer") |> expect_error()
})


test_that("DEBIT tells a missing value from an out-of-range one", {
  # `dplyr::between()` returns `NA` for `NA`, which used to make the range check
  # report the missing value as not being between 0 and 1:
  NA   |> debit(0.50, 1683, digits_x = 2, digits_sd = 2) |> expect_na()
  1.53 |> debit(0.50, 1683, digits_x = 2, digits_sd = 2) |> expect_error()
})


# Undecidable input -------------------------------------------------------

test_that("the three tests agree on what input is undecidable", {
  # All three reason about integer data, so a fractional or non-positive `n` or
  # `items` describes no data set at all -- there is nothing for the test to be
  # consistent or inconsistent *with*. They used to return verdicts anyway.

  # GRIM: `n` and `items` must be positive whole numbers. `n = 1` is fine, the
  # mean of a single value being that value.
  5.19 |> grim(n = 20.5, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_na()
  5.19 |> grim(n = 20, digits_x = 2, items_averaged = 1.5, data_type = "integer") |> expect_na()
  5.19 |> grim(n = 0, digits_x = 2, items_averaged = 1, data_type = "integer")    |> expect_na()
  5.19 |> grim(n = -5, digits_x = 2, items_averaged = 1, data_type = "integer")   |> expect_na()
  5.19 |> grim(n = Inf, digits_x = 2, items_averaged = 1, data_type = "integer")  |> expect_na()
  5    |> grim(n = 1, digits_x = 0, items_averaged = 1, data_type = "integer")    |> expect_true()

  # GRIMMER and DEBIT reconstruct a *sample* SD, so they divide by `n - 1` and
  # need an `n` of at least 2. At `n = 1`, GRIMMER used to reach `FALSE`
  # through a `NaN` that `na.rm = TRUE` swallowed, and DEBIT through an `Inf`
  # that compared as "above the upper bound".
  5 |> grimmer(sd = 0, n = 1, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")    |> expect_na()
  3 |> grimmer(sd = 1, n = 20.5, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_na()
  3 |> grimmer(sd = 1, n = 20, digits_x = 2, digits_sd = 2, items_averaged = 1.5, data_type = "integer") |>
    expect_na()
  3 |> grimmer(sd = 1, n = 0, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_na()

  0.5 |> debit(sd = 0.5, n = 1, digits_x = 2, digits_sd = 2)    |> expect_na()
  0.5 |> debit(sd = 0.5, n = 0, digits_x = 2, digits_sd = 2)    |> expect_na()
  0.5 |> debit(sd = 0.5, n = -5, digits_x = 2, digits_sd = 2)   |> expect_na()
  0.5 |> debit(sd = 0.5, n = 20.5, digits_x = 2, digits_sd = 2) |> expect_na()
  # A real `n` still gets a real verdict:
  0.5 |> debit(sd = 0.5, n = 20, digits_x = 2, digits_sd = 2) |>
    expect_false()
  0.36 |> debit(sd = 0.11, n = 20, digits_x = 2, digits_sd = 2) |>
    expect_type("logical")
})


test_that("undecidable input gives `NA` in the mappers, too", {
  out <- tibble::tibble(x = 5.19, n = 20.5, items_averaged = 1.5) |> grim_map(digits_x = 2, data_type = "integer")
  out$consistency |> expect_na()
  # `exp_incon` used to report a number next to an `NA` verdict, which states
  # two incompatible things about one row:
  out$exp_incon |> expect_na()

  # The reconstructed values of `show_rec` are `NA` throughout, rather than the
  # `NaN`s that an `n` of zero produced:
  out_rec <- tibble::tibble(x = 5.19, n = 0L) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", show_rec = TRUE)
  out_rec$consistency |> expect_na()
  for (col in c(
    "rec_sum", "sum_lower", "sum_upper", "rec_x_upper", "rec_x_lower"
  )) {
    out_rec[[col]] |> expect_na()
  }

  out_debit <- tibble::tibble(x = 0.5, sd = 0.5, n = 1L) |> debit_map(digits_x = 2, digits_sd = 2)
  out_debit$consistency |> expect_na()

  out_grimmer <- tibble::tibble(x = 5, sd = 0, n = 1L) |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
  out_grimmer$consistency |> expect_na()
  out_grimmer$reason |> expect_equal("No testable value set")
})


# An infinity is undecidable for the same reasons a missing value is: no data
# set has an infinite mean or SD, and an infinity has no decimal places to be
# reported with, which is why `decimal_places()` returns `NA` for it. It used to
# abort from inside `check_newly_numeric()` with "missing value where TRUE/FALSE
# needed", exactly as a missing value did before 1.0.0.

test_that("an infinite value is undecidable, not consistent", {
  Inf  |> grim(28, digits_x = 2, items_averaged = 1, data_type = "integer")                         |> expect_na()
  -Inf |> grim(28, digits_x = 2, items_averaged = 1, data_type = "integer")                         |> expect_na()
  Inf  |> grimmer(0.41, 40, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_na()
  1.03 |> grimmer(Inf, 40, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")  |> expect_na()

  # DEBIT has its own, more specific answer: a binary mean or SD outside of the
  # unit interval is an input error, and it says so rather than returning `NA`.
  Inf  |> debit(0.5, 100, digits_x = 2, digits_sd = 2) |> expect_error()
  0.53 |> debit(Inf, 100, digits_x = 2, digits_sd = 2) |> expect_error()
})


test_that("`grimmer()` names the infinity as its reason", {
  tibble::tibble(x = Inf, sd = 0.41, n = 40L) |>
    grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("reason") |>
    expect_equal("Infinite value")
})


test_that("the achievable means behind an infinity are `NA`", {
  # `seq()` used to abort with "'from' must be a finite number":
  Inf |> grim_values(28, digits_x = 2, items_averaged = 1)  |> expect_undecidable()
  Inf |> grim_closest(28, digits_x = 2, items_averaged = 1) |> expect_na()
})


test_that("an infinite value gives `NA` in the mappers, too", {
  # A real value set next to the infinite one still gets a real verdict:
  out_grim <- tibble::tibble(x = c(5.25, Inf), n = 28L) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  out_grim$consistency |> expect_equal(c(TRUE, NA))

  out_grimmer <- tibble::tibble(x = c(1.03, Inf), sd = 0.41, n = 40L) |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
  out_grimmer$consistency[2L] |> expect_na()
})


test_that("a missing `digits_*` makes a case undecidable, like a missing value", {
  # It used to fail inside `check_newly_numeric()` with "missing value where
  # TRUE/FALSE needed" -- from within `purrr::pmap()` in a mapper, where one row
  # with an unknown decimal count took the whole call down.
  5.19 |> grim(28, digits_x = NA, items_averaged = 1, data_type = "integer")                            |> expect_na()
  1.03 |> grimmer(0.41, 40, digits_x = 2, digits_sd = NA, items_averaged = 1, data_type = "integer")    |> expect_na()
  0.53 |> debit(0.50, 1683, digits_x = NA, digits_sd = 2)                        |> expect_na()
  5.19 |> grim_values(28, digits_x = NA, items_averaged = 1) |> expect_undecidable()
  5.19 |> grim_closest(28, digits_x = NA, items_averaged = 1)                    |> expect_na()

  out <- tibble::tibble(x = c(5.19, 5.2), n = 28L) |> grim_map(digits_x = c(2, NA), items_averaged = 1, data_type = "integer")
  out$consistency |> expect_equal(c(FALSE, NA))
  out$exp_incon |> expect_equal(c(0.72, NA))

  # A sequence mapper disperses every value on one decimal level, so it needs a
  # number there:
  tibble::tibble(x = 5.19, n = 28L) |>
    grim_map_seq(digits_x = NA, items_averaged = 1, data_type = "integer") |>
    expect_error("single number")
})

test_that("a missing value that can't change the verdict doesn't make it `NA`", {
  # With `sd` missing, a mean that fails GRIM fails for any SD:
  5.19 |> grimmer(NA, 28, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_false()
  5.19 |> grimmer(0.5, 28, digits_x = 2, digits_sd = NA, items_averaged = 1, data_type = "integer") |> expect_false()
  # With `x` missing, a negative SD, or one that no sum of whole numbers
  # admits, fails for any mean; two whole numbers have a sample SD of 0, 0.71,
  # 1.41, ...:
  NA |> grimmer(-1, 28, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_false()
  NA |> grimmer(0.3, 2, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_false()
  NA |> grimmer(0.71, 2, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_na()
  NA |> grimmer(0.3, 2, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", sd_meaning = "sd_sample_or_pop") |> expect_false()
  # As a population SD, 0.5 is possible, so the combined verdict stays open:
  NA |> grimmer(0.5, 2, digits_x = 2, digits_sd = 1, items_averaged = 1, data_type = "integer", sd_meaning = "sd_sample_or_pop") |> expect_na()
  # The reasons name the test that decided:
  grimmer_scalar(5.19, NA, 28, 2, 2, 1, show_reason = TRUE, data_type = "integer")[[2L]] |> expect_equal("GRIM inconsistent")
  grimmer_scalar(NA, 0.3, 2, 2, 2, 1, show_reason = TRUE, data_type = "integer")[[2L]]   |> expect_equal("GRIMMER inconsistent (test 1)")
  grimmer_scalar(NA, 0.71, 2, 2, 2, 1, show_reason = TRUE, data_type = "integer")[[2L]]  |> expect_equal("Missing value")
  # DEBIT: no number of 20 ones has an SD of 0.9, and 0.33 * 20 is no whole
  # number:
  NA   |> debit(0.9, 20, digits_x = 2, digits_sd = 2) |> expect_false()
  0.33 |> debit(NA, 20, digits_x = 2, digits_sd = 2)  |> expect_false()
  0.33 |> debit(0.5, 20, digits_x = 2, digits_sd = NA) |> expect_false()
  NA   |> debit(0.9, 20, digits_x = 2, digits_sd = 2, formula = "mean_n") |> expect_false()
  # Under `"mean_n"`, the mean is continuous, so it never fails on its own:
  0.33 |> debit(NA, 20, digits_x = 2, digits_sd = 2, formula = "mean_n")  |> expect_na()
  NA   |> debit(0.9, 20, digits_x = 2, digits_sd = 2, sd_meaning = "sd_sample_or_pop") |> expect_false()
  NA   |> debit(0.5, 2, digits_x = 2, digits_sd = 1, sd_meaning = "sd_sample_or_pop")  |> expect_na()
  # With `n` missing, some `n` always works:
  5.19 |> grimmer(NA, NA, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_na()
  0.33 |> debit(NA, NA, digits_x = 2, digits_sd = 2) |> expect_na()
  # `show_rec` returns the bounds of the known value:
  out <- tibble::tibble(x = c(NA, 0.33), sd = c(0.9, NA), n = 20) |> debit_map(digits_x = 2, digits_sd = 2, show_rec = TRUE)
  out$consistency |> expect_equal(c(FALSE, FALSE))
  out$sd_lower |> expect_equal(c(0.895, NA))
  out$sum_lower |> expect_equal(c(NA, 7))
})

test_that("the explanations agree with these verdicts", {
  grimmer_explain(5.19, NA, 28, 2, 2, 1, data_type = "integer")$consistency |> expect_false()
  grimmer_explain(NA, 0.3, 2, 2, 2, 1, data_type = "integer")$consistency   |> expect_false()
  grimmer_explain(NA, 0.3, 2, 2, 2, 1, sd_meaning = "sd_sample_or_pop", data_type = "integer")$consistency |> expect_false()
  grimmer_explain(NA, 0.5, 2, 2, 1, 1, sd_meaning = "sd_sample_or_pop", data_type = "integer")$consistency |> expect_na()
  grimmer_explain(NA, 0.71, 2, 2, 2, 1, data_type = "integer")$consistency  |> expect_na()
  debit_explain(NA, 0.9, 20, 2, 2)$consistency  |> expect_false()
  debit_explain(0.33, NA, 20, 2, 2)$consistency |> expect_false()
  debit_explain(NA, 0.9, 20, 2, 2, sd_meaning = "sd_sample_or_pop")$consistency |> expect_false()
  debit_explain(NA, 0.5, 2, 2, 1, sd_meaning = "sd_sample_or_pop")$consistency  |> expect_na()
  debit_explain(0.53, NA, 1683, 2, 2)$consistency |> expect_na()
  format(debit_explain(NA, 0.9, 20, 2, 2)) |> expect_match("none of these gives an SD")
  format(grimmer_explain(NA, 0.3, 2, 2, 2, 1, data_type = "integer")) |> expect_match("none passes")
})

test_that("sequence mappers disperse the known values of such a case", {
  df <- tibble::tibble(x = c(5.19, NA), sd = c(NA, 0.3), n = c(28, 2))
  out <- df |> grimmer_map_seq(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
  out$var[out$case == 1] |> unique() |> expect_equal(c("x", "n"))
  out$var[out$case == 2] |> unique() |> expect_equal(c("sd", "n"))
  out |> audit_seq() |> nrow() |> expect_equal(2L)
  df[1, ] |> grimmer_map_seq(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", var = "sd") |> expect_warning("`sd` is missing in every case")
  tibble::tibble(x = 0.33, sd = NA, n = 10) |> debit_map_seq(digits_x = 2, digits_sd = 2) |> nrow() |> expect_gt(0)
})

test_that("GRIMMER with `x` missing is fast for a tiny SD and a large `n`", {
  elapsed <- system.time(verdict <- grimmer(NA, 0.0001, 1e6, digits_x = 2, digits_sd = 4, items_averaged = 1, data_type = "integer"))[["elapsed"]]
  verdict |> expect_false()
  elapsed |> expect_lt(2)
  # One value set off by 1 has a sum of squared deviations of about 1, which an
  # SD of 0.0003 admits at `n = 1e7`:
  NA |> grimmer(0.0003, 1e7, digits_x = 2, digits_sd = 4, items_averaged = 1, data_type = "integer") |> expect_na()
  NA |> grimmer(0.0001, 1e9, digits_x = 2, digits_sd = 4, items_averaged = 1, data_type = "integer") |> expect_na()
})
