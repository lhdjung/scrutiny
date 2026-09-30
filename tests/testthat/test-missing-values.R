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
  NA |> grim(28, digits_x = 2) |> expect_na()
  5.19 |> grim(NA, digits_x = 2) |> expect_na()
  NA |> grimmer(0.41, 40, digits_x = 2, digits_sd = 2) |> expect_na()
  1.03 |> grimmer(NA, 40, digits_x = 2, digits_sd = 2) |> expect_na()
  1.03 |> grimmer(0.41, NA, digits_x = 2, digits_sd = 2) |> expect_na()
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
      NA   |> grim(28, digits_x = 2, rounding = rounding, symmetric = symmetric) |> expect_na()
      5.19 |> grim(NA, digits_x = 2, rounding = rounding, symmetric = symmetric) |> expect_na()
      verdict_first <- 5.19 |> grim(28, digits_x = 2, rounding = rounding, symmetric = symmetric)
      df_grim |>
        grim_map(digits_x = 2, rounding = rounding, symmetric = symmetric) |>
        purrr::pluck("consistency") |>
        expect_equal(c(verdict_first, NA, NA), info = info)
      NA |>
        grim_values(28, digits_x = 2, rounding = rounding, symmetric = symmetric) |>
        purrr::pluck(1L) |>
        expect_na()
      NA |>
        grim_closest(28, digits_x = 2, rounding = rounding, symmetric = symmetric) |>
        expect_na()
      NA_real_ |>
        unround(digits = 2, rounding = rounding, symmetric = symmetric) |>
        purrr::pluck("lower") |>
        expect_na()
    }
  }
})


test_that("a missing value does not excuse an unknown `rounding`", {
  # An undecidable case is still no reason to accept a rounding method that does
  # not exist -- that is an input error whatever `x` is:
  NA |> grim(28, digits_x = 2, rounding = "nonsense") |> expect_error()
  NA_real_ |> unround(digits = 2, rounding = "nonsense") |> expect_error()
})


test_that("they are still vectorized over the other values", {
  c(5.19, NA) |>
    grim(c(28, 28), digits_x = 2) |>
    expect_equal(c(FALSE, NA))
  c(1.03, NA) |>
    grimmer(c(0.41, 0.41), c(40, 40), digits_x = 2, digits_sd = 2) |>
    expect_equal(c(FALSE, NA))
  c(0.53, NA) |>
    debit(c(0.50, 0.50), c(1683, 1683), digits_x = 2, digits_sd = 2) |>
    expect_equal(c(TRUE, NA))
})


test_that("the mappers return `NA` instead of aborting", {
  df_grim |>
    grim_map(digits_x = 2) |>
    purrr::pluck("consistency") |>
    expect_equal(c(FALSE, NA, NA))
  df_grimmer |>
    grimmer_map(digits_x = 2, digits_sd = 2) |>
    purrr::pluck("consistency") |>
    expect_equal(c(FALSE, NA, NA))
  df_debit |>
    debit_map(digits_x = 2, digits_sd = 2) |>
    purrr::pluck("consistency") |>
    expect_equal(c(TRUE, NA, NA))
})


test_that("the `show_*` columns keep their shape and their types", {
  out <- df_grim |> grim_map(digits_x = 2, show_rec = TRUE)
  out |> expect_named(c(
    "x", "n", "digits_x", "consistency", "probability", "rec_sum",
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
    grimmer_map(digits_x = 2, digits_sd = 2) |>
    purrr::pluck("reason") |>
    expect_equal(c(
      "GRIMMER inconsistent (test 1)",
      "Missing value",
      "Missing value"
    ))

  out_debit <- df_debit |> debit_map(digits_x = 2, digits_sd = 2)
  out_debit$sd_lower[2:3] |> expect_equal(c(NA_real_, NA_real_))
  out_debit$sd_lower |> expect_type("double")
  # `rounding` is not reconstructed from the data, so it survives:
  out_debit$rounding |> expect_equal(rep("up_or_down", 3L))
})


test_that("`unround()`, `grim_values()`, and `grim_closest()` propagate `NA`", {
  bounds <- c(0.53, NA) |> unround(digits = 2)
  bounds$lower |> expect_equal(c(0.525, NA))
  bounds$upper |> expect_equal(c(0.535, NA))

  values <- c(5.19, NA) |> grim_values(c(32, 32), digits_x = 2)
  values[[1L]] |> expect_equal(5.1875)
  values[[2L]] |> expect_na()

  c(5.19, NA) |>
    grim_closest(c(28, 28), digits_x = 2) |>
    expect_equal(c(5.178571, NA), tolerance = 1e-6)
})


test_that("`audit()` counts an undecidable case as neither", {
  # One `FALSE`, one `NA`, one `TRUE`. Indexing rows by `NA` used to add a
  # phantom row, so `incons_cases` was 2 out of 3 here:
  out <- tibble::tibble(x = c(5.19, NA, 4.20), n = c(28L, 28L, 30L)) |> grim_map(digits_x = 2)
  out$consistency |> expect_equal(c(FALSE, NA, TRUE))

  out_audit <- out |> audit()
  out_audit$incons_cases |> expect_equal(1L)
  out_audit$all_cases |> expect_equal(3L)
  out_audit$incons_rate |> expect_equal(1 / 3)
})


test_that("the sequence mappers drop undecidable cases", {
  # An undecidable case has no values to disperse around, so it is dropped like
  # a consistent one. Only the genuinely inconsistent 5.19 / 28 is varied:
  out <- tibble::tibble(x = c(5.19, NA, 4.20), n = c(28L, 28L, 30L)) |> grim_map_seq(digits_x = 2, dispersion = 1:2)
  out$case |> unique() |> expect_equal(1L)
  out$x |> anyNA() |> expect_false()
  out$n |> anyNA() |> expect_false()

  out |> audit_seq() |> purrr::pluck("hits_total") |> expect_equal(4L)
})


test_that("a missing `digits_*` argument still errors, and so do bad values", {
  # `suppressMessages()` mutes the changelog hint that `error_digits_missing()`
  # prints via `on.exit()` as it unwinds:
  df_grim |> grim_map() |> suppressMessages() |> expect_error()
  NA |> grim(28) |> suppressMessages() |> expect_error()
  # `NA` is undecidable, but a mismatched `digits_x` is still an input error:
  5.19 |> grim(28, digits_x = 1) |> expect_error()
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
  5.19 |> grim(n = 20.5, digits_x = 2)            |> expect_na()
  5.19 |> grim(n = 20, digits_x = 2, items = 1.5) |> expect_na()
  5.19 |> grim(n = 0, digits_x = 2)               |> expect_na()
  5.19 |> grim(n = -5, digits_x = 2)              |> expect_na()
  5.19 |> grim(n = Inf, digits_x = 2)             |> expect_na()
  5    |> grim(n = 1, digits_x = 0)               |> expect_true()

  # GRIMMER and DEBIT reconstruct a *sample* SD, so they divide by `n - 1` and
  # need an `n` of at least 2. At `n = 1`, GRIMMER used to reach `FALSE`
  # through a `NaN` that `na.rm = TRUE` swallowed, and DEBIT through an `Inf`
  # that compared as "above the upper bound".
  5 |> grimmer(sd = 0, n = 1, digits_x = 2, digits_sd = 2)    |> expect_na()
  3 |> grimmer(sd = 1, n = 20.5, digits_x = 2, digits_sd = 2) |> expect_na()
  3 |> grimmer(sd = 1, n = 20, digits_x = 2, digits_sd = 2, items = 1.5) |>
    expect_na()
  3 |> grimmer(sd = 1, n = 0, digits_x = 2, digits_sd = 2) |> expect_na()

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
  out <- tibble::tibble(x = 5.19, n = 20.5, items = 1.5) |> grim_map(digits_x = 2)
  out$consistency |> expect_na()
  # `probability` used to report a number next to an `NA` verdict, which states
  # two incompatible things about one row:
  out$probability |> expect_na()

  # The reconstructed values of `show_rec` are `NA` throughout, rather than the
  # `NaN`s that an `n` of zero produced:
  out_rec <- tibble::tibble(x = 5.19, n = 0L) |> grim_map(digits_x = 2, show_rec = TRUE)
  out_rec$consistency |> expect_na()
  for (col in c(
    "rec_sum", "sum_lower", "sum_upper", "rec_x_upper", "rec_x_lower"
  )) {
    out_rec[[col]] |> expect_na()
  }

  out_debit <- tibble::tibble(x = 0.5, sd = 0.5, n = 1L) |> debit_map(digits_x = 2, digits_sd = 2)
  out_debit$consistency |> expect_na()

  out_grimmer <- tibble::tibble(x = 5, sd = 0, n = 1L) |> grimmer_map(digits_x = 2, digits_sd = 2, show_reason = TRUE)
  out_grimmer$consistency |> expect_na()
  out_grimmer$reason |> expect_equal("No testable value set")
})


test_that("`grim_ratio()` stays unclamped and unguarded", {
  # It is documented as the raw formula, and `grim_probability()` is the one
  # that reports an undecidable case as `NA`:
  5.19 |> grim_ratio(n = 20.5, digits_x = 2)       |> expect_equal((100 - 20.5) / 100)
  5.19 |> grim_probability(n = 20.5, digits_x = 2) |> expect_na()
})


# An infinity is undecidable for the same reasons a missing value is: no data
# set has an infinite mean or SD, and an infinity has no decimal places to be
# reported with, which is why `decimal_places()` returns `NA` for it. It used to
# abort from inside `check_newly_numeric()` with "missing value where TRUE/FALSE
# needed", exactly as a missing value did before 1.0.0.

test_that("an infinite value is undecidable, not consistent", {
  Inf  |> grim(28, digits_x = 2)                         |> expect_na()
  -Inf |> grim(28, digits_x = 2)                         |> expect_na()
  Inf  |> grimmer(0.41, 40, digits_x = 2, digits_sd = 2) |> expect_na()
  1.03 |> grimmer(Inf, 40, digits_x = 2, digits_sd = 2)  |> expect_na()

  # DEBIT has its own, more specific answer: a binary mean or SD outside of the
  # unit interval is an input error, and it says so rather than returning `NA`.
  Inf  |> debit(0.5, 100, digits_x = 2, digits_sd = 2) |> expect_error()
  0.53 |> debit(Inf, 100, digits_x = 2, digits_sd = 2) |> expect_error()
})


test_that("`grimmer()` names the infinity as its reason", {
  tibble::tibble(x = Inf, sd = 0.41, n = 40L) |>
    grimmer_map(digits_x = 2, digits_sd = 2, show_reason = TRUE) |>
    purrr::pluck("reason") |>
    expect_equal("Infinite value")
})


test_that("the achievable means behind an infinity are `NA`", {
  # `seq()` used to abort with "'from' must be a finite number":
  Inf |> grim_values(28, digits_x = 2)  |> expect_na()
  Inf |> grim_closest(28, digits_x = 2) |> expect_na()
})


test_that("an infinite value gives `NA` in the mappers, too", {
  # A real value set next to the infinite one still gets a real verdict:
  out_grim <- tibble::tibble(x = c(5.25, Inf), n = 28L) |> grim_map(digits_x = 2)
  out_grim$consistency |> expect_equal(c(TRUE, NA))

  out_grimmer <- tibble::tibble(x = c(1.03, Inf), sd = 0.41, n = 40L) |> grimmer_map(digits_x = 2, digits_sd = 2)
  out_grimmer$consistency[2L] |> expect_na()
})


test_that("a missing `digits_*` makes a case undecidable, like a missing value", {
  # It used to fail inside `check_newly_numeric()` with "missing value where
  # TRUE/FALSE needed" -- from within `purrr::pmap()` in a mapper, where one row
  # with an unknown decimal count took the whole call down.
  5.19 |> grim(28, digits_x = NA)                            |> expect_na()
  1.03 |> grimmer(0.41, 40, digits_x = 2, digits_sd = NA)    |> expect_na()
  0.53 |> debit(0.50, 1683, digits_x = NA, digits_sd = 2)    |> expect_na()
  5.19 |> grim_values(28, digits_x = NA) |> purrr::pluck(1L) |> expect_na()
  5.19 |> grim_closest(28, digits_x = NA)                    |> expect_na()

  out <- tibble::tibble(x = c(5.19, 5.2), n = 28L) |> grim_map(digits_x = c(2, NA))
  out$consistency |> expect_equal(c(FALSE, NA))
  out$probability |> expect_equal(c(0.72, NA))

  # A sequence mapper disperses every value on one decimal level, so it needs a
  # number there:
  tibble::tibble(x = 5.19, n = 28L) |>
    grim_map_seq(digits_x = NA) |>
    expect_error("single number")
})
