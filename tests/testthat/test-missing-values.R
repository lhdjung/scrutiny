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
  NA |> debit(0.50, 1683, digits_x = 2, digits_sd = 2, data_type = "integer") |> expect_na()
  0.53 |> debit(NA, 1683, digits_x = 2, digits_sd = 2, data_type = "integer") |> expect_na()
  0.53 |> debit(0.50, NA, digits_x = 2, digits_sd = 2, data_type = "integer") |> expect_na()
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
        grim_values(28, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = rounding, symmetric = symmetric) |>
        expect_undecidable()
      NA |>
        grim_closest(28, digits_x = 2, items_averaged = 1, data_type = "integer", rounding = rounding, symmetric = symmetric) |> _$x |>
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
    debit(c(0.50, 0.50), c(1683, 1683), digits_x = 2, digits_sd = 2, data_type = "integer") |>
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
    debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |>
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
  out_debit <- df_debit |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer", show_rec = TRUE)
  out_debit$sd_lower[2:3] |> expect_equal(c(0.495, NA_real_))
  out_debit$x_lower[2:3]  |> expect_equal(c(NA_real_, 0.525))
  out_debit$sd_lower |> expect_type("double")
})


test_that("`roundwork::unround()`, `grim_values()`, and `grim_closest()` propagate `NA`", {
  bounds <- c(0.53, NA) |> roundwork::unround(digits = 2)
  bounds$lower |> expect_equal(c(0.525, NA))
  bounds$upper |> expect_equal(c(0.535, NA))

  values <- c(5.19, NA) |> grim_values(c(32, 32), digits_x = 2, items_averaged = 1, data_type = "integer")
  values$case |> expect_equal(1:2)
  values$x    |> expect_equal(c(5.1875, NA))
  values$sum  |> expect_equal(c(166, NA))

  c(5.19, NA) |>
    grim_closest(c(28, 28), digits_x = 2, items_averaged = 1, data_type = "integer") |> _$x |>
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

  out |> audit() |> purrr::pluck("hits_total") |> expect_equal(4L)
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
  NA   |> debit(0.50, 1683, digits_x = 2, digits_sd = 2, data_type = "integer") |> expect_na()
  1.53 |> debit(0.50, 1683, digits_x = 2, digits_sd = 2, data_type = "integer") |> expect_error()
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

  0.5 |> debit(sd = 0.5, n = 1, digits_x = 2, digits_sd = 2, data_type = "integer")    |> expect_na()
  0.5 |> debit(sd = 0.5, n = 0, digits_x = 2, digits_sd = 2, data_type = "integer")    |> expect_na()
  0.5 |> debit(sd = 0.5, n = -5, digits_x = 2, digits_sd = 2, data_type = "integer")   |> expect_na()
  0.5 |> debit(sd = 0.5, n = 20.5, digits_x = 2, digits_sd = 2, data_type = "integer") |> expect_na()
  # A real `n` still gets a real verdict:
  0.5 |> debit(sd = 0.5, n = 20, digits_x = 2, digits_sd = 2, data_type = "integer") |>
    expect_false()
  0.36 |> debit(sd = 0.11, n = 20, digits_x = 2, digits_sd = 2, data_type = "integer") |>
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

  out_debit <- tibble::tibble(x = 0.5, sd = 0.5, n = 1L) |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer")
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
  Inf  |> debit(0.5, 100, digits_x = 2, digits_sd = 2, data_type = "integer") |> expect_error()
  0.53 |> debit(Inf, 100, digits_x = 2, digits_sd = 2, data_type = "integer") |> expect_error()
})


test_that("`grimmer()` names the infinity as its reason", {
  tibble::tibble(x = Inf, sd = 0.41, n = 40L) |>
    grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("reason") |>
    expect_equal("Infinite value")
})


test_that("the achievable means behind an infinity are `NA`", {
  # `seq()` used to abort with "'from' must be a finite number":
  Inf |> grim_values(28, digits_x = 2, items_averaged = 1, data_type = "integer")  |> expect_undecidable()
  Inf |> grim_closest(28, digits_x = 2, items_averaged = 1, data_type = "integer") |> _$x |> expect_na()
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
  # These are consistent with the decimal places they show, so more of them
  # could decide either way:
  5.2  |> grim(28, digits_x = NA, items_averaged = 1, data_type = "integer")                            |> expect_na()
  3    |> grimmer(1.58, 5, digits_x = 0, digits_sd = NA, items_averaged = 1, data_type = "integer")     |> expect_na()
  0.53 |> debit(0.50, 1683, digits_x = NA, digits_sd = 2, data_type = "integer")                        |> expect_na()
  5.2  |> grim_values(28, digits_x = NA, items_averaged = 1, data_type = "integer") |> expect_undecidable()
  5.2  |> grim_closest(28, digits_x = NA, items_averaged = 1, data_type = "integer")                    |> _$x |> expect_na()

  out <- tibble::tibble(x = c(5.19, 5.2), n = 28L) |> grim_map(digits_x = c(2, NA), items_averaged = 1, data_type = "integer")
  out$consistency |> expect_equal(c(FALSE, NA))
  out$exp_incon |> expect_equal(c(0.72, NA))

  # A sequence mapper disperses each value on its decimal level, so a case to
  # disperse needs a number there:
  tibble::tibble(x = 5.19, n = 28L) |>
    grim_map_seq(digits_x = NA, items_averaged = 1, data_type = "integer") |>
    expect_error("missing in a case to disperse")
})

test_that("a value set that is inconsistent with the decimal places it shows is so without `digits_*`", {
  # More decimal places only narrow the range of numbers that are rounded to
  # a value, so the answer is forced. These were `NA`:
  5.19 |> grim(28, digits_x = NA, items_averaged = 1, data_type = "integer")                                |> expect_false()
  51.9 |> grim(28, digits_x = NA, items_averaged = 1, data_type = "integer", x_meaning = "percentage")      |> expect_false()
  2.5  |> grimmer(1.05, 10, digits_x = 1, digits_sd = NA, items_averaged = 1, data_type = "integer")        |> expect_false()
  2.5  |> grimmer(1.05, 10, digits_x = NA, digits_sd = NA, items_averaged = 1, data_type = "integer")       |> expect_false()
  0.35 |> debit(0.11, 20, digits_x = 2, digits_sd = NA, data_type = "integer")                              |> expect_false()
  5.19 |> grim_values(28, digits_x = NA, items_averaged = 1, data_type = "integer") |> nrow()                                      |> expect_equal(0L)
  5.19 |> grim_closest(28, digits_x = NA, items_averaged = 1, data_type = "integer")                                              |> _$x |> expect_equal(145 / 28)
  grimmer_scalar(2.5, 1.05, 10, 1, NA, 1, show_reason = TRUE, data_type = "integer")[[2L]]                  |> expect_equal("GRIMMER inconsistent (test 1)")

  # Continuous data: a single value has a population SD of 0, which 0.5 is
  # not rounded from at any number of decimal places. This was `TRUE`:
  2   |> grimmer(0.5, 1, digits_x = 0, digits_sd = NA, items_averaged = 1, data_type = "continuous", sd_meaning = "sd_pop") |> expect_false()
  0.5 |> debit(0.5, 1, digits_x = 1, digits_sd = NA, data_type = "continuous", sd_meaning = "sd_pop")                     |> expect_false()
  2   |> grimmer(0, 1, digits_x = 0, digits_sd = NA, items_averaged = 1, data_type = "continuous", sd_meaning = "sd_pop")   |> expect_true()

  # Nothing else that is missing or undecidable is decided by it:
  5.19 |> grim(NA, digits_x = NA, items_averaged = 1, data_type = "integer") |> expect_na()
  5.19 |> grim(0, digits_x = NA, items_averaged = 1, data_type = "integer")  |> expect_na()
  Inf  |> grim(28, digits_x = NA, items_averaged = 1, data_type = "integer") |> expect_na()
  5.19 |> grim(28, digits_x = NA, items_averaged = 1, data_type = NA)        |> expect_na()

  # The rule itself: `FALSE` with the fewest decimal places is `FALSE` with
  # more, for every rounding method. Random values, so that some are on the
  # edge:
  set.seed(sweep_seed(318L))
  roundings <- c("up_or_down", "up", "down", "even", "ceiling", "floor", "trunc", "anti_trunc", "ceiling_or_floor")
  for (i in 1:300) {
    rounding <- sample(roundings, 1)
    n <- sample(2:60, 1)
    x <- round(runif(1, 0, 9), 2)
    sd <- round(runif(1, 0, 3), 2)
    at_digits <- function(digits) {
      grimmer(x, sd, n, digits_x = digits, digits_sd = digits, items_averaged = 1, data_type = "integer", rounding = rounding)
    }
    least <- max(decimal_places_scalar(x), decimal_places_scalar(sd))
    if (isFALSE(at_digits(least))) {
      (least + 1:3) |> vapply(at_digits, logical(1L)) |> any() |> expect_false()
    }
    shown <- function(k) {
      grimmer(
        x, sd, n,
        digits_x = decimal_places_scalar(x) + k, digits_sd = decimal_places_scalar(sd) + k,
        items_averaged = 1, data_type = "integer", rounding = rounding
      )
    }
    verdict <- at_digits(NA)
    if (isFALSE(shown(0))) {
      verdict |> expect_false()
    } else if (isTRUE(verdict)) {
      (0:4) |> vapply(shown, logical(1L)) |> all() |> expect_true()
    } else {
      verdict |> expect_na()
    }
  }
})

test_that("a value set that is consistent with any number of decimal places is so without `digits_*`", {
  # With so many decimal places that only the value shown passes, it passes
  # with any. These were `NA`:
  5.2  |> grim(5, digits_x = NA, items_averaged = 1, data_type = "integer")                                       |> expect_true()
  52   |> grim(25, digits_x = NA, items_averaged = 1, data_type = "integer", x_meaning = "percentage")            |> expect_true()
  3    |> grimmer(0, 5, digits_x = NA, digits_sd = NA, items_averaged = 1, data_type = "integer")                 |> expect_true()
  3    |> grimmer(1, 3, digits_x = 0, digits_sd = NA, items_averaged = 1, data_type = "integer")                  |> expect_true()
  0.5  |> debit(0.5, 4, digits_x = NA, digits_sd = NA, data_type = "integer", sd_meaning = "sd_pop")              |> expect_true()
  5.2  |> grim_closest(5, digits_x = NA, items_averaged = 1, data_type = "integer")                                                      |> _$x |> expect_equal(5.2)
  5.2  |> grim_values(5, digits_x = NA, items_averaged = 1, data_type = "integer") |> expect_undecidable()
  # Not under `"mean_n"`, where no data stand behind the SD:
  0.5  |> debit(0.5, 4, digits_x = NA, digits_sd = NA, data_type = "integer", sd_meaning = "sd_pop", formula = "mean_n") |> expect_na()
  # Consistent with the decimal places shown, but not with all of them:
  5.2  |> grim(28, digits_x = NA, items_averaged = 1, data_type = "integer")                                      |> expect_na()

  # The rule for GRIM: `x * n * items_averaged` is a whole number.
  set.seed(sweep_seed(1104L))
  for (i in 1:300) {
    digits <- sample(0:3, 1)
    x <- round(runif(1, 0, 9), digits)
    n <- sample(1:60, 1)
    items <- sample(1:3, 1)
    shown <- decimal_places_scalar(x)
    exact <- (round(x * 10^shown) * n * items) %% 10^shown == 0
    least <- grim(x, n, digits_x = shown, items_averaged = items, data_type = "integer")
    expected <- if (!least) FALSE else if (exact) TRUE else NA
    x |> grim(n, digits_x = NA, items_averaged = items, data_type = "integer") |> expect_identical(expected)
  }

  # The explanations agree:
  text <- function(explanation) format(explanation) |> paste(collapse = " ") |> gsub(pattern = "\\s+", replacement = " ")
  grim_explain(x = 5.2, n = 5, digits_x = NA, items_averaged = 1, data_type = "integer") |>
    text() |>
    expect_match("digits_x is missing, so the test below takes so many decimal places.*Consistent")
  grimmer_explain(x = 3, sd = 0, n = 5, digits_x = NA, digits_sd = NA, items_averaged = 1, data_type = "integer") |>
    text() |>
    expect_match("digits_x and digits_sd are missing, so.*Consistent")
  tibble::tibble(x = 0.5, sd = 0.5, n = 4) |>
    debit_map(digits_x = NA, digits_sd = 1, data_type = "integer", sd_meaning = "sd_pop") |>
    explain_map() |>
    dplyr::pull(explanation) |>
    grepl(pattern = "digits_x is missing") |>
    expect_true()
})

test_that("a negative SD is inconsistent whatever else is missing", {
  # No data set of any size has one, but a missing `n` or `items_averaged`
  # was checked first:
  5 |> grimmer(-1, NA, digits_x = 0, digits_sd = 0, items_averaged = 1, data_type = "integer")  |> expect_false()
  5 |> grimmer(-1, 10, digits_x = 0, digits_sd = 0, items_averaged = NA, data_type = "integer") |> expect_false()
  5 |> grimmer(-1, NA, digits_x = 0, digits_sd = NA, items_averaged = 1, data_type = "integer") |> expect_false()
  5 |> grimmer(-1, NA, digits_x = 0, digits_sd = 0, items_averaged = 1, data_type = NA)         |> expect_false()
  grimmer_scalar(5, -1, NA, 0, 0, 1, show_reason = TRUE, data_type = "integer")[[2L]]           |> expect_equal("Negative SD")
  # An `n` that describes no data set is still undecidable:
  5 |> grimmer(-1, 0, digits_x = 0, digits_sd = 0, items_averaged = 1, data_type = "integer")   |> expect_na()
  grimmer_explain(x = 5, sd = -1, n = NA, digits_x = 0, digits_sd = 0, items_averaged = 1, data_type = "integer") |>
    format() |>
    paste(collapse = " ") |>
    expect_match("no data set has a negative SD")
})

test_that("an explanation says why a missing `digits_*` doesn't leave the case open", {
  text <- function(explanation) format(explanation) |> paste(collapse = " ") |> gsub(pattern = "\\s+", replacement = " ")
  grim_explain(x = 5.19, n = 28, digits_x = NA, items_averaged = 1, data_type = "integer") |>
    text() |>
    expect_match("digits_x is missing, so the test below takes the fewest decimal places.*with 2 decimal places.*Inconsistent")
  grim_explain(x = 5.2, n = 28, digits_x = NA, items_averaged = 1, data_type = "integer") |>
    text() |>
    expect_match("can't be decided without it.*Undecidable")
  grimmer_explain(x = 2.5, sd = 1.05, n = 10, digits_x = 1, digits_sd = NA, items_averaged = 1, data_type = "integer") |>
    text() |>
    expect_match("digits_sd is missing, so.*Inconsistent")
  debit_explain(x = 0.35, sd = 0.11, n = 20, digits_x = 2, digits_sd = NA, data_type = "integer") |>
    text() |>
    expect_match("digits_sd is missing, so.*Inconsistent")

  # A mapper's rows are explained the same way:
  tibble::tibble(x = c(5.19, 5.19), n = 28L) |>
    grim_map(digits_x = c(2, NA), items_averaged = 1, data_type = "integer") |>
    explain_map() |>
    dplyr::pull(explanation) |>
    grepl(pattern = "digits_x is missing") |>
    expect_equal(c(FALSE, TRUE))
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
  NA   |> debit(0.9, 20, digits_x = 2, digits_sd = 2, data_type = "integer") |> expect_false()
  0.33 |> debit(NA, 20, digits_x = 2, digits_sd = 2, data_type = "integer")  |> expect_false()
  0.33 |> debit(0.5, 20, digits_x = 2, digits_sd = NA, data_type = "integer") |> expect_false()
  NA   |> debit(0.9, 20, digits_x = 2, digits_sd = 2, data_type = "integer", formula = "mean_n") |> expect_false()
  # Under `"mean_n"`, the mean is continuous, so it never fails on its own:
  0.33 |> debit(NA, 20, digits_x = 2, digits_sd = 2, data_type = "integer", formula = "mean_n")  |> expect_na()
  NA   |> debit(0.9, 20, digits_x = 2, digits_sd = 2, data_type = "integer", sd_meaning = "sd_sample_or_pop") |> expect_false()
  NA   |> debit(0.5, 2, digits_x = 2, digits_sd = 1, data_type = "integer", sd_meaning = "sd_sample_or_pop")  |> expect_na()
  # With `n` missing, some `n` always works:
  5.19 |> grimmer(NA, NA, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |> expect_na()
  0.33 |> debit(NA, NA, digits_x = 2, digits_sd = 2, data_type = "integer") |> expect_na()
  # `show_rec` returns the bounds of the known value:
  out <- tibble::tibble(x = c(NA, 0.33), sd = c(0.9, NA), n = 20) |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer", show_rec = TRUE)
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
  debit_explain(NA, 0.9, 20, 2, 2, data_type = "integer")$consistency  |> expect_false()
  debit_explain(0.33, NA, 20, 2, 2, data_type = "integer")$consistency |> expect_false()
  debit_explain(NA, 0.9, 20, 2, 2, sd_meaning = "sd_sample_or_pop", data_type = "integer")$consistency |> expect_false()
  debit_explain(NA, 0.5, 2, 2, 1, sd_meaning = "sd_sample_or_pop", data_type = "integer")$consistency  |> expect_na()
  debit_explain(0.53, NA, 1683, 2, 2, data_type = "integer")$consistency |> expect_na()
  format(debit_explain(NA, 0.9, 20, 2, 2, data_type = "integer")) |> expect_match("none of these gives an SD")
  format(grimmer_explain(NA, 0.3, 2, 2, 2, 1, data_type = "integer")) |> expect_match("none passes")
})

test_that("sequence mappers disperse the known values of such a case", {
  df <- tibble::tibble(x = c(5.19, NA), sd = c(NA, 0.3), n = c(28, 2))
  out <- df |> grimmer_map_seq(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
  out$var[out$case == 1] |> unique() |> expect_equal(c("x", "n"))
  out$var[out$case == 2] |> unique() |> expect_equal(c("sd", "n"))
  out |> audit() |> nrow() |> expect_equal(2L)
  df[1, ] |> grimmer_map_seq(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", var = "sd") |> expect_warning("`sd` is missing in every case")
  tibble::tibble(x = 0.33, sd = NA, n = 10) |> debit_map_seq(digits_x = 2, digits_sd = 2, data_type = "integer") |> nrow() |> expect_gt(0)
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


test_that("a missing `n`, `items_averaged`, or `x` that can't change the verdict doesn't make it `NA`", {
  f_grim <- function(x, n, digits_x, items_averaged, ...) grim(x, n, digits_x, items_averaged, "integer", ...)
  # Copies of the values have the same mean, so a mean that 1 value has, any
  # number of them have:
  5    |> f_grim(NA, 0, 1)  |> expect_true()
  5.2  |> f_grim(NA, 1, 5)  |> expect_true()
  5.18 |> f_grim(28, 2, NA) |> expect_true()
  5.25 |> f_grim(NA, 2, 1)  |> expect_na()
  5.25 |> f_grim(NA, 2, 0)  |> expect_na()
  # From 10^digits_x values on, every mean passes:
  NA   |> f_grim(100, 2, 1) |> expect_true()
  NA   |> f_grim(99, 2, 1)  |> expect_na()
  NA   |> f_grim(10, 1, NA) |> expect_true()
  NA   |> f_grim(NA, 0, 1)  |> expect_true()
  NA   |> f_grim(80, 2, 1, rounding = "ties_even") |> expect_true()

  # Data that are all the same have an SD of 0, and whole numbers with a mean
  # of about 3.2 one of at least 0.36:
  f_grimmer <- function(x, sd, n, ...) grimmer(x, sd, n, 1, 1, 1, "integer", ...)
  3   |> grimmer(0, NA, 0, 0, 1, "integer")                   |> expect_true()
  3.2 |> f_grimmer(0.2, NA)                                   |> expect_false()
  3.2 |> f_grimmer(0.2, NA, sd_meaning = "sd_sample_or_pop")  |> expect_false()
  3.2 |> f_grimmer(0.4, NA)                                   |> expect_na()
  3.2 |> f_grimmer(0.2, NA, sd_meaning = "se")                |> expect_na()
  3.2 |> grimmer(0.2, NA, 1, 1, NA, "integer")                |> expect_na()
  grimmer_scalar(3.2, 0.2, NA, 1, 1, 1, "integer", show_reason = TRUE) |> expect_equal(list(FALSE, "GRIMMER inconsistent (test 1)"))
  (2:300) |> vapply(\(n) f_grimmer(3.2, 0.2, n), logical(1L)) |> any() |> expect_false()
  # Data that are all the same need the mean of a single value, not of `n`:
  5.4 |> grimmer(0, 18, 1, 1, NA, "integer")                    |> expect_na()
  5   |> grimmer(0, 18, 0, 1, NA, "integer")                    |> expect_true()
  2   |> grimmer(0, 1, 0, 0, NA, "integer")                     |> expect_na()
  2   |> grimmer(0, 1, 0, 0, NA, "integer", sd_meaning = "sd_pop") |> expect_true()
  NA  |> grimmer(0, NA, 0, 2, 1, "integer")                     |> expect_true()
  (1:5) |> vapply(\(items) grimmer(5.4, 0, 18, 1, 1, items, "integer"), logical(1L)) |> expect_equal(c(FALSE, FALSE, FALSE, FALSE, TRUE))

  # Continuous data can have any SD, except a single value's population SD:
  f_cont <- function(sd, n, sd_meaning = "sd_pop") grimmer(7.6, sd, n, 1, 1, 1, "continuous", sd_meaning = sd_meaning)
  2.9 |> f_cont(NA)                                      |> expect_na()
  2.9 |> f_cont(1)                                       |> expect_false()
  2.9 |> f_cont(NA, sd_meaning = "sd_sample_or_pop")     |> expect_na()
  2.9 |> f_cont(NA, sd_meaning = "sd_sample")            |> expect_true()
  NA  |> f_cont(1)                                       |> expect_na()
  0   |> f_cont(NA)                                      |> expect_true()
  0.6 |> debit(0.3, NA, 1, 1, "continuous", sd_meaning = "sd_pop") |> expect_na()
  5.2 |> grim(1, 1, 1, "continuous")                     |> expect_true()
  grimmer_explain(7.6, 2.9, NA, 1, 1, 1, "continuous", sd_meaning = "sd_pop") |> format() |> paste(collapse = " ") |> expect_match("unless n is 1")

  # Binary data have an SD of about sqrt(p * (1 - p)), but an SE shrinks
  # with `n`:
  f_debit <- function(x, sd, ...) debit(x, sd, NA, 1, 1, "integer", ...)
  0   |> f_debit(0)                        |> expect_true()
  0.2 |> f_debit(0.2)                      |> expect_false()
  0.5 |> f_debit(0.9)                      |> expect_false()
  0.7 |> f_debit(0.4, sd_meaning = "se")   |> expect_false()
  0.5 |> f_debit(0.5)                      |> expect_na()
  0.5 |> f_debit(0.1, sd_meaning = "se")   |> expect_na()
  (2:500) |> vapply(\(n) debit(0.7, 0.4, n, 1, 1, "integer", sd_meaning = "se"), logical(1L)) |> any() |> expect_false()
  # No population SD fits, but a sample SD does with `n = 8`:
  0.5 |> debit(0.53, NA, 2, 2, "integer", sd_meaning = "sd_sample_or_pop") |> expect_na()
  0.5 |> debit(0.53, 8, 2, 2, "integer", sd_meaning = "sd_sample_or_pop")  |> expect_true()
  0.5 |> debit_explain(0.53, NA, 2, 2, "integer", sd_meaning = "sd_sample_or_pop") |> (\(e) e$consistency)() |> expect_na()

  # The explanations say why:
  text <- function(explanation) format(explanation) |> paste(collapse = " ") |> gsub(pattern = "\\s+", replacement = " ")
  grim_explain(5, NA, 0, 1, "integer")                  |> text() |> expect_match("n = NA.*takes 1 in its place.*Consistent")
  grim_explain(NA, 100, 2, 1, "integer")                |> text() |> expect_match("x is missing.*Consistent")
  grimmer_explain(3.2, 0.2, NA, 1, 1, 1, "integer")     |> text() |> expect_match("at least about 0.3571.*Inconsistent")
  debit_explain(0.7, 0.4, NA, 1, 1, "integer", sd_meaning = "se") |> text() |> expect_match("shrinks as n grows.*Inconsistent")
})


test_that("an item count the test can't take is named even if `n` is missing", {
  text <- function(explanation) format(explanation) |> paste(collapse = " ")
  grim_explain(5.19, NA, 2, 0, "integer")             |> text() |> expect_match("number of items to be a positive integer")
  grim_explain(5.19, 0, 2, NA, "integer")             |> text() |> expect_match("needs n to be a positive integer")
  grimmer_explain(5.19, 1, NA, 2, 0, 0, "integer")    |> text() |> expect_match("number of items to be a positive integer")
})
