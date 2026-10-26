test_that("`digits_x` missing leads to failure", {
  # `suppressMessages()` mutes the changelog hint that `error_digits_missing()`
  # prints via `on.exit()` as it unwinds:
  2.65 |> grim(30, items_averaged = 1) |> suppressMessages() |> expect_error()
  924  |> grim(0, items_averaged = 1)  |> suppressMessages() |> expect_error()
})


test_that("Return values are Boolean", {
  5.19 |> grim(28, digits_x = 2, items_averaged = 1)  |> expect_type("logical")
  0.00 |> grim(100, digits_x = 2, items_averaged = 1) |> expect_type("logical")
})


vec1 <- 5.19 |> seq(5.3, by = 0.01) |> round(2)

vec1_tested <- vec1 |>
  grim(28, digits_x = 2, items_averaged = 1) |>
  unname()

t <- TRUE
f <- FALSE

vec1_expected <- c(f, f, t, f, f, f, t, f, f, f, t, f)


test_that("Correct values are returned (basic)", {
  vec1_tested |> expect_equal(vec1_expected)
})


vec2 <- 0.150 |> seq(0.159, by = 0.001) |> round(3)

vec2_tested <- vec2 |>
  grim(120, digits_x = 3, items_averaged = 3) |>
  unname()

vec2_expected <- c(t, f, f, t, f, f, t, f, t, f)


vec3 <- 0.80 |> seq(0.89, by = 0.01) |> round(2)

vec3_tested <- vec3 |>
  grim(28, digits_x = 2, items_averaged = 2) |>
  unname()

vec3_expected <- c(t, f, t, f, t, f, t, t, t, t)


test_that("Correct values are returned (`items_averaged` argument)", {
  vec2_tested |> expect_equal(vec2_expected)
  vec3_tested |> expect_equal(vec3_expected)
})


vec4 <- 519:530

vec4_tested <- vec4 |>
  grim(28, digits_x = 0, items_averaged = 1, percent = TRUE) |>
  unname()

vec4_expected <- c(f, f, t, f, f, f, t, f, f, f, t, f)

vec5 <- 6:16

vec5_tested <- vec5 |>
  grim(50, digits_x = 0, items_averaged = 1, percent = TRUE) |>
  unname()

vec5_expected <- c(t, f, t, f, t, f, t, f, t, f, t)


test_that("Correct values are returned (`percent` argument)", {
  vec4_tested |> expect_equal(vec4_expected)
  vec5_tested |> expect_equal(vec5_expected)
})


vec <- 5 |> seq(5.99, by = 0.01) |> round(2)


test_that("The number of outputs matches the number of inputs", {
  vec |> grim(28, digits_x = 2, items_averaged = 1) |> expect_length(length(vec))
})


# Example vectors for the test below:
x_length <- rnorm(1, 30, 3) |>
  censor(25, 35) |>
  round()

x <- rnorm(x_length, 50, 20) |>
  censor(10, 90) |>
  round(2)


test_that("There are as many outputs as inputs", {
  x |> grim(50, digits_x = 2, items_averaged = 1) |> expect_length(x_length)
})


# Exact arithmetic at the rounding boundary (#86) -------------------------

# In all of these, the only candidate granule sits mathematically *exactly* on
# the exclusive upper bound of `rounding = "up"`, so it must be rejected. The
# granule and the bound are different floating-point representations of the
# same real number, which used to make the strict comparison `granule < bound`
# wrongly return `TRUE`.

test_that("granules exactly on the exclusive `\"up\"` bound are rejected", {
  # 3 / 40 is 0.075, which rounds up to 0.08, not to 0.07:
  0.07 |> grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "up") |> expect_false()
  0.17 |> grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "up") |> expect_false()
  0.07 |> grim(n = 80, digits_x = 2, items_averaged = 1, rounding = "up") |> expect_false()
  # Mirrored case with a negative mean:
  -0.03 |> grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "up") |> expect_false()
  -0.03 |> grim(n = 80, digits_x = 2, items_averaged = 1, rounding = "up") |> expect_false()
  # Three-decimal variant: 47 / 400 is 0.1175, which rounds up to 0.118:
  0.117 |> grim(n = 400, digits_x = 3, items_averaged = 1, rounding = "up") |> expect_false()
  0.117 |> grim(n = 800, digits_x = 3, items_averaged = 1, rounding = "up") |> expect_false()
})


test_that("granules exactly on an inclusive bound are accepted", {
  # The same granules as above, but now with rounding methods for which the
  # bound they sit on is inclusive:
  0.07 |> grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "up_or_down") |> expect_true()
  # Under `"down"` it is the *lower* bound that is exclusive, so the granule
  # 3 / 40 = 0.075 now sits on the inclusive upper bound of 0.07:
  0.07 |> grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "down") |> expect_true()
  -0.03 |> grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "up_or_down") |> expect_true()
  0.117 |> grim(n = 400, digits_x = 3, items_averaged = 1, rounding = "up_or_down") |> expect_true()
})


test_that("GRIM agrees with the rounding functions themselves", {
  # An independent oracle: enumerate the integer sums around `x * n` directly,
  # then ask the actual rounding functions whether the resulting granule rounds
  # back to `x`. GRIM must agree on every case.
  oracle <- function(x, n, digits, rounding) {
    sums <- seq(floor(x * n) - 2, ceiling(x * n) + 2)
    rounded <- roundwork::reround(sums / n, digits = digits, rounding = rounding)
    any(abs(rounded - x) < 1e-9)
  }

  # `n` values that are multiples of 40 (2 decimal places) or of 4 (1 decimal
  # place) are those where granules can fall exactly on a rounding boundary:
  cases <- expand.grid(
    digits = 1:2,
    n = c(4, 8, 20, 40, 64, 80, 160, 200, 400),
    rounding = c("up_or_down", "up", "down"),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(cases))) {
    digits <- cases$digits[i]
    n <- cases$n[i]
    rounding <- cases$rounding[i]
    x <- seq(0, 10^digits) / 10^digits
    x |>
      grim(n, digits_x = digits, items_averaged = 1, rounding = rounding) |>
      unname() |>
      expect_equal(vapply(x, oracle, logical(1), n, digits, rounding))
  }
})


# `symmetric` rounding ----------------------------------------------------

test_that("`symmetric` is honored by the consistency decision", {
  # The only granule around `-0.07 * 40` is `-3 / 40`, i.e. -0.075, which sits
  # exactly on a rounding bound. Without `symmetric`, `round_up()` shifts it
  # towards `+Inf` and it becomes -0.07; with `symmetric`, it mirrors the
  # rounding of 0.075 and becomes -0.08 instead.
  -0.075 |> roundwork::round_up(2, symmetric = FALSE) |> expect_equal(-0.07)
  -0.075 |> roundwork::round_up(2, symmetric = TRUE)  |> expect_equal(-0.08)

  -0.07 |> grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "up", symmetric = FALSE) |> expect_true()
  -0.07 |> grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "up", symmetric = TRUE)  |> expect_false()

  # Mirrored the other way around: with `symmetric`, `"down"` is what keeps
  # -0.075 at -0.07.
  -0.07 |> grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "down", symmetric = FALSE) |> expect_false()
  -0.07 |> grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "down", symmetric = TRUE)  |> expect_true()

  # Positive values are unaffected:
  for (rounding in c("up", "down", "up_or_down")) {
    x <- seq(0, 100) / 100
    expected <- x |> grim(40, digits_x = 2, items_averaged = 1, rounding = rounding, symmetric = FALSE) |> unname()
    x |>
      grim(40, digits_x = 2, items_averaged = 1, rounding = rounding, symmetric = TRUE) |>
      unname() |>
      expect_equal(expected)
  }
})


test_that("`symmetric` GRIM agrees with the rounding functions themselves", {
  oracle_symmetric <- function(x, n, digits, rounding) {
    sums <- seq(floor(x * n) - 3, ceiling(x * n) + 3)
    rounded <- roundwork::reround(
      sums / n,
      digits = digits,
      rounding = rounding,
      symmetric = TRUE
    )
    any(abs(rounded - x) < 1e-11, na.rm = TRUE)
  }

  for (rounding in c("up_or_down", "up", "down")) {
    for (n in c(4, 20, 40, 80, 160)) {
      x <- c(seq(-50, 50) / 100)
      x |>
        grim(n, digits_x = 2, items_averaged = 1, rounding = rounding, symmetric = TRUE) |>
        unname() |>
        expect_equal(
          vapply(x, oracle_symmetric, logical(1), n, 2, rounding),
          info = paste0("n = ", n, ", rounding = ", rounding)
        )
    }
  }
})


# `threshold` scope -------------------------------------------------------

test_that("`threshold` does not affect the non-`\"*_from\"` methods", {
  # `round_up()` and `round_down()` round from a fixed 5, so `threshold` must
  # not move GRIM's bounds for the methods that use them. The `"*_from"`
  # methods are the parameterized ones.
  x <- seq(0, 100) / 100

  for (rounding in c("up_or_down", "up", "down")) {
    baseline <- grim(x, 40, digits_x = 2, items_averaged = 1, rounding = rounding)
    for (threshold in c(1, 3, 7, 9)) {
      x |>
        grim(40, digits_x = 2, items_averaged = 1, rounding = rounding, threshold = threshold) |>
        unname() |>
        expect_equal(
          unname(baseline),
          info = paste0("rounding = ", rounding, ", threshold = ", threshold)
        )
    }
  }

  # By contrast, `"up_from"` does respond to it:
  up_from_9 <- x |> grim(40, digits_x = 2, items_averaged = 1, rounding = "up_from", threshold = 9) |> unname()
  x |>
    grim(40, digits_x = 2, items_averaged = 1, rounding = "up_from", threshold = 1) |>
    unname() |>
    identical(up_from_9) |>
    expect_false()
})


test_that("a string `n` is an error, not an unexplained `NA`", {
  5.19 |> grim("28", 2, items_averaged = 1) |> expect_error("`n` must be one of these types")
  tibble::tibble(x = 5.19, n = "28") |>
    grim_map(digits_x = 2, items_averaged = 1) |>
    expect_error("`n` must be one of these types")
  5.19 |> grim(NA, 2, items_averaged = 1) |> is.na() |> expect_true()
})


test_that("a plain `NA` for `items_averaged` is missing, not a type error", {
  5.19 |> grim(20, digits_x = 2, items_averaged = NA)                        |> expect_equal(NA)
  5.19 |> grimmer(1.2, 20, digits_x = 2, digits_sd = 1, items_averaged = NA) |> expect_equal(NA)
})


# `items` defaulted to 1, which let multi-item scales be tested as single items.
test_that("a missing `items_averaged` is a bespoke error everywhere", {
  df <- tibble::tibble(x = 5.19, n = 28)

  5.19 |> grim(28, 2)                         |> expect_error("Need to specify `items_averaged`")
  5.19 |> grimmer(1.2, 28, 2, 1)              |> expect_error("Need to specify `items_averaged`")
  5.19 |> grim_values(28, 2)                  |> expect_error("Need to specify `items_averaged`")
  5.19 |> grim_closest(28, 2)                 |> expect_error("Need to specify `items_averaged`")
  5.19 |> grim_probability(28, 2)             |> expect_error("Need to specify `items_averaged`")
  5.19 |> grim_explain(28, 2)                 |> expect_error("Need to specify `items_averaged`")
  5.19 |> grim_plot_values(28, 2)             |> expect_error("Need to specify `items_averaged`")
  df   |> grim_map(digits_x = 2)              |> expect_error("an `items_averaged` column")
  df   |> grim_map_seq(digits_x = 2)          |> expect_error("an `items_averaged` column")
  tibble::tibble(x1 = 5.2, x2 = 5.4, n = 40L) |> grim_map_total_n(digits_x = 1) |> expect_error("an `items_averaged` column")

  # An old `items` column is no substitute, and the message says why:
  df |> dplyr::mutate(items = 2) |> grim_map(digits_x = 2) |> expect_error("replaces `items`")

  # The new column satisfies it as well as the argument:
  df |> dplyr::mutate(items_averaged = 1) |> grim_map(digits_x = 2) |> expect_equal(grim_map(df, digits_x = 2, items_averaged = 1))
})


# It used to reach these through the dots, invisible to `formals()`, to
# tab-completion, and to the help page:
test_that("every GRIM and GRIMMER mapper has `items_averaged` right after the key arguments and the dots", {
  grim_map            |> formals() |> names() |> head(6L) |> expect_equal(c("data", "digits_x", "x", "n", "...", "items_averaged"))
  grim_map_seq        |> formals() |> names() |> head(6L) |> expect_equal(c("data", "digits_x", "x", "n", "...", "items_averaged"))
  grim_map_total_n    |> formals() |> names() |> head(7L) |> expect_equal(c("data", "digits_x", "x1", "x2", "n", "...", "items_averaged"))
  grimmer_map_seq     |> formals() |> names() |> head(8L) |> expect_equal(c("data", "digits_x", "digits_sd", "x", "sd", "n", "...", "items_averaged"))
  grimmer_map_total_n |> formals() |> names() |> head(10L) |> expect_equal(c("data", "digits_x", "digits_sd", "x1", "x2", "sd1", "sd2", "n", "...", "items_averaged"))
})


test_that("arguments after the dots can't be abbreviated", {
  # `items` used to partially match `items_averaged`, and `n` in a total-n
  # mapper both `n_min` and `n_max`:
  pigs1 |> grim_map(digits_x = 2, items = 2)     |> expect_error("Did you mean `items_averaged`")
  pigs1 |> grim_map_seq(digits_x = 2, items = 1) |> expect_error("Did you mean `items_averaged`")
  # An argument of the basic mapper, which a sequence or total-n mapper only
  # passes on, is suggested as well:
  pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1, round = "up") |> expect_error("Did you mean `rounding`")
  # An unnamed argument after the key arguments goes into the dots:
  pigs1 |> grim_map(2, "x", "n", 1) |> expect_error("1 unnamed argument after the key arguments")
  tibble::tibble(x1 = 5.19, x2 = 4.1, total = 40) |> grim_map_total_n(digits_x = 2, items_averaged = 1, n = "total", dispersion = 0) |> nrow() |> expect_equal(2L)
})


test_that("`percent` must be a single `TRUE` or `FALSE`", {
  # These used to fail in `if ()` with base R's message:
  1.4 |> grim(20, digits_x = 1, items_averaged = 1, percent = NA)             |> expect_error("`TRUE` or `FALSE`")
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, percent = c(TRUE, FALSE)) |> expect_error("`TRUE` or `FALSE`")
})

test_that("a `NULL` key argument and an out-of-range `digits_x` are errors", {
  1.4 |> grim(20, digits_x = NULL, items_averaged = 1) |> expect_error("must not be `NULL`")
  1.4 |> grim(20, digits_x = 16, items_averaged = 1)   |> expect_error("0 to 15")
})


test_that("integer `n` and `items_averaged` don't overflow their product", {
  1.5 |> grim(50000L, digits_x = 1, items_averaged = 50000L) |> expect_true()
  tibble::tibble(x = 1.5, n = 50000L, items_averaged = 50000L) |> grim_map(digits_x = 1) |> dplyr::pull(consistency) |> expect_true()
})


test_that("a factor `n` is rejected rather than tested by its level codes", {
  5.19 |> grim(factor(28), digits_x = 2, items_averaged = 1) |> expect_error("It is a factor")
  tibble::tibble(x = 5.19, n = factor(28)) |> grim_map(digits_x = 2, items_averaged = 1) |> expect_error("It is a factor")
})


test_that("a threshold cannot make a consistency test decide nothing", {
  # The practical consequence of the width invariant. With the old bounds,
  # `grim()` under `threshold = 9` passed *every* mean at `n = 57`, because the
  # reconstructed range was wide enough to reach a possible mean whatever the
  # reported value was. Moving the tie point shifts the window without resizing
  # it, so the count of consistent means does not depend on the threshold.
  x <- seq(0, 9.99, by = 0.01) |> sprintf(fmt = "%.2f") |> as.numeric()
  for (n in c(28, 57)) {
    at_5 <- x |> grim(n, digits_x = 2, items_averaged = 1, rounding = "up_or_down") |> sum()
    for (threshold in c(1, 3, 7, 9)) {
      x |>
        grim(
          n,
          digits_x = 2,
          items_averaged = 1,
          rounding = "up_from_or_down_from",
          threshold = threshold
        ) |>
        sum() |>
        expect_equal(
          at_5,
          label = paste("`grim()` at n =", n, "| threshold =", threshold)
        )
    }
  }
})


test_that("a mean of zero is decidable under `\"anti_trunc\"`", {
  # `anti_trunc()` leaves zero alone, so a mean of zero pins the sum to zero:
  0    |> grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "anti_trunc") |> expect_true()
  0.01 |> grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "anti_trunc") |> expect_false()
})


test_that("invalid rounding arguments are errors from the function called", {
  # roundwork checks these arguments itself, but its errors would name the
  # roundwork function that scrutiny called, such as `sum_range()`:
  caller_of <- function(expr) {
    tryCatch(expr, error = function(cnd) as.character(cnd$call[[1L]]))
  }

  5.19 |> grim(28, digits_x = 2, items_averaged = 1, rounding = 2)                       |> expect_error("must be a string")
  5.19 |> grim(28, digits_x = 2, items_averaged = 1, rounding = "up", symmetric = NA)    |> expect_error("`TRUE` or `FALSE`")
  5.19 |> grim(28, digits_x = 2, items_averaged = 1, rounding = "nope")                  |> expect_error("designated string values")
  5.19 |> grim(28, digits_x = 2, items_averaged = 1, rounding = "up_from", threshold = 0) |> expect_error("greater than 0")

  tibble::tibble(x = -5.19, n = 28) |>
    grim_map(digits_x = 2, items_averaged = 1, rounding = "ties_up", symmetric = TRUE) |>
    expect_error("must not be given")

  5.19 |> grim(28, digits_x = 2, items_averaged = 1, rounding = 2)                 |> caller_of() |> expect_equal("grim")
  5.19 |> grim_values(28, digits_x = 2, items_averaged = 1, rounding = 2)          |> caller_of() |> expect_equal("grim_values")
  5.19 |> grim_explain(28, digits_x = 2, items_averaged = 1, rounding = 2)         |> caller_of() |> expect_equal("grim_explain")
  5.19 |> grim_probability(28, digits_x = 2, items_averaged = 1, rounding = 2)     |> caller_of() |> expect_equal("grim_probability")
  5.19 |> grim_plot_values(28, digits_x = 2, items_averaged = 1, rounding = 2)     |> caller_of() |> expect_equal("grim_plot_values")
  tibble::tibble(x = 5.19, n = 28) |> grim_map(digits_x = 2, items_averaged = 1, rounding = 2) |> caller_of() |> expect_equal("grim_map")
  tibble::tibble(x = 5.19, n = 28) |> grim_map_seq(digits_x = 2, items_averaged = 1, rounding = 2) |> caller_of() |> expect_equal("grim_map_seq")
  # With no rows, there is no first row to fail on:
  tibble::tibble(x = numeric(), n = numeric()) |> grim_map(digits_x = 2, items_averaged = 1, rounding = 2) |> expect_error("must be a string")
})
