test_that("`digits_x` missing leads to failure", {
  # `suppressMessages()` mutes the changelog hint that `error_digits_missing()`
  # prints via `on.exit()` as it unwinds:
  expect_error(suppressMessages(grim(2.65, 30)))
  expect_error(suppressMessages(grim(924, 0)))
})


test_that("Return values are Boolean", {
  expect_type(grim(5.19, 28, digits_x = 2), "logical")
  expect_type(grim(0.00, 100, digits_x = 2), "logical")
})


vec1 <- as.numeric(seq_endpoint(5.19, 5.3))

vec1_tested <- vec1 |>
  grim(28, digits_x = 2) |>
  unname()

t <- TRUE
f <- FALSE

vec1_expected <- c(f, f, t, f, f, f, t, f, f, f, t, f)


test_that("Correct values are returned (basic)", {
  expect_equal(vec1_tested, vec1_expected)
})


vec2 <- as.numeric(seq_endpoint(0.150, 0.159))

vec2_tested <- vec2 |>
  grim(120, digits_x = 3, items = 3) |>
  unname()

vec2_expected <- c(t, f, f, t, f, f, t, f, t, f)


vec3 <- as.numeric(seq_endpoint(0.80, 0.89))

vec3_tested <- vec3 |>
  grim(28, digits_x = 2, items = 2) |>
  unname()

vec3_expected <- c(t, f, t, f, t, f, t, t, t, t)


test_that("Correct values are returned (`items` argument)", {
  expect_equal(vec2_tested, vec2_expected)
  expect_equal(vec3_tested, vec3_expected)
})


vec4 <- as.numeric(seq_endpoint(519, 530))

vec4_tested <- vec4 |>
  grim(28, digits_x = 0, percent = TRUE) |>
  unname()

vec4_expected <- c(f, f, t, f, f, f, t, f, f, f, t, f)

vec5 <- as.numeric(seq_endpoint(6, 16))

vec5_tested <- vec5 |>
  grim(50, digits_x = 0, percent = TRUE) |>
  unname()

vec5_expected <- c(t, f, t, f, t, f, t, f, t, f, t)


test_that("Correct values are returned (`percent` argument)", {
  expect_equal(vec4_tested, vec4_expected)
  expect_equal(vec5_tested, vec5_expected)
})


vec <- as.numeric(seq_endpoint(5, 5.99))


test_that("The number of outputs matches the number of inputs", {
  expect_length(grim(vec, 28, digits_x = 2), length(vec))
})


# Example vectors for the test below:
x_length <- rnorm(1, 30, 3) |>
  censor(25, 35) |>
  round()

x <- rnorm(x_length, 50, 20) |>
  censor(10, 90) |>
  round(2)


test_that("There are as many outputs as inputs", {
  grim(x, 50, digits_x = 2) |> expect_length(x_length)
})


# Exact arithmetic at the rounding boundary (#86) -------------------------

# In all of these, the only candidate granule sits mathematically *exactly* on
# the exclusive upper bound of `rounding = "up"`, so it must be rejected. The
# granule and the bound are different floating-point representations of the
# same real number, which used to make the strict comparison `granule < bound`
# wrongly return `TRUE`.

test_that("granules exactly on the exclusive `\"up\"` bound are rejected", {
  # 3 / 40 is 0.075, which rounds up to 0.08, not to 0.07:
  expect_false(grim(x = 0.07, n = 40, digits_x = 2, rounding = "up"))
  expect_false(grim(x = 0.17, n = 40, digits_x = 2, rounding = "up"))
  expect_false(grim(x = 0.07, n = 80, digits_x = 2, rounding = "up"))
  # Mirrored case with a negative mean:
  expect_false(grim(x = -0.03, n = 40, digits_x = 2, rounding = "up"))
  expect_false(grim(x = -0.03, n = 80, digits_x = 2, rounding = "up"))
  # Three-decimal variant: 47 / 400 is 0.1175, which rounds up to 0.118:
  expect_false(grim(x = 0.117, n = 400, digits_x = 3, rounding = "up"))
  expect_false(grim(x = 0.117, n = 800, digits_x = 3, rounding = "up"))
})


test_that("granules exactly on an inclusive bound are accepted", {
  # The same granules as above, but now with rounding methods for which the
  # bound they sit on is inclusive:
  expect_true(grim(x = 0.07, n = 40, digits_x = 2, rounding = "up_or_down"))
  # Under `"down"` it is the *lower* bound that is exclusive, so the granule
  # 3 / 40 = 0.075 now sits on the inclusive upper bound of 0.07:
  expect_true(grim(x = 0.07, n = 40, digits_x = 2, rounding = "down"))
  expect_true(grim(x = -0.03, n = 40, digits_x = 2, rounding = "up_or_down"))
  expect_true(grim(x = 0.117, n = 400, digits_x = 3, rounding = "up_or_down"))
})


test_that("GRIM agrees with the rounding functions themselves", {
  # An independent oracle: enumerate the integer sums around `x * n` directly,
  # then ask the actual rounding functions whether the resulting granule rounds
  # back to `x`. GRIM must agree on every case.
  oracle <- function(x, n, digits, rounding) {
    sums <- seq(floor(x * n) - 2, ceiling(x * n) + 2)
    rounded <- reround(sums / n, digits = digits, rounding = rounding)
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
    expect_equal(
      unname(grim(x, n, digits_x = digits, rounding = rounding)),
      vapply(x, oracle, logical(1), n, digits, rounding)
    )
  }
})


# `symmetric` rounding ----------------------------------------------------

test_that("`symmetric` is honored by the consistency decision", {
  # The only granule around `-0.07 * 40` is `-3 / 40`, i.e. -0.075, which sits
  # exactly on a rounding bound. Without `symmetric`, `round_up()` shifts it
  # towards `+Inf` and it becomes -0.07; with `symmetric`, it mirrors the
  # rounding of 0.075 and becomes -0.08 instead.
  expect_equal(round_up(-0.075, 2, symmetric = FALSE), -0.07)
  expect_equal(round_up(-0.075, 2, symmetric = TRUE), -0.08)

  expect_true(
    grim(-0.07, n = 40, digits_x = 2, rounding = "up", symmetric = FALSE)
  )
  expect_false(
    grim(-0.07, n = 40, digits_x = 2, rounding = "up", symmetric = TRUE)
  )

  # Mirrored the other way around: with `symmetric`, `"down"` is what keeps
  # -0.075 at -0.07.
  expect_false(
    grim(-0.07, n = 40, digits_x = 2, rounding = "down", symmetric = FALSE)
  )
  expect_true(
    grim(-0.07, n = 40, digits_x = 2, rounding = "down", symmetric = TRUE)
  )

  # Positive values are unaffected:
  for (rounding in c("up", "down", "up_or_down")) {
    x <- seq(0, 100) / 100
    expect_equal(
      unname(grim(x, 40, digits_x = 2, rounding = rounding, symmetric = TRUE)),
      unname(grim(x, 40, digits_x = 2, rounding = rounding, symmetric = FALSE))
    )
  }
})


test_that("`symmetric` GRIM agrees with the rounding functions themselves", {
  oracle_symmetric <- function(x, n, digits, rounding) {
    sums <- seq(floor(x * n) - 3, ceiling(x * n) + 3)
    rounded <- reround(
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
      expect_equal(
        unname(grim(
          x, n,
          digits_x = 2, rounding = rounding, symmetric = TRUE
        )),
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
    baseline <- grim(x, 40, digits_x = 2, rounding = rounding)
    for (threshold in c(1, 3, 7, 9)) {
      expect_equal(
        unname(grim(
          x, 40,
          digits_x = 2, rounding = rounding, threshold = threshold
        )),
        unname(baseline),
        info = paste0("rounding = ", rounding, ", threshold = ", threshold)
      )
    }
  }

  # By contrast, `"up_from"` does respond to it:
  expect_false(identical(
    unname(grim(x, 40, digits_x = 2, rounding = "up_from", threshold = 1)),
    unname(grim(x, 40, digits_x = 2, rounding = "up_from", threshold = 9))
  ))
})
