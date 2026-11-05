# Basic linear testing function -------------------------------------------

test_that("`is_seq_linear()` returns `TRUE` when it should", {
  1             |> is_seq_linear() |> expect_true()
  1:10          |> is_seq_linear() |> expect_true()
  c(3, 10)      |> is_seq_linear() |> expect_true()
  seq(4, 12, 2) |> is_seq_linear() |> expect_true()
})


test_that("`is_seq_linear()` returns `FALSE` when it should", {
  c(1:10, 12)  |> is_seq_linear() |> expect_false()
  c(3, 12, 14) |> is_seq_linear() |> expect_false()
  c(7, 2, 20)  |> is_seq_linear() |> expect_false()
})


# With `test_linear = TRUE` (the default) ---------------------------------

test_that("`is_seq_ascending()` with the default `test_linear = TRUE`
          returns `TRUE` when it should", {
  1:10          |> is_seq_ascending() |> expect_true()
  c(3, 10)      |> is_seq_ascending() |> expect_true()
  seq(4, 12, 2) |> is_seq_ascending() |> expect_true()
})


test_that("`is_seq_ascending()` with the default `test_linear = TRUE`
          returns `FALSE` when it should", {
  1            |> is_seq_ascending() |> expect_false()
  c(1:10, 12)  |> is_seq_ascending() |> expect_false()
  c(3, 12, 14) |> is_seq_ascending() |> expect_false()
  c(7, 2, 20)  |> is_seq_ascending() |> expect_false()
})


test_that("`is_seq_descending()` with the default `test_linear = TRUE`
          returns `TRUE` when it should", {
  1:10          |> rev() |> is_seq_descending() |> expect_true()
  c(3, 10)      |> rev() |> is_seq_descending() |> expect_true()
  seq(4, 12, 2) |> rev() |> is_seq_descending() |> expect_true()
})


test_that("`is_seq_descending()` with the default `test_linear = TRUE`
          returns `FALSE` when it should", {
  1 |> is_seq_descending() |> expect_false()
  c(1:10, 12)  |> rev() |> is_seq_descending() |> expect_false()
  c(3, 12, 14) |> rev() |> is_seq_descending() |> expect_false()
  c(7, 2, 20)  |> rev() |> is_seq_descending() |> expect_false()
})


test_that("`is_seq_dispersed()` with the default `test_linear = TRUE`
          returns `FALSE` when it should", {
  50    |> seq_disperse() |> is_seq_dispersed(from = 50)    |> expect_true()
  7.3   |> seq_disperse() |> is_seq_dispersed(from = 7.3)   |> expect_true()
  0.009 |> seq_disperse() |> is_seq_dispersed(from = 0.009) |> expect_true()
})


test_that("`is_seq_dispersed()` with the default `test_linear = TRUE`
          returns `FALSE` when it should", {
  1             |> is_seq_dispersed(from = 3) |> expect_false()
  1:10          |> is_seq_dispersed(from = 3) |> expect_false()
  c(3, 10)      |> is_seq_dispersed(from = 3) |> expect_false()
  seq(4, 12, 2) |> is_seq_dispersed(from = 3) |> expect_false()
})


# With `test_linear = FALSE` ----------------------------------------------

f <- FALSE

test_that("`is_seq_ascending()` with `test_linear = FALSE`
          returns `TRUE` when it should", {
  1:10          |> is_seq_ascending(test_linear = f) |> expect_true()
  c(3, 10)      |> is_seq_ascending(test_linear = f) |> expect_true()
  c(1:10, 12)   |> is_seq_ascending(test_linear = f) |> expect_true()
  c(3, 12, 14)  |> is_seq_ascending(test_linear = f) |> expect_true()
  seq(4, 12, 2) |> is_seq_ascending(test_linear = f) |> expect_true()
})


test_that("`is_seq_ascending()` with `test_linear = FALSE`
          returns `FALSE` when it should", {
  1            |> is_seq_ascending(test_linear = f) |> expect_false()
  c(7, 2, 20)  |> is_seq_ascending(test_linear = f) |> expect_false()
})


test_that("`is_seq_descending()` with `test_linear = FALSE`
          returns `TRUE` when it should", {
  1:10          |> rev() |> is_seq_descending(test_linear = f) |> expect_true()
  c(3, 10)      |> rev() |> is_seq_descending(test_linear = f) |> expect_true()
  seq(4, 12, 2) |> rev() |> is_seq_descending(test_linear = f) |> expect_true()
  c(1:10, 12)   |> rev() |> is_seq_descending(test_linear = f) |> expect_true()
  c(3, 12, 14)  |> rev() |> is_seq_descending(test_linear = f) |> expect_true()
})


test_that("`is_seq_descending()` with `test_linear = FALSE`
          returns `FALSE` when it should", {
  1            |> rev() |> is_seq_descending(test_linear = f) |> expect_false()
  c(7, 2, 20)  |> rev() |> is_seq_descending(test_linear = f) |> expect_false()
})


test_that("`is_seq_dispersed()` with `test_linear = FALSE`
          returns `FALSE` when it should", {
  50    |> seq_disperse() |> is_seq_dispersed(from = 50   , test_linear = f) |> expect_true()
  7.3   |> seq_disperse() |> is_seq_dispersed(from = 7.3  , test_linear = f) |> expect_true()
  0.009 |> seq_disperse() |> is_seq_dispersed(from = 0.009, test_linear = f) |> expect_true()
})


test_that("`is_seq_dispersed()` with `test_linear = FALSE`
          returns `FALSE` when it should", {
  1             |> is_seq_dispersed(from = 3, test_linear = f) |> expect_false()
  1:10          |> is_seq_dispersed(from = 3, test_linear = f) |> expect_false()
  c(3, 10)      |> is_seq_dispersed(from = 3, test_linear = f) |> expect_false()
  seq(4, 12, 2) |> is_seq_dispersed(from = 3, test_linear = f) |> expect_false()
})


# Special tests for `is_seq_dispersed()` ----------------------------------

test_that("`is_seq_dispersed()` passes its special tests, returning `TRUE`", {
  c(3:7) |> is_seq_dispersed(from = 5, test_linear = f) |> expect_true()
})


test_that("`is_seq_dispersed()` passes its special tests, returning `NA`", {
  c(NA, 3:7, NA)         |> is_seq_dispersed(from = 5, test_linear = f) |> expect_na()
  c(NA, NA, 3:7, NA, NA) |> is_seq_dispersed(from = 5, test_linear = f) |> expect_na()
  c(NA, 3:6, NA, NA)     |> is_seq_dispersed(from = 5, test_linear = f) |> expect_na()
})


test_that("`is_seq_dispersed()` passes its special tests, returning `FALSE`", {
  c(NA, 3:7)     |> is_seq_dispersed(from = 5, test_linear = f) |> expect_false()
  c(3:7, NA)     |> is_seq_dispersed(from = 5, test_linear = f) |> expect_false()
  c(3:7, NA, NA) |> is_seq_dispersed(from = 5, test_linear = f) |> expect_false()
  c(NA, NA, 3:7) |> is_seq_dispersed(from = 5, test_linear = f) |> expect_false()
})


# Missing values ----------------------------------------------------------

test_that("gaps are judged by the step the known values imply", {
  # The gaps used to be bridged at a step of one decimal unit, whatever the
  # known values said, so a sequence with a step of 2 was `FALSE`:
  c(1, NA, 5, 7)       |> is_seq_linear()     |> expect_na()
  c(1, NA, 5, 8)       |> is_seq_linear()     |> expect_false()
  c(2, NA, NA, 8, 10)  |> is_seq_linear()     |> expect_na()
  c(0.1, NA, 0.5, 0.7) |> is_seq_linear()     |> expect_na()
  c(9, NA, 5, 3)       |> is_seq_descending() |> expect_na()
  c(3, NA, 5, 6, 7)    |> is_seq_dispersed(from = 5) |> expect_na()
  c(3, NA, 5, 6, 8)    |> is_seq_dispersed(from = 5) |> expect_false()
  # Two known values fix the step and so the central value, even with no
  # mirrored pair known in full. These used to be `NA`:
  c(1, 2, NA, NA, NA)        |> is_seq_dispersed(from = 10)  |> expect_false()
  c(1, 2, 3, NA, NA, NA, NA) |> is_seq_dispersed(from = 100) |> expect_false()
  c(1, 2, NA, NA, NA)        |> is_seq_dispersed(from = 3)   |> expect_na()
  c(1, 2, NA, NA, NA)        |> is_seq_dispersed(from = 10, test_linear = FALSE) |> expect_na()
  # The string `"NA"` is missing, with no coercion warning:
  c("1", "NA", "3") |> is_seq_linear()              |> expect_silent()
  c("1", "NA", "3") |> is_seq_dispersed(from = "2") |> expect_na()
  # A `from` of `"NA"` or `"NaN"` is missing, too. It used to be an error:
  1:3 |> is_seq_dispersed(from = "NA")  |> expect_na()
  1:3 |> is_seq_dispersed(from = "NaN") |> expect_na()
  c(1, 2, 4) |> is_seq_dispersed(from = "NaN") |> expect_false()
  # Strings that are all missing leave no known value. These were errors:
  c("NaN", "NaN", "NaN") |> is_seq_linear()              |> expect_na()
  c("NA", "NA")          |> is_seq_linear()              |> expect_true()
  c("NaN", "NaN")        |> is_seq_ascending()           |> expect_na()
  c("NA", "NA", "NaN")   |> is_seq_dispersed(from = 2)   |> expect_na()
  c("NA", "NA", "NaN")   |> is_seq_dispersed(from = NA)  |> expect_na()

  # The examples from `vignette("devtools")`:
  c(1, 2, NA, 4)             |> is_seq_linear()    |> expect_na()
  c(1, 2, NA, NA, NA, 6)     |> is_seq_linear()    |> expect_na()
  c(1, 2, NA, 10)            |> is_seq_linear()    |> expect_false()
  c(1, 2, NA, NA, NA, 10)    |> is_seq_linear()    |> expect_false()
  c(NA, NA, 1, 2, NA, 4, NA) |> is_seq_linear()    |> expect_na()
  c(1, 2, NA, 1)             |> is_seq_ascending() |> expect_false()
})


# Non-numeric input -------------------------------------------------------

test_that("strings and factors are tested by their numeric values", {
  # A string vector passed `is_numeric_like()` and then failed in `diff()`; a
  # factor was tested by its integer codes, so `factor(c(1, 2, 4))` was linear.
  c("1", "2", "3")     |> is_seq_ascending() |> expect_true()
  c("1", NA, "3", "4") |> is_seq_linear()    |> expect_na()
  factor(c(1, 2, 4))   |> is_seq_linear()    |> expect_false()

  # A string `from` used to be assigned to `x` by mistake:
  c(4.9, 5.0, 5.1)       |> is_seq_dispersed(from = "5.0") |> expect_true()
  c("4.9", "5.0", "5.1") |> is_seq_dispersed(from = 5)     |> expect_true()
  c(4.9, 5.0, 5.1)       |> is_seq_dispersed(from = "abc") |> expect_false()
})


test_that("a vector of nothing but `NA` returns `NA`, whatever its type", {
  c(NA, NA, NA)                     |> is_seq_linear()     |> expect_na()
  rep(NA_character_, 3L)            |> is_seq_linear()     |> expect_na()
  c(NA, NA, NA)                     |> is_seq_ascending()  |> expect_na()
  c(NA, NA, NA) |> is_seq_dispersed(from = 5) |> expect_na()
})

# `NA` is only for a question that the missing values could decide either way.
# These were `NA` although no values could make a single one ascending, or one
# or two of them not linear:
test_that("`NA`s that cannot change the answer do not make it `NA`", {
  NA         |> is_seq_linear()              |> expect_true()
  c(NA, NA)  |> is_seq_linear()              |> expect_true()
  c(NA, 5)   |> is_seq_linear()              |> expect_true()
  c("5", NA) |> is_seq_linear()              |> expect_true()
  c(NA, "a") |> is_seq_linear()              |> expect_false()
  NA         |> is_seq_ascending()           |> expect_false()
  NA         |> is_seq_descending()          |> expect_false()
  NA         |> is_seq_dispersed(from = 5)   |> expect_false()
  c(NA, NA)  |> is_seq_ascending()           |> expect_na()
  c(NA, NA)  |> is_seq_descending()          |> expect_na()
})


test_that("the signs of the steps count, and known values can disprove", {
  # Absolute steps made zigzags linear:
  c(1, 2, 1)    |> is_seq_linear() |> expect_false()
  c(5, 4, 5, 4) |> is_seq_linear() |> expect_false()
  c(5, 4, 3)    |> is_seq_linear() |> expect_true()
  # These were `NA` although the known values already rule them out:
  c(2, NA, 1) |> is_seq_ascending(test_linear = FALSE) |> expect_false()
  c(1, NA, 2) |> is_seq_descending(test_linear = FALSE) |> expect_false()
  c(1, NA, 3, 4, 100) |>
    is_seq_dispersed(from = 3, test_linear = FALSE) |>
    expect_false()
  c(1, 2, NA, 4, 5) |> is_seq_dispersed(from = 99) |> expect_false()
  # ...and these remain open:
  c(1, NA, 2) |> is_seq_ascending(test_linear = FALSE) |> expect_na()
  c(1, 2, NA, 4, 5) |> is_seq_dispersed(from = 3) |> expect_na()
})


test_that("edge cases don't fail in `if ()`", {
  # Each of these used to be "missing value where TRUE/FALSE needed":
  1:3            |> is_seq_dispersed(from = NA_real_) |> expect_na()
  character(0)   |> is_seq_linear()    |> expect_true()
  c(1, Inf, Inf) |> is_seq_linear()    |> expect_false()
  c(1, 2, Inf)   |> is_seq_ascending() |> expect_false()
  c(Inf, Inf)    |> is_seq_ascending(test_linear = FALSE) |> expect_false()
  c(-Inf, 0, Inf) |> is_seq_dispersed(from = 0) |> expect_false()
})


test_that("an infinity only rules out what it has to", {
  c(1, 2, Inf) |> is_seq_ascending(test_linear = FALSE)  |> expect_true()
  c(Inf, 2, 1) |> is_seq_descending(test_linear = FALSE) |> expect_true()
  Inf          |> is_seq_linear()                        |> expect_true()
  c(1, Inf)    |> is_seq_linear()                        |> expect_true()
  c(1, Inf)    |> is_seq_ascending()                     |> expect_true()
  c(Inf, 1)    |> is_seq_ascending()                     |> expect_false()
  c(Inf, NA)   |> is_seq_descending()                    |> expect_na()
})


# No number comes after `Inf` in an ascending sequence, so a missing one there
# can't be filled; and an infinity among more than two values is never linear,
# even if it is the only known value:
test_that("a missing value beyond an infinity rules the sequence out", {
  c(1, Inf, NA)  |> is_seq_ascending(test_linear = FALSE)  |> expect_false()
  c(Inf, NA)     |> is_seq_ascending(test_linear = FALSE)  |> expect_false()
  c(NA, -Inf, 0) |> is_seq_ascending(test_linear = FALSE)  |> expect_false()
  c(1, -Inf, NA) |> is_seq_descending(test_linear = FALSE) |> expect_false()
  c(NA, Inf, 0)  |> is_seq_descending(test_linear = FALSE) |> expect_false()
  c(1, NA, Inf)  |> is_seq_ascending(test_linear = FALSE)  |> expect_na()
  c(Inf, NA, NA) |> is_seq_linear()                        |> expect_false()
  c(NA, Inf, NA) |> is_seq_dispersed(from = Inf)           |> expect_false()
})


# No number is centered with an infinity on any value, nor is an infinity the
# center of a dispersed sequence:
test_that("a known infinity rules out a dispersed sequence", {
  c(Inf, 1, NA)   |> is_seq_dispersed(from = 1, test_linear = FALSE)   |> expect_false()
  c(-Inf, NA, NA) |> is_seq_dispersed(from = 1, test_linear = FALSE)   |> expect_false()
  c(1, NA, NA)    |> is_seq_dispersed(from = Inf, test_linear = FALSE) |> expect_false()
  c(Inf, NA, NA)  |> is_seq_dispersed(from = NA, test_linear = FALSE)  |> expect_false()
  c(NA, 1, NA)    |> is_seq_dispersed(from = 1, test_linear = FALSE)   |> expect_na()
  c(1, Inf, 2)    |> is_seq_dispersed(from = Inf, test_linear = FALSE) |> expect_false()
})


test_that("a missing `from` is `NA` only if some center is possible", {
  1:4          |> is_seq_dispersed(from = NA) |> expect_false()
  c(1, 5, 2)   |> is_seq_dispersed(from = NA) |> expect_false()
  c(1, 5, 2)   |> is_seq_dispersed(from = NA, test_linear = FALSE) |> expect_false()
  c(1, 3, 5)   |> is_seq_dispersed(from = NA) |> expect_na()
  c(NA, 3, NA) |> is_seq_dispersed(from = NA) |> expect_na()
  c(1, NA, 6)  |> is_seq_dispersed(from = NA) |> expect_na()
})


test_that("a factor `from` counts by its label", {
  c(9, 10, 11) |> is_seq_dispersed(from = factor(10)) |> expect_true()
})


test_that("`test_linear` and `tolerance` are checked", {
  # A missing `tolerance` made every test `FALSE`, and a missing `test_linear`
  # failed in `if ()`:
  1:3 |> is_seq_linear(tolerance = NA)        |> expect_error("single non-negative number")
  1:3 |> is_seq_linear(tolerance = c(1, 2))   |> expect_error("single non-negative number")
  1:3 |> is_seq_ascending(tolerance = -1)     |> expect_error("single non-negative number")
  1:3 |> is_seq_ascending(test_linear = NA)   |> expect_error("`TRUE` or `FALSE`")
  1:3 |> is_seq_descending(test_linear = "a") |> expect_error("`TRUE` or `FALSE`")
})

test_that("no values could be dispersed around an infinite `from`", {
  c(NA, NA, NA) |> is_seq_dispersed(from = Inf)     |> expect_false()
  c(NA, NA, NA) |> is_seq_dispersed(from = -Inf)    |> expect_false()
  c(NA_real_, NA_real_, NA_real_) |> is_seq_dispersed(from = "Inf") |> expect_false()
  c(NA, 5, NA)  |> is_seq_dispersed(from = Inf)     |> expect_false()
  c(NA, NA, NA) |> is_seq_dispersed(from = 5)       |> expect_na()
})

test_that("anything but a numeric, character, or factor vector is an error", {
  mean                          |> is_seq_linear()          |> expect_error("It is a function")
  NULL                          |> is_seq_ascending()       |> expect_error("It is NULL")
  list(1, 2, 3)                 |> is_seq_linear()          |> expect_error("It is a list")
  c(TRUE, FALSE)                |> is_seq_descending()      |> expect_error("It is a logical vector")
  matrix(1:4, 2)                |> is_seq_linear()          |> expect_error("must be a numeric")
  tibble::tibble(a = c(NA, NA)) |> is_seq_linear()          |> expect_error("It is a tibble")
  1:3                           |> is_seq_dispersed(from = list(2)) |> expect_error("`from` must be")
  # A logical vector of `NA`s is missing values, and a string may be no number:
  c(NA, NA, NA)                 |> is_seq_linear()          |> expect_na()
  c("a", "b")                   |> is_seq_linear()          |> expect_false()
  factor(c("1", "2", "3"))      |> is_seq_linear()          |> expect_true()
})

test_that("`tolerance` admits the rounding error of arithmetic on the values", {
  # `dplyr::near()` compares with `<`, so a `tolerance` of 0 failed everything:
  1:3          |> is_seq_linear(tolerance = 0)                |> expect_true()
  1:3          |> is_seq_ascending(tolerance = 0)             |> expect_true()
  c(1, 2, 3)   |> is_seq_dispersed(from = 2, tolerance = 0)   |> expect_true()
  c(1, NA, 3)  |> is_seq_linear(tolerance = 0)                |> expect_na()
  c(1, NA, 4)  |> is_seq_dispersed(from = 2, tolerance = 0)   |> expect_false()
  c(0.1, 0.2, 0.3) |> is_seq_linear(tolerance = 0)            |> expect_true()
  # Steps between large values are off by more than the default tolerance:
  s <- seq_disperse("123456789.01", dispersion = 1:3)
  s |> is_seq_linear()                                        |> expect_true()
  s |> is_seq_dispersed(from = "123456789.01")                |> expect_true()
  c(123456789.01, 123456789.02, 123456789.04) |> is_seq_linear() |> expect_false()
})

test_that("a missing string is a missing number", {
  c(NA_character_, NA_character_) |> is_seq_linear() |> expect_true()
  NA_character_                   |> is_seq_linear() |> expect_true()
  factor(NA)                      |> is_seq_linear() |> expect_true()
  c("NA", "NA")                   |> is_seq_linear() |> expect_true()
  c("1", NA_character_)           |> is_seq_linear() |> expect_true()
  c(NA_character_, NA, NA)        |> is_seq_linear() |> expect_na()
})

test_that("the tolerance floor tells steps apart in the 15th significant digit", {
  c(9e14, 9e14 + 1, 9e14 + 3)                                    |> is_seq_linear()                  |> expect_false()
  c(9e14 - 1, 9e14, 9e14 + 2)                                    |> is_seq_dispersed(from = 9e14)    |> expect_false()
  c("996757367080.986", "996757367080.987", "996757367080.989") |> is_seq_linear()                  |> expect_false()
  c("996757367080.986", "996757367080.987", "996757367080.988") |> is_seq_linear()                  |> expect_true()
  c(990000000.000000, 990000000.000001, 990000000.000003)       |> is_seq_linear(tolerance = 0)     |> expect_false()
})

test_that("an empty logical vector is judged like `numeric()`", {
  logical() |> is_seq_linear()    |> expect_equal(is_seq_linear(numeric()))
  logical() |> is_seq_ascending() |> expect_equal(is_seq_ascending(numeric()))
})

test_that("integers whose differences pass the integer range are tested as doubles", {
  # `diff()` overflowed to `NA`, with a warning, and the result was `FALSE`:
  big <- c(-1500000000L, 1500000000L)
  big                             |> is_seq_ascending()            |> expect_true()
  rev(big)                        |> is_seq_descending()           |> expect_true()
  c(-2000000000L, 0L, 2000000000L) |> is_seq_linear()               |> expect_true()
  c(-2000000000L, 0L, 2000000000L) |> is_seq_dispersed(from = 0L)   |> expect_true()
})
