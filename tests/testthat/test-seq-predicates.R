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
  c(NA_character_, NA_character_)   |> is_seq_linear()     |> expect_na()
  c(NA, NA, NA)                     |> is_seq_ascending()  |> expect_na()
  c(NA, NA, NA) |> is_seq_dispersed(from = 5) |> expect_na()
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
