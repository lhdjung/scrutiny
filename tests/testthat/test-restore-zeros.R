round_to <- rnorm(50, 2, 1) |>
  censor(1, 4) |>
  round()

numbers <- rnorm(50, 100, 30) |>
  round(round_to) |>
  restore_zeros(width = 4)


test_that("The number of decimal places checks out", {
  numbers |>
    decimal_places() |>
    call_on(\(x) x == 4) |>
    all() |>
    expect_true()
})


test_that("The total number of characters checks out", {
  numbers |>
    stringr::str_length() |>
    call_on(\(x) x - 5) |>
    expect_equal(integer_places(numbers))
})

test_that("`width` is checked correctly", {
  1:5 |> restore_zeros(width = 1)   |> expect_no_error()
  1:5 |> restore_zeros(width = 1:5) |> expect_no_error()
  1:5 |> restore_zeros(width = 1:2) |> expect_error()
  1:5 |> restore_zeros(width = 1.2) |> expect_error()
})

test_that("`check_width` works correctly", {
  c(0.12, 0.123, 0.1234) |> restore_zeros(width = 2) |> expect_error()
  c(0.12, 0.123, 0.1234) |> restore_zeros(width = 2, check_width = "never") |> expect_no_error()
})


test_that("The `*_df()` variant produces correct results", {
  iris |> restore_zeros_df() |> expect_no_error()

  iris |> restore_zeros_df(contains("Sepal")) |> expect_no_error()

  iris |>
    restore_zeros_df(contains("Sepal")) |>
    purrr::map_chr(typeof) |>
    unname() |>
    expect_equal(c("character", "character", "double", "double", "integer"))

  iris |>
    dplyr::select(1:4) |>
    restore_zeros_df() |>
    purrr::map_lgl(is.character) |>
    all() |>
    expect_true()

  iris |>
    dplyr::select(5) |>
    restore_zeros_df() |>
    dplyr::pull(1) |>
    expect_s3_class("factor")
})


test_that("the `check_decimals` argument works correctly", {
  iris |>
    dplyr::mutate(Sepal.Length = trunc(Sepal.Length)) |>
    restore_zeros_df(check_decimals = TRUE) |>
    dplyr::pull(1) |>
    expect_type("double")

  iris |>
    dplyr::mutate(Sepal.Length = trunc(Sepal.Length)) |>
    restore_zeros_df(check_decimals = FALSE) |>
    dplyr::pull(1) |>
    expect_type("character") |>
    expect_no_warning()
})


# `is_numeric_like()` is `NA` for a column of nothing but missing values, which
# `where()` rejected with an error:
test_that("`restore_zeros_df()` handles all-`NA` columns", {
  data <- tibble::tibble(a = c("1.5", "2"), b = NA_character_)
  out <- data |> restore_zeros_df(width = 2)
  out$a |> expect_equal(c("1.50", "2.00"))
  out$b |> expect_equal(c(NA_character_, NA_character_))
  data |> restore_zeros_df(check_decimals = TRUE) |> expect_no_error()
})

# Comma columns used to fail the numeric-like check and were skipped without
# a word, and `sep_out` defaulted to a decimal point rather than to `sep_in`:
test_that("`restore_zeros_df()` reads and keeps `sep_in`", {
  tibble::tibble(x = c("1,5", "2,25")) |>
    restore_zeros_df(sep_in = ",") |>
    dplyr::pull(x) |>
    expect_equal(c("1,50", "2,25"))
  tibble::tibble(x = c("1,5", "2,25")) |>
    restore_zeros_df(sep_in = ",", sep_out = ".") |>
    dplyr::pull(x) |>
    expect_equal(c("1.50", "2.25"))
})

test_that("invalid arguments in `restore_zeros_df()` are caught", {
  iris |> restore_zeros_df(.check_decimals = TRUE) |> expect_error()
  iris |> restore_zeros_df(wooh = TRUE) |> expect_error()
})


test_that("missing values stay missing", {
  # `stringr::str_split_fixed()` gives `NA` an empty mantissa, which counts as
  # fewer decimal places than the target, so `sprintf()` used to format the
  # missing value into the string `"NA"`.
  c(1.5, NA, 2) |> restore_zeros(width = 3) |> expect_equal(c("1.500", NA, "2.000"))
  c("1.5", NA)  |> restore_zeros(width = 2) |> expect_equal(c("1.50", NA))
})


test_that("scientific notation, binary noise, and non-numbers are handled", {
  # `as.character(0.0001)` is `"1e-04"`, whose decimals used to be counted after
  # the point and whose zeros were appended to the exponent.
  c(0.0001, 0.5) |> restore_zeros() |> expect_equal(c("0.0001", "0.5000"))
  c(1.5e-5, 0.25) |> restore_zeros() |> expect_equal(c("0.000015", "0.250000"))
  0.0001 |> restore_zeros(width = 2) |> expect_error("more decimal places")
  # Zeros are appended literally, not formatted from the binary value:
  0.1 |> restore_zeros(width = 20) |> expect_equal(paste0("0.1", strrep("0", 19)))
  # A string that is not a number becomes `NA`, not the string `"NA"`:
  c("5%", "2.25") |>
    restore_zeros() |>
    expect_equal(c(NA, "2.25")) |>
    expect_warning("not numbers became `NA`")
  # The string `"NA"` is a missing value, not a non-number:
  c("5.3", "NA") |> restore_zeros() |> expect_silent()
  c("5.3", "NA") |> restore_zeros() |> expect_equal(c("5.3", NA))
  # `sep_in` is a literal string:
  c("1.5", "2.25") |> restore_zeros(sep_in = ".") |> expect_equal(c("1.50", "2.25"))
  c("1,5", "3") |> restore_zeros(width = 2, sep_in = ",") |> expect_equal(c("1,50", "3,00"))
})


test_that("unusual spellings of numbers are written out plainly", {
  c(".5", "+1.5", "-.25") |>
    restore_zeros() |>
    expect_equal(c("0.50", "1.50", "-0.25"))
  "0x1A" |> restore_zeros(width = 3) |> expect_equal("26.000")
  c("1.50E+2", "-1.5e-3", "1e5", "+00.5") |>
    restore_zeros() |>
    expect_equal(c("150.0000", "-0.0015", "100000.0000", "0.5000"))
  # Every digit given is kept, not printed from the double, which holds only
  # about 17 of them:
  "+0.12345678901234567890" |> restore_zeros(width = 22) |> expect_equal("0.1234567890123456789000")
  "1.2345678901234567890e1" |> restore_zeros(width = 18) |> expect_equal("12.345678901234567890")
  # An exponent too large to write out is `NA`. These overflowed the integer
  # range or the longest string R allows:
  c("1e-5", "0e99999999999", "1e-2147483647") |> restore_zeros()  |> expect_warning("too large to write out")
  c("1e-5", "0e99999999999", "1e-2147483647") |> restore_zeros()  |> suppressWarnings() |> expect_equal(c("0.00001", NA, NA))
  "1e-1000"                                   |> restore_zeros(width = 1000) |> nchar()  |> expect_equal(1002L)
})

test_that("a near-whole `width` is compared as the whole number it is shown as", {
  c(1.555, 2.25) |> restore_zeros(width = 3 - 1e-12) |> expect_equal(c("1.555", "2.250"))
})


# `dplyr::select()` re-adds the grouping columns and `across()` runs per group,
# so a grouped tibble used to fail with "Can't select columns past the end" or
# to pad nothing at all:
test_that("`restore_zeros_df()` works on grouped tibbles and skips dates", {
  grouped <- tibble::tibble(a = c(1.5, 2), b = c(1.25, 3)) |> dplyr::group_by(b)
  out <- grouped |> restore_zeros_df()
  out$a |> expect_equal(c("1.5", "2.0"))
  out$b |> expect_equal(c("1.25", "3.00"))
  out |> dplyr::is_grouped_df() |> expect_false()
  grouped |> restore_zeros_df(cols = a) |> dplyr::pull(a) |> expect_equal(c("1.5", "2.0"))
  dates <- tibble::tibble(d = as.Date("2026-10-22") + 0:1, x = c(1.5, 2))
  dates |> restore_zeros_df() |> dplyr::pull(d) |> expect_equal(dates$d)
})


test_that("`NaN` is a missing value, and `width` must be a number", {
  c(1.5, NaN, 2) |> restore_zeros() |> expect_silent()
  c(1.5, NaN, 2) |> restore_zeros() |> expect_equal(c("1.5", NA, "2.0"))
  # These used to fail in `if ()`:
  1.5 |> restore_zeros(width = NA)  |> expect_error("whole number")
  1.5 |> restore_zeros(width = Inf) |> expect_error("whole number")
  1.5 |> restore_zeros(width = "2") |> expect_error("whole number")
})


test_that("a number is read as `decimal_places()` reads it", {
  # `as.character(1e-300)` is "9.99999999999999e-301", which counted as 315
  # decimal places, so the documented default `width` was an error:
  1e-300       |> restore_zeros(width = 300) |> decimal_places() |> expect_equal(300L)
  c(1e-300, 1) |> restore_zeros()            |> decimal_places() |> expect_equal(c(300L, 300L))
  c(1e-300, 1) |> restore_zeros()            |> as.numeric()     |> expect_equal(c(1e-300, 1))
})


test_that("`restore_zeros_df()` does not warn about columns without decimals or single rows", {
  pigs1        |> restore_zeros_df() |> expect_no_warning()
  pigs1[1L, ]  |> restore_zeros_df() |> expect_no_warning()
  c(1, 2)      |> restore_zeros()    |> expect_warning(class = "scrutiny_warning_no_zeros")
})

test_that("a `width` too large to pad to is an error", {
  # `strrep()` returned `NA` for it, which was pasted into `"1.5NA"`:
  c(1.5, 2.25) |> restore_zeros(width = 1e10)   |> expect_error("from 0 to 100000")
  1.5          |> restore_zeros(width = 100000) |> nchar() |> expect_equal(100002L)
})


test_that("the flags of `restore_zeros_df()` must be flags", {
  iris |> restore_zeros_df(check_numeric_like = "no") |> expect_error("`check_numeric_like` must be `TRUE` or `FALSE`")
  iris |> restore_zeros_df(check_decimals = NA)       |> expect_error("`check_decimals` must be `TRUE` or `FALSE`")
})

test_that("a column named like an argument doesn't replace it", {
  # dplyr inlines a function written inside `across()`, which then finds the
  # column `width` before the argument:
  df <- tibble::tibble(length = c(1.25, 3, 4.5), width = c(2, 4, 1))
  df |> restore_zeros_df()          |> expect_equal(tibble::tibble(length = c("1.25", "3.00", "4.50"), width = c("2", "4", "1")))
  df |> restore_zeros_df(width = 3) |> _$width |> expect_equal(c("2.000", "4.000", "1.000"))
})


test_that("a hexadecimal number with a fraction keeps its value", {
  # It was written out to the decimal places of its spelling, which has none:
  c("0x1p-2", "0x.4", "0x1A") |> restore_zeros(width = 2) |> expect_equal(c("0.25", "0.25", "26.00"))
})


test_that("`NaN` in any spelling is missing, not a non-number", {
  c("nan", "1.5") |> restore_zeros(width = 2) |> expect_equal(c(NA, "1.50")) |> expect_no_warning()
  c("NaN", "1.5") |> restore_zeros(width = 2) |> expect_equal(c(NA, "1.50")) |> expect_no_warning()
})


test_that("`restore_zeros_df()` warns about a selected column that isn't numeric-like", {
  df <- tibble::tibble(a = c("x", "y"), b = c(1.5, 2))
  # Skipped by default, as in `decimal_places_df()`; converted under
  # `check_numeric_like = FALSE`:
  df |> restore_zeros_df(a)    |> expect_warning("1 column was excluded: `a`", fixed = TRUE)
  df |> restore_zeros_df(c(a, b)) |> expect_warning("1 column was excluded: `a`", fixed = TRUE)
  df |> restore_zeros_df(a, check_numeric_like = FALSE) |> expect_warning("had its non-numeric values replaced", fixed = TRUE)
  df |> restore_zeros_df(b) |> expect_no_warning()
  # Not under the default, where a column of labels is nothing to report:
  df   |> restore_zeros_df() |> expect_no_warning()
  iris |> restore_zeros_df() |> expect_no_warning()
  # But a column whose values were replaced always is:
  df |> restore_zeros_df(check_numeric_like = FALSE) |> expect_warning("had its non-numeric values replaced", fixed = TRUE)
  df |> restore_zeros_df(a) |> suppressWarnings() |> expect_equal(df)
})


test_that("a blank cell is missing, so its column is padded all the same", {
  c("1.2", "", " ") |> restore_zeros(width = 2) |> expect_silent() |> expect_equal(c("1.20", NA, NA))
  data <- read.csv(text = "study,m\nA,0.50\nB,\nC,1.2\n", colClasses = "character")
  data |> restore_zeros_df() |> dplyr::pull(m) |> expect_equal(c("0.50", NA, "1.20"))
})


test_that("the `*_df()` functions don't warn about a column with no values", {
  df <- tibble::tibble(a = c("1.5", "2.25"), e = NA, b = c("", "NA"))
  df |> restore_zeros_df(c(a, e, b), check_numeric_like = FALSE) |> expect_no_warning()
  df |> decimal_places_df(c(a, e, b), check_numeric_like = FALSE) |> expect_no_warning()
  df |> restore_zeros_df(c(e, b))                                 |> expect_no_warning()
  tibble::tibble(a = "1 (2)", e = NA) |> split_by_parens(check_delim = FALSE) |> expect_no_warning()
})
