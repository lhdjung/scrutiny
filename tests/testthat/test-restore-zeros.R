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
    expect_warning()
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
})
