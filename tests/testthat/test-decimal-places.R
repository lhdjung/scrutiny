# Long vectors with decimal numbers:
x1 <- iris$Petal.Length
x2 <- mtcars$qsec
x3 <- randu$y
x4 <- airquality$Wind
x5 <- attenu$accel

x6_digits <- rnorm(10000, 6, 3) |>
  censor(0, 13) |>
  round(0)

x6 <- rnorm(10000, 100, 15) |>
  round(x6_digits)

# fmt: skip
out_expected_x1 <- c(
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,
  1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1,
  1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
  1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0,
  1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1
)

# fmt: skip
out_expected_x2 <- c(
  2, 2, 2, 2, 2, 2, 2, 0, 1, 1, 1, 1, 1, 0, 2,
  2, 2, 2, 2, 1, 2, 2, 1, 2, 2, 1, 1, 1, 1, 1,
  1, 1
)

# fmt: skip
out_expected_x3 <- c(
  6, 6, 6, 6, 6, 5, 5, 6, 6, 6, 6, 5, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5,
  6, 6, 6, 6, 6, 6, 6, 5, 6, 3, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 5, 6,
  6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 5,
  6, 6, 6, 6, 6, 6, 4, 6, 6, 6, 6, 6, 6, 5, 6,
  5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 5, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 5, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 5, 6, 6, 6,
  6, 4, 5, 6, 6, 6, 6, 6, 6, 4, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5,
  6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 5,
  6, 6, 6, 5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 5, 6,
  6, 6, 6, 6, 6, 6, 5, 5, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5,
  6, 6, 6, 6, 6, 5, 6, 6, 5, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6
)

# fmt: skip
out_expected_x4 <- c(
  1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1,
  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,
  1, 0, 1
)

# fmt: skip
out_expected_x5 <- c(
  3, 3, 3, 3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 3, 3, 3, 3,
  3, 3, 3, 3, 3, 2, 3, 3, 3, 3, 2, 3, 3, 3, 2,
  3, 2, 2, 2, 2, 2, 3, 2, 3, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2,
  2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 3, 2, 1, 2, 2, 3, 3, 3, 3, 2, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 3, 2, 3, 3,
  3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
  3, 3, 3, 3, 2, 3, 2, 2, 3, 2, 2, 3, 2, 2, 3,
  3, 3
)


test_that("`decimal_places()` counts accurately", {
  x1 |> decimal_places() |> expect_equal(out_expected_x1)
  x2 |> decimal_places() |> expect_equal(out_expected_x2)
  x3 |> decimal_places() |> expect_equal(out_expected_x3)
  x4 |> decimal_places() |> expect_equal(out_expected_x4)
  x5 |> decimal_places() |> expect_equal(out_expected_x5)

  # With `NA` element
  c(2.4, 63, NA, 5.34) |> decimal_places() |> expect_equal(c(1L, 0L, NA, 2L))
})


out_scalar_x1 <- x1 |> purrr::map_int(decimal_places_scalar)
out_scalar_x2 <- x2 |> purrr::map_int(decimal_places_scalar)
out_scalar_x3 <- x3 |> purrr::map_int(decimal_places_scalar)
out_scalar_x4 <- x4 |> purrr::map_int(decimal_places_scalar)
out_scalar_x5 <- x5 |> purrr::map_int(decimal_places_scalar)
out_scalar_x6 <- x6 |> purrr::map_int(decimal_places_scalar)


test_that("Both functions return the same count for each individual number", {
  x1 |> decimal_places() |> expect_equal(out_scalar_x1)
  x2 |> decimal_places() |> expect_equal(out_scalar_x2)
  x3 |> decimal_places() |> expect_equal(out_scalar_x3)
  x4 |> decimal_places() |> expect_equal(out_scalar_x4)
  x5 |> decimal_places() |> expect_equal(out_scalar_x5)
  x6 |> decimal_places() |> expect_equal(out_scalar_x6)
})


test_that("`decimal_places_scalar()` conditions work as expected", {
  25  |> decimal_places_scalar() |> expect_identical(0L)
  2.7 |> decimal_places_scalar() |> expect_identical(1L)
  NA  |> decimal_places_scalar() |> expect_identical(NA_integer_)
  NA  |> decimal_places_scalar() |> is.na() |> expect_true()
})


iris <- iris |>
  tibble::as_tibble() |>
  dplyr::mutate(dplyr::across(everything(), as.character)) |>
  dplyr::slice(1:5)

iris_counted <- decimal_places_df(iris[1:4])

test_that("", {
  iris_counted[[1]] |> expect_equal(as.integer(c(1, 1, 1, 1, 0)))
  iris_counted[[2]] |> expect_equal(as.integer(c(1, 0, 1, 1, 1)))
  iris_counted[[3]] |> expect_equal(as.integer(c(1, 1, 1, 1, 1)))
  iris_counted[[4]] |> expect_equal(as.integer(c(1, 1, 1, 1, 1)))
})

test_that("`decimal_places_df()` throws a warning if and only if it should", {
  iris |> decimal_places_df() |> expect_warning()
  iris |> dplyr::select(1:4) |> decimal_places_df() |> expect_no_warning()
  iris |> decimal_places_df(check_numeric_like = FALSE) |> expect_warning()
  iris |> dplyr::select(1:4) |> decimal_places_df(check_numeric_like = FALSE) |> expect_no_warning()
})


# Scientific notation -----------------------------------------------------

# R writes small and large numbers in scientific notation by itself:
# `as.character(0.0001)` is `"1e-04"`. The digits after the decimal point are
# then not the decimal places of the number, so the exponent has to be applied.

test_that("`decimal_places()` accounts for the exponent", {
  decimal_places(c("1e-5", "1.5e3", "1e+05", "2.75e-2")) |>
    expect_equal(c(5L, 0L, 0L, 4L))
  decimal_places(c(1e-5, 1e-4, 1e5)) |> expect_equal(c(5L, 4L, 0L))
})


test_that("`decimal_places_scalar()` accounts for the exponent", {
  vapply(
    list("1e-5", "1.5e3", "1e+05", "2.75e-2", 1e-5, 1e-4, 1e5),
    decimal_places_scalar,
    integer(1L)
  ) |>
    expect_equal(c(5L, 0L, 0L, 4L, 5L, 4L, 0L))
})


test_that("the two functions agree on ordinary numbers", {
  values <- c("1.0", "1", "-2.750", "3.", "7.3900", "0.05")
  decimal_places(values) |>
    expect_equal(vapply(values, decimal_places_scalar, integer(1L), USE.NAMES = FALSE))
})


# The two functions are separate implementations of one rule -- trim, split off
# a trailing exponent, count the run of digits right after the first separator,
# subtract the exponent, floor at zero -- because neither can be expressed in
# terms of the other without a large cost: the scalar one runs once per row via
# `check_newly_numeric()`, and the vectorized one runs over whole columns. What
# keeps them from drifting apart is this corpus, not their similarity. It has to
# include padded and malformed strings, since clean numbers are exactly where
# they never disagreed.

test_that("the two functions agree over a generated corpus", {
  signs <- c("", "-", "+")
  ints <- c("0", "1", "42", "")
  mantissas <- c("", ".0", ".5", ".00", ".750", ".0001")
  exponents <- c("", "e3", "e-3", "E+05", "e-10", "e0")
  pads <- c("", " ", "  ", "\t")

  values <- as.vector(outer(
    outer(paste0(rep(signs, each = length(ints)), ints), mantissas, paste0),
    exponents,
    paste0
  ))
  values <- as.vector(outer(paste0(pads, values), pads, paste0))

  # Malformed input has to agree too, and it is where they used to differ:
  values <- c(
    values,
    "5.30%", "1.5abc", "1.2.3", "3.7,", "1.50a", "1.5e", "e5",
    "Inf", "NaN", "NA", "", " ", NA_character_
  )

  from_scalar <- vapply(
    values, decimal_places_scalar, integer(1L), USE.NAMES = FALSE
  )
  decimal_places(values) |> expect_equal(from_scalar)

  # Not a vacuous comparison -- the corpus has to exercise the whole range:
  expect_gt(length(unique(from_scalar[!is.na(from_scalar)])), 5L)
  expect_true(anyNA(from_scalar))
})


test_that("only the digit run after the separator counts", {
  # Not every character after it: `str_length()` on the mantissa used to make
  # `"5.30%"` three decimal places and `"1.2.3"` three.
  decimal_places(c("5.30%", "1.5abc", "1.2.3", "3.7,")) |>
    expect_equal(c(2L, 1L, 1L, 1L))
})


test_that("both functions trim whitespace before reading the exponent", {
  # The exponent is matched at the end of the string, so a trailing space used
  # to hide it from `decimal_places_scalar()`.
  decimal_places_scalar("1.5e3 ") |> expect_equal(0L)
  decimal_places_scalar("  2.75e-2  ") |> expect_equal(4L)
  decimal_places("1.5e3 ") |> expect_equal(0L)
})


test_that("sequence functions step on the right decimal level below 0.001", {
  # `decimal_places_scalar()` sets the step size in all of these. When it read
  # `1e-04` as having no decimal places, they stepped by whole numbers instead.
  seq_endpoint(from = 0.0001, to = 0.0005) |>
    expect_equal(c("0.0001", "0.0002", "0.0003", "0.0004", "0.0005"))
  seq_disperse(from = 7.22, by = 1e-4, dispersion = 1:2) |>
    expect_equal(c("7.2198", "7.2199", "7.2200", "7.2201", "7.2202"))
  seq_distance(from = 0.0001, length_out = 3L) |>
    expect_equal(c("0.0001", "0.0002", "0.0003"))
})


test_that("non-finite values have no decimal places", {
  # `NaN` is a missing value everywhere else in the package -- `is.na(NaN)` is
  # `TRUE` -- so counting it as zero decimal places while counting a literal
  # `NA` as `NA` was inconsistent. An infinity has no decimal places in any
  # meaningful sense either. Both used to come out as `0`, because the string
  # they are coerced to has no decimal point.
  decimal_places(c(Inf, -Inf, NaN, NA)) |>
    expect_equal(rep(NA_integer_, 4L))
  decimal_places(c("Inf", "-Inf", "NaN", NA_character_, "  NaN  ")) |>
    expect_equal(rep(NA_integer_, 5L))

  # The scalar version agrees, as it must:
  for (value in list(Inf, -Inf, NaN, NA, "Inf", "-Inf", "NaN")) {
    decimal_places_scalar(value) |> expect_equal(NA_integer_)
  }

  # Ordinary values are untouched, including the documented odd ones:
  input <- c("2.851", "5.30%", "1e-5", "3.70", "  6.0   ")
  expected <- c(3L, 2L, 5L, 2L, 1L)
  decimal_places(input) |> expect_equal(expected)
  vapply(input, decimal_places_scalar, integer(1L), USE.NAMES = FALSE) |>
    expect_equal(expected)
})
