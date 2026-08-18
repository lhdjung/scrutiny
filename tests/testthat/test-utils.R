test_that("The `globalVariables()` call returns these variables as strings", {
  utils::globalVariables(c(
    ".", "where", "desc", "all_of", "contains", "everything", "x", "items",
    "frac"
  )) |> expect_type("character")
})


test_that("`wrong_spec_string()` returns a string", {
  4 |> wrong_spec_string() |> expect_type("character")
})


sd_rec_scalar <- reconstruct_sd_scalar("mean_n", "0.3", 30, 12, 15)

test_that("`reconstruct_sd_scalar()` returns correct values", {
  sd_rec_scalar |> expect_type("double")
  sd_rec_scalar |> expect_equal(0.4660916)
})


sd_rec <- reconstruct_sd("mean_n", "0.3", 30, 12, 15)

test_that("`reconstruct_sd()` returns correct values", {
  sd_rec |> expect_type("double")
  sd_rec |> expect_equal(0.4660916)
})


test_that("`integer_places()` returns correct values", {
  integer_places(1.2)     |> expect_equal(1)
  integer_places(11.2)    |> expect_equal(2)
  integer_places(111.2)   |> expect_equal(3)
  integer_places(1111.2)  |> expect_equal(4)
  integer_places(11111.2) |> expect_equal(5)

  integer_places(1.22222) |> expect_equal(1)
  integer_places(11.2222) |> expect_equal(2)
  integer_places(111.222) |> expect_equal(3)
  integer_places(1111.22) |> expect_equal(4)
  integer_places(11111.2) |> expect_equal(5)
})


test_that("`an_a()` returns correct values", {
  an_a("start") |> expect_equal("a")
  an_a("end")   |> expect_equal("an")
})


test_that("`an_a_type()` returns correct values", {
  an_a_type("bla") |> expect_equal("a string")
  an_a_type(4)     |> expect_equal("a double (numeric value)")
})


test_that("`is_whole_number()` returns correct values", {
  is_whole_number(1)   |> expect_true()
  is_whole_number(985) |> expect_true()
  is_whole_number(37)  |> expect_true()

  is_whole_number(0.2)    |> expect_false()
  is_whole_number(25.05)  |> expect_false()
  is_whole_number(75.489) |> expect_false()
})

# fmt: skip
parcel_letters_expected <- c(
  "a", "c", "e", "g", "i", "k", "m", "o",
  "q", "s", "u", "w", "y"
)

test_that("`parcel_nth_elements()` returns correct values", {
  parcel_nth_elements(letters, 2) |> expect_equal(parcel_letters_expected)
  parcel_nth_elements(1:10, 2) |> expect_equal(c(1, 3, 5, 7, 9))
  parcel_nth_elements(1:10, 5) |> expect_equal(c(1, 6))
})


df_test <- tibble::tibble(
  a = c(1, 2, 3, 1, 2, 3),
)


df_test_2 <- tibble::tibble(
  a = 1,
  b = 2,
  c = 3
) |>
  reverse_column_order()

df_expected_2 <- tibble::tibble(
  c = 3,
  b = 2,
  a = 1
)

test_that("`reverse_column_order()` returns correct values", {
  df_test_2 |> expect_equal(df_expected_2)
})


vec_test_1 <- 1:50 |>
  as.double() |>
  censor(25, 40)

vec_expected_1 <- rep(25, 25) |>
  append(26:39) |>
  append(rep(40, 11))

test_that("`censor()` returns correct values", {
  vec_test_1 |> expect_equal(vec_expected_1)
})


vec_test_2 <- 1:7
vec_test_2 <- add_class(vec_test_2, "silly test")

df_test_3 <- add_class(df_test, "dummy class")
df_test_4 <- add_class(df_test, c("dummy class 1", "dummy class 2"))

test_that("`add_class()` really does add one or more classes", {
  vec_test_2 |> expect_s3_class("silly test")
  df_test_3  |> expect_s3_class("dummy class")
  df_test_4  |> expect_s3_class("dummy class 1")
  df_test_4  |> expect_s3_class("dummy class 2")
})


# Vectors with lengths 5, 3, 2, and 3 (they are all > 1):
numbers <- 1:5
nephews <- c("Huey", "Dewey", "Louie")
norberts <- c("Lammert", "Röttgen")
nikes <- c("Air Max", "Zoom Freak", "Phantom")

n_list <- list(numbers, nephews, norberts, nikes)

test_that("`check_lengths_congruent()` throws an error when it should", {
  list(numbers, nephews, norberts, nikes) |> check_lengths_congruent() |> expect_error()
  list(numbers, nephews, norberts)        |> check_lengths_congruent() |> expect_error()
  list(numbers, nephews, nikes)           |> check_lengths_congruent() |> expect_error()
  list(numbers, norberts, nikes)          |> check_lengths_congruent() |> expect_error()
  list(nephews, norberts, nikes)          |> check_lengths_congruent() |> expect_error()
})

test_that("`check_lengths_congruent()` throws a warning when it should", {
  list(nephews, nikes)     |> check_lengths_congruent(warn = TRUE) |> expect_warning()
  list(numbers, numbers)   |> check_lengths_congruent(warn = TRUE) |> expect_warning()
  list(nephews, nephews)   |> check_lengths_congruent(warn = TRUE) |> expect_warning()
  list(norberts, norberts) |> check_lengths_congruent(warn = TRUE) |> expect_warning()
  list(nikes, nikes)       |> check_lengths_congruent(warn = TRUE) |> expect_warning()
})


test_that("`check_lengths_congruent()` remains silent when it should", {
  list(nephews, nikes)     |> check_lengths_congruent(warn = FALSE) |> expect_silent()
  list(numbers, numbers)   |> check_lengths_congruent(warn = FALSE) |> expect_silent()
  list(nephews, nephews)   |> check_lengths_congruent(warn = FALSE) |> expect_silent()
  list(norberts, norberts) |> check_lengths_congruent(warn = FALSE) |> expect_silent()
  list(nikes, nikes)       |> check_lengths_congruent(warn = FALSE) |> expect_silent()
})


# Not aligning pipes here because the lengths are too different
test_that("`check_lengths_congruent()` remains silent when it should", {
  list(nephews, nikes) |> check_lengths_congruent(warn = FALSE) |> expect_silent()
  list("a", "b", c("c", "d", "e")) |> check_lengths_congruent() |> expect_silent()
  list("a", "b") |> check_lengths_congruent() |> expect_silent()
  list(1, 2, 3, 4, 5) |> check_lengths_congruent() |> expect_silent()
  list("a", "b", "c", "d", "e") |> check_lengths_congruent() |> expect_silent()
  list(1, c(1, 2), 3, 4, 5) |> check_lengths_congruent() |> expect_silent()
  list("a", c("a", "b"), "c", "d", "e") |> check_lengths_congruent() |> expect_silent()
})


test_that("`check_length()` throws an error when it should", {
  check_length(numbers, 1) |> expect_error()
  check_length(nephews, 1) |> expect_error()
})

test_that("`check_length()` remains silent when it should", {
  check_length(numbers, 5) |> expect_silent()
  check_length(nephews, 3) |> expect_silent()
})


test_that("`check_type()` remains silent when the type is correct", {
  numbers  |> check_type("integer")   |> expect_silent()
  nephews  |> check_type("character") |> expect_silent()
  norberts |> check_type("character") |> expect_silent()
  nikes    |> check_type("character") |> expect_silent()
})


test_that("`check_type()` remains silent when the correct type is
          contained in a string vector of length > 1, along with
          wrong types", {
  numbers  |> check_type(c("integer",   "THESE")) |> expect_silent()
  nephews  |> check_type(c("character", "TYPES")) |> expect_silent()
  norberts |> check_type(c("character", "ARE"  )) |> expect_silent()
  nikes    |> check_type(c("character", "WRONG")) |> expect_silent()
})


test_that("`check_type()` throws an error if the type is wrong", {
  numbers  |> check_type("character") |> expect_error()
  nephews  |> check_type("double")    |> expect_error()
  norberts |> check_type("integer")   |> expect_error()
  nikes    |> check_type("logical")   |> expect_error()
})


# `check_lengths_congruent()` ---------------------------------------------

test_that("`check_lengths_congruent()` accepts arguments of equal length", {
  # Two arguments of the same length are congruent -- that is the whole point of
  # the check -- so they warn about being paired but must not error. Up to
  # scrutiny 1.0.0 they did error whenever a length-1 argument sat between them
  # in the list, because the deduplication of lengths was indexed by the lengths
  # of *all* arguments rather than of those longer than 1, and so silently did
  # nothing. `reround_to_fraction(c(0.4, 0.6), denominator = 2, digits = c(1,
  # 2))` hit exactly that.
  a2 <- 1:2
  b2 <- 3:4
  s1 <- 1

  
  list(a2, b2, s1) |> check_lengths_congruent() |> suppressWarnings() |> expect_no_error()
  list(a2, s1, b2, s1, s1) |> check_lengths_congruent() |> suppressWarnings() |> expect_no_error()
  
  list(a2, s1, s1) |> check_lengths_congruent() |> expect_no_condition()
  list(s1, s1, s1) |> check_lengths_congruent() |> expect_no_condition()
  
  # The pairing warning still fires for the congruent case:
  list(a2, b2) |> check_lengths_congruent() |> expect_warning()
})

test_that("`check_lengths_congruent()` rejects genuinely unequal lengths", {
  a2 <- 1:2
  b2 <- 3:4
  c3 <- 1:3

  list(a2, c3) |> check_lengths_congruent() |> expect_error()
  list(a2, 1, c3, 1) |> check_lengths_congruent() |> expect_error()
  list(a2, b2, c3) |> check_lengths_congruent() |> expect_error(regexp = "c3")
})


# `check_newly_numeric()` decides whether `x` can be written with `digits`
# decimal places. It used to answer by counting the decimal places in the string
# representation of `x`, which cost three regular expressions per key value per
# row and made it the most expensive part of a mapper call (#92). The fast path
# added there is one-sided: `round()` settles the values that pass, and anything
# it leaves undecided falls through to the old string comparison, so the two
# must agree on every verdict.

passes_check_newly_numeric <- function(x, digits) {
  tryCatch(
    {
      check_newly_numeric(x, digits)
      TRUE
    },
    condition = function(cnd) FALSE
  )
}


test_that("`check_newly_numeric()` accepts a value that fits `digits`", {
  expect_silent(check_newly_numeric(5.19, 2))
  expect_silent(check_newly_numeric(5.19, 5))
  expect_silent(check_newly_numeric(5, 0))
  expect_silent(check_newly_numeric(0, 0))
  expect_silent(check_newly_numeric(-5.19, 2))

  # These are not the doubles for `0.3` and `0.8`, so the fast path cannot
  # settle them; the string comparison behind it can, and does:
  expect_silent(check_newly_numeric(0.1 + 0.2, 1))
  expect_silent(check_newly_numeric(0.1 + 0.7, 1))
})


test_that("`check_newly_numeric()` rejects a value with more decimal places", {
  expect_error(check_newly_numeric(5.195, 2))
  expect_error(check_newly_numeric(-5.195, 2))
  expect_error(check_newly_numeric(2.675, 2))

  # A tiny value is not a whole number scaled up, however close to zero it is:
  expect_error(check_newly_numeric(1e-20, 2))

  # A negative `digits` is not a way to demand whole hundreds:
  expect_error(check_newly_numeric(500, -2))
})


test_that("`check_newly_numeric()` agrees with counting decimal places", {
  set.seed(1010)

  x <- c(
    0, 1, -1, 5.19, -5.19, 0.1 + 0.2, 0.1 + 0.7, 2.675, 1e-20, 1e-16, 1 / 3,
    pi, 123456789012345.5, 1e6 + 0.5, 1e-4, 1e-5, 1e5, 8.7,
    runif(30, -1e5, 1e5) |> round(3),
    runif(30, -1, 1) |> round(7),
    runif(15, -10, 10)
  )

  for (digits in 0:5) {
    fast <- vapply(x, passes_check_newly_numeric, logical(1L), digits = digits)
    counted <- digits >= vapply(x, decimal_places_scalar, integer(1L))
    expect_equal(fast, counted)
  }
})


# `is_decidable_n_items()` states the one condition under which a value set can
# be decided at all, and it states it twice: with `&&` for the single values the
# `*_scalar()` functions pass it once per row, and with `&` for the columns
# `grim_probability()` passes it once per call. The two must not drift apart.

test_that("`is_decidable_n_items()` agrees between its two paths", {
  grid <- expand.grid(
    n = c(28, 1, 2, 0, -5, 20.5, 28.0000000001, NA, NaN, Inf, -Inf),
    items = c(1, 2, 3, 0, 1.5, -1, NA, Inf),
    min_n = c(1, 2)
  )

  for (min_n in c(1, 2)) {
    rows <- grid[grid$min_n == min_n, ]

    scalar <- rows |> 
      nrow() |> 
      seq_len() |> 
      vapply(
        function(i) is_decidable_n_items(rows$n[i], rows$items[i], min_n),
        logical(1L)
      )

    # Vectors take the other branch, whole columns at a time:
    vectorized <- is_decidable_n_items(rows$n, rows$items, min_n)
    expect_identical(scalar, vectorized)

    # Neither is ever `NA`, whatever went in:
    expect_false(anyNA(vectorized))
  }
})


test_that("`is_decidable_n_items()` recycles like the vector path", {
  c(28, 20.5) |> is_decidable_n_items(1)         |> expect_identical(c(TRUE, FALSE))
  28          |> is_decidable_n_items(c(1, 1.5)) |> expect_identical(c(TRUE, FALSE))
  numeric(0)  |> is_decidable_n_items(1)         |> expect_identical(logical(0))
})
