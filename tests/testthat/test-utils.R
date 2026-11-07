test_that("The `globalVariables()` call returns these variables as strings", {
  c(
    ".", "where", "desc", "all_of", "contains", "everything", "x", "items",
    "frac"
  ) |>
    utils::globalVariables() |>
    expect_type("character")
})


test_that("`wrong_spec_string()` returns a string", {
  4 |> wrong_spec_string() |> expect_type("character")
})


test_that("`integer_places()` returns correct values", {
  1.2     |> integer_places() |> expect_equal(1)
  11.2    |> integer_places() |> expect_equal(2)
  111.2   |> integer_places() |> expect_equal(3)
  1111.2  |> integer_places() |> expect_equal(4)
  11111.2 |> integer_places() |> expect_equal(5)

  1.22222 |> integer_places() |> expect_equal(1)
  11.2222 |> integer_places() |> expect_equal(2)
  111.222 |> integer_places() |> expect_equal(3)
  1111.22 |> integer_places() |> expect_equal(4)
  11111.2 |> integer_places() |> expect_equal(5)
})


test_that("`an_a()` returns correct values", {
  "start" |> an_a() |> expect_equal("a")
  "end"   |> an_a() |> expect_equal("an")
})


test_that("`an_a_type()` returns correct values", {
  "bla" |> an_a_type() |> expect_equal("a string")
  4     |> an_a_type() |> expect_equal("a double (numeric value)")
})


test_that("`is_whole_number()` returns correct values", {
  1   |> is_whole_number() |> expect_true()
  985 |> is_whole_number() |> expect_true()
  37  |> is_whole_number() |> expect_true()

  0.2    |> is_whole_number() |> expect_false()
  25.05  |> is_whole_number() |> expect_false()
  75.489 |> is_whole_number() |> expect_false()
})


df_test <- tibble::tibble(
  a = c(1, 2, 3, 1, 2, 3),
)


vec_test_1 <- 1:50 |>
  as.double() |>
  censor(25, 40)

vec_expected_1 <- rep(25, 25) |>
  append(26:39) |>
  append(rep(40, 11))

test_that("`censor()` returns correct values", {
  vec_test_1 |> expect_equal(vec_expected_1)
})


vec_test_2 <- 1:7 |> add_class("silly test")

df_test_3 <- df_test |> add_class("dummy class")
df_test_4 <- df_test |> add_class(c("dummy class 1", "dummy class 2"))

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

test_that("`check_length()` throws an error when it should", {
  numbers |> check_length(1) |> expect_error()
  nephews |> check_length(1) |> expect_error()
})

test_that("`check_length()` remains silent when it should", {
  numbers |> check_length(5) |> expect_silent()
  nephews |> check_length(3) |> expect_silent()
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
  5.19  |> check_newly_numeric(2) |> expect_silent()
  5.19  |> check_newly_numeric(5) |> expect_silent()
  5     |> check_newly_numeric(0) |> expect_silent()
  0     |> check_newly_numeric(0) |> expect_silent()
  -5.19 |> check_newly_numeric(2) |> expect_silent()

  # These are not the doubles for `0.3` and `0.8`, so the fast path cannot
  # settle them; the string comparison behind it can, and does:
  (0.1 + 0.2) |> check_newly_numeric(1) |> expect_silent()
  (0.1 + 0.7) |> check_newly_numeric(1) |> expect_silent()
})


test_that("`check_newly_numeric()` rejects a value with more decimal places", {
  5.195  |> check_newly_numeric(2) |> expect_error()
  -5.195 |> check_newly_numeric(2) |> expect_error()
  2.675  |> check_newly_numeric(2) |> expect_error()

  # A tiny value is not a whole number scaled up, however close to zero it is:
  1e-20 |> check_newly_numeric(2) |> expect_error()

  # A negative `digits` is not a way to demand whole hundreds:
  500 |> check_newly_numeric(-2) |> expect_error()

  # The limit, `x * 10^(digits + 1) < 2^52`, counts the integer digits, too:
  33.33333333333333 |> check_newly_numeric(14) |> expect_error("significant digits")
  33.33333333333333 |> check_newly_numeric(13) |> expect_silent()
  123456789012345   |> check_newly_numeric(0)  |> expect_silent()
  1234567890123456  |> check_newly_numeric(0)  |> expect_error("significant digits")
  123456789012345.5 |> check_newly_numeric(1)  |> expect_error("significant digits")
  # 15 significant digits are too many if they start above 4.5:
  450359962737049   |> check_newly_numeric(0)  |> expect_silent()
  450359962737050   |> check_newly_numeric(0)  |> expect_error("significant digits")
  999999999999999   |> check_newly_numeric(0)  |> expect_error("significant digits")
  99999.9999999999  |> check_newly_numeric(10) |> expect_error("significant digits")
  1e15              |> check_newly_numeric(0)  |> expect_error("has 16")
  1234567.89        |> check_newly_numeric(9)  |> expect_error("has 16")
  # Leading zeros are not significant:
  0.5               |> check_newly_numeric(15) |> expect_error("has 15")
})


test_that("`check_newly_numeric()` agrees with counting decimal places", {
  set.seed(1010)

  x <- c(
    0, 1, -1, 5.19, -5.19, 0.1 + 0.2, 0.1 + 0.7, 2.675, 1e-20, 1e-16, 1 / 3,
    pi, 1234567890.5, 1e6 + 0.5, 1e-4, 1e-5, 1e5, 8.7,
    runif(30, -1e5, 1e5) |> round(3),
    runif(30, -1, 1) |> round(7),
    runif(15, -10, 10)
  )

  for (digits in 0:5) {
    fast <- vapply(x, passes_check_newly_numeric, logical(1L), digits = digits)
    counted <- digits >= vapply(x, decimal_places_scalar, integer(1L))
    fast |> expect_equal(counted)
  }
})


# `is_decidable_n_items()` states the one condition under which a value set can
# be decided at all, and it states it twice: with `&&` for the single values the
# `*_scalar()` functions pass it once per row, and with `&` for the columns
# `grim_exp_incon()` passes it once per call. The two must not drift apart.

test_that("`is_decidable_n_items()` agrees between its two paths", {
  grid <- expand.grid(
    n = c(28, 1, 2, 0, -5, 20.5, 28.0000000001, NA, NaN, Inf, -Inf),
    items = c(1, 2, 3, 0, 1.5, -1, NA, Inf),
    min_n = c(1, 2)
  )

  for (min_n in c(1, 2)) {
    rows <- grid[grid$min_n == min_n, ]

    scalar <- rows |> nrow() |> seq_len() |> vapply(
      function(i) is_decidable_n_items(rows$n[i], rows$items[i], min_n),
      logical(1L)
    )

    # Vectors take the other branch, whole columns at a time:
    vectorized <- is_decidable_n_items(rows$n, rows$items, min_n)
    scalar |> expect_identical(vectorized)

    # Neither is ever `NA`, whatever went in:
    vectorized |> anyNA() |> expect_false()
  }
})


test_that("`is_decidable_n_items()` recycles like the vector path", {
  c(28, 20.5) |> is_decidable_n_items(1)         |> expect_identical(c(TRUE, FALSE))
  28          |> is_decidable_n_items(c(1, 1.5)) |> expect_identical(c(TRUE, FALSE))
  numeric(0)  |> is_decidable_n_items(1)         |> expect_identical(logical(0))
})


# A missing `data_type` could be integer or continuous, so `test_data_type()`
# gives the integer result only where the continuous one is the same. This
# restates that rule over every edge case at once, for each test and premise,
# rather than checking that the rule is right; the tests' own files do that.
# `grim_total()` decides continuous data in a vectorized path of its own.

test_that("a missing `data_type` gives a result only where both data types agree", {
  # Element-wise: `NA` is no result, so two `NA`s agree:
  agree <- function(a, b) (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  expect_na_rule <- function(test) {
    integer <- test("integer")
    continuous <- test("continuous")
    expected <- integer
    expected[!agree(integer, continuous)] <- NA
    NA |> test() |> expect_identical(expected)
  }
  n <- c(NA, 0, 1, 2, 3, 20.5, -1, 7, 25)
  grid_grim <- tidyr::expand_grid(x = c(5.19, 3.4, NA, Inf), n = n, items_averaged = c(NA, 0, 1, 2))
  grid <- tidyr::expand_grid(grid_grim, sd = c(-0.5, 0, 0.5, 1.3, NA, Inf))
  grid_debit <- tidyr::expand_grid(x = c(0, 0.25, 0.5, NA), sd = c(0, 0.5, 0.52, NA), n = n)

  grim_grid <- function(type) grim(grid_grim$x, grid_grim$n, 2, grid_grim$items_averaged, type)
  expect_na_rule(grim_grid)
  for (sd_meaning in SD_MEANINGS) {
    expect_na_rule(function(type) grimmer(grid$x, grid$sd, grid$n, 2, 1, grid$items_averaged, type, sd_meaning = sd_meaning))
    expect_na_rule(function(type) debit(grid_debit$x, grid_debit$sd, grid_debit$n, 2, 2, type, sd_meaning = sd_meaning))
  }

  # No number of inconsistencies where `grim()` has no verdict. The reverse
  # need not hold: a mean that passes with any `n` has a verdict, but the
  # number depends on `n`. The numbers follow the rule above themselves: 3.40
  # at `n = 25` passes as either type, but has `NA` for a missing one, since
  # integers could fail and continuous data could not.
  total <- function(type) grim_total(grid_grim$x, grid_grim$n, 2, grid_grim$items_averaged, type)
  for (type in c("integer", "continuous")) {
    type |> total() |> is.na() |> _[is.na(grim_grid(type))] |> all() |> expect_true()
  }
  expect_na_rule(total)
})


test_that("every `sd_meaning` and `formula` default spells out its constant", {
  # The scalar functions check against `SD_MEANINGS` and `DEBIT_FORMULAS`, so a
  # default that lists other values would offer choices that fail:
  constants <- list(sd_meaning = SD_MEANINGS, formula = DEBIT_FORMULAS)
  ns <- asNamespace("scrutiny")
  n_checked <- 0L
  for (name in ls(ns)) {
    fn <- get(name, envir = ns)
    if (!is.function(fn)) {
      next
    }
    defaults <- formals(fn)
    for (arg in intersect(names(defaults), names(constants))) {
      # Internal helpers take a single, resolved value, or one with no default:
      if (identical(defaults[[arg]], quote(expr = )) || is.character(defaults[[arg]])) {
        next
      }
      defaults[[arg]] |> eval() |> expect_equal(constants[[arg]], info = paste0(name, "(", arg, ")"))
      n_checked <- n_checked + 1L
    }
  }
  # `sd_meaning` in the scalar, vectorized, and explain functions of GRIMMER and
  # DEBIT, `grimmer_plot_line()` and `debit_plot_line()`, `grimmer_values()`,
  # `grimmer_closest()`, and `debit_values()` and their scalar functions, the
  # four `sd_binary_*()` functions, and all six mappers; `formula` in DEBIT's
  # three functions, `debit_plot_line()`, `debit_values()` and its scalar
  # function, and three mappers:
  n_checked |> expect_equal(33L)
})

test_that("roundwork accepts every value in `ROUNDING_METHODS`", {
  for (rounding in ROUNDING_METHODS) {
    rounding |> check_rounding_args() |> expect_null()
  }
})

test_that("a mapper records an enum setting it was not given as its first value", {
  out <- tibble::tibble(x = 0.30, sd = 0.46, n = 10) |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer")
  out |> scrutiny_args() |> _$sd_meaning |> expect_equal("sd_sample")
  out |> scrutiny_args() |> _$formula |> expect_equal("exact")
})

test_that("`compare_products()` and `floor_product_ratio()` are exact beyond 2^53", {
  (2^52 + 1) |> compare_products(2^52 - 1, 2^52, 2^52)     |> expect_equal(-1)
  (2^52 + 1) |> compare_products(2^52 + 1, 2^52, 2^52 + 2) |> expect_equal(1)
  3          |> compare_products(2^52 + 1, 2^52 + 1, 3)    |> expect_equal(0)
  3 |> floor_product_ratio(2^52 + 1, 3) |> expect_equal(list(floor = 2^52 + 1, exact = TRUE))
  3 |> floor_product_ratio(2^51 + 1, 2) |> expect_equal(list(floor = 3 * 2^50 + 1, exact = FALSE))
})

test_that("`whole_add()` keeps a sum from 2^53 on as a big number", {
  (2^53 - 1) |> whole_add(1) |> is_big() |> expect_true()
  (2^53 - 1) |> whole_add(1) |> whole_cmp(big_or_double(big(2^53))) |> expect_equal(0)
  (2^53 - 1) |> whole_add(2) |> whole_format() |> expect_equal("9007199254740993")
  (2^53 - 2) |> whole_add(1) |> expect_equal(2^53 - 1)
})


test_that("an invalid `threshold` or `symmetric` is an error, even if the rounding method ignores it", {
  # `rounding_scale()` failed on these with a base R error, and then they were
  # accepted, and recorded in the print header:
  5.19 |> grim(28, 2, 1, threshold = NA, data_type = "integer")                                      |> expect_error("must be a finite number")
  0.5  |> debit(0.5, 10, 2, 2, threshold = "5", data_type = "integer")                               |> expect_error("must be a finite number")
  5.19 |> grim(28, 2, 1, rounding = "up_from", threshold = NA, data_type = "integer")                |> expect_error("must be a finite number")
  pigs1 |> grim_map(2, items_averaged = 1, data_type = "integer", threshold = "5")                   |> expect_error("must be a finite number")
  5.19 |> grim(28, 2, 1, symmetric = "no", data_type = "integer")                                    |> expect_error("`TRUE` or `FALSE`")
})


test_that("the too-many-digits error states the limit it applies", {
  # "has 15" read like a contradiction of "about 15 significant digits":
  0.5 |> grim(10, 15, 1, data_type = "integer") |> expect_error("or 14 if they start with 4.5 or more")
  -81883629588554 |> grim(11, 0, 1, rounding = "up_from", threshold = 0.5, data_type = "integer") |> expect_error("That costs 1 digit")
})

test_that("a data frame without names is rejected like one with empty names", {
  df <- data.frame(5.19, 28)
  names(df) <- NULL
  df |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("Columns 1, 2 have none")
})

test_that("`gcd()` is exact past a quotient of 2^52", {
  # `%%` warned and, without long doubles, could lose the remainder:
  gcd(7, 10^20)      |> expect_no_warning() |> expect_equal(1)
  gcd(2500, 10^22)   |> expect_equal(2500)
  gcd(2^53 + 2, 6)   |> expect_equal(2)
  0 |> debit(0, 10, 1, 15, rounding = "up_from", threshold = 0.07, data_type = "integer") |> expect_no_warning() |> expect_true()
})

test_that("an `integer64` vector is an error, not an `NA` result", {
  # It is a double by type, but 28 is stored as 1.4e-322. Without bit64, which
  # is no dependency, the class alone stands in for it:
  n64 <- structure(c(28, 30), class = "integer64")
  n64_single <- structure(28, class = "integer64")
  msg <- "must not be a 64-bit integer vector"
  5.25 |> grim(n64_single, 2, items_averaged = 1, data_type = "integer")                                              |> expect_error(paste("`n`", msg))
  5.25 |> grim_explain(n64_single, 2, items_averaged = 1, data_type = "integer")                                      |> expect_error(paste("`n`", msg))
  tibble::tibble(x = c(5.25, 2.5), n = n64) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")      |> expect_error(paste("`n`", msg))
  tibble::tibble(x1 = 5.25, x2 = 1.5, n = n64_single) |> grim_map_total_n(2, items_averaged = 1, data_type = "integer") |> expect_error(paste("`n`", msg))
  tibble::tibble(a = n64, b = 1:2, total = 3:4) |> add_up_to_map(summands = c(a, b))           |> expect_error(paste("`a`", msg))
  list(n64) |> add_up_to(58)                                                                   |> expect_error(paste("`summands`", msg))
  # A column that is not tested, such as an ID, is none of the test's business:
  tibble::tibble(x = c(5.25, 2.5), n = c(28, 30), id = n64) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> _$consistency |> expect_equal(c(TRUE, TRUE))
})

test_that("`tibble::num()` columns are tested as plain numbers", {
  # `as.character()` and `as.integer()` failed on them:
  expected <- c(FALSE, TRUE)
  tibble::tibble(x = tibble::num(c(5.19, 3.20), digits = 2), n = c(28, 30)) |> grim_map_seq(2, items_averaged = 1, data_type = "integer") |> nrow() |> expect_identical(20L)
  tibble::tibble(x = c(5.19, 3.20), n = tibble::num(c(28, 30)))             |> grim_map(2, items_averaged = 1, data_type = "integer")     |> _$consistency |> expect_equal(expected)
  tibble::tibble(x1 = 5.19, x2 = 3.2, n = tibble::num(50))                  |> grim_map_total_n(2, items_averaged = 1, data_type = "integer") |> nrow() |> expect_gt(0L)
})


test_that("`sd_denominator_ratio()` agrees with `sd_denominator()`", {
  n <- c(2, 3, 10, 281, 1e6, 2^53)
  for (sd_meaning in setdiff(SD_MEANINGS, "sd_sample_or_pop")) {
    (n * 0 + sd_denominator_ratio(n, sd_meaning)) |> expect_equal(n / sd_denominator(n, sd_meaning))
  }
})
