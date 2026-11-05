df1 <- disperse(n = 20, dispersion = 0:5)

test_that("The function returns a tibble", {
  df1 |> expect_s3_class("tbl_df")
})

test_that("It has the right dimensions", {
  df1 |> dim() |> expect_equal(c(12, 2))
})

test_that("It starts with the right values", {
  df1$n[1] |> expect_equal(20)
  df1$n_change[1] |> expect_equal(0)
})

test_that("It ends with the right values", {
  df1$n[12] |> expect_equal(25)
  df1$n_change[12] |> expect_equal(5)
})


df2 <- disperse(0, dispersion = 0:5, n_min = 1)

test_that("`n_min` controls the dimensions correctly", {
  df2 |> dim() |> expect_equal(c(0, 2))
})


test_that("`dispersion` must consist of whole numbers", {
  # `n_change` used to be truncated toward zero by `as.integer()`, so a
  # fractional step left `n` and `n_change` describing different things.
  10 |> disperse(dispersion = c(0.5, 1.5)) |> expect_error("whole numbers")
  10 |> disperse(dispersion = 1.5)         |> expect_error("whole numbers")
  # A string used to fail in base R's `round()`:
  10 |> disperse(dispersion = "a")         |> expect_error("must be one of these types")
})

test_that("a near-whole `dispersion` is compared to the limits as a whole number", {
  # `seq(0, 1, 0.1) * 10` has 3.0000000000000004, so `4 - 3` was below `n_min`
  # and the pair (1, 7) went missing:
  steps <- seq(0, 1, 0.1) * 10
  4 |> disperse(dispersion = steps)       |> purrr::pluck("n") |> expect_equal(c(4L, 4L, 3L, 5L, 2L, 6L, 1L, 7L))
  9 |> disperse_total(dispersion = steps) |> purrr::pluck("n") |> expect_equal(c(4L, 5L, 3L, 6L, 2L, 7L, 1L, 8L))
})

test_that("near-whole limits are compared as whole numbers", {
  # `100 * 0.29` is 28.999999999999996, which cut off the pair (25, 29):
  27 |> disperse(dispersion = 0:3, n_max = 100 * 0.29)        |> purrr::pluck("n") |> expect_equal(c(27L, 27L, 26L, 28L, 25L, 29L))
  31 |> disperse(dispersion = 0:3, n_min = 29 + 4e-15)        |> purrr::pluck("n") |> expect_equal(c(31L, 31L, 30L, 32L, 29L, 33L))
  c(27, 28) |> disperse2(dispersion = 0:3, n_max = 100 * 0.29) |> purrr::pluck("n") |> expect_equal(c(27L, 28L, 26L, 29L))
  55 |> disperse_total(dispersion = 0:3, n_max = 100 * 0.29)  |> purrr::pluck("n") |> expect_equal(c(27L, 28L, 26L, 29L))
})

test_that("`n` and `n_change` agree", {
  out <- disperse(n = 10)
  (out$n - 10L) |> expect_equal(out$n_change)
})


test_that("`n` is integer, as it is in the mappers", {
  # The mappers store `n` as integer because a sample size is a whole number.
  # The dispersion helpers that feed them, and the reverse function that reads
  # their output back, returned doubles -- so the column changed type on the
  # way in and back out again.
  20        |> disperse()       |> purrr::pluck("n")        |> expect_type("integer")
  c(25, 26) |> disperse2()      |> purrr::pluck("n")        |> expect_type("integer")
  40        |> disperse_total() |> purrr::pluck("n")        |> expect_type("integer")
  51        |> disperse_total() |> purrr::pluck("n")        |> expect_type("integer")
  20        |> disperse()       |> purrr::pluck("n_change") |> expect_type("integer")

  tibble::tibble(x1 = 4.52, x2 = 5.23, n = 40L) |>
    grim_map_total_n(digits_x = 2, items_averaged = 1) |>
    reverse_map_total_n() |>
    purrr::pluck("n") |>
    expect_type("integer")

  # The values themselves are unchanged:
  20 |>
    disperse() |>
    purrr::pluck("n") |>
    expect_equal(c(20L, 20L, 19L, 21L, 18L, 22L, 17L, 23L, 16L, 24L, 15L, 25L))
})


test_that("`disperse_total()` requires a whole-number total", {
  # `disperse_total(7.5)` used to return group sizes of 3.25 and 4.25.
  7.5 |> disperse_total() |> expect_error("whole number")
  NA  |> disperse_total() |> expect_error("whole number")
})


test_that("`disperse_total()` returns no rows if the limits leave no pairs", {
  # `seq(1, 0, by = 2)` used to fail here, for an odd total only:
  out_odd <- disperse_total(51, n_min = 30)
  out_even <- disperse_total(50, n_min = 30)
  out_odd |> expect_equal(out_even)
  out_odd |> nrow() |> expect_equal(0L)
  1 |> disperse_total() |> nrow() |> expect_equal(0L)
})


test_that("`n`, `n_min`, and `n_max` must be numbers", {
  # A missing limit used to turn every pair into `NA`:
  20       |> disperse(n_max = NA)  |> expect_error("single number")
  20       |> disperse(n_min = 1:2) |> expect_error("single number")
  "20"     |> disperse()            |> expect_error("must be one of these types")
  NA_real_ |> disperse()            |> expect_error("not be missing")
  Inf      |> disperse()            |> expect_error("missing or infinite")
  -4       |> disperse_total(n_min = NULL) |> expect_error("non-negative")
  # These used to fail with raw R errors from the arithmetic:
  c(NA, 26)    |> disperse2()      |> expect_error("missing or infinite")
  c("25", "26") |> disperse2()     |> expect_error("must be one of these types")
  c(TRUE, FALSE) |> disperse2()    |> expect_error("must be one of these types")
  "40"         |> disperse_total() |> expect_error("must be one of these types")
  TRUE         |> disperse_total() |> expect_error("must be one of these types")
  Inf          |> disperse_total() |> expect_error("whole number")
})

test_that("`disperse2()` applies the limits to the group sizes themselves", {
  # The limits used to be compared to the midpoint, so a fractional `n_min`
  # let a pair through that broke it:
  c(25, 26) |> disperse2(n_min = 24.5) |> purrr::pluck("n") |> expect_equal(c(25L, 26L))
  c(25, 26) |> disperse2(n_min = 24, dispersion = 0:2) |> purrr::pluck("n") |> expect_equal(c(25L, 26L, 24L, 27L))
  c(25, 26) |> disperse2(n_max = 26.5, dispersion = 0:2) |> purrr::pluck("n") |> expect_equal(c(25L, 26L))
})

test_that("a `constant` element of the wrong length is an error that names it", {
  20 |> disperse(constant = "a")                |> expect_error("`constant` must have length 2")
  20 |> disperse(constant = list(z = 1, 3:4))   |> expect_error("`constant$z` must have length 2", fixed = TRUE)
  20 |> disperse(constant = list(1:2, 1:3))     |> expect_error("`constant$constant2` must have length 2", fixed = TRUE)
  # No purrr "In index" context around an error about the argument:
  tibble::tibble(x1 = 4.5, x2 = 5.2, n = 40) |>
    grim_map_total_n(digits_x = 1, items_averaged = 1, constant = list(z = 1)) |>
    expect_error("`constant$z` must have length 2", fixed = TRUE, class = "rlang_error") |>
    conditionMessage() |>
    expect_no_match("In index")
})

test_that("a partly named `constant` list gets default names for the rest", {
  out <- disperse(20, dispersion = 0:1, constant = list(a = 1:2, 3:4))
  out |> colnames() |> expect_equal(c("n", "n_change", "a", "constant2"))
})


test_that("an odd total's limits are checked as given", {
  # The message showed `n_min + 0.5`, and a string failed in base R arithmetic:
  51 |> disperse_total(n_min = c(1, 2)) |> expect_error("It is `1` and `2`")
  51 |> disperse_total(n_min = "2")     |> expect_error("single number")
})


test_that("a total within tolerance of a whole number is split as that number", {
  # `40 - 1e-9` passed the whole-number check but was split as odd, into 19.5
  # and 20.5:
  (40 - 1e-9) |> disperse_total() |> expect_equal(disperse_total(40))
})


test_that("a near-whole `n` is compared to the limits as that number", {
  # `2 - 1e-9` lost the pair (1, 3) to `n_min = 1`, and `10 + 1e-9` the pair
  # (8, 12) to `n_max = 12`, though both were shown as whole numbers:
  (2 - 1e-9)  |> disperse(dispersion = 0:2)             |> expect_equal(disperse(2, dispersion = 0:2))
  (10 + 1e-9) |> disperse(dispersion = 0:3, n_max = 12) |> expect_equal(disperse(10, dispersion = 0:3, n_max = 12))
  c(2 - 1e-9, 3) |> disperse2(dispersion = 0:2)         |> expect_equal(disperse2(c(2, 3), dispersion = 0:2))
})

test_that("an infinite `dispersion` is an error", {
  disperse(5, dispersion = c(0, Inf))        |> expect_error("missing values or infinities")
  disperse2(c(25, 26), dispersion = Inf)     |> expect_error("missing values or infinities")
  disperse_total(50, dispersion = Inf)       |> expect_error("missing values or infinities")
})

test_that("a repeated step counts once", {
  disperse(5, dispersion = c(1, 1)) |> expect_equal(disperse(5, dispersion = 1))
  disperse_total(50, dispersion = c(1, 1)) |> nrow() |> expect_equal(2L)
})


test_that("a `constant` named like an output column is an error", {
  # It replaced `n` without a word:
  37 |> disperse_total(constant = list(n = c(1, 2)))                  |> expect_error("`n` is taken")
  37 |> disperse_total(constant = list(k = c(1, 2), n_change = 1:2)) |> expect_error("`n_change` is taken")
})

test_that("group sizes from 2^52 on are refused", {
  # A double skips whole numbers from 2^53 on, and `%%` warned of a loss of
  # accuracy:
  2^60 |> disperse_total() |> expect_error("too large to disperse exactly")
  2^53 |> disperse()       |> expect_error("too large to disperse exactly")
  2^52 |> disperse()       |> expect_error("beyond 2\\^52")
  # An odd total is split around a half, which a double only holds below 2^52;
  # the pairs used to add up to one less than the total:
  (2^53 - 3) |> disperse_total(dispersion = 0:2) |> expect_error("beyond 2\\^52")
  (2^52 - 0.5) |> disperse(dispersion = 0:1)     |> expect_error("beyond 2\\^52")
  # The mean of these is rounded to a whole number:
  c(2^52 + 1, 2^52 + 2) |> disperse2(dispersion = 0:1) |> expect_error("beyond 2\\^52")
  out <- (2^52 - 3) |> disperse_total(dispersion = 0:2)
  (out$n[c(TRUE, FALSE)] + out$n[c(FALSE, TRUE)]) |> expect_equal(rep(2^52 - 3, 3))
})

test_that("a name repeated in `constant` is an error", {
  # Only the last of the columns was kept:
  5 |> disperse(1, constant = list(a = 1:2, a = 3:4))             |> expect_error("`a` occurs more than once")
  tibble::tibble(x1 = 5.19, x2 = 4.06, n = 65) |> grim_map_total_n(digits_x = 2, items_averaged = 1, constant = list(a = 1:2, a = 3:4)) |> expect_error("`a` occurs more than once")
})

test_that("an integer `n` near the integer limit is an error, not an `NA` row", {
  # `n + dispersion` overflowed with a base R warning, and `n - dispersion`
  # for a negative `n`:
  2147483647L  |> disperse(dispersion = 0:1) |> expect_error("too large to disperse as an integer")
  -2147483647L |> disperse(dispersion = 0:1) |> expect_error("too large to disperse as an integer")
  2147483647   |> disperse(dispersion = 0:1) |> _$n |> anyNA() |> expect_false()
})
