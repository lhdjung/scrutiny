df1 <- unround(c(3.6, "5.20", 5.174)) |>
  suppressMessages()


test_that("The output is a tibble", {

  df1 |> expect_s3_class("tbl_df")
})

test_that("It has correct dimensions", {
  df1 |> dim() |> expect_equal(c(3, 7))
})


colnames_expected <- c(
  "range",
  "rounding",
  "lower",
  "incl_lower",
  "x",
  "incl_upper",
  "upper"
)

test_that("It has correct column names", {
  df1 |> colnames() |> expect_setequal(colnames_expected)
})


test_that("Its columns have the correct types", {
  df1[[1]] |> expect_type("character")
  df1[[2]] |> expect_type("character")
  df1[[3]] |> expect_type("double")
  df1[[4]] |> expect_type("logical")
  df1[[5]] |> expect_type("character")
  df1[[6]] |> expect_type("logical")
  df1[[7]] |> expect_type("double")
})


test_that("A non-string `x` specification throws an error
          (if `digits` is `NULL`; the default)", {
  unround(4.5) |> expect_error()
})

test_that("The function throws an error if `rounding` is misspecified", {
  unround("4.50", rounding = "doesn't exist") |> expect_error()
})


df2 <- unround("4.50", digits = 1)
df3 <- unround("4.50", digits = 1:5)


test_that("", {
  df2$lower |> expect_equal(4.45)
  df2$upper |> expect_equal(4.55)
})


test_that("", {
  df3$lower |> expect_equal(c(4.450000, 4.495000, 4.499500, 4.499950, 4.499995))
  df3$upper |> expect_equal(c(4.550000, 4.505000, 4.500500, 4.500050, 4.500005))
})


test_that("A vector-valued `digits` yields a well-formed tibble", {
  # The row count used to be taken from `length(x)` while the columns took
  # whatever length `paste0()` recycling produced, so this was a tibble
  # claiming one row but holding five-element columns.
  df3 |> nrow() |> expect_equal(5L)
  df3 |> vapply(length, integer(1L)) |> unname() |> expect_equal(rep(5L, 7L))
  df3 |> as.data.frame() |> nrow() |> expect_equal(5L)
})


# `unround()` is the inverse of `reround()`: a value just inside the
# reconstructed range must round back to `x`, and a value just outside it must
# not. This is the property that matters, and it ties the bounds to the
# rounding functions they claim to invert.
test_that("`unround()` bounds agree with the rounding they invert", {
  # `eps` is far below the granularity of the bounds (0.001 here) but far above
  # the dust that `round_up_from()` and friends subtract from `threshold`:
  eps <- 1e-6
  methods <- c(
    "up_or_down", "up", "down", "ceiling", "floor",
    "trunc", "anti_trunc", "up_from", "down_from"
  )
  for (m in methods) {
    bounds <- unround("0.53", rounding = m, threshold = 6)
    rounds_to_x <- function(value) {
      any(dplyr::near(
        reround(value, digits = 2, rounding = m, threshold = 6),
        0.53
      ))
    }
    expect_true(rounds_to_x(bounds$lower + eps), label = paste(m, "inside lower"))
    expect_true(rounds_to_x(bounds$upper - eps), label = paste(m, "inside upper"))
    expect_false(rounds_to_x(bounds$lower - eps), label = paste(m, "beyond lower"))
    expect_false(rounds_to_x(bounds$upper + eps), label = paste(m, "beyond upper"))
  }
})


test_that("`unround()` supports the same rounding methods as GRIM", {
  # These four used to throw an error, so `debit_map()` rejected rounding
  # methods that `grim()` accepted.
  for (m in c(
    "up_from", "down_from", "up_from_or_down_from", "ceiling_or_floor"
  )) {
    unround("0.53", rounding = m, threshold = 6) |>
      nrow() |>
      expect_equal(1L)
  }
})


test_that("`threshold` only affects the `*_from` rounding methods", {
  # `round_up()` and `round_down()` round from a fixed 5, so reconstructing
  # their bounds must not depend on `threshold`. `unround()` used to widen the
  # range anyway, reconstructing a rounding that never happens.
  for (m in c("up_or_down", "up", "down")) {
    from_5 <- unround("0.53", rounding = m, threshold = 5)
    from_6 <- unround("0.53", rounding = m, threshold = 6)
    expect_equal(from_5$lower, from_6$lower)
    expect_equal(from_5$upper, from_6$upper)
    expect_equal(from_5$lower, 0.525)
    expect_equal(from_5$upper, 0.535)
  }
  # ...whereas the parameterized methods do respond to it:
  unround("0.53", rounding = "up_from", threshold = 6)$lower |>
    expect_equal(0.526)
})


test_that("`symmetric` mirrors the bounds of a negative `x`", {
  plain <- unround("-0.53", rounding = "up", symmetric = FALSE)
  mirrored <- unround("-0.53", rounding = "up", symmetric = TRUE)
  # With `symmetric`, rounding a negative number mirrors its absolute value,
  # so the inclusive end swaps:
  expect_true(plain$incl_lower)
  expect_false(plain$incl_upper)
  expect_false(mirrored$incl_lower)
  expect_true(mirrored$incl_upper)
})


test_that("`\"anti_trunc\"` bounds match `round_anti_trunc()`", {
  # `round_anti_trunc()` always rounds away from zero, so for a positive `x`,
  # the bound it can be reached from is the lower one, and for a negative `x`,
  # the upper one. The negative case used to carry the signs of the positive
  # one.
  bounds_positive <- unround("0.70", rounding = "anti_trunc")
  expect_true(bounds_positive$incl_lower)
  expect_false(bounds_positive$incl_upper)
  expect_equal(round_anti_trunc(bounds_positive$lower, 1), 0.7)
  expect_false(round_anti_trunc(bounds_positive$upper, 1) == 0.7)

  bounds_negative <- unround("-0.70", rounding = "anti_trunc")
  expect_false(bounds_negative$incl_lower)
  expect_true(bounds_negative$incl_upper)
  expect_equal(round_anti_trunc(bounds_negative$upper, 1), -0.7)
  expect_false(round_anti_trunc(bounds_negative$lower, 1) == -0.7)
})
