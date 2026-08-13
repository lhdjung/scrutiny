# These functions had no tests at all, which is how a regression in them got
# past the whole suite and only showed up in `R CMD check`'s examples.

test_that("`reround_to_fraction()` rounds to fractions of the denominator", {
  # A denominator of 1 is ordinary rounding:
  expect_equal(reround_to_fraction(0.4, denominator = 1, rounding = "even"), 0)
  # ...and higher denominators round to the nearest fraction of themselves:
  expect_equal(reround_to_fraction(0.4, denominator = 2, rounding = "even"), 0.5)
  expect_equal(
    reround_to_fraction(0.25, denominator = 3, rounding = "even"),
    1 / 3
  )
})

test_that("a compound method returns both of its results per input value", {
  # `0.25 * 2` is exactly 0.5, so the two branches part company:
  expect_equal(reround_to_fraction(0.25, denominator = 2), c(0.5, 0))
  expect_equal(
    reround_to_fraction(0.25, denominator = 2, rounding = "ceiling_or_floor"),
    c(0.5, 0)
  )
  # ...whereas away from a tie they agree:
  expect_equal(reround_to_fraction(0.4, denominator = 2), c(0.5, 0.5))

  # Two values per element of `x`, in `reround()`'s interleaved layout. The two
  # procedures used to be paired with the elements of `x` instead, so this
  # returned two values rather than four:
  expect_equal(
    reround_to_fraction(c(0.25, 0.4), denominator = 2),
    c(0.5, 0, 0.5, 0.5)
  )
})

test_that("a single method returns one value per input value", {
  for (rounding in c("even", "up", "down", "ceiling", "floor", "ties_away")) {
    expect_length(
      reround_to_fraction(
        c(0.25, 0.4, 0.6),
        denominator = 2,
        rounding = rounding
      ),
      3L
    )
  }
})

test_that("`digits` keeps each branch on its own rounding procedure", {
  # The subsequent rounding to `digits` must not re-apply the compound method,
  # which would double the values a second time; each branch continues with the
  # procedure that produced it.
  expect_length(reround_to_fraction(0.4, denominator = 2, digits = 1), 2L)
  expect_equal(reround_to_fraction(0.4, denominator = 2, digits = 1), c(0.5, 0.5))

  # One `digits` value per element of `x`, spread over both branches:
  expect_equal(
    reround_to_fraction(c(0.4, 0.6), denominator = 2, digits = 1),
    c(0.5, 0.5, 0.5, 0.5)
  )
  expect_length(
    reround_to_fraction(c(0.4, 0.6), denominator = 2, digits = 1),
    4L
  )

  # `digits = Inf`, the default, means no subsequent rounding at all:
  expect_equal(
    reround_to_fraction(0.12345, denominator = 3, digits = Inf),
    reround_to_fraction(0.12345, denominator = 3)
  )
})

test_that("`reround_to_fraction_level()` rounds at the given decimal level", {
  expect_equal(
    reround_to_fraction_level(0.12345, denominator = 2, digits = 0),
    c(0, 0)
  )
  expect_equal(
    reround_to_fraction_level(0.12345, denominator = 2, digits = 1),
    c(0.1, 0.1)
  )
  expect_equal(
    reround_to_fraction_level(0.12345, denominator = 2, digits = 2),
    c(0.125, 0.125)
  )
  expect_equal(
    reround_to_fraction_level(c(0.12345, 0.678), denominator = 2, digits = 1),
    c(0.1, 0.1, 0.7, 0.7)
  )
})

test_that("`denominator` must be 1 or greater", {
  expect_error(reround_to_fraction(0.4, denominator = 0))
  expect_error(reround_to_fraction_level(0.4, denominator = 0.5))
})


test_that("`digits = \"auto\"` works in both functions", {
  # `reround_to_fraction()` used to resolve `"auto"` only *after* validating
  # `digits` as a whole number, so the string reached `is_whole_number()` and
  # failed with "non-numeric argument to mathematical function".
  expect_equal(
    reround_to_fraction(0.12345, denominator = 2, digits = "auto"),
    reround_to_fraction(0.12345, denominator = 2, digits = 1)
  )
  expect_equal(
    reround_to_fraction(0.12345, denominator = 20, digits = "auto"),
    reround_to_fraction(0.12345, denominator = 20, digits = 3)
  )
  expect_no_error(
    reround_to_fraction_level(0.12345, denominator = 2, digits = "auto")
  )
})

test_that("`digits` may have one value per element of `x`", {
  # This is what the argument is documented to accept; it used to fail the
  # length-congruence check even though `x` and `digits` were the same length.
  # (The pairing warning is the documented behavior for two arguments of equal
  # length greater than 1; the point here is that it is a warning, not an
  # error.)
  expect_no_error(
    reround_to_fraction(c(0.4, 0.6), denominator = 2, digits = c(1, 2)) |>
      suppressWarnings()
  )
  expect_equal(
    reround_to_fraction(c(0.4, 0.6), denominator = 2, digits = c(1, 2)) |>
      suppressWarnings(),
    reround_to_fraction(c(0.4, 0.6), denominator = 2)
  )
  # A genuine length mismatch is still an error:
  expect_error(
    reround_to_fraction(c(0.4, 0.6), denominator = 2, digits = c(1, 2, 3))
  )
  # ...as is a non-whole `digits`:
  expect_error(reround_to_fraction(0.4, denominator = 2, digits = 1.5))
})
