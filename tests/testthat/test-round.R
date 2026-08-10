x <- runif(100000, min = 1900, max = 2100)
x_up <- x |> round_up(2) |> trunc_reverse()
x_down <- x |> round_down(2) |> trunc_reverse()

# Avoid whole numbers that will be rounded up, artificially leading to a
# difference of 1:
x_up <- x_up[x_up != 0]
x_down <- x_down[x_down != 0]


# Threshold 5 (via the wrappers) ------------------------------------------

test_that("`round_up()` works correctly", {
  expect_equal(
    x_up,
    x_up |> trunc_reverse() |> round_up(2)
  )
})

test_that("`round_down()` works correctly", {
  expect_equal(
    x_down,
    x_down |> trunc_reverse() |> round_down(2)
  )
})


# Threshold 3 -------------------------------------------------------------

x_up_from_3 <- x |> round_up_from(2, threshold = 3)
x_down_from_3 <- x |> round_down_from(2, threshold = 3)

test_that("`round_up_from()` works correctly", {
  expect_equal(
    x_up,
    x_up |> trunc_reverse() |> round_up_from(2, threshold = 3)
  )
})

test_that("`round_down_from()` works correctly", {
  expect_equal(
    x_down,
    x_down |> trunc_reverse() |> round_down_from(2, threshold = 3)
  )
})


# Threshold 8 -------------------------------------------------------------

x_up_from_8 <- x |> round_up_from(2, threshold = 8)
x_down_from_8 <- x |> round_down_from(2, threshold = 8)

test_that("`round_up_from()` works correctly", {
  expect_equal(
    x_up,
    x_up |> trunc_reverse() |> round_up_from(2, threshold = 8)
  )
})

test_that("`round_down_from()` works correctly", {
  expect_equal(
    x_down,
    x_down |> trunc_reverse() |> round_down_from(2, threshold = 8)
  )
})


# Exact decimal boundaries ------------------------------------------------

# Shifting a number by `digits` decimal places is inexact: `0.28 * 100` is
# 28.000000000000004, and `0.29 * 100` is 28.999999999999996. Rounding the
# shifted value away from the number it stands for would move it a whole step.

test_that("`round_ceiling()` and `round_floor()` are exact at whole steps", {
  x <- seq(0, 1000) / 100

  expect_equal(round_ceiling(x, 2), x)
  expect_equal(round_floor(x, 2), x)
  expect_equal(round_trunc(x, 2), x)
  expect_equal(round_trunc(-x, 2), -x)

  # A value one unit below `x` anti-truncates to `x`, and `x` itself to one
  # unit above it:
  expect_equal(round_anti_trunc(x[-1] - 0.01, 2), x[-1])

  # The individual cases that used to fail:
  expect_equal(round_ceiling(0.28, 2), 0.28)
  expect_equal(round_floor(0.29, 2), 0.29)

  # Values genuinely inside a step still move to its edge:
  expect_equal(round_ceiling(0.281, 2), 0.29)
  expect_equal(round_floor(0.289, 2), 0.28)
})
