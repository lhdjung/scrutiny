vec <- seq_distance(0.01, string_output = FALSE)


test_that("`rounding_bias()` computes the mean bias by default", {
  # Rounding up from 5 on this evenly spaced vector biases upward, rounding down
  # from 5 biases downward, and the two biases mirror each other -- the vector's
  # only tie is at 0.05:
  bias_up <- rounding_bias(vec, digits = 1)
  bias_down <- rounding_bias(vec, digits = 1, rounding = "down")
  expect_true(bias_up > 0)
  expect_true(bias_down < 0)
  expect_equal(bias_up, -bias_down)
})


test_that("`rounding_bias(mean = FALSE)` returns one bias per input value", {
  out <- rounding_bias(vec, digits = 1, rounding = "up", mean = FALSE)
  expect_length(out, length(vec))
  # `x` can be reconstructed from first rounding it, then subtracting the bias:
  expect_true(all(dplyr::near(
    reround(vec, 1L, "up") - out,
    vec
  )))
})


test_that("`rounding_bias()` rejects compound rounding methods", {
  # These return two rounded values per input value, so no single bias is
  # defined. The check was lost for a while, and the function then silently
  # returned twice as many values as inputs (under `mean = FALSE`), or their
  # meaningless average (under the default):
  vec |> rounding_bias(digits = 1, rounding = "up_or_down")           |> expect_error()
  vec |> rounding_bias(digits = 1, rounding = "up_from_or_down_from") |> expect_error()
  vec |> rounding_bias(digits = 1, rounding = "ceiling_or_floor")     |> expect_error()
})


test_that("no pairing warning for arguments that cannot be paired", {
  # `rounding`, `threshold`, and `symmetric` describe one rounding procedure,
  # and `reround()` requires each of them to have length 1. They were still
  # being length-checked against `x` here, so a vector `rounding` first drew a
  # warning about values "getting paired" and then errored for being unpairable.
  expect_warning(
    tryCatch(
      rounding_bias(c(1.25, 2.35), digits = 1, rounding = c("up", "down")),
      error = function(e) NULL
    ),
    regexp = NA
  )
  # The error itself is still the right one:
  c(1.25, 2.35) |>
    rounding_bias(digits = 1, rounding = c("up", "down")) |>
    expect_error()
  # `x` and `digits` are still paired, and still warn about it:
  c(1.25, 2.35) |> rounding_bias(digits = c(1, 2)) |> expect_warning()
})
