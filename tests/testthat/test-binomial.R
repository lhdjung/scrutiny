test_that("`grim_binomial()` leaves undecidable cases out", {
  skip_if_not_installed("poibin")
  # A row with an `NA` verdict has an `NA` probability, and one such row used
  # to make the p-value `NA` for the whole table:
  df <- tibble::tibble(x = c(5.19, 5.19, 5.20), n = c(28L, NA, 30L))
  out_all       <- df        |> grim_map(digits_x = 2) |> grim_binomial()
  out_decidable <- df[-2L, ] |> grim_map(digits_x = 2) |> grim_binomial()
  out_all |> expect_equal(out_decidable)
  out_all$parameter |> expect_equal(2L)
})


# `poibin::ppoibin()` crashes the whole R session when given no probabilities,
# which is what is left once every row is undecidable.
test_that("the binomial functions reject empty input instead of crashing", {
  skip_if_not_installed("poibin")
  tibble::tibble(x = c(1.23, 1.24), n = c(NA, NA)) |>
    grim_map(digits_x = 2) |>
    grim_binomial() |>
    expect_error("No value sets to test")
  0.5 |>
    grim_binomial_power(0.9, k = 0) |>
    expect_error("at least one value set")
})
