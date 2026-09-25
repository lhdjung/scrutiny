test_that("`grim_binomial()` leaves undecidable cases out", {
  skip_if_not_installed("poibin")
  # A row with an `NA` verdict has an `NA` probability, and one such row used
  # to make the p-value `NA` for the whole table:
  df <- tibble::tibble(x = c(5.19, 5.19, 5.20), n = c(28L, NA, 30L))
  out_all <- grim_map(df, digits_x = 2) |> grim_binomial()
  out_decidable <- grim_map(df[-2L, ], digits_x = 2) |> grim_binomial()
  out_all |> expect_equal(out_decidable)
  out_all$parameter |> expect_equal(2L)
})


# `poibin::ppoibin()` crashes the whole R session when given no probabilities,
# which is what is left once every row is undecidable.
test_that("the binomial functions reject empty input instead of crashing", {
  skip_if_not_installed("poibin")
  grim_map(tibble::tibble(x = c(1.23, 1.24), n = c(NA, NA)), digits_x = 2) |>
    grim_binomial() |>
    expect_error("No value sets to test")
  grim_binomial_power(0.5, 0.9, k = 0) |>
    expect_error("at least one value set")
})
