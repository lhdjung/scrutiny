test_that("`seq_endpoint()` and `seq_distance()` stay on the decimal level", {
  # `seq()` drifts off the level of `by`, so these used to fail in
  # `restore_zeros()`, or to return values unequal to the decimals they print as:
  4.6 |> seq_endpoint(4.2) |> expect_equal(c("4.6", "4.5", "4.4", "4.3", "4.2"))
  4.6 |> seq_endpoint(0.1) |> expect_length(46L)
  -0.3 |> seq_endpoint(0.3) |> purrr::pluck(4L) |> expect_equal("0.0")
  0.7 |> seq_endpoint(1.0, string_output = FALSE) |>
    expect_identical(c(0.7, 0.8, 0.9, 1))
  # With `by`, the decimal places of `from` count too:
  1.25 |> seq_distance(by = 1, length_out = 3) |>
    expect_equal(c("1.25", "2.25", "3.25"))
  "1.50" |> seq_distance(by = 0.5, length_out = 3) |>
    expect_equal(c("1.50", "2.00", "2.50"))
  1.5 |> seq_distance(length_out = 0) |> expect_error("at least 1")
})
