test_that("`seq_endpoint()` and `seq_distance()` stay on the decimal level", {
  # `seq()` drifts off the level of `by`, so these used to fail in
  # `restore_zeros()`, or to return values unequal to the decimals they print as:
  seq_endpoint(4.6, 4.2) |> expect_equal(c("4.6", "4.5", "4.4", "4.3", "4.2"))
  seq_endpoint(4.6, 0.1) |> expect_length(46L)
  seq_endpoint(-0.3, 0.3)[4L] |> expect_equal("0.0")
  seq_endpoint(0.7, 1.0, string_output = FALSE) |>
    expect_identical(c(0.7, 0.8, 0.9, 1))
  # With `by`, the decimal places of `from` count too:
  seq_distance(1.25, by = 1, length_out = 3) |>
    expect_equal(c("1.25", "2.25", "3.25"))
  seq_distance("1.50", by = 0.5, length_out = 3) |>
    expect_equal(c("1.50", "2.00", "2.50"))
  seq_distance(1.5, length_out = 0) |> expect_error("at least 1")
})
