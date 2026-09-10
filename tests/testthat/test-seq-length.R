x_orig <- seq(from = 0.1, to = 0.5, by = 0.1)
x <- x_orig


test_that("numeric values are handled correctly by the prefix form", {
  x |> seq_length(10) |> expect_equal(seq(from = 0.1, to = 1, by = 0.1))
  x |> seq_length(0)  |> expect_equal(numeric(0))
})


test_that("strings are handled correctly by the prefix form", {
  x |> as.character() |> seq_length(10) |> expect_equal(seq_endpoint(from = 0.1, to = 1))
  x |> as.character() |> seq_length(0)  |> expect_equal(character(0))
})


test_that("numeric values are handled correctly by the replacement form", {
  seq_length(x) <- 10
  x |> expect_equal(seq(from = 0.1, to = 1, by = 0.1))
  seq_length(x) <- 0
  x |> expect_equal(numeric(0))
})


test_that("strings are handled correctly by the replacement form", {
  x <- as.character(x_orig)
  seq_length(x) <- 10
  x |> expect_equal(seq_endpoint(from = 0.1, to = 1))
  seq_length(x) <- 0
  x |> expect_equal(character(0))
})


test_that("the sequence is extended by its own step, in its own direction", {
  # The step used to be one unit of the last decimal place, whatever the
  # sequence's step was, and a descending sequence was extended upward:
  c(2, 4, 6) |> seq_length(5) |> expect_equal(c(2, 4, 6, 8, 10))
  c(6, 4, 2) |> seq_length(5) |> expect_equal(c(6, 4, 2, 0, -2))
  3:7        |> seq_length(8) |> expect_identical(3:10)
  c(2, 2, 2) |> seq_length(5) |> expect_equal(rep(2, 5))
  c("0.10", "0.20", "0.30") |>
    seq_length(5) |>
    expect_equal(c("0.10", "0.20", "0.30", "0.40", "0.50"))
})
