# The explanations are built from the numbers that decide the tests, and
# `grimmer_explain()` and `debit_explain()` abort if their own reading of these
# numbers disagrees with the verdict. A sweep over many value sets is what puts
# that check to work.

test_that("explanations agree with the tests (sweep)", {
  grid <- tidyr::expand_grid(
    x = c(0.05, 0.36, 0.5, 1.7, 3, 5.19, 5.23),
    sd = c(0, 0.11, 0.21, 0.49, 1.3, 2.55),
    n = c(2, 9, 20, 28, 35, 101),
    digits = 2:3,
    rounding = c("up_or_down", "up", "down", "even", "ceiling", "trunc")
  )

  for (i in seq_len(nrow(grid))) {
    row <- grid[i, ]
    args <- list(
      x = row$x,
      n = row$n,
      digits_x = row$digits,
      rounding = row$rounding
    )
    args_sd <- c(args, list(sd = row$sd, digits_sd = row$digits))

    args |> do.call(what = grim_explain)          |> getElement("consistency") |> expect_identical(do.call(grim, args))
    args_sd |> do.call(what = grimmer_explain)    |> getElement("consistency") |> expect_identical(do.call(grimmer, args_sd))
    if (row$x <= 1 && row$sd <= 1) {
      args_sd |> do.call(what = debit_explain)    |> getElement("consistency") |> expect_identical(do.call(debit, args_sd))
    }
  }
})

test_that("GRIMMER with scale bounds agrees with the test (sweep)", {
  grid <- tidyr::expand_grid(
    x = c(1.5, 2.95, 3, 3.1, 4.5),
    sd = c(0.5, 1.2, 2.04, 2.08, 2.1),
    n = c(5, 20, 31),
    items = 1:2
  )

  for (i in seq_len(nrow(grid))) {
    args <- c(as.list(grid[i, ]), list(digits_x = 2, digits_sd = 2, min_val = 1, max_val = 5))
    args |> do.call(what = grimmer_explain) |> getElement("consistency") |> expect_identical(do.call(grimmer, args))
  }
})

test_that("explanations print as expected", {
  expect_snapshot({
    grim_explain(x = 5.19, n = 28, digits_x = 2)
    grim_explain(x = 5.19, n = 32, digits_x = 2)
    grim_explain(x = 71, n = 43, digits_x = 0, percent = TRUE)
    grim_explain(x = 2.84, n = 16, digits_x = 2, items = 2, rounding = "up")
    grim_explain(x = 5.2, n = 20.5, digits_x = 1)
    grimmer_explain(x = 5.23, sd = 2.55, n = 35, digits_x = 2, digits_sd = 2)
    grimmer_explain(x = 5.19, sd = 2.55, n = 28, digits_x = 2, digits_sd = 2)
    grimmer_explain(x = 3.1, sd = 1.3, n = 200, digits_x = 1, digits_sd = 1)
    grimmer_explain(
      x = 3.0, sd = 2.1, n = 20, digits_x = 1, digits_sd = 1,
      min_val = 1, max_val = 5
    )
    debit_explain(x = 0.36, sd = 0.11, n = 20, digits_x = 2, digits_sd = 2)
    debit_explain(x = 0.35, sd = 0.49, n = 20, digits_x = 2, digits_sd = 2)
    debit_explain(x = 0.36, sd = 0.49, n = 1000, digits_x = 2, digits_sd = 2)
  })
})

test_that("a missing `digits_x` gets the usual error", {
  5.19 |> grim_explain(n = 28) |> expect_error("digits_x")
})

test_that("only a single case is explained", {
  c(5.19, 5.2) |> grim_explain(n = 28, digits_x = 2) |> expect_error("length 1")
})
