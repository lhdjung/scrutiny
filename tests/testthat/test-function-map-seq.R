test_that("an `.args_by_row` argument other than `digits_*` must be a single value", {
  # It reached the re-tests with one value per row of `data`, not of the rows
  # tested, and failed with an error about their number:
  f  <- function(a, b, digits_a, w = 1) (a + b) * w < 10
  fm <- function_map(.fn = f, .reported = c("a", "b"), .name_test = "FW", .args_by_row = c("digits_a", "w"))
  fs <- function_map_seq(.fn = fm, .reported = c("a", "b"), .name_test = "FW")
  d  <- tibble::tibble(a = c(1.2, 2.5, 4.1), b = 1:3)
  d |> fs(digits_a = 1, w = c(1, 2, 3)) |> expect_error("`w` must be a single value here")
  d |> fs(digits_a = 1, w = 2)          |> nrow() |> expect_gt(0L)
})
