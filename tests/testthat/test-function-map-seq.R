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


test_that("the output has the same columns in the same order whatever is dispersed", {
  # A variable without dispersed rows moved `reason` behind `case`, and with
  # no rows at all, the column was missing:
  df <- tibble::tibble(x = c(5.19, 5.18), sd = c(1.21, 1.3), n = c(28, 28))
  map_seq <- function(...) {
    df |>
      grimmer_map_seq(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer", ...) |>
      suppressWarnings()
  }
  cols <- map_seq() |> colnames()
  cols |> expect_equal(c(
    "x", "sd", "n", "items_averaged", "data_type", "digits_x", "digits_sd",
    "consistency", "reason", "diff_var", "case", "var"
  ))
  map_seq(out_min = 10)   |> colnames() |> expect_equal(cols)
  map_seq(dispersion = 0) |> colnames() |> expect_equal(cols)
  map_seq(dispersion = 0) |> dplyr::pull(reason) |> expect_type("character")

  cols_rec <- pigs1 |>
    grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer", show_rec = TRUE) |>
    colnames()
  pigs1 |>
    grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer", show_rec = TRUE, out_min = 10) |>
    colnames() |>
    expect_equal(cols_rec)
})


test_that("the `digits_*` arguments of sequence and total-n mappers have no default", {
  # They were `NULL`, which the usage showed next to "There is no default":
  mappers <- list(
    grim_map_seq, grimmer_map_seq, debit_map_seq,
    grim_map_total_n, grimmer_map_total_n, debit_map_total_n
  )
  for (mapper in mappers) {
    digits <- formals(mapper)[grepl("^digits_", names(formals(mapper)))]
    length(digits) |> expect_gt(0L)
    digits |> vapply(rlang::is_missing, logical(1L)) |> all() |> expect_true()
  }

  # Left out or `NULL`, the error is the same as before:
  pigs1 |> grim_map_seq(items_averaged = 1, data_type = "integer")                  |> expect_error("Need to specify `digits_x`")
  pigs1 |> grim_map_seq(digits_x = NULL, items_averaged = 1, data_type = "integer") |> expect_error("digits_x")
})
