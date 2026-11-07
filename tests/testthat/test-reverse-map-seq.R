# Define example data -----------------------------------------------------

df_schlim <- tibble::tibble(y = 16:100, n = 3:87)
df_grim <- pigs1 |> dplyr::mutate(items_averaged = 1, data_type = "integer")
df_debit <- pigs3 |> dplyr::mutate(data_type = "integer")


# Create a mock `*_seq()` function ----------------------------------------

schlim_scalar <- function(y, n) {
  y <- as.numeric(y)
  n <- as.numeric(n)
  y / 3 > n
}

schlim_map <- function(data) {
  y <- data$y
  n <- data$n
  consistency <- purrr::map2_lgl(y, n, schlim_scalar)
  tibble::tibble(y, n, consistency) |> add_class("scrutiny_schlim_map") # See section "S3 classes" below
}

schlim_map_seq <- function_map_seq(
  .fn = schlim_map,
  .reported = c("y", "n"),
  .name_test = "SCHLIM",
  .name_class = "scrutiny_schlim_map_seq"
)


# Apply the reversal ------------------------------------------------------

df_schlim_rec <- df_schlim |>
  schlim_map_seq(include_consistent = TRUE) |>
  reverse_map_seq()

df_grim_rec <- df_grim |>
  grim_map_seq(
    digits_x = 2,
    include_consistent = TRUE,
    data_type = "integer"
  ) |>
  reverse_map_seq()

df_debit_rec <- df_debit |>
  debit_map_seq(
    digits_x = 2,
    digits_sd = 2,
    data_type = "integer",
    include_consistent = TRUE
  ) |>
  reverse_map_seq()


# Test for equality with the original -------------------------------------

test_that("It works with SCHLIM (toy test)", {
  df_schlim |> expect_equal(df_schlim_rec)
})

test_that("It works with GRIM", {
  df_grim |> expect_equal(df_grim_rec)
})

test_that("It works with DEBIT", {
  df_debit |> expect_equal(df_debit_rec)
})


test_that("a case whose dispersion was clipped to nothing stays aligned", {
  # With `out_min == out_max == 25`, case 1's `x` and case 2's `n` disperse to
  # no rows at all, so they are only on the rows of the other variable:
  df <- tibble::tibble(x = c(25.0, 24.9, 25.2), n = c(26, 25, 30), items_averaged = 1, data_type = "integer")
  out <- df |> grim_map_seq(digits_x = 1, out_min = 25, out_max = 25, include_consistent = TRUE, data_type = "integer")
  out |> reverse_map_seq() |> expect_equal(df)
  audit <- out |> audit_seq()
  audit$x |> expect_equal(df$x)
  audit$n |> expect_equal(df$n)
  # With both limits at 25, the only dispersed value either variable can take is
  # 25 itself, so case 3 has one candidate per variable:
  audit$hits_x |> expect_equal(c(0L, 1L, 1L))
  audit$hits_n |> expect_equal(c(1L, 0L, 1L))
})

test_that("a variable clipped to nothing in every case keeps its `audit_seq()` columns", {
  # `out_max = 15` is below every dispersed `n` of `pigs1`, so `n` has no rows:
  out <- pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer", var = c("n", "x"), out_max = 15)
  out$var |> unique() |> expect_equal("x")
  audit <- out |> audit_seq()
  audit |> colnames() |> expect_contains(c("hits_n", "hits_x", "diff_n", "diff_n_up", "diff_n_down"))
  audit$hits_n |> expect_equal(rep(0L, nrow(audit)))
})

test_that("a variable without a `digits_*` column is recovered when its dispersion skips steps", {
  df <- tibble::tibble(absolute = 40, percentage = 35, n = 120)
  for (dispersion in list(1, c(2, 4, 6))) {
    df |> is_percentage_of_map_seq(digits_percentage = 0, var = "absolute", dispersion = dispersion) |> reverse_map_seq() |> _$absolute |> expect_equal(40)
    df |> is_percentage_of_map_seq(digits_percentage = 0, var = "absolute", dispersion = dispersion) |> audit_seq()       |> nrow()     |> expect_equal(1L)
  }
})

test_that("a missing column is an error, not wrong counts or a base R error", {
  out <- tibble::tibble(x = c(5.19, 4.56), n = c(28, 31)) |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
  out |> dplyr::select(-diff_var) |> audit_seq()        |> expect_error("Missing: `diff_var`")
  out |> dplyr::select(-case)     |> audit_seq()        |> expect_error("Missing: `case`")
  out |> dplyr::select(-var)      |> audit_seq()        |> expect_error("Missing: `var`")
  out |> dplyr::select(-diff_var) |> reverse_map_seq()  |> expect_error("Missing: `diff_var`")
  out |> dplyr::select(-consistency) |> reverse_map_seq() |> expect_error("Missing: `consistency`")
})

test_that("a renamed or removed tested column is an error, not a dispersed value", {
  out <- tibble::tibble(x = c(5.19, 2.50, 4.56), n = c(28, 10, 30)) |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
  out |> dplyr::rename(mean = x) |> reverse_map_seq() |> expect_error("Missing: `x`")
  out |> dplyr::rename(mean = x) |> audit_seq()       |> expect_error("Missing: `x`")
  out |> dplyr::select(-n)       |> audit_seq()       |> expect_error("Missing: `n`")
})

test_that("the tested columns are found wherever they were moved", {
  out <- tibble::tibble(x = c(5.19, 2.50, 4.56), n = c(28, 10, 30)) |> grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
  out |> dplyr::relocate(consistency)         |> reverse_map_seq() |> expect_equal(reverse_map_seq(out))
  out |> dplyr::relocate(consistency)         |> audit_seq()       |> expect_equal(audit_seq(out))
  out |> dplyr::relocate(case, var, diff_var) |> audit_seq()       |> expect_equal(audit_seq(out))
})

test_that("a whole-number variable other than `n` is recovered from a single dispersed value", {
  out <- tibble::tibble(absolute = 0, percentage = 5, n = 20) |> is_percentage_of_map_seq(digits_percentage = 0, var = "absolute", dispersion = 1)
  out |> nrow() |> expect_equal(1L)
  out |> reverse_map_seq() |> expect_equal(tibble::tibble(absolute = 0L, percentage = 5, n = 20L))
  out |> audit_seq() |> _$hits_absolute |> expect_equal(1L)
})
