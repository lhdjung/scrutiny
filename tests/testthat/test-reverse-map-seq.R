# Define example data -----------------------------------------------------

df_schlim <- tibble::tibble(y = 16:100, n = 3:87)
df_grim <- pigs1
df_debit <- pigs3


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
  .fun = schlim_map,
  .reported = c("y", "n"),
  .name_test = "SCHLIM",
  .name_class = "scrutiny_schlim_map_seq"
)


# Apply the reversal ------------------------------------------------------

df_schlim_rec <- df_schlim |>
  schlim_map_seq(include_consistent = TRUE) |>
  reverse_map_seq()

df_grim_rec <- df_grim |>
  grim_map_seq(digits_x = 2, include_consistent = TRUE) |>
  reverse_map_seq()

df_debit_rec <- df_debit |>
  debit_map_seq(digits_x = 2, digits_sd = 2, include_consistent = TRUE) |>
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
  df <- tibble::tibble(x = c(25.0, 24.9, 25.2), n = c(26, 25, 30))
  out <- df |> grim_map_seq(digits_x = 1, out_min = 25, out_max = 25, include_consistent = TRUE)
  out |> reverse_map_seq() |> expect_equal(df)
  audit <- out |> audit_seq()
  audit$x |> expect_equal(df$x)
  audit$n |> expect_equal(df$n)
  # With both limits at 25, the only dispersed value either variable can take is
  # 25 itself, so case 3 has one candidate per variable:
  audit$hits_x |> expect_equal(c(0L, 1L, 1L))
  audit$hits_n |> expect_equal(c(1L, 0L, 1L))
})
