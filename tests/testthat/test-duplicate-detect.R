# Expected output ---------------------------------------------------------

pigs4_exp <- tibble::tibble(
  snout = c("4.73", "8.13", "4.22", "4.22", "5.17"),
  snout_dup = rep(c(FALSE, TRUE), c(1L, 4L)),
  tail = c("6.88", "7.33", "5.17", "7.57", "8.13"),
  tail_dup = c(FALSE, FALSE, TRUE, FALSE, TRUE),
  wings = c("6.09", "8.27", "4.4", "5.92", "5.17"),
  wings_dup = rep(c(FALSE, TRUE), c(4L, 1L)),
) |>
  structure(class = c("scrutiny_dup_detect", "tbl_df", "tbl", "data.frame"))

pigs4_missings <- pigs4
pigs4_missings[3, 1] <- NA
pigs4_missings[5, 2] <- NA

pigs4_missings_exp <- tibble::tibble(
  snout = c("4.73", "8.13", NA, "4.22", "5.17"),
  snout_dup = c(FALSE, FALSE, NA, FALSE, TRUE),
  tail = c("6.88", "7.33", "5.17", "7.57", NA),
  tail_dup = c(FALSE, FALSE, TRUE, FALSE, NA),
  wings = c("6.09", "8.27", "4.4", "5.92", "5.17"),
  wings_dup = rep(c(FALSE, TRUE), c(4L, 1L)),
) |>
  structure(class = c("scrutiny_dup_detect", "tbl_df", "tbl", "data.frame"))


# Testing -----------------------------------------------------------------

test_that("`duplicate_detect()` is deprecated", {
  pigs4 |> duplicate_detect() |> lifecycle::expect_deprecated()
})

test_that("`duplicate_detect()` works correctly by default", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  pigs4 |> duplicate_detect() |> expect_equal(pigs4_exp)
})

test_that("`duplicate_detect()` works correctly with missings", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  pigs4_missings |> duplicate_detect() |> expect_equal(pigs4_missings_exp)
})

test_that("`duplicate_tally()` does not count missing values as matches", {
  # `x == x[i]` is `NA` against a missing value, and indexing by `NA` returns an
  # element, so every count was off by the number of missing values:
  c(1, 1, NA) |> duplicate_tally() |> purrr::pluck("value_n") |> expect_equal(c(2L, 2L, NA))
  out <- tibble::tibble(a = c(1, 2), b = c(1, NA)) |> duplicate_tally()
  out$a_n |> expect_equal(c(2L, 1L))
  out$b_n |> expect_equal(c(2L, NA))
})


test_that("`duplicate_detect()` and `duplicate_tally()` work without rows and on matrices", {
  rlang::local_options(lifecycle_verbosity = "quiet")
  numeric(0) |> duplicate_detect() |> colnames() |> expect_equal(c("value", "value_dup"))
  pigs4[0, ] |> duplicate_tally()  |> colnames() |> expect_equal(c("snout", "snout_n", "tail", "tail_n", "wings", "wings_n"))
  c(1, 1, 2, 3) |>
    matrix(nrow = 2) |>
    duplicate_detect() |>
    expect_equal(tibble::tibble(col1 = c("1", "1"), col1_dup = TRUE, col2 = c("2", "3"), col2_dup = FALSE), ignore_attr = "class")
})


# `as.character(100000)` is `"1e+05"`, so an integer and a double `100000` were
# no duplicates:
test_that("`duplicate_tally()` compares numbers as written out in full", {
  out <- tibble::tibble(a = c(100000L, 2L), b = c(100000, 0.0001)) |> duplicate_tally()
  out$a_n |> expect_equal(c(2L, 1L))
  out$b_n |> expect_equal(c(2L, 1L))
  tibble::tibble(a = 100000L, b = 100000) |> duplicate_tally(ignore = 1e5) |> dplyr::pull(a_n) |> expect_na()
})
