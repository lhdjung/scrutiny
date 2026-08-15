# `audit()` ---------------------------------------------------------------

# `audit()` for GRIM
data_grim <- grim_map(pigs1, digits_x = 2)
audit_grim <- audit(data_grim)

test_that("`audit()` summarizes GRIM tests accurately", {
  expect_s3_class(audit_grim, "data.frame")
  expect_equal(as.numeric(audit_grim$all_cases - audit_grim$incons_cases),
               length(data_grim$consistency[data_grim$consistency]))
})


# `audit()` for DEBIT
data_debit <- debit_map(pigs3, digits_x = 2, digits_sd = 2)
audit_debit <- audit(data_debit)

test_that("`audit()` summarizes DEBIT tests accurately", {
  expect_equal(as.numeric(audit_debit$incons_cases), 1)
  expect_true(dplyr::near(as.numeric(audit_debit$all_cases), 7))
  expect_true(dplyr::near(round(audit_debit$incons_rate, 3), 0.143))
})


# `audit_seq()` -----------------------------------------------------------

data_grim_seq <- grim_map_seq(pigs1, digits_x = 2)
data_grimmer_seq <- grimmer_map_seq(pigs5, digits_x = 2, digits_sd = 2)
data_debit_seq <- debit_map_seq(pigs3, digits_x = 2, digits_sd = 2)

# The scrutiny class is removed for the GRIM tibble because the latter is tested
# as an example for equality with tibbles that don't have that class:
audit_seq_grim <- data_grim_seq |> audit_seq() |> unclass_scr()
audit_seq_grimmer <- data_grimmer_seq |> audit_seq()
audit_seq_debit <- data_debit_seq |> audit_seq()

data_seq_grim_different_dispersion1 <- tibble::tibble(
  x = 4.74,
  n = 25L,
  consistency = FALSE,
  hits_total = 1L,
  hits_x = 0L,
  hits_n = 1L,
  diff_x = NA_integer_,
  diff_x_up = NA_integer_,
  diff_x_down = NA_integer_,
  diff_n = 9L,
  diff_n_up = 9L,
  diff_n_down = NA_integer_,
) |>
  structure(class = c("scrutiny_audit_seq", "tbl_df", "tbl", "data.frame"))

data_seq_grim_different_dispersion2 <- tibble::tibble(
  x = 5.23,
  n = 29L,
  consistency = FALSE,
  hits_total = 3L,
  hits_x = 1L,
  hits_n = 2L,
  diff_x = 5L,
  diff_x_up = 5L,
  diff_x_down = NA_integer_,
  diff_n = 3L,
  diff_n_up = NA_integer_,
  diff_n_down = -3L,
) |>
  structure(class = c("scrutiny_audit_seq", "tbl_df", "tbl", "data.frame"))

data_incons <- pigs1 |>
  grim_map(digits_x = 2) |>
  dplyr::filter(!consistency) |>
  dplyr::select(x, n, consistency) |>
  unclass_scr()


test_that("`audit_seq()` has correct output", {
  audit_seq_grim |> dim() |> expect_equal(c(8, 12))
  audit_seq_grim[1:3] |> expect_equal(data_incons)
  audit_seq_grim[[4]] |> expect_equal(c(4, 6, 6, 7, 3, 6, 8, 6))
  audit_seq_grim[[5]] |> expect_equal(c(2, 3, 3, 3, 3, 3, 4, 3))
  audit_seq_grim[[6]] |> expect_equal(c(2, 3, 3, 4, 0, 3, 4, 3))
  audit_seq_grim[[7]] |> expect_equal(c(2, 1, 1, 1, 1, 1, 1, 1))
})


hits_total_is_correct <- function(audit_seq_output) {
  expected <- audit_seq_output |>
    dplyr::rowwise() |>
    dplyr::select(starts_with("hits"), -hits_total) |>
    dplyr::mutate(hits_total_expected = sum(dplyr::c_across(everything()))) |>
    dplyr::pull(hits_total_expected)
  all(expected == audit_seq_output$hits_total)
}


test_that("the `hits_total` column correctly sums up
          the other `hits_` columns", {
  audit_seq_grim    |> hits_total_is_correct() |> expect_true()
  audit_seq_grimmer |> hits_total_is_correct() |> expect_true()
  audit_seq_debit   |> hits_total_is_correct() |> expect_true()
})


test_that("`audit_seq()` replays the original call's test arguments
          when re-testing the reconstructed data", {
  # 71% of 43 participants is GRIM-inconsistent. `percent` changes the verdict
  # but leaves no trace in the output columns, so it used to be dropped by
  # `audit_seq()`, whose `consistency` column then wrongly said `TRUE`:
  tibble::tibble(x = 71, n = 43) |>
    grim_map_seq(digits_x = 0, percent = TRUE, include_consistent = TRUE) |>
    audit_seq() |>
    dplyr::pull(consistency) |>
    expect_false()

  # Same for `threshold`: 0.24 with `n = 21` is consistent when rounding up
  # from 5 but not when rounding up from 9:
  tibble::tibble(x = 0.24, n = 21) |>
    grim_map_seq(
      digits_x = 2,
      rounding = "up_from",
      threshold = 9,
      include_consistent = TRUE
    ) |>
    audit_seq() |>
    dplyr::pull(consistency) |>
    expect_false()
})


test_that("changing `dispersion` in the sequence mapper is
          correctly captured by `audit_seq()`", {
  pigs1[1:2, ] |>
    grim_map_seq(digits_x = 2, dispersion = c(7, 8, 9)) |>
    audit_seq() |>
    expect_equal(data_seq_grim_different_dispersion1)

  tibble::tibble(x = 5.23, n = 29) |>
    grim_map_seq(digits_x = 2, dispersion = c(3, 5, 7)) |>
    audit_seq() |>
    expect_equal(data_seq_grim_different_dispersion2)
})


test_that("`audit_seq()` orders its columns by `var`", {
  # `split()` sorts its groups alphabetically, and undoing that sort takes
  # `rank()`, not `order()`. The two are inverses of each other and agree only
  # up to three variables that don't form a cycle.
  out <- pigs5[1:3, ] |>
    grimmer_map_seq(digits_x = 2, digits_sd = 2, var = c("sd", "x", "n")) |>
    audit_seq()
  grep("^hits_", colnames(out), value = TRUE) |>
    expect_equal(c("hits_total", "hits_sd", "hits_x", "hits_n"))
})


test_that("`audit_seq()` results don't depend on the order of `var`", {
  a <- pigs5[1:3, ] |>
    grimmer_map_seq(digits_x = 2, digits_sd = 2) |>
    audit_seq()
  b <- pigs5[1:3, ] |>
    grimmer_map_seq(digits_x = 2, digits_sd = 2, var = c("n", "x", "sd")) |>
    audit_seq()
  a[, sort(colnames(a))] |> expect_equal(b[, sort(colnames(b))])
})


# `audit_cols_minimal()` has always counted inconsistencies with `na.rm`,
# because a case the test could not decide is not an inconsistent one. The
# summaries built on top of it did not, so a single missing `n` turned
# `mean_grim_prob` -- and `incons_to_prob`, derived from it -- into `NA` for
# the whole table.

test_that("`audit()` summarizes the decidable cases despite an undecidable one", {
  out <- grim_map(
    tibble::tibble(x = c(7.22, 5.19), n = c(38, NA)),
    digits_x = 2
  ) |>
    audit()

  out$mean_grim_prob |> is.na() |> expect_false()
  out$incons_to_prob |> is.na() |> expect_false()
  out$all_cases |> expect_equal(2L)
  out$incons_cases |> expect_equal(1L)
})

test_that("`audit()` on DEBIT output ignores missing values in its means", {
  out <- debit_map(
    tibble::tibble(x = c(0.53, NA), sd = c(0.5, 0.5), n = c(1683L, 1683L)),
    digits_x = 2,
    digits_sd = 2
  ) |>
    audit()

  out$mean_x |> is.na() |> expect_false()
  out$mean_x |> expect_equal(0.53)
})
