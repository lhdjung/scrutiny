df1 <- tibble::tibble(
  x1 = runif(50, 2, 8) |> round(2),
  x2 = runif(50, 2, 8) |> round(2),
  n = runif(50, 50, 80) |> round()
)

# fmt: skip
df2 <- tibble::tribble(
  ~x1  , ~x2  , ~n  ,
  3.43 , 5.28 ,  90 ,
  2.97 , 4.42 , 103
)


# These expected outputs were created using `constructive::construct()`:

df2_rows_1_3_expected <- tibble::tibble(
  x = c(
    3.43,
    5.28,
    3.43,
    5.28,
    3.43,
    5.28,
    3.43,
    5.28,
    3.43,
    5.28,
    3.43,
    5.28,
    2.97,
    4.42,
    2.97,
    4.42,
    2.97,
    4.42,
    2.97,
    4.42,
    2.97,
    4.42,
    2.97,
    4.42,
    5.28,
    3.43,
    5.28,
    3.43,
    5.28,
    3.43,
    5.28,
    3.43,
    5.28,
    3.43,
    5.28,
    3.43,
    4.42,
    2.97,
    4.42,
    2.97,
    4.42,
    2.97,
    4.42,
    2.97,
    4.42,
    2.97,
    4.42,
    2.97
  ),
  n = rep(
    c(
      45L,
      45L,
      44L,
      46L,
      43L,
      47L,
      42L,
      48L,
      41L,
      49L,
      40L,
      50L,
      51L,
      52L,
      50L,
      53L,
      49L,
      54L,
      48L,
      55L,
      47L,
      56L,
      46L,
      57L
    ),
    2
  ),
  n_change = rep(c(0L, 0L, -1L, 1L, -2L, 2L, -3L, 3L, -4L, 4L, -5L, 5L), 4),
  items_averaged = rep(1, 48L),
  digits_x = rep(2, 48L),
  consistency = rep(
    c(
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE
    ),
    c(
      2L,
      2L,
      1L,
      2L,
      3L,
      2L,
      1L,
      1L,
      1L,
      1L,
      3L,
      1L,
      3L,
      1L,
      3L,
      3L,
      3L,
      2L,
      3L,
      1L,
      3L,
      1L,
      5L
    )
  ),
  both_consistent = rep(
    c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    c(2L, 2L, 6L, 2L, 16L, 2L, 18L)
  ),
  probability = grim_probability(x, n, 2, items_averaged = 1),
  case = rep(rep(1:2, 2), each = 12L),
  dir = factor(
    rep(c("forth", "back"), each = 24L),
    levels = c("forth", "back")
  ),
) |>
  structure(
    scrutiny = list(
      settings = dplyr::bind_rows(
        grim_settings_default,
        settings_tbl(
          list(dispersion = 0:5, n_min = 1L, n_max = NULL, constant = NULL),
          TRUE,
          "grim_map_total_n"
        )
      )
    ),
    class = c(
      "scrutiny_map_total_n",
      "scrutiny_grim_map_total_n",
      "scrutiny_grim_map",
      "tbl_df",
      "tbl",
      "data.frame"
    )
  )

# The 45/45 split of the even total 90 is tested in the "forth" direction only,
# so the "back" direction starts at the next step:
df2_rows_1_3_expected <- df2_rows_1_3_expected[-(25:26), ]


# The function itself -----------------------------------------------------

df1_tested <- df1 |>
  grim_map_total_n(digits_x = 2, items_averaged = 1, dispersion = 0:5)
df2_tested <- df2 |>
  grim_map_total_n(digits_x = 2, items_averaged = 1, dispersion = 0:5)


test_that("The output is a tibble", {
  df1_tested |> expect_s3_class("tbl_df")
  df2_tested |> expect_s3_class("tbl_df")
})

test_that("It has correct dimensions", {
  # The 50/50 split of an even total is tested in one direction only, so each
  # even total has two rows fewer:
  df1_tested |> dim() |> expect_equal(c(1200 - 2 * sum(df1$n %% 2 == 0), 10))
  df2_tested |> dim() |> expect_equal(c(  46, 10))
})

test_that("It has correct values", {
  # This doesn't work with `df1`; its values are randomly generated!
  df2 |> grim_map_total_n(digits_x = 2, items_averaged = 1) |> expect_equal(df2_rows_1_3_expected)
})


colnames_exp <- c(
  "x",
  "n",
  "n_change",
  "items_averaged",
  "digits_x",
  "consistency",
  "both_consistent",
  "probability",
  "case",
  "dir"
)


test_that("It has correct column names", {
  df1_tested |> expect_named(colnames_exp)
  df2_tested |> expect_named(colnames_exp)
})


# An `items` column used to be dropped, so the groups were tested with one item.
test_that("an `items_averaged` column works like the argument", {
  df <- tibble::tibble(x1 = 4.74, x2 = 2.57, n = 50L)
  from_arg <- df |> grim_map_total_n(digits_x = 2, items_averaged = 2)
  from_col <- df |> dplyr::mutate(items_averaged = 2) |> grim_map_total_n(digits_x = 2)
  from_col |> expect_equal(from_arg)
  df_grimmer <- tibble::tibble(x1 = 4.74, x2 = 2.57, sd1 = 1.21, sd2 = 0.98, n = 50L)
  from_arg <- df_grimmer |> grimmer_map_total_n(digits_x = 2, digits_sd = 2, items_averaged = 2)
  from_col <- df_grimmer |> dplyr::mutate(items_averaged = 2) |> grimmer_map_total_n(digits_x = 2, digits_sd = 2)
  from_col |> expect_equal(from_arg)
})
