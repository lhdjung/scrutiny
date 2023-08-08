

df1 <- tibble::tibble(
  x1 = runif(50, 2, 8)   %>% round(2) %>% restore_zeros(2),
  x2 = runif(50, 2, 8)   %>% round(2) %>% restore_zeros(2),
  n  = runif(50, 50, 80) %>% round()
)

df2 <- tibble::tribble(
  ~x1,    ~x2,   ~n,
  "3.43", "5.28", 90,
  "2.97", "4.42", 103
)


# These expected outputs were created using `constructive::construct()`:

df2_rows_1_3_expected <- tibble::tibble(
  x = c(
    "3.43", "5.28", "3.43", "5.28", "3.43", "5.28", "3.43", "5.28", "3.43",
    "5.28", "3.43", "5.28", "2.97", "4.42", "2.97", "4.42", "2.97", "4.42",
    "2.97", "4.42", "2.97", "4.42", "2.97", "4.42", "5.28", "3.43", "5.28",
    "3.43", "5.28", "3.43", "5.28", "3.43", "5.28", "3.43", "5.28", "3.43",
    "4.42", "2.97", "4.42", "2.97", "4.42", "2.97", "4.42", "2.97", "4.42",
    "2.97", "4.42", "2.97"
  ),
  n = rep(
    c(45, 45, 44, 46, 43, 47, 42, 48, 41, 49, 40, 50, 51, 52, 50, 53, 49, 54, 48, 55, 47, 56, 46, 57),
    2
  ),
  n_change = rep(c(0, 0, -1, 1, -2, 2, -3, 3, -4, 4, -5, 5), 4),
  consistency = rep(
    c(
      FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE,
      TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE,
      TRUE, FALSE, TRUE, FALSE
    ),
    c(
      2L, 2L, 1L, 2L, 3L, 2L, 1L, 1L, 1L, 1L, 3L, 1L, 3L, 1L, 3L,
      3L, 3L, 2L, 3L, 1L, 3L, 1L, 5L
    )
  ),
  both_consistent = c(
    "1" = FALSE, "1" = FALSE, "2" = TRUE, "2" = TRUE, "3" = FALSE,
    "3" = FALSE, "4" = FALSE, "4" = FALSE, "5" = FALSE, "5" = FALSE,
    "6" = TRUE, "6" = TRUE, "7" = FALSE, "7" = FALSE, "8" = FALSE,
    "8" = FALSE, "9" = FALSE, "9" = FALSE, "10" = FALSE, "10" = FALSE,
    "11" = FALSE, "11" = FALSE, "12" = FALSE, "12" = FALSE, "1" = FALSE,
    "1" = FALSE, "2" = FALSE, "2" = FALSE, "3" = TRUE, "3" = TRUE,
    "4" = FALSE, "4" = FALSE, "5" = FALSE, "5" = FALSE, "6" = FALSE,
    "6" = FALSE, "7" = FALSE, "7" = FALSE, "8" = FALSE, "8" = FALSE,
    "9" = FALSE, "9" = FALSE, "10" = FALSE, "10" = FALSE, "11" = FALSE,
    "11" = FALSE, "12" = FALSE, "12" = FALSE
  ),
  ratio = rep(
    c(
      0.55, 0.55, 0.56, 0.54, 0.57, 0.53, 0.58, 0.52, 0.59, 0.51, 0.6, 0.5, 0.49,
      0.48, 0.5, 0.47, 0.51, 0.46, 0.52, 0.45, 0.53, 0.44, 0.54, 0.43
    ), 2
  ),
  case = rep(rep(1:2, 2), each = 12L),
  dir = rep(c("forth", "back"), each = 24L),
) %>%
  structure(
    class = c(
      "scr_map_total_n", "scr_grim_map", "scr_rounding_up_or_down", "tbl_df", "tbl",
      "data.frame"
    )
  )



# The function itself -----------------------------------------------------

df1_tested <- df1 %>% grim_map_total_n(dispersion = 0:5)
df2_tested <- df2 %>% grim_map_total_n(dispersion = 0:5)


test_that("The output is a tibble", {
  df1_tested %>% tibble::is_tibble() %>% expect_true()
  df2_tested %>% tibble::is_tibble() %>% expect_true()
})

test_that("It has correct dimensions", {
  df1_tested %>% dim() %>% expect_equal(c(1200, 8))
  df2_tested %>% dim() %>% expect_equal(c(  48, 8))
})

test_that("It has correct values", {
  # This doesn't work with `df1`; its values are randomly generated!
  df2 %>% grim_map_total_n() %>% expect_equal(df2_rows_1_3_expected)
})


colnames_exp <- c(
  "x", "n", "n_change", "consistency", "both_consistent", "ratio", "case", "dir"
)


test_that("It has correct column names", {
  df1_tested %>% colnames() %>% expect_equal(colnames_exp)
  df2_tested %>% colnames() %>% expect_equal(colnames_exp)
})


