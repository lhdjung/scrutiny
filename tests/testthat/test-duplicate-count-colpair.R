
# Example input -----------------------------------------------------------

df1 <- mtcars

df2 <- tibble(
  a = 1:5,
  b = 3:7,
  c = c(5, NA, NA, 8, 9)
)

df3 <- tibble::tibble(
  a = c(1:3, NA, 5),
  b = c(NA, 3:6),
  c = c(3:4, NA, NA, NA)
)


# Expected output ---------------------------------------------------------

# Created via `constructive::construct()`:
df1_exp <- tibble::tibble(
  x = rep(
    c(
      "cyl", "vs", "gear", "vs", "am", "cyl", "drat", "mpg", "drat", "mpg", "cyl",
      "disp", "hp", "drat", "wt", "qsec", "vs", "am"
    ),
    c(
      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 9L, 7L, 8L, 7L, 3L, 5L,
      4L, 1L, 1L
    )
  ),
  y = c(
    "carb", "am", "carb", "carb", "carb", "gear", "wt", "qsec", "gear", "carb",
    "cyl", "disp", "hp", "drat", "wt", "vs", "am", "gear", "carb", "disp", "hp",
    "drat", "wt", "qsec", "vs", "am", "hp", "drat", "wt", "qsec", "vs", "am",
    "gear", "carb", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "qsec",
    "vs", "am", "qsec", "vs", "am", "gear", "carb", "vs", "am", "gear", "carb",
    "gear", "gear"
  ),
  count = rep(
    c(32L, 27L, 14L, 13L, 11L, 3L, 2L, 1L, 0L),
    rep(c(2L, 1L, 2L, 45L), c(1L, 6L, 1L, 1L))
  ),
  total_x = rep(32L, 55L),
  total_y = rep(32L, 55L),
  rate_x = rep(
    c(1, 0.84375, 0.4375, 0.40625, 0.34375, 0.09375, 0.0625, 0.03125, 0),
    rep(c(2L, 1L, 2L, 45L), c(1L, 6L, 1L, 1L))
  ),
  rate_y = rep(
    c(1, 0.84375, 0.4375, 0.40625, 0.34375, 0.09375, 0.0625, 0.03125, 0),
    rep(c(2L, 1L, 2L, 45L), c(1L, 6L, 1L, 1L))
  ),
) %>%
  structure(class = c("scr_dup_count_colpair", "tbl_df", "tbl", "data.frame"))


df2_exp <- tibble::tibble(
  x = c("a", "a", "b"),
  y = c("b", "c", "c"),
  count = c(3L, 1L, 1L),
  total_x = rep(5L, 3L),
  total_y = c(5L, 3L, 3L),
  rate_x = c(0.6, 0.2, 0.2),
  rate_y = c(0.6, 0.3333333333333333148296, 0.3333333333333333148296),
) %>%
  structure(class = c("scr_dup_count_colpair", "tbl_df", "tbl", "data.frame"))

df2_ignore_exp <- tibble::tibble(
  x = c("a", "a", "b"),
  y = c("b", "c", "c"),
  count = c(2L, 1L, 1L),
  total_x = rep(4L, 3L),
  total_y = c(4L, 3L, 3L),
  rate_x = c(0.5, 0.25, 0.25),
  rate_y = c(0.5, 0.3333333333333333148296, 0.3333333333333333148296),
) %>%
  structure(class = c("scr_dup_count_colpair", "tbl_df", "tbl", "data.frame"))

df3_exp <- tibble::tibble(
  x = c("a", "b", "a"),
  y = c("b", "c", "c"),
  count = c(2L, 2L, 1L),
  total_x = rep(4L, 3L),
  total_y = c(4L, 2L, 2L),
  rate_x = c(0.5, 0.5, 0.25),
  rate_y = c(0.5, 1, 0.5),
) %>%
  structure(class = c("scr_dup_count_colpair", "tbl_df", "tbl", "data.frame"))


# Testing -----------------------------------------------------------------

test_that("`duplicate_count_colpair()` works correctly by default", {
  df1 %>% duplicate_count_colpair() %>% expect_equal(df1_exp)
  df2 %>% duplicate_count_colpair() %>% expect_equal(df2_exp)
  df3 %>% duplicate_count_colpair() %>% expect_equal(df3_exp)
})

test_that("`duplicate_count_colpair()` with `ignore` works correctly", {
  df2 %>% duplicate_count_colpair(ignore = 3) %>% expect_equal(df2_ignore_exp)
})

