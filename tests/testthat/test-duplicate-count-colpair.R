
df1 <- mtcars %>%
  duplicate_count_colpair()

test_that("The output has the right type, dimensions, and column names", {
  df1 %>% tibble::is_tibble() %>% expect_true()
  df1 %>% dim() %>% expect_equal(c(55, 5))
  df1 %>% colnames() %>% expect_equal(c("x", "y", "count", "rate_x", "rate_y"))
})

# Created via `dput()`, then manually restructured with copy and paste:
df1_exp <- structure(
  list(x = c(
    "cyl", "vs", "gear", "vs", "am", "cyl",
    "drat", "mpg", "drat", "drat", "mpg", "mpg", "mpg", "mpg", "mpg",
    "mpg", "mpg", "mpg", "mpg", "cyl", "cyl", "cyl", "cyl", "cyl",
    "cyl", "cyl", "disp", "disp", "disp", "disp", "disp", "disp",
    "disp", "disp", "hp", "hp", "hp", "hp", "hp", "hp", "hp", "drat",
    "drat", "drat", "wt", "wt", "wt", "wt", "wt", "qsec", "qsec",
    "qsec", "qsec", "vs", "am"
  ), y = c(
    "carb", "am", "carb", "carb",
    "carb", "gear", "wt", "qsec", "gear", "carb", "cyl", "disp",
    "hp", "drat", "wt", "vs", "am", "gear", "carb", "disp", "hp",
    "drat", "wt", "qsec", "vs", "am", "hp", "drat", "wt", "qsec",
    "vs", "am", "gear", "carb", "drat", "wt", "qsec", "vs", "am",
    "gear", "carb", "qsec", "vs", "am", "qsec", "vs", "am", "gear",
    "carb", "vs", "am", "gear", "carb", "gear", "gear"
  ), count = c(
    32L,
    32L, 27L, 14L, 13L, 11L, 3L, 2L, 1L, 1L, 0L, 0L, 0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
    0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
  ), rate_x = c(
    1, 1, 0.84375, 0.4375,
    0.40625, 0.34375, 0.09375, 0.0625, 0.03125, 0.03125, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

  ), rate_y = c(
    1, 1, 0.84375, 0.4375, 0.40625, 0.34375, 0.09375,
    0.0625, 0.03125, 0.03125, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  )), row.names = c(NA, -55L), class = c(
    "scr_dup_count_colpair",
    "tbl_df", "tbl", "data.frame"
  )
)


test_that("The output has the right values", {
  df1 %>% expect_equal(df1_exp)
})

