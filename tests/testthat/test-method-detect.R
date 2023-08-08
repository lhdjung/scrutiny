
# Expected output ---------------------------------------------------------

pigs4_exp <- tibble::tibble(
  term = c("snout", "tail", "wings", ".total"),
  dup_count = c(4L, 2L, 1L, 7L),
  total_count = c(5L, 5L, 5L, 15L),
  dup_rate = c(0.8, 0.4, 0.2, 0.4666666666666666740682),
)

iris_exp <- tibble::tibble(
  term = c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "Species", ".total"),
  dup_count = c(150L, 148L, 144L, 150L, 150L, 742L),
  total_count = rep(c(150L, 750L), c(5L, 1L)),
  dup_rate = c(1, 0.9866666666666666918317, 0.96, 1, 1, 0.989333333333333286852),
)

mtcars_exp <- tibble::tibble(
  term = c("am", "carb", "cyl", "disp", "drat", "gear", "hp", "mpg", "qsec", "vs", "wt", ".total"),
  dup_count = c(32L, 32L, 32L, 9L, 20L, 32L, 17L, 16L, 6L, 32L, 7L, 235L),
  total_count = rep(c(32L, 352L), c(11L, 1L)),
  dup_rate = c(1, 1, 1, 0.28125, 0.625, 1, 0.53125, 0.5, 0.1875, 1, 0.21875, 0.6676136363636363535434),
)


# With some values ignored:
pigs4_ignore_exp <- tibble::tibble(
  term = c("snout", "tail", "wings", ".total"),
  dup_count = c(4L, 2L, 1L, 7L),
  total_count = c(5L, 5L, 5L, 15L),
  dup_rate = c(0.8, 0.4, 0.2, 0.4666666666666666740682),
)

iris_ignore_exp <- tibble::tibble(
  term = c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "Species", ".total"),
  dup_count = c(130L, 140L, 134L, 148L, 150L, 702L),
  total_count = rep(c(150L, 750L), c(5L, 1L)),
  dup_rate = c(
    0.8666666666666666962726, 0.9333333333333333481363, 0.8933333333333333126092,
    0.9866666666666666918317, 1, 0.936
  ),
)

mtcars_ignore_exp <- tibble::tibble(
  term = c("am", "carb", "cyl", "disp", "drat", "gear", "hp", "mpg", "qsec", "vs", "wt", ".total"),
  dup_count = c(32L, 21L, 14L, 9L, 20L, 20L, 17L, 14L, 6L, 32L, 7L, 192L),
  total_count = rep(c(32L, 352L), c(11L, 1L)),
  dup_rate = c(
    1, 0.65625, 0.4375, 0.28125, 0.625, 0.625, 0.53125, 0.4375, 0.1875, 1,
    0.21875, 0.5454545454545454141737
  ),
)


# Testing -----------------------------------------------------------------

test_that("`audit()` for `duplicate_detect()` works correctly by default", {
  pigs4  %>% duplicate_detect() %>% audit() %>% expect_equal(pigs4_exp)
  iris   %>% duplicate_detect() %>% audit() %>% expect_equal(iris_exp)
  mtcars %>% duplicate_detect() %>% audit() %>% expect_equal(mtcars_exp)
})

test_that("`audit()` for `duplicate_detect()` works correctly with some values ignored", {
  pigs4 %>%
    duplicate_detect(ignore = c(4.221, 6.887)) %>%
    audit() %>%
    expect_equal(pigs4_ignore_exp)

  iris %>%
    duplicate_detect(ignore = c(5, 3.9, 1.4)) %>%
    audit() %>%
    expect_equal(iris_ignore_exp)

  mtcars %>%
    duplicate_detect(ignore = c(19.2, 6, 4)) %>%
    audit() %>%
    expect_equal(mtcars_ignore_exp)
})

