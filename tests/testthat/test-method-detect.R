
# Expected output ---------------------------------------------------------

pigs4_exp <- tibble::tibble(
  variable = c("snout", "tail", ".total"),
  n_duplicated = c(2L, 2L, 4L),
  n_total = c(5L, 5L, 10L),
  dup_rate = rep(0.4, 3L),
)

iris_exp <- tibble::tibble(
  variable = c(
    "Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width",
    "Species", ".total"
  ),
  n_duplicated = c(150L, 147L, 149L, 148L, 148L, 742L),
  n_total = rep(c(150L, 750L), c(5L, 1L)),
  dup_rate = c(
    1, 0.98, 0.9933333333333332904047, 0.9866666666666666918317,
    0.9866666666666666918317, 0.989333333333333286852
  ),
)

mtcars_exp <- tibble::tibble(
  variable = c(
    "am", "carb", "cyl", "disp", "drat", "gear", "hp", "mpg", "qsec",
    "vs", "wt", ".total"
  ),
  n_duplicated = c(23L, 19L, 23L, 20L, 25L, 16L, 27L, 24L, 17L, 22L, 19L, 235L),
  n_total = rep(c(32L, 352L), c(11L, 1L)),
  dup_rate = c(
    0.71875, 0.59375, 0.71875, 0.625, 0.78125, 0.5, 0.84375, 0.75, 0.53125,
    0.6875, 0.59375, 0.6676136363636363535434
  ),
)

# With some values ignored:
pigs4_ignore_exp <- tibble::tibble(
  variable = c("snout", "tail", ".total"),
  n_duplicated = c(1L, 1L, 2L),
  n_total = c(5L, 5L, 10L),
  dup_rate = rep(0.2, 3L),
)

iris_ignore_exp <- tibble::tibble(
  variable = c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width", "Species", ".total"),
  n_duplicated = c(3L, 7L, 5L, 7L, 6L, 28L),
  n_total = rep(c(10L, 50L), c(5L, 1L)),
  dup_rate = c(0.3, 0.7, 0.5, 0.7, 0.6, 0.56),
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
    dplyr::slice(1:10) %>%
    duplicate_detect(ignore = c(5, 3.9, 1.4)) %>%
    audit() %>%
    expect_equal(iris_ignore_exp)
})

