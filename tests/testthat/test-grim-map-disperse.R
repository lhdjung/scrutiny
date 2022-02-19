
df <- tibble::tibble(
  x1 = runif(20, 2, 8) %>% round(2) %>% restore_zeros(),
  x2 = runif(20, 2, 8) %>% round(2) %>% restore_zeros(),
  n  = runif(20, 30, 60) %>% round()
)

df_tested <- grim_map_disperse(df, dispersion = 0:5)


test_that("The output is a tibble", {
  df_tested %>% tibble::is_tibble() %>% expect_true()
})

test_that("It has the right dimensions", {
  df_tested %>% dim() %>% expect_equal(c(20, 6))
})

test_that("It has the right column names", {
  df_tested %>% colnames() %>% expect_equal(c(
    "x1", "x2", "n", "hits_total", "hits_forth", "hits_back"
  ))
})
