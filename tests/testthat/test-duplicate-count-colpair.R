
df1 <- mtcars %>%
  duplicate_count_colpair()

test_that("The output has the right type, dimensions, and column names", {
  df1 %>% tibble::is_tibble() %>% expect_true()
  df1 %>% dim() %>% expect_equal(c(55, 5))
  df1 %>% colnames() %>% expect_equal(c("x", "y", "count", "rate_x", "rate_y"))
})

