

df1 <- tibble::as_tibble(BJsales)
df1_test <- duplicate_count(df1)

vec1 <- df1$x
vec1_test <- duplicate_count(vec1)


test_that("With a data frame input, `duplicate_count()`
          returns a data frame with correct dimensions", {
  df1_test %>% nrow() %>% expect_equal(22)
  df1_test %>% ncol() %>% expect_equal(2)
})


test_that("With a numeric vector input, `duplicate_count()`
          returns a data frame with correct dimensions", {
  vec1_test %>% nrow() %>% expect_equal(22)
  vec1_test %>% ncol() %>% expect_equal(2)
})



df1_test_false <- duplicate_count(df1, numeric_only = FALSE)

test_that("The `numeric_only` argument makes no difference
          (when set to `FALSE` by the user) if each vector
          is numeric", {
  df1_test_false %>% expect_equal(vec1_test)
})




df2 <- tibble::as_tibble(iris)
df2_tested <- duplicate_count(df2)
df2_tested_false <- duplicate_count(df2, numeric_only = FALSE)


test_that("If `numeric_only` is `FALSE`, the dimensions are
          still the same", {
  df2_tested_false %>% nrow() %>% expect_equal(df2_tested %>% nrow())
  df2_tested_false %>% ncol() %>% expect_equal(df2_tested %>% ncol())
})





