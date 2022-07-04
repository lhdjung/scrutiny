

df1 <- disperse(n = 20, dispersion = 0:5)

test_that("The function returns a tibble", {
  df1 %>% tibble::is_tibble() %>% expect_true()
})

test_that("It has the right dimensions", {
  df1 %>% nrow() %>% expect_equal(12)
  df1 %>% ncol() %>% expect_equal(2)
})

test_that("It starts with the right values", {
  df1$n[1] %>% expect_equal(20)
  df1$n_change[1] %>% expect_equal(0)
})

test_that("It ends with the right values", {
  df1$n[12] %>% expect_equal(25)
  df1$n_change[12] %>% expect_equal(5)
})


df2 <- disperse(0, dispersion = 0:5, n_min = 1)

test_that("`n_min` controls the dimensions correctly", {
  df2 %>% dim() %>% expect_equal(c(0, 2))
})

