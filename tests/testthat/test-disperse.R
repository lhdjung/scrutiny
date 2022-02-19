

df1 <- disperse(n = 20, dispersion = 0:5)

test_that("The function returns a tibble", {
  tibble::is_tibble(df1) %>% expect_true()
})

test_that("It has the right dimensions", {
  (nrow(df1) == 12) %>% expect_true()
  (ncol(df1) == 2) %>% expect_true()
})

test_that("It starts with the right values", {
  (df1$n[1] == 20) %>% expect_true()
  (df1$n_change[1] == "n_minus_0") %>% expect_true()
})

test_that("It ends with the right values", {
  (df1$n[12] == 25) %>% expect_true()
  (df1$n_change[12] == "n_plus_5") %>% expect_true()
})


df2 <- disperse(0, dispersion = 0:5, n_min = 1)

test_that("", {
  (dim(df2) == c(0, 2)) %>% all() %>% expect_true()
})

