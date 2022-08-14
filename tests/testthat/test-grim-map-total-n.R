

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


colnames_exp <- c(
  "x", "n", "n_change", "consistency", "both_consistent", "ratio", "case", "dir"
)


test_that("It has correct column names", {
  df1_tested %>% colnames() %>% expect_equal(colnames_exp)
  df2_tested %>% colnames() %>% expect_equal(colnames_exp)
})


