

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
  df1_tested %>% dim() %>% expect_equal(c(1200, 9))
  df2_tested %>% dim() %>% expect_equal(c(  48, 9))
})


colnames_exp <- c(
  "x", "n", "n_change", "items", "consistency", "both_consistent",
  "ratio", "case", "dir"
)


test_that("It has correct column names", {
  df1_tested %>% colnames() %>% expect_equal(colnames_exp)
  df2_tested %>% colnames() %>% expect_equal(colnames_exp)
})



# S3  method for `audit()` ------------------------------------------------

df1_audit <- df1_tested %>% audit()
df2_audit <- df2_tested %>% audit()

colnames_exp_audit <- c(
  "x1", "x2", "n", "hits_forth", "hits_back", "hits_total",
  "scenarios_total", "hit_rate"
)

test_that("[`audit()`] It has correct column names", {
  df1_audit %>% colnames() %>% expect_equal(colnames_exp_audit)
  df2_audit %>% colnames() %>% expect_equal(colnames_exp_audit)
})


test_that("It has correct values", {
  df2_audit$hits_forth      %>% unname() %>% expect_equal(c(2, 0))
  df2_audit$hits_back       %>% unname() %>% expect_equal(c(1, 0))
  df2_audit$hits_total      %>% unname() %>% expect_equal(c(3, 0))
  df2_audit$scenarios_total %>% unname() %>% expect_equal(c(12, 12))
  df2_audit$hit_rate        %>% unname() %>% expect_equal(c(0.25, 0))
})


