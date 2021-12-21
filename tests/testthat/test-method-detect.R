

audit_iris <- iris %>%
  duplicate_detect() %>%
  audit()

audit_mtcars <- mtcars %>%
  duplicate_detect() %>%
  audit()

test_that("The output has correct dimensions", {
  audit_iris   %>% ncol() %>% expect_equal(4)
  audit_iris   %>% nrow() %>% expect_equal(5)
  audit_mtcars %>% ncol() %>% expect_equal(4)
  audit_mtcars %>% nrow() %>% expect_equal(12)
})


output_colnames <- c("variable", "n_duplicated", "n_total", "dup_rate")

test_that("The output has correct column names", {
  names(audit_iris)   %>% expect_equal(output_colnames)
  names(audit_mtcars) %>% expect_equal(output_colnames)
})

n_dup_expected_iris   <- c(150, 148, 144, 150, 592)
n_dup_expected_mtcars <- c(32, 32, 32, 9, 20, 32, 17, 16, 6, 32, 7, 235)

test_that("The output has correct values", {
  audit_iris$n_duplicated   %>% expect_equal(n_dup_expected_iris)
  audit_mtcars$n_duplicated %>% expect_equal(n_dup_expected_mtcars)
})
