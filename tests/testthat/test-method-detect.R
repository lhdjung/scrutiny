

audit_iris <- iris %>%
  duplicate_detect() %>%
  audit()

audit_mtcars <- mtcars %>%
  duplicate_detect() %>%
  audit()

test_that("The output has correct dimensions", {
  expect_true(ncol(audit_iris) == 4)
  expect_true(nrow(audit_iris) == 5)
  expect_true(ncol(audit_mtcars) == 4)
  expect_true(nrow(audit_mtcars) == 12)
})


output_colnames <- c("variable", "n_duplicated", "n_total", "dup_rate")

test_that("The output has correct column names", {
  expect_equal(names(audit_iris), output_colnames)
  expect_equal(names(audit_mtcars), output_colnames)
})

test_that("The output has correct values", {
  expect_equal(audit_iris$n_duplicated, c(150, 148, 144, 150, 592))
  expect_equal(audit_mtcars$n_duplicated, c(32, 32, 32, 9, 20, 32,
                                            17, 16, 6, 32, 7, 235))
})
