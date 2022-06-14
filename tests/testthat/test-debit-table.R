

# NOTE: The tests for `debit_table()` are implicitly included in
# test-debit-map.R, so the present file only tests the checks.

x1 <- pigs3$x
x2 <- pigs3$sd
x3 <- randu$x

x4 <- iris$Sepal.Length
x5 <- Orange$age
x6 <- Loblolly$height

x7 <- c(0.1, 0.5, 12)



test_that("`check_debit_inputs()` remains silent when it should", {
  check_debit_inputs(x1, "dummy", "also dummy") %>% expect_silent()
  check_debit_inputs(x2, "dummy", "also dummy") %>% expect_silent()
  check_debit_inputs(x3, "dummy", "also dummy") %>% expect_silent()
})


test_that("`check_debit_inputs()` throws an error when it should", {
  check_debit_inputs(x4, "dummy", "also dummy") %>% expect_error()
  check_debit_inputs(x5, "dummy", "also dummy") %>% expect_error()
})


test_that("It throws an error, even with only a single offender", {
  check_debit_inputs(x7, "dummy", "also dummy") %>% expect_error()
})




test_that("`check_debit_inputs_all()` remains silent when it should", {
  check_debit_inputs_all(x1, x2) %>% expect_silent()
  check_debit_inputs_all(x2, x3) %>% expect_silent()
  check_debit_inputs_all(x3, x1) %>% expect_silent()
})


test_that("`check_debit_inputs_all()` throws an error when it should", {
  check_debit_inputs_all(x4, x5) %>% expect_error()
  check_debit_inputs_all(x5, x6) %>% expect_error()
  check_debit_inputs_all(x6, x4) %>% expect_error()
})


test_that("`check_debit_inputs_all()` throws an error when it should,
          even if only one one of the two vectors contains offenders", {
  check_debit_inputs_all(x1, x5) %>% expect_error()
  check_debit_inputs_all(x2, x6) %>% expect_error()
  check_debit_inputs_all(x3, x4) %>% expect_error()
})




