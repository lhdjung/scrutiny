# NOTE: `debit_scalar()` is tested in test-debit.R and test-debit-map.R, so the
# present file only tests the input-range checks.

x1 <- pigs3$x
x2 <- pigs3$sd
x3 <- randu$x

x4 <- iris$Sepal.Length
x5 <- Orange$age
x6 <- Loblolly$height

x7 <- c(0.1, 0.5, 12)


test_that("`check_debit_inputs()` remains silent when it should", {
  x1 |> check_debit_inputs("dummy", "also dummy") |> expect_silent()
  x2 |> check_debit_inputs("dummy", "also dummy") |> expect_silent()
  x3 |> check_debit_inputs("dummy", "also dummy") |> expect_silent()
})


test_that("`check_debit_inputs()` throws an error when it should", {
  x4 |> check_debit_inputs("dummy", "also dummy") |> expect_error()
  x5 |> check_debit_inputs("dummy", "also dummy") |> expect_error()
})


test_that("It throws an error, even with only a single offender", {
  x7 |> check_debit_inputs("dummy", "also dummy") |> expect_error()
})


test_that("`check_debit_inputs_all()` remains silent when it should", {
  x1 |> check_debit_inputs_all(x2) |> expect_silent()
  x2 |> check_debit_inputs_all(x3) |> expect_silent()
  x3 |> check_debit_inputs_all(x1) |> expect_silent()
})


test_that("`check_debit_inputs_all()` throws an error when it should", {
  x4 |> check_debit_inputs_all(x5) |> expect_error()
  x5 |> check_debit_inputs_all(x6) |> expect_error()
  x6 |> check_debit_inputs_all(x4) |> expect_error()
})


test_that("`check_debit_inputs_all()` throws an error when it should,
          even if only one one of the two vectors contains offenders", {
  x1 |> check_debit_inputs_all(x5) |> expect_error()
  x2 |> check_debit_inputs_all(x6) |> expect_error()
  x3 |> check_debit_inputs_all(x4) |> expect_error()
})
