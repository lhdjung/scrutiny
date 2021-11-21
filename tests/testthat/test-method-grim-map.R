

audit_pigs1 <- pigs1 %>%
  grim_map() %>%
  audit()

audit_pigs2 <- pigs2 %>%
  grim_map(percent = TRUE) %>%
  suppressMessages() %>%
  audit()



test_that("The output is a tibble", {
  expect_true(tibble::is_tibble(audit_pigs1))
  expect_true(tibble::is_tibble(audit_pigs2))
})


test_that("The output has correct dimensions", {
  expect_equal(ncol(audit_pigs1), 7)
  expect_equal(nrow(audit_pigs1), 1)
  expect_equal(ncol(audit_pigs2), 7)
  expect_equal(nrow(audit_pigs2), 1)
})


test_that("The output has correct values", {
  expect_equal(as.numeric(audit_pigs1$incons_cases), 8)
  expect_equal(as.numeric(audit_pigs2$incons_cases), 5)
  expect_equal(as.numeric(audit_pigs1$incons_rate), 8 / 12)
  expect_equal(as.numeric(audit_pigs2$incons_rate), 5 / 6)
})

