

audit_pigs1 <- pigs1 %>%
  grim_map() %>%
  audit()

audit_pigs2 <- pigs2 %>%
  grim_map(percent = TRUE) %>%
  suppressMessages() %>%
  audit()



test_that("The output is a tibble", {
  audit_pigs1 %>% expect_s3_class("tbl_df")
  audit_pigs2 %>% expect_s3_class("tbl_df")
})


test_that("The output has correct dimensions", {
  audit_pigs1 %>% dim() %>% expect_equal(c(1, 7))
  audit_pigs2 %>% dim() %>% expect_equal(c(1, 7))
})


test_that("The output has correct values", {
  audit_pigs1$incons_cases %>% as.numeric() %>% expect_equal(8)
  audit_pigs2$incons_cases %>% as.numeric() %>% expect_equal(5)
  audit_pigs1$incons_rate  %>% as.numeric() %>% expect_equal(8 / 12)
  audit_pigs2$incons_rate  %>% as.numeric() %>% expect_equal(5 / 6)
})

