

plot1 <- pigs3 %>%
  debit_map(digits_x = 2, digits_sd = 2) %>%
  debit_plot()


test_that("`debit_plot()` returns a ggplot object", {
  plot1 %>%  expect_s3_class("ggplot")
})

test_that("The S3 inheritance check works correctly", {
  mtcars %>% debit_plot() %>% expect_error()
})


