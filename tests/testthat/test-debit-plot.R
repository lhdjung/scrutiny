

plot1 <- pigs3 %>%
  debit_map() %>%
  debit_plot()


test_that("`debit_plot()` returns a ggplot object", {
  plot1 %>% ggplot2::is.ggplot() %>% expect_true()
})

test_that("The S3 inheritance check works correctly", {
  mtcars %>% debit_plot() %>% expect_error()
})


