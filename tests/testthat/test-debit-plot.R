

plot <- pigs3 %>%
  debit_map() %>%
  debit_plot()

test_that("`debit_plot()` returns a ggplot object", {
  plot %>% ggplot2::is.ggplot() %>% expect_true()
})
