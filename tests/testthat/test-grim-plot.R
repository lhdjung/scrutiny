

plot <- pigs1 %>%
  grim_map() %>%
  grim_plot()

test_that("`grim_plot()` returns a ggplot object", {
  plot %>% ggplot2::is.ggplot() %>% expect_true()
})
