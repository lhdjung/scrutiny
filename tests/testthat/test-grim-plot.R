
plot <- pigs1 %>%
  grim_map(digits_x = 2) %>%
  grim_plot(digits = 2)

test_that("`grim_plot()` returns a ggplot object", {
  plot %>% expect_s3_class("ggplot")
})
