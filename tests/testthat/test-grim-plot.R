
plot <- pigs1 %>%
  grim_map() %>%
  grim_plot()

test_that("`grim_plot()` returns a ggplot object", {
  plot %>% expect_s3_class("ggplot")
})
