test_that("`grim_plot_rates()` summarises one point per study", {
  skip_if_not_installed("ggrepel")
  data <- pigs1 |> grim_map(digits_x = 2)
  data$study <- c("A", "B", "B", "B", "B", "A", "C", "A", "C", "C", "A", "C")
  rates <- data |> grim_plot_rates(study) |> _$data
  rates$observed   |> expect_equal(c(0, 1, 1))
  rates$value_sets |> expect_equal(c(4L, 4L, 4L))
  rates$expected[1L] |> expect_equal(mean(data$probability[data$study == "A"]))
  pigs1 |> grim_plot_rates(study) |> expect_error("output of `grim_map()`", fixed = TRUE)
})


test_that("`grim_plot_rates()` selects `study` with tidyselect", {
  skip_if_not_installed("ggrepel")
  data <- pigs1 |> grim_map(digits_x = 2)
  data$study <- rep(c("A", "B"), 6)
  by_name   <- data |> grim_plot_rates(study)
  by_string <- data |> grim_plot_rates("study")
  by_string$data |> expect_equal(by_name$data)
  data |> grim_plot_rates(c(x, n)) |> expect_error("exactly one column")
})


test_that("`grim_plot_rates()` requires `consistency` and `probability`", {
  skip_if_not_installed("ggrepel")
  data <- pigs1 |> grim_map(digits_x = 2)
  data$study <- rep(c("A", "B"), 6)
  data |> dplyr::select(-probability) |> grim_plot_rates(study) |> expect_error("Missing: `probability`")
})


test_that("`grim_plot_rates()` drops value sets without a study", {
  skip_if_not_installed("ggrepel")
  data <- pigs1 |> grim_map(digits_x = 2)
  data$study <- c(NA, rep(c("A", "B"), length.out = 11))
  data |> grim_plot_rates(study) |> expect_warning("Dropping 1 value set without a study")
  plot <- data |> grim_plot_rates(study) |> suppressWarnings()
  plot$data$study |> expect_equal(c("A", "B"))
})
