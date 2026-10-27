test_that("`grim_plot_rates()` summarises one point per study", {
  skip_if_not_installed("ggrepel")
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1)
  data$study <- c("A", "B", "B", "B", "B", "A", "C", "A", "C", "C", "A", "C")
  rates <- data |> grim_plot_rates(study) |> _$data
  rates$observed   |> expect_equal(c(0, 1, 1))
  rates$value_sets |> expect_equal(c(4L, 4L, 4L))
  rates$expected[1L] |> expect_equal(mean(data$exp_incon[data$study == "A"]))
  pigs1 |> grim_plot_rates(study) |> expect_error("output of `grim_map()`", fixed = TRUE)
})


test_that("`grim_plot_rates()` selects `study` with tidyselect", {
  skip_if_not_installed("ggrepel")
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1)
  data$study <- rep(c("A", "B"), 6)
  by_name   <- data |> grim_plot_rates(study)
  by_string <- data |> grim_plot_rates("study")
  by_string$data |> expect_equal(by_name$data)
  data |> grim_plot_rates(c(x, n)) |> expect_error("exactly one column")
  data |> dplyr::rename(ratio = study) |> grim_plot_rates(ratio) |> expect_error("can't be named `ratio`")
})


test_that("`grim_plot_rates()` requires `consistency` and `exp_incon`", {
  skip_if_not_installed("ggrepel")
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1)
  data$study <- rep(c("A", "B"), 6)
  data |> dplyr::select(-exp_incon) |> grim_plot_rates(study) |> expect_error("Missing: `exp_incon`")
})


test_that("`grim_plot_rates()` drops value sets without a study", {
  skip_if_not_installed("ggrepel")
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1)
  data$study <- c(NA, rep(c("A", "B"), length.out = 11))
  data |> grim_plot_rates(study) |> expect_warning("Dropping 1 value set without a study")
  plot <- data |> grim_plot_rates(study) |> suppressWarnings()
  plot$data$study |> expect_equal(c("A", "B"))
})


test_that("`grim_plot_rates()` rejects sequence and total-n output", {
  skip_if_not_installed("ggrepel")
  pigs1 |> grim_map_seq(digits_x = 2, items_averaged = 1) |> grim_plot_rates(case) |> expect_error("hypothetical")
})


test_that("`grim_plot_rates()` errors on a missing `study` and on nothing to plot", {
  skip_if_not_installed("ggrepel")
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1)
  data$study <- rep(c("A", "B"), 6)
  data |> grim_plot_rates() |> expect_error("`study` is missing")
  data |> dplyr::mutate(exp_incon = 0) |> grim_plot_rates(study) |> expect_error("can be plotted")
  data[0L, ] |> grim_plot_rates(study) |> expect_error("can be plotted")
})
