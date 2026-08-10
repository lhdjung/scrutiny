plot <- pigs1 |>
  grim_map(digits_x = 2) |>
  grim_plot(digits = 2)

test_that("`grim_plot()` returns a ggplot object", {
  plot |> expect_s3_class("ggplot")
})

test_that("`grim_plot()` picks up `digits_x` from `grim_map()` output
          without needing `digits`", {
  pigs1 |> grim_map(digits_x = 2) |> grim_plot() |> expect_s3_class("ggplot")
})

mixed_digits <- dplyr::bind_rows(
  grim_map(pigs1, digits_x = 2),
  grim_map(tibble::tibble(x = 4.7, n = 20), digits_x = 1)
)

test_that("`grim_plot()` errors by default on mixed `digits_x`", {
  mixed_digits |> grim_plot() |> expect_error()
})

test_that("`split_by_digits = TRUE` returns one plot per decimal count", {
  plots <- mixed_digits |> grim_plot(split_by_digits = TRUE)
  plots |> expect_type("list")
  plots |> names() |> expect_equal(c("digits_1", "digits_2"))
  plots[[1]] |> expect_s3_class("ggplot")
  plots[[2]] |> expect_s3_class("ggplot")
})
