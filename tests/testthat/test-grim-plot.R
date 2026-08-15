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
  # `suppressMessages()` mutes the success alert that `grim_plot()` prints when
  # it made more than one plot:
  plots <- suppressMessages(mixed_digits |> grim_plot(split_by_digits = TRUE))
  plots |> expect_type("list")
  plots |> names() |> expect_equal(c("digits_1", "digits_2"))
  plots[[1]] |> expect_s3_class("ggplot")
  plots[[2]] |> expect_s3_class("ggplot")
})


# The y-axis is the fractional portion of a mean, bounded at 0 and 1. Those
# bounds used to be scale limits, which discard a tile as soon as one of its
# edges falls outside -- so a mean of `5.00`, with a fractional portion of
# exactly 0, was dropped whole. `pigs1` contains such a value, and the warning
# that would have said so went into a `suppressWarnings()` around the `print()`.

test_that("no data is dropped from the plot", {
  data <- grim_map(pigs1, digits_x = 2)
  layer_data <- ggplot2::ggplot_build(grim_plot(data))$data[[2L]]
  nrow(layer_data) |> expect_equal(nrow(data))
  layer_data$ymin |> is.na() |> any() |> expect_false()
  layer_data$ymax |> is.na() |> any() |> expect_false()
})

test_that("plotting raises no warnings of its own", {
  grim_map(pigs1, digits_x = 2) |> grim_plot() |> expect_no_warning()
  grim_map(pigs2, digits_x = 1, percent = TRUE) |>
    grim_plot() |>
    expect_no_warning()
  # The gradient branch, for more than two decimal places:
  grim_map(tibble::tibble(x = c(5.1234, 6.2345), n = c(40L, 50L)), digits_x = 4) |>
    grim_plot() |>
    expect_no_warning()
})

test_that("`show_raster = FALSE` works", {
  grim_map(pigs1, digits_x = 2) |>
    grim_plot(show_raster = FALSE) |>
    expect_no_error()
})

test_that("undrawable input is an error with an explanation", {
  grim_map(pigs1[0L, ], digits_x = 2) |>
    grim_plot() |>
    expect_error("no rows")
})


# The y-axis is the fractional portion of `abs(x)`. GRIM's granularity is the
# same on both sides of zero -- the achievable means are `k / n` for every whole
# number `k` -- so a negative mean belongs at the same height as its absolute
# value, not below the axis, where it used to be dropped without a word.

test_that("negative means are plotted at the fractional part of their absolute value", {
  data <- grim_map(
    tibble::tibble(x = c(-7.22, -5.19, -5.00), n = c(38L, 40L, 40L)),
    digits_x = 2
  )
  layer_data <- ggplot2::ggplot_build(grim_plot(data))$data[[2L]]
  layer_data$y |> expect_equal(c(0.22, 0.19, 0))
  nrow(layer_data) |> expect_equal(nrow(data))
  layer_data$ymin |> is.na() |> any() |> expect_false()
})

test_that("a mean and its negative are drawn in the same place", {
  y_of <- function(x) {
    data <- grim_map(tibble::tibble(x = x, n = 40L), digits_x = 2)
    ggplot2::ggplot_build(grim_plot(data))$data[[2L]]$y
  }
  y_of(-2.51) |> expect_equal(y_of(2.51))
})

test_that("negative means raise no warning, whatever the decimal count", {
  grim_map(tibble::tibble(x = c(-7.22, -5.19), n = c(38L, 40L)), digits_x = 2) |>
    grim_plot() |>
    expect_no_warning()
  # The gradient branch:
  grim_map(tibble::tibble(x = -5.1234, n = 40L), digits_x = 4) |>
    grim_plot() |>
    expect_no_warning()
})
