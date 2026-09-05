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
  layer_data |> nrow() |> expect_equal(nrow(data))
  layer_data$ymin |> anyNA() |> expect_false()
  layer_data$ymax |> anyNA() |> expect_false()
})

test_that("plotting raises no warnings of its own", {
  pigs1 |> grim_map(digits_x = 2) |> grim_plot() |> expect_no_warning()
  pigs2 |>
    grim_map(digits_x = 1, percent = TRUE) |>
    grim_plot() |>
    expect_no_warning()
  # The gradient branch, for more than two decimal places:
  tibble::tibble(x = c(5.1234, 6.2345), n = c(40L, 50L)) |>
    grim_map(digits_x = 4) |>
    grim_plot() |>
    expect_no_warning()
})

test_that("`show_raster = FALSE` works", {
  pigs1 |>
    grim_map(digits_x = 2) |>
    grim_plot(show_raster = FALSE) |>
    expect_no_error()
})

test_that("undrawable input is an error with an explanation", {
  pigs1[0L, ] |>
    grim_map(digits_x = 2) |>
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
  layer_data |> nrow() |> expect_equal(nrow(data))
  anyNA(layer_data$ymin) |> expect_false()
})

test_that("a mean and its negative are drawn in the same place", {
  y_of <- function(x) {
    data <- grim_map(tibble::tibble(x = x, n = 40L), digits_x = 2)
    ggplot2::ggplot_build(grim_plot(data))$data[[2L]]$y
  }
  y_of(-2.51) |> expect_equal(y_of(2.51))
})

test_that("negative means raise no warning, whatever the decimal count", {
  tibble::tibble(x = c(-7.22, -5.19), n = c(38L, 40L)) |>
    grim_map(digits_x = 2) |>
    grim_plot() |>
    expect_no_warning()
  # The gradient branch:
  tibble::tibble(x = -5.1234, n = 40L) |>
    grim_map(digits_x = 4) |>
    grim_plot() |>
    expect_no_warning()
})


test_that("`grim_plot()` returns the plot instead of printing it", {
  # It used to end on `print()` and return invisibly, so `p <- grim_plot(g)`
  # drew a plot the caller had not asked for, and `grim_plot(g) + labs(...)`
  # drew two. `debit_plot()` has always returned its object normally.
  data <- grim_map(pigs1, digits_x = 2)
  expect_silent(invisible(capture.output(p <- grim_plot(data))))
  expect_s3_class(p, "ggplot")
  # Visible, so auto-printing draws it at the console:
  expect_true(withVisible(grim_plot(data))$visible)
  # ...and it composes without a stray canvas:
  expect_s3_class(grim_plot(data) + ggplot2::labs(title = "x"), "ggplot")
})


test_that("undecidable value sets are dropped out loud", {
  # A tile's color comes from `consistency`, so an `NA` verdict has no color
  # and ggplot2 dropped the row with a bare "Removed 1 row containing missing
  # values" -- which names neither the column nor the reason.
  data <- grim_map(
    tibble::tibble(x = c(5.19, 5.19), n = c(28L, NA_integer_)),
    digits_x = 2
  )
  expect_warning(p <- grim_plot(data), "undecidable")
  layer_data <- ggplot2::ggplot_build(p)$data[[2L]]
  layer_data |> nrow() |> expect_equal(1L)

  # If nothing can be decided, there is nothing to draw at all:
  tibble::tibble(x = 5.19, n = NA_integer_) |>
    grim_map(digits_x = 2) |>
    grim_plot() |>
    expect_error("could be decided")
})


test_that("`digits_x = 0` rows are reported, not dropped in silence", {
  # A mean reported with no decimal places has a fractional portion of zero, so
  # `split_by_digits` leaves it out. It used to do so without a word, and the
  # success message counted only the plots that were made.
  data <- grim_map(
    tibble::tibble(x = c(5, 5.19), n = c(28L, 28L)),
    digits_x = c(0, 2)
  )
  expect_warning(
    plots <- grim_plot(data, split_by_digits = TRUE),
    "digits_x = 0"
  )
  plots |> names() |> expect_equal("digits_2")

  # With nothing but zero-decimal means there is no plot to split at all, and
  # the raster lookup used to fail with R's own "object
  # 'grim_raster_0_up_or_down_n' not found":
  zero_only <- grim_map(tibble::tibble(x = 5, n = 28L), digits_x = 0)
  zero_only |> grim_plot(split_by_digits = TRUE) |> expect_error()
  zero_only |> grim_plot() |> expect_error("must be at least 1")
})


test_that("percentages are plotted on the grid they were tested on", {
  # `grim_map(percent = TRUE)` divides `x` by 100 and raises its decimal count
  # by 2 before testing. `grim_plot()` did neither, so a percentage reported as
  # `67.4` was drawn at a fractional portion of `0.4` against the raster for
  # one decimal place, while the verdict coloring that tile was reached at
  # three. The y-axis label promised "% (as decimal)" the whole time.
  data <- suppressMessages(grim_map(pigs2, digits_x = 1, percent = TRUE))
  # Three effective decimal places, so this is the gradient branch, whose tile
  # layer is the last one rather than the second:
  built <- ggplot2::ggplot_build(grim_plot(data))$data
  layer_data <- built[[length(built)]]
  layer_data$y |> expect_equal(pigs2$x / 100)

  # A whole-number percentage does have a fractional portion as a decimal, so
  # `digits_x = 0` is plottable here although it is not for a plain mean:
  tibble::tibble(x = 71, n = 43L) |>
    grim_map(digits_x = 0, percent = TRUE) |>
    grim_plot() |>
    expect_s3_class("ggplot")
})
