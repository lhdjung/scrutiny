# `show_labels = TRUE`, the default, needs ggrepel:
skip_if_not_installed("ggrepel")

plot1 <- pigs3 |>
  debit_map(digits_x = 2, digits_sd = 2) |>
  debit_plot()


test_that("`debit_plot()` returns a ggplot object", {
  plot1 |>  expect_s3_class("ggplot")
})

test_that("The S3 inheritance check works correctly", {
  mtcars |> debit_plot() |> expect_error()
})

test_that("the DEBIT lines for the smallest and largest `n` bound a band", {
  # A single line used to take the whole `n` column, recycled along the curve:
  data <- tibble::tibble(x = c(.5, .3, .6), sd = c(.71, .47, .55), n = c(2, 50, 3)) |>
    debit_map(digits_x = 2, digits_sd = 2)
  built <- data |> debit_plot() |> ggplot2::ggplot_build() |> purrr::pluck("data")
  at_half <- function(l, col) l[[col]][which.min(abs(l$x - 0.5))]
  expected <- sqrt(c(2, 50) / c(1, 49) * 0.25)
  c(
    at_half(built[[1]], "ymax"),
    at_half(built[[1]], "ymin")
  ) |>
    expect_equal(expected)

  built[2:3] |> 
    vapply(at_half, 1, col = "y") |> 
    expect_equal(expected)

  # With a single `n`, there is one line and no band:
  built <- ggplot2::ggplot_build(plot1)$data
  built[[1]]$y[which.min(abs(built[[1]]$x - 0.5))] |>
    expect_equal(sqrt(1683 / 1682 * 0.25))

  plot1$layers |>
    vapply(function(l) inherits(l$geom, "GeomRibbon"), TRUE) |>
    any() |>
    expect_false()
})

test_that("`debit_plot()` returns the plot instead of printing it", {
  p <- pigs3 |> debit_map(digits_x = 2, digits_sd = 2) |> debit_plot() |> expect_silent()

  p |> expect_s3_class("ggplot")
})

test_that("rectangles are sized per row, and `NA` rows are dropped out loud", {
  data <- tibble::tibble(
    x = c(.5, .35),
    sd = c(.5, .48),
    n = c(40, 40)
  ) |>
    debit_map(digits_x = c(1, 2), digits_sd = 2)
  built <- data |> debit_plot() |> ggplot2::ggplot_build() |> purrr::pluck("data")
  rects <- built[[length(built) - 2L]]
  (rects$xmax - rects$xmin) |> expect_equal(c(0.1, 0.01))
  # Each rectangle is drawn over a point at the reported values, so that it
  # shows up even if it is too small to see:
  points <- built[[length(built) - 3L]]
  points[c("x", "y")] |> expect_equal(data.frame(x = c(.5, .35), y = c(.5, .48)))
  # Trailing zeros are restored in the labels:
  built[[length(built)]]$label |> expect_equal(c("0.5; 0.50", "0.35; 0.48"))

  data_na <- tibble::tibble(
    x = c(.5, NA),
    sd = c(.5, .48),
    n = c(40, 40)
  ) |>
    debit_map(digits_x = 2, digits_sd = 2)
  
  expect_warning(p <- debit_plot(data_na), "Dropping 1 value set")
  p |> expect_s3_class("ggplot")
})


test_that("the band between the DEBIT lines is clipped, not cut off", {
  df <- tibble::tibble(x = c(0.52, 0.31), sd = c(0.50, 0.46), n = c(5, 8))
  p <- df |> debit_map(digits_x = 2, digits_sd = 2) |> debit_plot(show_labels = FALSE)
  band <- ggplot2::ggplot_build(p)$data[[1L]]
  band$ymin |> anyNA() |> expect_false()
  band$ymax |> anyNA() |> expect_false()
})


# Without the bounds from `show_rec = TRUE`, the plot used to fail with a vctrs
# error about subsetting columns that don't exist:
test_that("`debit_plot()` explains missing reconstruction columns", {
  pigs3 |>
    debit_map(digits_x = 2, digits_sd = 2, show_rec = FALSE) |>
    debit_plot() |>
    expect_error("show_rec = TRUE")
})

test_that("under `formula = \"exact\"`, the attainable means are marked", {
  # One 1 in 20 is the only binary sample with a mean reported as 0.05. Its SD,
  # 0.2236, misses the rectangle of an SD reported as 0.21, though the line
  # crosses it:
  data <- tibble::tibble(x = 0.05, sd = 0.21, n = 20)
  is_point_layer <- function(l) inherits(l$geom, "GeomPoint")

  built <- data |>
    debit_map(digits_x = 2, digits_sd = 2) |>
    debit_plot(show_labels = FALSE) |>
    ggplot2::ggplot_build()
  marks <- built$data[[length(built$data)]]
  marks[c("x", "y")] |> expect_equal(data.frame(x = 0.05, y = sqrt(0.05)))

  data |>
    debit_map(digits_x = 2, digits_sd = 2, formula = "mean_n") |>
    debit_plot(show_labels = FALSE) |>
    call_on(\(p) p$layers) |>
    vapply(is_point_layer, TRUE) |>
    sum() |>
    expect_equal(1)
})

test_that("`debit_plot()` requires the recorded `formula`", {
  pigs3 |>
    debit_map(digits_x = 2, digits_sd = 2) |>
    structure(scrutiny = NULL) |>
    debit_plot() |>
    expect_error("formula")
})

test_that("thinning the marks keeps every cell of the grid that has one", {
  n <- 5e5
  x_range <- c(-0.05, 1.05)
  y_range <- c(-0.01, 0.73)
  cells <- function(k) {
    x <- floor((k / n - x_range[1]) / diff(x_range) * 2000)
    y <- floor((sd_binary_1_n(k, n) - y_range[1]) / diff(y_range) * 2000)
    unique(paste(x, y))
  }
  k_all  <- 0:n
  k_thin <- thin_binary_means(0, n, n, x_range, y_range)

  k_thin |> length() |> expect_lt(n / 20)
  k_thin |> cells() |> sort() |> expect_equal(sort(cells(k_all)))
  10 |> thin_binary_means(9, n, x_range, y_range) |> expect_length(0)
})

test_that("`debit_plot()` says when it thins the marks", {
  tibble::tibble(x = 0.5, sd = 0.5, n = 1e6) |>
    debit_map(digits_x = 1, digits_sd = 1) |>
    debit_plot(show_labels = FALSE) |>
    expect_message("only some")

  pigs3 |>
    debit_map(digits_x = 2, digits_sd = 2) |>
    debit_plot(show_labels = FALSE) |>
    expect_no_message()
})
