# `show_labels = TRUE`, the default, needs ggrepel:
skip_if_not_installed("ggrepel")

plot1 <- pigs3 |>
  debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |>
  debit_plot()


test_that("`debit_plot()` returns a ggplot object", {
  plot1 |>  expect_s3_class("ggplot")
})

test_that("The S3 inheritance check works correctly", {
  mtcars |> debit_plot() |> expect_error()
})

test_that("the DEBIT lines for the smallest and largest `n` bound a band", {
  # A single line used to take the whole `n` column, recycled along the curve:
  data <- tibble::tibble(x = c(0.5, 0.3, 0.6), sd = c(0.71, 0.47, 0.55), n = c(2, 50, 3)) |>
    debit_map(digits_x = 2, digits_sd = 2, data_type = "integer")
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
  p <- pigs3 |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |> debit_plot() |> expect_silent()

  p |> expect_s3_class("ggplot")
})

test_that("rectangles are sized per row, and `NA` rows are dropped out loud", {
  data <- tibble::tibble(
    x = c(0.5, 0.35),
    sd = c(0.5, 0.48),
    n = c(40, 40)
  ) |>
    debit_map(digits_x = c(1, 2), digits_sd = 2, data_type = "integer")
  built <- data |> debit_plot() |> ggplot2::ggplot_build() |> purrr::pluck("data")
  rects <- built[[length(built) - 2L]]
  (rects$xmax - rects$xmin) |> expect_equal(c(0.1, 0.01))
  # Each rectangle is drawn over a point at the reported values, so that it
  # shows up even if it is too small to see:
  points <- built[[length(built) - 3L]]
  points[c("x", "y")] |> expect_equal(data.frame(x = c(0.5, 0.35), y = c(0.5, 0.48)))
  # Trailing zeros are restored in the labels:
  built[[length(built)]]$label |> expect_equal(c("0.5; 0.50", "0.35; 0.48"))

  data_na <- tibble::tibble(
    x = c(0.5, NA),
    sd = c(0.5, 0.48),
    n = c(40, 40)
  ) |>
    debit_map(digits_x = 2, digits_sd = 2, data_type = "integer")
  
  expect_warning(p <- debit_plot(data_na), "Dropping 1 value set")
  p |> expect_s3_class("ggplot")
})


test_that("the band between the DEBIT lines is clipped, not cut off", {
  df <- tibble::tibble(x = c(0.52, 0.31), sd = c(0.50, 0.46), n = c(5, 8))
  p <- df |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |> debit_plot(show_labels = FALSE)
  band <- ggplot2::ggplot_build(p)$data[[1L]]
  band$ymin |> anyNA() |> expect_false()
  band$ymax |> anyNA() |> expect_false()
})


# Without the bounds, the plot used to fail with a vctrs error about subsetting
# columns that don't exist:
test_that("`debit_plot()` explains missing reconstruction columns", {
  pigs3 |>
    debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |>
    dplyr::select(-sd_lower, -sd_upper) |>
    debit_plot() |>
    expect_error("sd_lower")
})

test_that("under `formula = \"exact\"`, the attainable means are marked", {
  # One 1 in 20 is the only binary sample with a mean reported as 0.05. Its SD,
  # 0.2236, misses the rectangle of an SD reported as 0.21, though the line
  # crosses it:
  data <- tibble::tibble(x = 0.05, sd = 0.21, n = 20)
  is_point_layer <- function(l) inherits(l$geom, "GeomPoint")

  built <- data |>
    debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |>
    debit_plot(show_labels = FALSE) |>
    ggplot2::ggplot_build()
  marks <- built$data[[length(built$data)]]
  marks[c("x", "y")] |> expect_equal(data.frame(x = 0.05, y = sqrt(0.05)))

  data |>
    debit_map(digits_x = 2, digits_sd = 2, data_type = "integer", formula = "mean_n") |>
    debit_plot(show_labels = FALSE) |>
    call_on(\(p) p$layers) |>
    vapply(is_point_layer, TRUE) |>
    sum() |>
    expect_equal(1)
})

test_that("`debit_plot()` requires the recorded `formula`", {
  pigs3 |>
    debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |>
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
    debit_map(digits_x = 1, digits_sd = 1, data_type = "integer") |>
    debit_plot(show_labels = FALSE) |>
    expect_message("only some")

  pigs3 |>
    debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |>
    debit_plot(show_labels = FALSE) |>
    expect_no_message()
})

test_that("colors come from a scale that another one replaces", {
  data <- tibble::tibble(
    x = c(0.35, 0.05),
    sd = c(0.48, 0.21),
    n = c(40, 20)
  ) |>
    debit_map(digits_x = 2, digits_sd = 2, data_type = "integer")

  p <- data |> debit_plot(show_labels = FALSE)

  rects <- function(p) {
    is_rect <- p$layers |> vapply(\(l) inherits(l$geom, "GeomRect"), TRUE)
    ggplot2::ggplot_build(p)$data[[which(is_rect)]]
  }

  p |> rects() |> call_on(\(r) r$fill) |> expect_equal(unname(COLORS_CONSISTENCY))

  scale_new <- ggplot2::scale_colour_manual(
    values = c("TRUE" = "darkgreen", "FALSE" = "orange"),
    aesthetics = c("colour", "fill")
  )
  p_new <- suppressMessages(p + scale_new)
  p_new |> rects() |> call_on(\(r) r$fill)   |> expect_equal(c("darkgreen", "orange"))
  p_new |> rects() |> call_on(\(r) r$colour) |> expect_equal(c("darkgreen", "orange"))
})


test_that("`debit_plot()` says that there are no rows to plot", {
  # The bound columns are also missing from 0-row output, and that used to be
  # what the message blamed:
  pigs3[0, ] |>
    debit_map(digits_x = 3, digits_sd = 3, data_type = "integer") |>
    debit_plot() |>
    expect_error("no rows")
})

test_that("`debit_plot()` draws the line of the SD type that was tested", {
  data <- tibble::tibble(x = c(0.3, 1), sd = c(0.46, 0), n = c(10, 1))
  at_half <- function(l) l$y[which.min(abs(l$x - 0.5))]
  lines_at_half <- function(plot) {
    is_line <- plot$layers |> vapply(function(l) inherits(l$geom, "GeomFunction"), TRUE)
    plot |> ggplot2::ggplot_build() |> _$data[is_line] |> vapply(at_half, 1)
  }

  # A group of 1 has no sample SD, so it has no sample line:
  data |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer", sd_meaning = "sd_pop") |> debit_plot() |> lines_at_half() |> expect_equal(c(0.5, 0.5))
  data |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer", sd_meaning = "sd_sample_or_pop")     |> debit_plot() |> lines_at_half() |> expect_equal(c(sqrt(10 / 9 * 0.25), 0.5, 0.5))
  # An SE is the sample SD divided by `sqrt(n)`:
  data[1L, ] |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer", sd_meaning = "se")     |> debit_plot() |> lines_at_half() |> expect_equal(sqrt(10 / 9 * 0.25 / 10))
})

test_that("`debit_plot()` scales the y-axis to SEs", {
  # It was fixed to SDs, from 0 to at least 0.5, so SEs sat at the bottom:
  y_range <- function(plot) plot |> ggplot2::ggplot_build() |> _$layout$panel_params[[1L]]$y.range
  tibble::tibble(x = 0.52, sd = 0.012, n = 1700) |> debit_map(digits_x = 2, digits_sd = 3, data_type = "integer", sd_meaning = "se") |> debit_plot() |> y_range() |> max() |> expect_lt(0.02)
  tibble::tibble(x = 0.52, sd = 0.12, n = 17)    |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer")                     |> debit_plot() |> y_range() |> max() |> expect_gt(0.5)
})

test_that("`debit_plot()` draws the attainable means of a large `n`", {
  data <- tibble::tibble(x = 0.5, sd = 0.5, n = 150000) |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer")
  build <- data |> debit_plot(show_labels = FALSE) |> ggplot2::ggplot_build() |> expect_no_warning()
  marks <- build$data[[length(build$data)]]
  marks$y |> anyNA() |> expect_false()
})

test_that("the flags must be `TRUE` or `FALSE`", {
  data <- pigs3 |> debit_map(digits_x = 2, digits_sd = 2, data_type = "integer")
  data |> debit_plot(show_labels = NA)        |> expect_error("`show_labels` must be `TRUE` or `FALSE`")
  data |> debit_plot(show_full_scale = "yes") |> expect_error("`show_full_scale` must be `TRUE` or `FALSE`")
  data |> debit_plot(show_theme_other = NA)   |> expect_error("`show_theme_other` must be `TRUE` or `FALSE`")
})


test_that("a dropped `data_type` column is named as such", {
  # The re-test asked for a `data_type` argument, which `debit_plot()` lacks:
  pigs3 |>
    debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |>
    dplyr::select(-data_type) |>
    debit_plot() |>
    expect_error("lacks columns that `debit_plot\\(\\)` needs")
})


test_that("dropped continuous rows are named as not binary", {
  # The warning said that DEBIT only applies to whole numbers:
  tibble::tibble(x = c(0.35, 0.4), sd = c(0.48, 0.49), n = 20, data_type = c("integer", "continuous")) |>
    debit_map(digits_x = 2, digits_sd = 2) |>
    debit_plot() |>
    expect_warning("only applies to binary data")
})
