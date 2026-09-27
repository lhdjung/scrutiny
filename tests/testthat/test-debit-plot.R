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
  built <- ggplot2::ggplot_build(debit_plot(data))$data
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
  expect_silent(invisible(capture.output(
  p <- pigs3 |> 
    debit_map(digits_x = 2, digits_sd = 2) |> 
    debit_plot()
  )))

  expect_s3_class(p, "ggplot")
})

test_that("rectangles are sized per row, and `NA` rows are dropped out loud", {
  data <- tibble::tibble(
    x = c(.5, .35),
    sd = c(.5, .48),
    n = c(40, 40)
  ) |>
    debit_map(digits_x = c(1, 2), digits_sd = 2)
  built <- ggplot2::ggplot_build(debit_plot(data))$data
  rects <- built[[length(built) - 1L]]
  (rects$xmax - rects$xmin) |> expect_equal(c(0.1, 0.01))
  # Each rectangle is drawn over a point at the reported values, so that it
  # shows up even if it is too small to see:
  points <- built[[length(built) - 2L]]
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
  expect_s3_class(p, "ggplot")
})


test_that("the band between the DEBIT lines is clipped, not cut off", {
  df <- tibble::tibble(x = c(0.52, 0.31), sd = c(0.50, 0.46), n = c(5, 8))
  p <- debit_plot(
    debit_map(df, digits_x = 2, digits_sd = 2),
    show_labels = FALSE
  )
  band <- ggplot2::ggplot_build(p)$data[[1L]]
  anyNA(band$ymin) |> expect_false()
  anyNA(band$ymax) |> expect_false()
})
