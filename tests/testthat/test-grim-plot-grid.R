plot <- pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  grim_plot_grid(digits_x = 2)

test_that("`grim_plot_grid()` returns a ggplot object", {
  plot |> expect_s3_class("ggplot")
})

test_that("`grim_plot_grid()` picks up `digits_x` from `grim_map()` output
          without needing `digits_x`", {
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> grim_plot_grid() |> expect_s3_class("ggplot")
})

mixed_digits <- pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  dplyr::bind_rows(grim_map(
    tibble::tibble(x = 4.7, n = 20),
    digits_x = 1,
    items_averaged = 1,
    data_type = "integer"
  ))

test_that("`grim_plot_grid()` errors by default on mixed `digits_x`", {
  mixed_digits |> grim_plot_grid() |> expect_error()
})

test_that("`split_by_digits = TRUE` returns one plot per decimal count", {
  # `suppressMessages()` mutes the success alert that `grim_plot_grid()` prints when
  # it made more than one plot:
  plots <- mixed_digits |> grim_plot_grid(split_by_digits = TRUE) |> suppressMessages()
  plots |> expect_type("list")
  plots |> names() |> expect_equal(c("digits_1", "digits_2"))
  plots[[1]] |> expect_s3_class("ggplot")
  plots[[2]] |> expect_s3_class("ggplot")
})

test_that("`split_by_digits = TRUE` multiplies `items_averaged` into `n` once", {
  data <- tibble::tibble(x = c(5.19, 5.2), n = c(14, 14)) |>
    grim_map(digits_x = c(2, 1), items_averaged = 2, data_type = "integer")
  tile_n <- function(p) p$layers[[2]]$data$n
  plots <- data |> grim_plot_grid(split_by_digits = TRUE) |> suppressMessages()
  plots |> purrr::map(tile_n) |> unlist() |> unique() |> expect_equal(28)
  data[1, ] |> grim_plot_grid() |> tile_n() |> expect_equal(28)
})


# The y-axis is the fractional portion of a mean, bounded at 0 and 1. Those
# bounds used to be scale limits, which discard a tile as soon as one of its
# edges falls outside -- so a mean of `5.00`, with a fractional portion of
# exactly 0, was dropped whole. `pigs1` contains such a value, and the warning
# that would have said so went into a `suppressWarnings()` around the `print()`.

test_that("no data is dropped from the plot", {
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  layer_data <- data |> grim_plot_grid() |> ggplot2::ggplot_build() |> purrr::pluck("data", 2L)
  layer_data |> nrow() |> expect_equal(nrow(data))
  layer_data$ymin |> anyNA() |> expect_false()
  layer_data$ymax |> anyNA() |> expect_false()
})

test_that("plotting raises no warnings of its own", {
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> grim_plot_grid() |> expect_no_warning()
  pigs2 |>
    grim_map(digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |>
    grim_plot_grid() |>
    expect_no_warning()
  # The gradient branch, for more than two decimal places:
  tibble::tibble(x = c(5.1234, 6.2345), n = c(40L, 50L)) |>
    grim_map(digits_x = 4, items_averaged = 1, data_type = "integer") |>
    grim_plot_grid() |>
    expect_no_warning()
})

test_that("`show_raster = FALSE` works", {
  pigs1 |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    grim_plot_grid(show_raster = FALSE) |>
    expect_no_error()
})

test_that("undrawable input is an error with an explanation", {
  pigs1[0L, ] |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    grim_plot_grid() |>
    expect_error("no rows")
})


# The y-axis is the fractional portion of `abs(x)`. GRIM's granularity is the
# same on both sides of zero -- the achievable means are `k / n` for every whole
# number `k` -- so a negative mean belongs at the same height as its absolute
# value, not below the axis, where it used to be dropped without a word.

test_that("negative means are plotted at the fractional part of their absolute value", {
  data <- tibble::tibble(x = c(-7.22, -5.19, -5.00), n = c(38L, 40L, 40L)) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  layer_data <- data |> grim_plot_grid() |> ggplot2::ggplot_build() |> purrr::pluck("data", 2L)
  layer_data$y |> expect_equal(c(0.22, 0.19, 0))
  layer_data |> nrow() |> expect_equal(nrow(data))
  layer_data$ymin |> anyNA() |> expect_false()
})

test_that("a mean and its negative are drawn in the same place", {
  y_of <- function(x) {
    tibble::tibble(x = x, n = 40L) |>
      grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
      grim_plot_grid() |>
      ggplot2::ggplot_build() |>
      purrr::pluck("data", 2L, "y")
  }
  -2.51 |> y_of() |> expect_equal(y_of(2.51))
})

test_that("negative means raise no warning, whatever the decimal count", {
  tibble::tibble(x = c(-7.22, -5.19), n = c(38L, 40L)) |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    grim_plot_grid() |>
    expect_no_warning()
  # The gradient branch:
  tibble::tibble(x = -5.1234, n = 40L) |>
    grim_map(digits_x = 4, items_averaged = 1, data_type = "integer") |>
    grim_plot_grid() |>
    expect_no_warning()
})


test_that("`grim_plot_grid()` returns the plot instead of printing it", {
  # It used to end on `print()` and return invisibly, so `p <- grim_plot_grid(g)`
  # drew a plot the caller had not asked for, and `grim_plot_grid(g) + labs(...)`
  # drew two. `debit_plot()` has always returned its object normally.
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  p <- data |> grim_plot_grid() |> expect_silent()
  p |> expect_s3_class("ggplot")
  # Visible, so auto-printing draws it at the console:
  data |> grim_plot_grid() |> withVisible() |> purrr::pluck("visible") |> expect_true()
  # ...and it composes without a stray canvas:
  data |>
    grim_plot_grid() |>
    call_on(\(p) p + ggplot2::labs(title = "x")) |>
    expect_s3_class("ggplot")
})


test_that("undecidable value sets are dropped out loud", {
  # A tile's color comes from `consistency`, so an `NA` verdict has no color
  # and ggplot2 dropped the row with a bare "Removed 1 row containing missing
  # values" -- which names neither the column nor the reason.
  data <- tibble::tibble(x = c(5.19, 5.19), n = c(28L, NA_integer_)) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  expect_warning(p <- grim_plot_grid(data), "undecidable")
  layer_data <- ggplot2::ggplot_build(p)$data[[2L]]
  layer_data |> nrow() |> expect_equal(1L)

  # If nothing can be decided, there is nothing to draw at all:
  tibble::tibble(x = 5.19, n = NA_integer_) |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    grim_plot_grid() |>
    expect_error("could be decided")
})


test_that("`digits_x = 0` rows are reported, not dropped in silence", {
  # A mean reported with no decimal places has a fractional portion of zero, so
  # `split_by_digits` leaves it out. It used to do so without a word, and the
  # success message counted only the plots that were made.
  data <- tibble::tibble(x = c(5, 5.19), n = c(28L, 28L)) |> grim_map(digits_x = c(0, 2), items_averaged = 1, data_type = "integer")
  expect_warning(
    plots <- grim_plot_grid(data, split_by_digits = TRUE),
    "digits_x = 0"
  )
  plots |> names() |> expect_equal("digits_2")

  # With nothing but zero-decimal means there is no plot to split at all, and
  # the raster lookup used to fail with R's own "object
  # 'grim_raster_0_up_or_down_n' not found":
  zero_only <- tibble::tibble(x = 5, n = 28L) |> grim_map(digits_x = 0, items_averaged = 1, data_type = "integer")
  zero_only |> grim_plot_grid(split_by_digits = TRUE) |> expect_error()
  zero_only |> grim_plot_grid() |> expect_error("must be at least 1")
})


test_that("percentages are plotted on the grid they were tested on", {
  # `grim_map(x_meaning = "percentage")` divides `x` by 100 and raises its decimal count
  # by 2 before testing. `grim_plot_grid()` did neither, so a percentage reported as
  # `67.4` was drawn at a fractional portion of `0.4` against the raster for
  # one decimal place, while the verdict coloring that tile was reached at
  # three. The y-axis label promised "% (as decimal)" the whole time.
  data <- pigs2 |> grim_map(digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |> suppressMessages()
  # Three effective decimal places, so this is the gradient branch, whose tile
  # layer is the last one rather than the second:
  built <- data |> grim_plot_grid() |> ggplot2::ggplot_build() |> purrr::pluck("data")
  layer_data <- built[[length(built)]]
  layer_data$y |> expect_equal(pigs2$x / 100)

  # A whole-number percentage does have a fractional portion as a decimal, so
  # `digits_x = 0` is plottable here although it is not for a plain mean:
  tibble::tibble(x = 71, n = 43L) |>
    grim_map(digits_x = 0, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |>
    grim_plot_grid() |>
    expect_s3_class("ggplot")
})


test_that("`n` sets the maximum of the x-axis", {
  # Only the breaks used to stop at `n`; the axis still ran to 100:
  p <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> grim_plot_grid(n = 50)
  x_range <- ggplot2::ggplot_build(p)$layout$panel_params[[1L]]$x.range
  x_range[2L] |> expect_equal(50, tolerance = 0.01)
})

test_that("`split_by_digits = TRUE` always returns a named list", {
  # With a single decimal count or without a raster, it used to return a plain
  # ggplot object:
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  data |> grim_plot_grid(split_by_digits = TRUE) |> names() |> expect_equal("digits_2")
  data |>
    grim_plot_grid(split_by_digits = TRUE, show_raster = FALSE) |>
    names() |>
    expect_equal("digits_2")
  data |> grim_plot_grid(split_by_digits = TRUE, digits_x = 2) |> expect_error("can't be combined")
})


test_that("tiles sit at `n * items_averaged`", {
  out <- tibble::tibble(x = 3.43, n = 71) |> grim_map_seq(digits_x = 2, items_averaged = 2, data_type = "integer", include_consistent = TRUE)
  layers <- out |> grim_plot_grid() |> ggplot2::ggplot_build() |> purrr::pluck("data")
  tiles <- layers[[length(layers)]]
  tiles$x |> unique() |> sort() |> expect_equal(sort(unique(out$n * out$items_averaged)))
})


test_that("GRIMMER output without a `reason` column plots without warnings", {
  pigs5 |>
    grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    dplyr::select(-reason) |>
    grim_plot_grid() |>
    expect_no_warning()
})

test_that("the value sets' colors come from a scale that another one replaces", {
  data <- tibble::tibble(x = c(5.19, 5.18), n = c(28L, 28L)) |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  p <- data |> grim_plot_grid()
  tiles <- function(p) {
    built <- ggplot2::ggplot_build(p)$data
    built[[length(built)]]
  }

  p |> tiles() |> call_on(\(t) t$fill) |> expect_equal(unname(COLORS_CONSISTENCY[c("FALSE", "TRUE")]))
  data$consistency |> expect_equal(c(FALSE, TRUE))

  scale_new <- ggplot2::scale_colour_manual(
    values = c("TRUE" = "darkgreen", "FALSE" = "orange"),
    aesthetics = c("colour", "fill")
  )
  p_new <- suppressMessages(p + scale_new)
  p_new |> tiles() |> call_on(\(t) t$fill)   |> expect_equal(c("orange", "darkgreen"))
  p_new |> tiles() |> call_on(\(t) t$colour) |> expect_equal(c("orange", "darkgreen"))

  # The raster's fill is set, not mapped, so the scale leaves it alone. Its
  # tiles are the inconsistent cells, in white on the grey of the consistent:
  p_new |> ggplot2::ggplot_build() |> purrr::pluck("data", 1L, "fill") |> unique() |> expect_equal("white")
  p_new$theme$panel.background$fill |> expect_equal("grey75")
})

test_that("`grim_plot_grid()` names the columns it lacks", {
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> dplyr::select(x) |> grim_plot_grid() |> expect_error("Missing: `n` and `consistency`")
})

test_that("`grim_plot_grid()` falls back to `rounding` if `data` has none recorded", {
  data <- tibble::tibble(x = 5.19, n = 28, digits_x = 2, consistency = TRUE)
  class(data) <- c("scrutiny_grim_map", class(data))
  data |> grim_plot_grid() |> expect_s3_class("ggplot")
  data |> grim_plot_grid(rounding = "up_from", show_raster = TRUE) |> expect_s3_class("ggplot")
})


test_that("the raster follows a `threshold` recorded or given", {
  expected <- generate_grim_grid(2, "up_from", 9)
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "up_from", threshold = 9) |> grim_plot_grid() |> purrr::pluck("data") |> expect_identical(expected)
  grim_plot_grid(show_data = FALSE, digits_x = 2, rounding = "up_from", threshold = 9) |> purrr::pluck("data") |> expect_identical(expected)
})


test_that("`rounding` or `threshold` that disagree with the mapper call are an error", {
  out <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "ceiling")
  out |> grim_plot_grid(rounding = "up_or_down") |> expect_error("disagrees with the mapper call")
  out |> grim_plot_grid(threshold = 7)          |> expect_error("disagrees with the mapper call")
  out |> grim_plot_grid(rounding = "ceiling")   |> expect_s3_class("ggplot")
  out |> grim_plot_grid(threshold = 5L)         |> expect_s3_class("ggplot")
  out |> grim_plot()                            |> expect_s3_class("ggplot") |> suppressWarnings()
  # Nor do the plots for each number of decimal places get the defaults:
  mixed <- tibble::tibble(x = c(5.19, 5.2), n = c(28, 30)) |> grim_map(digits_x = c(2, 1), items_averaged = 1, data_type = "integer", rounding = "ceiling")
  pdf(NULL)
  mixed |> grim_plot_grid(split_by_digits = TRUE) |> length() |> expect_equal(2L) |> suppressMessages()
  dev.off()
})


test_that("`n` must be a single finite number of at least 1", {
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  data |> grim_plot_grid(n = -5)        |> expect_error("at least 1")
  data |> grim_plot_grid(n = c(10, 20)) |> expect_error("at least 1")
  data |> grim_plot_grid(n = Inf)       |> expect_error("at least 1")
})


test_that("the gradient ends at `10^digits` and the axis breaks are round", {
  p <- tibble::tibble(x = c(5.193, 4.271), n = c(3000L, 400L)) |> grim_map(digits_x = 3, items_averaged = 1, data_type = "integer") |> grim_plot_grid()
  p$layers |> purrr::map("geom_params") |> purrr::map("xmax") |> unlist(use.names = FALSE) |> expect_equal(1000)
  p |> ggplot2::ggplot_build() |> purrr::pluck("layout", "panel_params", 1L, "x", "breaks") |> expect_equal(seq(0, 3000, 500))
})


test_that("`grim_plot_grid()` doesn't overflow the integer range", {
  tibble::tibble(x = c(1.53, 1.2), n = c(50000L, 20L), items_averaged = c(50000L, 1L)) |> grim_map(digits_x = 2, data_type = "integer") |> grim_plot_grid() |> expect_no_warning()
})

test_that("`grim_plot()` is a deprecated alias of `grim_plot_grid()`", {
  rlang::local_options(lifecycle_verbosity = "warning")
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  data |> grim_plot(digits = 2) |> expect_warning(class = "lifecycle_warning_deprecated")
  # Old and new argument names both reach `grim_plot_grid()`:
  rlang::local_options(lifecycle_verbosity = "quiet")
  expected <- data |> grim_plot_grid(digits_x = 2, color_raster = "red") |> ggplot2::layer_data(1L)
  data |> grim_plot(digits = 2, raster_color = "red")   |> ggplot2::layer_data(1L) |> expect_equal(expected)
  data |> grim_plot(digits_x = 2, color_raster = "red") |> ggplot2::layer_data(1L) |> expect_equal(expected)
  # Not both at once:
  data |> grim_plot(digits = 2, digits_x = 2)                |> expect_error("`digits` and `digits_x` can't both be given")
  data |> grim_plot(raster_color = "red", color_raster = "red") |> expect_error("`raster_color` and `color_raster` can't both be given")
  # By position, as in scrutiny 0.6.1, where `n` came fifth:
  expected <- data |> grim_plot_grid(n = 40) |> ggplot2::layer_data(1L)
  data |> grim_plot(TRUE, TRUE, TRUE, 40) |> ggplot2::layer_data(1L) |> expect_equal(expected)
  # Removed arguments are ignored, with a warning:
  rlang::local_options(lifecycle_verbosity = "warning")
  data |> grim_plot(color_cons = "blue", tile_alpha = 0.5) |> expect_warning("color_cons") |> expect_warning("tile_alpha") |> expect_warning("grim_plot_grid")
})

test_that("the flags must be `TRUE` or `FALSE`", {
  data <- pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
  data |> grim_plot_grid(show_data = NA)       |> expect_error("`show_data` must be `TRUE` or `FALSE`")
  data |> grim_plot_grid(show_raster = "yes")  |> expect_error("`show_raster` must be `TRUE` or `FALSE`")
  data |> grim_plot_grid(show_gradient = NA)   |> expect_error("`show_gradient` must be `TRUE` or `FALSE`")
  data |> grim_plot_grid(split_by_digits = NA) |> expect_error("`split_by_digits` must be `TRUE` or `FALSE`")
})


test_that("`digits_x` and `tile_size` are checked", {
  # These failed with R's own errors:
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> grim_plot_grid(digits_x = NA)      |> expect_error("whole number")
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> grim_plot_grid(digits_x = "2")     |> expect_error("whole number")
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> grim_plot_grid(tile_size = "a")  |> expect_error("single positive number")
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> grim_plot_grid(tile_size = NA)   |> expect_error("single positive number")
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |> grim_plot_grid(tile_size = -1)   |> expect_error("single positive number")
})


test_that("continuous data are left out, with a warning", {
  data <- pigs1 |> dplyr::mutate(data_type = rep(c("integer", "continuous"), 6L)) |> grim_map(digits_x = 2, items_averaged = 1)
  expect_warning(p <- grim_plot_grid(data), "Dropping 6 value sets of continuous data")
  p |> ggplot2::layer_data(2L) |> nrow() |> expect_equal(6L)
  pigs1 |> grim_map(digits_x = 2, items_averaged = 1, data_type = "continuous") |> grim_plot_grid() |> expect_error("All value sets")
})


test_that("an inconsistent value set without a mean or its decimal places is dropped", {
  # GRIMMER says `FALSE` with these missing if no mean goes with the SD. The
  # row then failed a base R `if ()`, or was left to ggplot2 to remove:
  df <- tibble::tibble(x = c(5.23, 5.2), sd = c(2.55, 0.03), n = c(35, 40))
  out <- df |> grimmer_map(digits_x = c(2, NA), digits_sd = 2, items_averaged = 1, data_type = "integer")
  out$consistency |> expect_equal(c(FALSE, FALSE))
  out |> grim_plot_grid()                       |> expect_warning("Dropping 1 value set without a place") |> suppressMessages()
  out |> grim_plot_grid(split_by_digits = TRUE) |> expect_warning("Dropping 1 value set without a place") |> suppressMessages()

  out <- dplyr::mutate(df, x = c(5.23, NA)) |>
    grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
  plot <- out |> grim_plot_grid() |> expect_warning("Dropping 1 value set without a place") |> suppressMessages()

  out[2L, ] |> grim_plot_grid() |> expect_error("No value set in `data` has a place") |> suppressMessages()
  out[2L, ] |> grim_plot_grid() |> expect_error("missing in the only row")             |> suppressMessages()
})


test_that("a value set decided with `n` or `items_averaged` missing is dropped", {
  # It was left to ggplot2 to remove, with a vague message:
  out <- tibble::tibble(x = c(5, 5.19, 5.5), n = c(NA, 28, 30), items_averaged = c(1, 1, NA)) |>
    grim_map(digits_x = 2, data_type = "integer")
  out$consistency |> expect_equal(c(TRUE, FALSE, TRUE))
  expect_warning(plot <- grim_plot_grid(out), "Dropping 2 value sets without a place")
  plot |> ggplot2::layer_data(2L) |> nrow() |> expect_equal(1L)
  out[-2L, ] |> grim_plot_grid() |> expect_error("`items_averaged`, or `digits_x` is missing")
})


test_that("negative means get the raster of the mirrored rounding method", {
  # The raster was the one for positive means, so under `"ceiling"` or
  # `"floor"`, nearly half the tiles sat on a cell of the other color:
  disagreements <- function(x, n, ...) {
    out <- tibble::tibble(x = x, n = n) |>
      grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", ...)
    raster <- ggplot2::ggplot_build(grim_plot_grid(out))$data[[1L]]
    on_white <- paste(out$n, round(abs(out$x) %% 1 * 100)) %in% paste(raster$x, round(raster$y * 100))
    sum(on_white == out$consistency)
  }
  set.seed(sweep_seed(11L))
  x <- -round(runif(200, 0.01, 9), 2)
  x <- x[round(abs(x) %% 1, 2) != 0]
  n <- sample(2:99, length(x), replace = TRUE)

  for (rounding in c("ceiling", "floor", "up", "down", "ties_up", "ties_down", "ties_away", "trunc", "up_or_down")) {
    disagreements(x, n, rounding = rounding) |> expect_equal(0L)
  }
  disagreements(x, n, rounding = "up", symmetric = TRUE)                    |> expect_equal(0L)
  disagreements(x, n, rounding = "up_from", threshold = 3)                  |> expect_equal(0L)
  disagreements(x, n, rounding = "up_from_or_down_from", threshold = 3)     |> expect_equal(0L)
  disagreements(-x, n, rounding = "ceiling")                                |> expect_equal(0L)

  # With both signs, one raster can't serve all tiles:
  tibble::tibble(x = c(-2.57, 2.57), n = 7) |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer", rounding = "ceiling") |>
    grim_plot_grid() |>
    expect_warning("The raster is the one for positive means")
  tibble::tibble(x = c(-2.57, 2.57), n = 7) |>
    grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
    grim_plot_grid() |>
    expect_no_warning()
})


test_that("three decimal places without the gradient leave the panel plain", {
  # The tile that stands in for the raster was drawn in white at the origin:
  plot <- grim_plot_grid(show_data = FALSE, digits_x = 3, n = 20, show_gradient = FALSE)
  ggplot2::ggplot_build(plot)$data[[1L]]$fill |> expect_equal(NA)
  # And the panel is white, not the grey of consistent cells:
  plot$theme$panel.background$fill |> expect_equal("white")
  grim_plot_grid(show_data = FALSE, digits_x = 3, n = 20)$theme$panel.background$fill |> expect_equal("grey75")
})


test_that("a near-whole `digits_x` is used as the whole number", {
  # `digits <= 2` failed for 2 + 1e-12, which got the gradient instead of the
  # raster:
  plot <- grim_plot_grid(show_data = FALSE, digits_x = 2 + 1e-12, n = 20)
  plot |> ggplot2::ggplot_build() |> _$data[[1L]] |> nrow() |> expect_gt(1L)
  grim_plot_grid(show_data = FALSE, digits_x = 1 - 1e-12, n = 20) |> expect_no_error()
})


test_that("GRIMMER inconsistencies that are dropped are not announced", {
  out <- tibble::tibble(x = c(NA, 5.20, 3.40), sd = c(0.01, 1.5, 1.2), n = c(10, 20, 20)) |>
    grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
  out |> grim_plot_grid() |> expect_message("Visualizing 2 GRIMMER inconsistencies") |> suppressWarnings()
})


test_that("GRIMMER inconsistencies are only announced if drawn, and once per plot", {
  out <- pigs5 |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
  out |> grim_plot_grid() |> expect_message("GRIMMER inconsistenc")
  out |> grim_plot_grid(show_data = FALSE) |> expect_no_message() |> suppressWarnings()
  messages <- character()
  withCallingHandlers(
    grim_plot_grid(out, split_by_digits = TRUE),
    message = function(cnd) {
      messages <<- c(messages, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  )
  messages |> grepl(pattern = "GRIMMER inconsistenc") |> sum() |> expect_equal(1L)
})
