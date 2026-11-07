# The squares are the `geom_tile()` layer after the two `annotate()` calls that
# draw the strip and the baseline, one per attainable value.
squares <- function(p) p$layers[[3L]]$data$x

# Their fill, and their axis labels: a function that formats the usual breaks
# if the squares are too close to be labeled.
fills <- function(p) ggplot2::layer_data(p, 3L)$fill
axis_labels <- function(p) p$scales$get_scales("x")$labels

# The marker is one grob in the last layer: the verdict label, whose color is
# the marker's, and the labels of the nearest attainable values.
marker <- function(p) {
  children <- p$layers[[length(p$layers)]]$geom_params$grob$children
  verdict <- children[["verdict"]]
  neighbors <- children[startsWith(names(children), "neighbor-label-")]
  list(
    label = verdict$label,
    colour = verdict$gp$col,
    neighbors = vapply(neighbors, function(g) g$label, "", USE.NAMES = FALSE)
  )
}

# Draws `p` on a device of the given size and returns the box of each part of
# the marker in inches, as grid resolves it there: a named list of
# `c(l, r, b, t)`. The parts inherit the font size from their parent gTree,
# so its `gp` is pushed before measuring them.
marker_boxes <- function(p, width, height) {
  grDevices::pdf(NULL, width = width, height = height)
  on.exit(grDevices::dev.off())
  print(p)
  grid::grid.force()
  path <- grid::grid.grep("verdict", grep = TRUE, viewports = TRUE)
  grid::seekViewport(attr(path, "vpPath"))
  tree_path <- path |>
    as.character() |>
    strsplit("::") |>
    purrr::pluck(1L) |>
    utils::head(-1L)
  tree <- grid::grid.get(grid::gPath(tree_path))
  grid::pushViewport(grid::viewport(gp = tree$gp))
  lapply(tree$children, function(g) {
    c(
      l = grid::convertX(grid::grobX(g, "west"), "in", valueOnly = TRUE),
      r = grid::convertX(grid::grobX(g, "east"), "in", valueOnly = TRUE),
      b = grid::convertY(grid::grobY(g, "south"), "in", valueOnly = TRUE),
      t = grid::convertY(grid::grobY(g, "north"), "in", valueOnly = TRUE)
    )
  })
}

boxes_overlap <- function(a, b) {
  a[["l"]] < b[["r"]] &&
    b[["l"]] < a[["r"]] &&
    a[["b"]] < b[["t"]] &&
    b[["b"]] < a[["t"]]
}

test_that("the marker's labels keep clear of each other at any size", {
  plots <- list(
    grim = grim_plot_line(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = "integer", zoom = "wide"),
    # Wider labels, and a neighbor right next to the reported value:
    percent = grim_plot_line(x = 67.4, n = 400, digits_x = 2, items_averaged = 1, data_type = "integer", x_meaning = "percentage", zoom = "wide"),
    grimmer = grimmer_plot_line(x = 5.23, sd = 2.55, n = 35, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
  )
  sizes <- list(c(8, 3.5), c(4.5, 3.5), c(8, 2), c(3, 3))

  for (name in names(plots)) {
    for (size in sizes) {
      info <- paste0(name, " at ", size[1L], " x ", size[2L], " in")
      boxes <- marker_boxes(plots[[name]], size[1L], size[2L])
      labels <- boxes[startsWith(names(boxes), "neighbor-label-")]
      elbows <- boxes[startsWith(names(boxes), "neighbor-elbow-")]
      arrow_x <- boxes$arrow[["l"]]
      labels |> length() |> expect_gt(0L)

      for (label in labels) {
        label |> boxes_overlap(boxes$verdict) |> expect_false(info = info)
      }
      if (length(labels) == 2L) {
        labels[[1L]] |> boxes_overlap(labels[[2L]]) |> expect_false(info = info)
      }
      # A connector stays on its own side of the arrow, so it can never run
      # through the arrowhead:
      for (elbow in elbows) {
        (arrow_x < elbow[["l"]] || arrow_x > elbow[["r"]]) |> expect_true(info = info)
      }
      # The top margin holds the whole stack:
      boxes$verdict[["t"]] |> call_on(\(x) x <= size[2L]) |> expect_true(info = info)
    }
  }
})

test_that("`grim_plot_line()` draws a square per attainable value only", {
  p <- grim_plot_line(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = "integer", zoom = "wide")
  p |> expect_s3_class("ggplot")
  # 28 attainable means per unit at `n = 28`, plus the whole number at the
  # far end of the window:
  p |> squares() |> length() |> expect_equal(29L)
  p |> squares() |> call_on(\(x) 5.19 %in% x) |> expect_false()
  # Square in any device: the panel's aspect ratio converts the side.
  tile <- p |> ggplot2::layer_data(3L)
  ratio <- (tile$ymax - tile$ymin)[1L] / (tile$xmax - tile$xmin)[1L]
  ratio |> expect_equal(1 / diff(range(p$coordinates$limits$x)) / p$theme$aspect.ratio)
})

test_that("the marker says and shows the verdict", {
  incon <- 5.19 |> grim_plot_line(n = 28, digits_x = 2, items_averaged = 1, data_type = "integer") |> marker()
  con   <- 5.19 |> grim_plot_line(n = 26, digits_x = 2, items_averaged = 1, data_type = "integer") |> marker()
  incon$label  |> expect_equal("5.19\ninconsistent")
  con$label    |> expect_equal("5.19\nconsistent")
  incon$colour |> expect_equal("#ff2a00")
  con$colour   |> expect_equal("#0047ff")
  # The squares are labeled, so the nearest ones need no labels of their own:
  incon$neighbors |> expect_length(0L)
  con$neighbors   |> expect_length(0L)
  # Without room for labels on the axis, 145 / 28 and 146 / 28 get them:
  wide <- 5.19 |> grim_plot_line(n = 28, digits_x = 2, items_averaged = 1, data_type = "integer", zoom = "wide") |> marker()
  wide$neighbors |> expect_equal(c("5.18", "5.21"))

  pct <- 67.4 |> grim_plot_line(n = 400, digits_x = 2, items_averaged = 1, data_type = "integer", x_meaning = "percentage", zoom = "wide") |> marker()
  pct$label     |> expect_match("^67.40%\n")
  pct$neighbors |> expect_equal(c("67.25%", "67.50%"))

  p <- grimmer_plot_line(x = 5.23, sd = 2.55, n = 35, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
  p |> marker() |> purrr::pluck("label") |> expect_equal("2.55\ninconsistent")
  p <- grimmer_plot_line(x = 5.23, sd = 2.55, n = 31, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
  p |> marker() |> purrr::pluck("label") |> expect_equal("2.55\nconsistent")
})

test_that("`grim_plot_line()` needs `digits_x` and a decidable `n`", {
  5.19         |> grim_plot_line(n = 28, items_averaged = 1, data_type = "integer")                 |> expect_error("digits_x")
  5.19         |> grim_plot_line(n = 28.5, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_error("whole")
  5.19         |> grim_plot_line(n = 28, digits_x = 2, items_averaged = 1, data_type = "integer", x_meaning = NA) |> expect_error("`x_meaning`")
  c(5.19, 5.2) |> grim_plot_line(n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")   |> expect_error()
  # The errors name the function the user called, not a helper:
  err <- rlang::catch_cnd(grim_plot_line(x = 5.19, n = 28.5, digits_x = 2, items_averaged = 1, data_type = "integer"))
  err$call[[1L]] |> expect_equal(quote(grim_plot_line))
  err <- rlang::catch_cnd(grim_plot_line(x = 5.19, n = 1:2, digits_x = 2, items_averaged = 1, data_type = "integer"))
  err$call[[1L]] |> expect_equal(quote(grim_plot_line))
})

test_that("`grimmer_plot_line()` draws a square per attainable value only", {
  p <- grimmer_plot_line(x = 5.23, sd = 2.55, n = 31, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")
  p |> expect_s3_class("ggplot")
  p |> squares() |> length() |> call_on(\(len) len > 0L && len < 101L) |> expect_true()
  p |> squares() |> call_on(\(x) 2.55 %in% x) |> expect_true()
})

test_that("`grimmer_plot_line()` checks its own arguments", {
  5.23 |> grimmer_plot_line(sd = 2.55, n = 31, items_averaged = 1, data_type = "integer") |> expect_error("digits")
  5.23 |>
    grimmer_plot_line(sd = 2.55, n = 1, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer") |>
    expect_error("at least 2")
})

test_that("the comb plots raise no warnings", {
  # A deprecation warns once per session, so earlier tests could hide one:
  rlang::local_options(lifecycle_verbosity = "warning")
  67.4 |> grim_plot_line(n = 40, digits_x = 1, items_averaged = 1, data_type = "integer", x_meaning = "percentage") |> expect_no_warning()
  -2.51 |> grim_plot_line(n = 20, digits_x = 2, items_averaged = 1, data_type = "integer") |> expect_no_warning()
  2.74 |>
    grimmer_plot_line(sd = 0.96, n = 63, digits_x = 2, digits_sd = 2, items_averaged = 2, data_type = "integer") |>
    expect_no_warning()
})


test_that("non-finite or missing input is an error that names the argument", {
  # These used to fail inside `seq()` or an `if()` with base R errors:
  NA_real_ |> grim_plot_line(20, 2, items_averaged = 1, data_type = "integer")  |> expect_error("`x` must be a finite")
  Inf      |> grim_plot_line(20, 2, items_averaged = 1, data_type = "integer")  |> expect_error("`x` must be a finite")
  5.19     |> grim_plot_line(20, NA, items_averaged = 1, data_type = "integer") |> expect_error("`digits_x` must be")
  NA_real_ |>
    grimmer_plot_line(1.2, 20, digits_x = 2, digits_sd = 1, items_averaged = 1, data_type = "integer") |>
    expect_error("`x` must be a finite")
  5.19 |>
    grimmer_plot_line(-1.2, 20, digits_x = 2, digits_sd = 1, items_averaged = 1, data_type = "integer") |>
    expect_error("`sd` can't be negative")
})

test_that("large `n` is not labeled in scientific notation", {
  5.19 |>
    grim_plot_line(100000, 2, items_averaged = 1, data_type = "integer") |>
    purrr::pluck("labels", "x") |>
    expect_equal("Mean (n = 100000)")
})

# `plot_comb()` would fail on an `NA` from `grimmer()`, in `rep()` or `if ()`.
# Every input that makes `grimmer()` return `NA` is rejected up front, so valid
# input must always be drawn. A new `NA` branch in `grimmer_scalar()` without a
# matching check in `grimmer_plot_line()` fails here:
test_that("`grimmer_plot_line()` draws every valid input", {
  # The edges -- an SD of zero, the smallest `n` -- with
  # the rounding methods taking turns, since a full cross would take minutes:
  grid <- tidyr::expand_grid(
    x = c(0.5, 3.17),
    sd = c(0, 1.25),
    n = c(2, 28),
    items = 1:2
  )
  roundings <- c("up_or_down", "down", "even", "anti_trunc", "ties_away")
  grid$rounding <- rep_len(roundings, nrow(grid))
  grid |>
    purrr::pmap(function(x, sd, n, items, rounding) {
      grimmer_plot_line(
        x = x, sd = sd, n = n, digits_x = 2, digits_sd = 2, items_averaged = items,
        data_type = "integer",
        rounding = rounding
      )
    }) |>
    purrr::map_lgl(ggplot2::is_ggplot) |>
    all() |>
    expect_true()
})


test_that("the percentage comb stays within 0 and 100", {
  grim_plot_line(100, 3, 0, 1, x_meaning = "percentage", data_type = "integer") |> squares() |> max() |> expect_equal(100)
  grim_plot_line(0, 3, 0, 1, x_meaning = "percentage", data_type = "integer")   |> squares() |> min() |> expect_equal(0)
})


test_that("near-whole arguments are drawn as the whole numbers they are", {
  # The minimum `n` was compared to the raw value, so these were errors:
  1.5 |> grim_plot_line(1 - 1e-9, digits_x = 1 - 1e-9, items_averaged = 1, data_type = "integer")                     |> ggplot2::layer_data() |> expect_equal(ggplot2::layer_data(grim_plot_line(1.5, 1, digits_x = 1, items_averaged = 1, data_type = "integer")))
  1.5 |> grimmer_plot_line(0.71, 2 - 1e-9, digits_x = 1, digits_sd = 2 - 1e-9, items_averaged = 1, data_type = "integer") |> ggplot2::layer_data() |> expect_equal(ggplot2::layer_data(grimmer_plot_line(1.5, 0.71, 2, digits_x = 1, digits_sd = 2, items_averaged = 1, data_type = "integer")))
})


test_that("a comb with too many values is an error, not a hang", {
  # Each value is a test, and their number grows tenfold per decimal place:
  5.1234567 |> grim_plot_line(28, 7, 1, zoom = "wide", data_type = "integer")                 |> expect_error("10,000,001 values")
  5.21      |> grimmer_plot_line(1.623457, 28, 2, 6, 1, zoom = "wide", data_type = "integer") |> expect_error("1,000,001 values")
  5.1234    |> grim_plot_line(28, 4, 1, zoom = "wide", data_type = "integer")                 |> expect_s3_class("ggplot")
  # Zoomed in, the window stops widening at that limit:
  5.1234567 |> grim_plot_line(28, 7, 1, data_type = "integer") |> suppressWarnings() |> expect_s3_class("ggplot")
  5.21      |> grimmer_plot_line(1.623457, 28, 2, 6, 1, data_type = "integer") |> expect_s3_class("ggplot")
})


test_that("the zoomed window shows the reported value and its nearest attainable ones", {
  # 10 values to either side, of which these are attainable at `n = 28`:
  5.19 |> grim_plot_line(28, 2, 1, data_type = "integer") |> squares() |> expect_equal(c(5.11, 5.14, 5.18, 5.21, 5.25, 5.29))
  # 5.14 is within reach, so the window doesn't widen to 5.29 on the other side:
  p <- 5.15 |> grim_plot_line(7, 2, 1, data_type = "integer")
  p |> squares() |> expect_equal(5.14)
  # The axis line spans the window and 3% of it to either side:
  axis <- p |> ggplot2::layer_data(2L)
  (axis$xend - axis$x) |> expect_equal(0.2 * 1.06)
  # Attainable percentages at `n = 2` are 0, 50, and 100. The window widens to
  # the nearest one, 0, 200 values away, and stops 3 values beyond it, at the
  # limit, rather than reaching 50:
  p <- 20 |> grim_plot_line(2, 1, 1, x_meaning = "percentage", data_type = "integer")
  p |> squares() |> expect_equal(0)
  axis <- p |> ggplot2::layer_data(2L)
  (axis$xend - axis$x) |> expect_equal(40.3 * 1.06)
  0.3 |> grim_plot_line(2, 1, 1, x_meaning = "percentage", data_type = "integer") |> squares() |> expect_equal(0)
  # `"medium"` reaches the nearest attainable value on either side, even for a
  # consistent value, and three values beyond the farther one:
  67.4 |> grim_plot_line(40, 1, 1, x_meaning = "percentage", zoom = "medium", data_type = "integer") |> squares() |> expect_equal(c(65.0, 67.5, 70.0))
  67.5 |> grim_plot_line(40, 1, 1, x_meaning = "percentage", zoom = "medium", data_type = "integer") |> squares() |> expect_equal(c(65.0, 67.5, 70.0))
  20   |> grim_plot_line(2, 1, 1, x_meaning = "percentage", zoom = "medium", data_type = "integer")   |> squares() |> expect_equal(c(0, 50))
  # `"wide"` shows the whole unit of the proportion, 0% to 100%, with n + 1
  # attainable values, as ticks that are drawn with an outline:
  p <- 67.4 |> grim_plot_line(40, 1, 1, x_meaning = "percentage", zoom = "wide", data_type = "integer")
  p |> squares() |> expect_equal(seq(0, 100, by = 2.5))
  p |> ggplot2::layer_data(3L) |> _$colour |> unique() |> expect_equal("grey75")
  5.19 |> grim_plot_line(28, 2, 1, zoom = "wide", data_type = "integer") |> ggplot2::layer_data(3L) |> _$colour |> unique() |> expect_equal(NA)
  5.23 |> grimmer_plot_line(2.55, 35, 2, 2, 1, zoom = "medium", data_type = "integer") |> squares() |> range() |> expect_equal(c(2.45, 2.65))
  # A consistent value keeps the 10 values to either side:
  5.19 |> grim_plot_line(26, 2, 1, data_type = "integer") |> squares() |> range() |> expect_equal(c(5.12, 5.27))
  # No more than the whole window with few decimal places:
  5 |> grim_plot_line(28, 0, 1, data_type = "integer") |> squares() |> expect_equal(c(4, 5, 6))
  5.23 |> grimmer_plot_line(2.55, 35, 2, 2, 1, data_type = "integer") |> squares() |> range() |> expect_equal(c(2.45, 2.65))
  5.19 |> grim_plot_line(28, 2, 1, zoom = NA, data_type = "integer")   |> expect_error("`zoom`")
  5.19 |> grim_plot_line(28, 2, 1, zoom = TRUE, data_type = "integer") |> expect_error("`zoom`")
})


test_that("squares are labeled if there is room, and grey unless reported", {
  incon <- 5.19 |> grim_plot_line(28, 2, 1, data_type = "integer")
  con   <- 5.19 |> grim_plot_line(26, 2, 1, data_type = "integer")
  incon |> axis_labels() |> expect_equal(c("5.11", "5.14", "5.18", "5.21", "5.25", "5.29"))
  incon |> fills() |> unique() |> expect_equal("grey75")
  con   |> fills() |> call_on(\(f) f[squares(con) == 5.19]) |> expect_equal("#0047ff")
  con   |> fills() |> call_on(\(f) f[squares(con) != 5.19]) |> unique() |> expect_equal("grey75")
  # Every value is attainable at `n = 300`, so the squares are too close:
  5.19 |> grim_plot_line(300, 2, 1, data_type = "integer")                          |> axis_labels() |> expect_type("closure")
  5.19 |> grim_plot_line(28, 2, 1, zoom = "wide", data_type = "integer")            |> axis_labels() |> expect_type("closure")
  67.4 |> grim_plot_line(40, 1, 1, x_meaning = "percentage", data_type = "integer") |> axis_labels() |> expect_equal("67.5%")

  p <- 5.23 |> grimmer_plot_line(2.55, 31, 2, 2, 1, color_con = "orange", color_attainable = "black", data_type = "integer")
  p |> fills() |> unique() |> sort() |> expect_equal(c("black", "orange"))
  p |> purrr::pluck("labels", "x") |> expect_equal("SD (mean = 5.23, n = 31)")
})


test_that("a string `n` or `items_averaged` is a type error, not an undecidable case", {
  5.19 |> grim_plot_line("28", 2, 1, data_type = "integer")              |> expect_error("`n` must be one of these types")
  5.19 |> grimmer_plot_line(1.2, 28, 2, 1, "1", data_type = "integer") |> expect_error("`items_averaged` must be one of these types")
})


test_that("ticks without a label per square are at values that can be reported", {
  # ggplot2's own breaks lay between them, such as 0.25 with one decimal
  # place, and were labeled as rounded, some of them twice:
  axis <- function(plot) {
    params <- ggplot2::ggplot_build(plot)$layout$panel_params[[1L]]$x
    list(breaks = params$get_breaks(), labels = params$get_labels())
  }
  expect_ticks_reportable <- function(plot, digits) {
    ticks <- axis(plot)
    ticks$breaks |> expect_equal(round(ticks$breaks, digits))
    ticks$labels |> anyDuplicated() |> expect_equal(0L)
    ticks$labels |> sub(pattern = "%", replacement = "") |> as.numeric() |> expect_equal(ticks$breaks)
    # Evenly spaced, without a tick at each end a fraction of a step away:
    ticks$breaks |> stats::na.omit() |> diff() |> range() |> diff() |> expect_equal(0, tolerance = 1e-8)
  }

  grimmer_plot_line(x = 5.19, sd = 1, n = 28, digits_x = 2, digits_sd = 0, items_averaged = 1, data_type = "integer")                  |> expect_ticks_reportable(0)
  grimmer_plot_line(x = 5.19, sd = 0.4, n = 28, digits_x = 2, digits_sd = 1, items_averaged = 1, data_type = "integer", zoom = "wide") |> expect_ticks_reportable(1)
  grimmer_plot_line(x = 5.19, sd = 0, n = 28, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")                  |> expect_ticks_reportable(2)
  grim_plot_line(x = 2150.5, n = 40, digits_x = 1, items_averaged = 1, data_type = "integer", zoom = "wide")                           |> expect_ticks_reportable(1)
  grim_plot_line(x = 5.27, n = 300, digits_x = 2, items_averaged = 1, data_type = "integer")                                          |> expect_ticks_reportable(2)
  grimmer_plot_line(x = 5.27, sd = 1.31, n = 300, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "integer")               |> expect_ticks_reportable(2)
})

test_that("zero is not labeled as negative", {
  # A step just below zero rounded to a negative zero, labeled "-0.00":
  labels <- function(plot) ggplot2::ggplot_build(plot)$layout$panel_params[[1L]]$x$get_labels()
  grim_plot_line(x = -0.02, n = 30, digits_x = 2, items_averaged = 1, data_type = "integer") |> labels() |> expect_no_match("-0.00")
  grim_plot_line(x = 0.01, n = 31, digits_x = 2, items_averaged = 1, data_type = "integer")  |> labels() |> expect_no_match("-0.00")
  grim_plot_line(x = -0.02, n = 30, digits_x = 2, items_averaged = 1, data_type = "integer") |> labels() |> expect_contains("0.00")
})


test_that("a row without any attainable value in reach says so", {
  # The search for the nearest one stops after some values, which left an
  # empty row with no hint that attainable values exist:
  grim_plot_line(x = 5.12345, n = 3, digits_x = 5, items_averaged = 1, data_type = "integer") |> expect_warning("No attainable value is close enough")
  grim_plot_line(x = 5.12, n = 3, digits_x = 2, items_averaged = 1, data_type = "integer")    |> expect_no_warning()
  # Not if the whole window was searched, as for a mean that fails GRIM, which
  # no SD goes with:
  grimmer_plot_line(x = 5.19, sd = 0.4, n = 28, digits_x = 2, digits_sd = 1, items_averaged = 1, data_type = "integer") |> expect_no_warning()
  # Under `"medium"`, a side without a neighbor is short, too:
  grim_plot_line(x = 5.33333, n = 3, digits_x = 5, items_averaged = 1, data_type = "integer", zoom = "medium") |> expect_warning("on both sides")
})


test_that("`zoom = \"medium\"` searches as far as a side cut short allows", {
  # The search stopped at 9,999 values per side, short of 100% here, although
  # nothing lies below 0%:
  0 |> grim_plot_line(1, 2, 1, x_meaning = "percentage", zoom = "medium", data_type = "integer") |> squares() |> expect_equal(c(0, 100))
})


test_that("`data_type` is required and must be `\"integer\"`", {
  5.19 |> grim_plot_line(28, digits_x = 2, items_averaged = 1)                           |> expect_error("Need to specify `data_type`")
  5.19 |> grim_plot_line(28, digits_x = 2, items_averaged = 1, data_type = "continuous") |> expect_error("only applies to whole numbers")
  5.19 |> grim_plot_line(28, digits_x = 2, items_averaged = 1, data_type = NA)           |> expect_error("can't be `NA`")
  5.23 |> grimmer_plot_line(2.55, 31, digits_x = 2, digits_sd = 2, items_averaged = 1)                           |> expect_error("Need to specify `data_type`")
  5.23 |> grimmer_plot_line(2.55, 31, digits_x = 2, digits_sd = 2, items_averaged = 1, data_type = "continuous") |> expect_error("only applies to whole numbers")
})


test_that("`debit_plot_line()` draws the SDs that `debit()` admits", {
  p <- 0.35 |> debit_plot_line(0.49, 20, 2, 2, "integer")
  p |> expect_s3_class("ggplot")
  p |> squares() |> expect_equal(0.49)
  # Each SD in the window is a square exactly if `debit()` admits it:
  wide <- 0.35 |> debit_plot_line(0.41, 20, 2, 2, "integer", zoom = "wide") |> squares()
  admitted <- seq(0, 1, by = 0.01) |> call_on(\(sd) sd[debit(0.35, sd, 20, 2, 2, "integer")])
  wide |> expect_equal(admitted)
  0.35 |> debit_plot_line(0.41, 20, 2, 2, "integer", sd_meaning = "sd_pop", zoom = "wide") |> squares() |> expect_equal(0.48)
  0.5  |> debit_plot_line(0.5, 300, 1, 1, "integer", formula = "mean_n")                    |> squares() |> expect_equal(0.5)
})

test_that("`debit_plot_line()` checks its own arguments", {
  0.35 |> debit_plot_line(0.49, 20, 2)                                     |> expect_error("digits_sd")
  0.35 |> debit_plot_line(0.49, 20, 2, 2)                                  |> expect_error("Need to specify `data_type`")
  0.35 |> debit_plot_line(0.49, 20, 2, 2, "continuous")                    |> expect_error("only applies to whole numbers")
  0.35 |> debit_plot_line(0.49, 20, 2, 2, NA)                              |> expect_error("can't be `NA`")
  0.35 |> debit_plot_line(0.49, 1, 2, 2, "integer")                        |> expect_error("at least 2")
  0.35 |> debit_plot_line(0.49, 1, 2, 2, "integer", sd_meaning = "sd_pop") |> expect_s3_class("ggplot")
  1.35 |> debit_plot_line(0.49, 20, 2, 2, "integer")                       |> expect_error()
  c(0.35, 0.4) |> debit_plot_line(0.49, 20, 2, 2, "integer")               |> expect_error("length 1")
})
