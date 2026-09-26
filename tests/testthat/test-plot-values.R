# The teeth are three `geom_segment()` layers after the two `annotate()` calls
# that draw the strip and the baseline: unattainable, ruled out by the scale,
# attainable. Their row counts are the verdicts the comb draws.
teeth_counts <- function(p) {
  built <- ggplot2::ggplot_build(p)$data
  vapply(built[3:5], nrow, integer(1L))
}

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
  tree_path <- utils::head(strsplit(as.character(path), "::")[[1L]], -1L)
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
    grim = grim_plot_values(x = 5.19, n = 28, digits_x = 2),
    # Wider labels, and a neighbor right next to the reported value:
    percent = grim_plot_values(x = 67.4, n = 40, digits_x = 1, percent = TRUE),
    # A single neighbor, with the marker near the right end of the window:
    grimmer = grimmer_plot_values(
      x = 3.00, sd = 2.08, n = 20, digits_x = 2, digits_sd = 2,
      min_val = 1, max_val = 5
    )
  )
  sizes <- list(c(8, 3.5), c(4.5, 3.5), c(8, 2), c(3, 3))

  for (name in names(plots)) {
    for (size in sizes) {
      info <- paste0(name, " at ", size[1L], " x ", size[2L], " in")
      boxes <- marker_boxes(plots[[name]], size[1L], size[2L])
      labels <- boxes[startsWith(names(boxes), "neighbor-label-")]
      elbows <- boxes[startsWith(names(boxes), "neighbor-elbow-")]
      arrow_x <- boxes$arrow[["l"]]
      expect_gt(length(labels), 0L)

      for (label in labels) {
        expect_false(boxes_overlap(label, boxes$verdict), info = info)
      }
      if (length(labels) == 2L) {
        expect_false(boxes_overlap(labels[[1L]], labels[[2L]]), info = info)
      }
      # A connector stays on its own side of the arrow, so it can never run
      # through the arrowhead:
      for (elbow in elbows) {
        expect_true(arrow_x < elbow[["l"]] || arrow_x > elbow[["r"]], info = info)
      }
      # The top margin holds the whole stack:
      expect_true(boxes$verdict[["t"]] <= size[2L], info = info)
    }
  }
})

test_that("`grim_plot_values()` draws one tooth per value in the unit window", {
  p <- grim_plot_values(x = 5.19, n = 28, digits_x = 2)
  expect_s3_class(p, "ggplot")
  counts <- teeth_counts(p)
  expect_equal(sum(counts), 101L)
  # 28 attainable means per unit at `n = 28`, plus the whole number at the
  # far end of the window:
  expect_equal(counts[[3L]], 29L)
  expect_equal(counts[[2L]], 0L)
})

test_that("the marker says and shows the verdict", {
  incons <- marker(grim_plot_values(x = 5.19, n = 28, digits_x = 2))
  cons <- marker(grim_plot_values(x = 5.19, n = 26, digits_x = 2))
  expect_equal(incons$label, "5.19\ninconsistent")
  expect_equal(cons$label, "5.19\nconsistent")
  expect_equal(incons$colour, "#c1522e")
  expect_equal(cons$colour, "#2f7d74")
  # 145 / 28 and 146 / 28:
  expect_equal(incons$neighbors, c("5.18", "5.21"))
  expect_length(cons$neighbors, 0L)

  # Attainable percentages at `n = 40` are 2.5 points apart, so the window
  # reaches 3 points beyond the unit on either side:
  pct <- marker(grim_plot_values(x = 67.4, n = 40, digits_x = 1, percent = TRUE))
  expect_match(pct$label, "^67.4%\n")
  expect_equal(pct$neighbors, c("65.0%", "67.5%"))

  # The scale bounds rule the SD out, so the verdict is an inconsistency
  # although GRIMMER alone would admit it:
  p <- grimmer_plot_values(
    x = 3.00, sd = 2.08, n = 20, digits_x = 2, digits_sd = 2,
    min_val = 1, max_val = 5
  )
  expect_equal(marker(p)$label, "2.08\ninconsistent")
  # Nothing attainable lies above it on the scale, so only one neighbor:
  expect_equal(marker(p)$neighbors, "2.05")
  p <- grimmer_plot_values(x = 5.23, sd = 2.55, n = 31, digits_x = 2, digits_sd = 2)
  expect_equal(marker(p)$label, "2.55\nconsistent")
})

test_that("`grim_plot_values()` needs `digits_x` and a decidable `n`", {
  expect_error(grim_plot_values(x = 5.19, n = 28), "digits_x")
  expect_error(grim_plot_values(x = 5.19, n = 28.5, digits_x = 2), "whole")
  expect_error(grim_plot_values(x = c(5.19, 5.2), n = 28, digits_x = 2))
  # The errors name the function the user called, not a helper:
  err <- rlang::catch_cnd(grim_plot_values(x = 5.19, n = 28.5, digits_x = 2))
  expect_equal(err$call[[1L]], quote(grim_plot_values))
  err <- rlang::catch_cnd(grim_plot_values(x = 5.19, n = 1:2, digits_x = 2))
  expect_equal(err$call[[1L]], quote(grim_plot_values))
})

test_that("`grimmer_plot_values()` marks what the scale bounds rule out", {
  p <- grimmer_plot_values(
    x = 3.00, sd = 2.08, n = 20, digits_x = 2, digits_sd = 2,
    min_val = 1, max_val = 5
  )
  expect_s3_class(p, "ggplot")
  counts <- teeth_counts(p)
  # The SD of ten 1s and ten 5s is about 2.05; everything GRIMMER admits above
  # that is out of reach on the scale:
  expect_gt(counts[[2L]], 0L)
  expect_gt(counts[[3L]], 0L)

  # Without the bounds there is no third state:
  p <- grimmer_plot_values(
    x = 5.23, sd = 2.55, n = 31, digits_x = 2, digits_sd = 2
  )
  expect_equal(teeth_counts(p)[[2L]], 0L)
  expect_equal(sum(teeth_counts(p)), 101L)
})

test_that("`grimmer_plot_values()` checks its own arguments", {
  expect_error(grimmer_plot_values(x = 5.23, sd = 2.55, n = 31), "digits")
  expect_error(
    grimmer_plot_values(x = 5.23, sd = 2.55, n = 1, digits_x = 2, digits_sd = 2),
    "at least 2"
  )
  expect_error(
    grimmer_plot_values(
      x = 5.23, sd = 2.55, n = 31, digits_x = 2, digits_sd = 2, min_val = 1
    ),
    "max_val"
  )
})

test_that("the comb plots raise no warnings", {
  expect_no_warning(grim_plot_values(x = 67.4, n = 40, digits_x = 1, percent = TRUE))
  expect_no_warning(grim_plot_values(x = -2.51, n = 20, digits_x = 2))
  expect_no_warning(grimmer_plot_values(
    x = 2.74, sd = 0.96, n = 63, digits_x = 2, digits_sd = 2, items = 2
  ))
})


test_that("non-finite or missing input is an error that names the argument", {
  # These used to fail inside `seq()` or an `if()` with base R errors:
  grim_plot_values(NA_real_, 20, 2) |> expect_error("`x` must be a finite")
  grim_plot_values(Inf, 20, 2) |> expect_error("`x` must be a finite")
  grim_plot_values(5.19, 20, NA) |> expect_error("`digits_x` must be")
  grimmer_plot_values(NA_real_, 1.2, 20, digits_x = 2, digits_sd = 1) |>
    expect_error("`x` must be a finite")
  grimmer_plot_values(5.19, -1.2, 20, digits_x = 2, digits_sd = 1) |>
    expect_error("`sd` can't be negative")
})

test_that("large `n` is not labeled in scientific notation", {
  grim_plot_values(5.19, 100000, 2)$labels$x |>
    expect_equal("Reported mean (n = 100000)")
})
