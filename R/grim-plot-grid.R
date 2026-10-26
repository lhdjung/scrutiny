#' Visualize GRIM test results
#'
#' @description `grim_plot_grid()` plots the output of [`grim_map()`] or
#'   [`grimmer_map()`]. Each value set is placed by its sample size and the
#'   fractional part of its mean, in blue if consistent and red if not, on a
#'   background raster that shows which combinations are GRIM-consistent at
#'   all.
#'
#' @section Background raster: For one or two decimal places, the raster
#'   marks every combination of sample size and fractional part that is
#'   inconsistent with a dark tile. It depends on the number of decimal places,
#'   which is read from the `digits_x` column of `data`, and on the rounding
#'   method, which is read from the `grim_map()` call. Rasters are precomputed
#'   for every rounding method except `"up_from"`, `"down_from"`, and
#'   `"up_from_or_down_from"`; with these, `grim_plot_grid()` errors unless
#'   `show_raster = FALSE`. With three or more decimal places, the tiles
#'   would be too small, so a gradient shows the probability of inconsistency
#'   instead.
#'
#'   A raster only applies to one number of decimal places, so `grim_plot_grid()`
#'   errors if `digits_x` varies across rows. Use `split_by_digits = TRUE` to
#'   get one plot per number of decimal places instead, or `digits` to choose
#'   one.
#'
#'   With `percent = TRUE` in the `grim_map()` call, percentages are plotted
#'   as the proportions they were tested as, with two more decimal places.
#'
#'   Set `show_data = FALSE` to plot the raster alone, controlled by `digits`,
#'   `rounding`, and `n`.
#'
#'   Under the default `rounding = "up_or_down"`, few values are inconsistent
#'   at sample sizes 40 and 80 with two decimal places (4 and 8 with one),
#'   because many achievable means there are ties, which the default accepts
#'   in both directions.
#'
#' @section Negative means: The y-axis is the fractional part of the absolute
#'   value of `x`, so a mean of `-2.51` is drawn at `0.51`. GRIM's granularity
#'   is symmetric around zero, so this is the value set's place on the grid.
#'   For a one-directional rounding method such as `"up"`, the raster behind a
#'   negative value is the one for the mirrored method, since a tie in a
#'   negative number goes the other way (unless `symmetric = TRUE`). The tile's
#'   color comes from the `consistency` column and is unaffected.
#'
#' @param data Data frame returned by [`grim_map()`] or [`grimmer_map()`],
#'   or by their `*_seq()` and `*_total_n()` variants.
#' @param show_data Logical. If `FALSE`, only the raster is drawn. Default is
#'   `TRUE`.
#' @param show_gradient Logical. With three or more decimal places, should a
#'   gradient be drawn? Default is `TRUE`.
#' @param split_by_digits Logical. If `TRUE`, return a named list with one
#'   plot per number of decimal places instead of erroring when they differ.
#'   Cannot be combined with `digits`. Default is `FALSE`.
#' @param digits Integer. Number of decimal places for the raster. Default is
#'   `NULL`, in which case it is read from the `digits_x` column, or failing
#'   that, inferred from `x`. For percentages, give the number of decimal
#'   places as reported; 2 are added.
#' @param n Integer. Maximal value on the x-axis. Default is `NULL`, in which
#'   case the axis spans the raster and the data.
#' @param rounding String. Only used if `show_data` is `FALSE` or if `data`
#'   has no rounding method recorded by a mapper, as when built by hand.
#'   Default is `"up_or_down"`.
#' @param show_raster Logical. Should the raster be drawn? Default is `TRUE`.
#' @param tile_size Numeric. Size of the tiles for value sets. Default is
#'   `1.5`.
#' @param raster_color String. Color of the raster, and of the gradient that
#'   replaces it with three or more decimal places. Default is `"grey75"`.
#'
#' @include utils.R
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876
#'
#' @return A ggplot object. With `split_by_digits = TRUE`, a named list of
#'   ggplot objects, one per distinct non-zero number of decimal places (e.g.,
#'   `list(digits_1 = ..., digits_2 = ...)`), returned invisibly after each
#'   plot is printed.
#'
#' @export
#'
#' @examples
#' # `grim_plot_grid()` reads the number of decimal places
#' # from the `digits_x` column:
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1) |>
#'   grim_plot_grid()
#'
#' # The raster follows the rounding method of the `grim_map()` call:
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1, rounding = "ceiling") |>
#'   grim_plot_grid()
#'
#' # Percentages are plotted as proportions:
#' pigs2 |>
#'   grim_map(digits_x = 1, items_averaged = 1, percent = TRUE) |>
#'   grim_plot_grid()
#'
#' # Other colors for the value sets:
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1) |>
#'   grim_plot_grid() +
#'   ggplot2::scale_colour_manual(
#'     values = c("TRUE" = "darkgreen", "FALSE" = "orange"),
#'     aesthetics = c("colour", "fill"),
#'     guide = "none"
#'   )

grim_plot_grid <- function(
  data = NULL,
  show_data = TRUE,
  show_raster = TRUE,
  show_gradient = TRUE,
  split_by_digits = FALSE,
  n = NULL,
  digits = NULL,
  rounding = "up_or_down",
  tile_size = 1.5,
  raster_color = "grey75"
) {
  # Checks ----

  inherits_grim <- inherits(data, "scrutiny_grim_map")
  inherits_grimmer <- inherits(data, "scrutiny_grimmer_map")

  if (!inherits_grim) {
    if (inherits_grimmer) {
      # Without a `reason` column, GRIMMER inconsistencies can't be told apart:
      reason <- data[["reason"]]
      reason <- reason[!is.na(reason)]
      n_grimmer_cases <- reason |>
        stringr::str_detect("GRIMMER") |>
        which() |>
        length()
      if (n_grimmer_cases > 0L) {
        msg_incons <- if (n_grimmer_cases == 1L) {
          "inconsistency"
        } else {
          "inconsistencies"
        }
        # "Also" if there are GRIM inconsistencies as well, which the plot is
        # about; consistent value sets are no inconsistencies of either kind:
        if (any(stringr::str_detect(reason, "^GRIM inconsistent"))) {
          msg_viz <- "Also visualizing"
        } else {
          msg_viz <- "Visualizing"
        }
        cli::cli_alert("{msg_viz} {n_grimmer_cases} GRIMMER {msg_incons}.")
      }
    } else if (show_data) {
      abort_in_export(
        "!" = "`grim_plot_grid()` needs GRIM or GRIMMER test results.",
        "x" = "`data` is not the output of `grim_map()`, `grim_map_seq()`, \\
        or `grim_map_total_n()`; or of the respective `grimmer_*()` functions.",
        "i" = "The only exception is an \"empty\" plot that shows the \\
        background raster but no empirical test results. Create such a plot \\
        with `show_data = FALSE`."
      )
    }
  }

  if (show_data) {
    check_cols_needed(data, c("x", "n", "consistency"))
  }

  if (!show_data && any(inherits_grim, inherits_grimmer)) {
    msg_grimmer <- if (inherits_grimmer) {
      " and GRIMMER"
    } else {
      ""
    }
    cli::cli_warn(c(
      "Test results are not visualized.",
      "!" = "You set `show_data` to `FALSE`, but still passed \\
      GRIM{msg_grimmer} test results to `grim_plot_grid()`.",
      "!" = "Only the background raster or gradient will be shown, not the \\
      tested data."
    ))
  }

  if (!is.null(digits)) {
    if (split_by_digits) {
      abort_in_export(
        "!" = "`digits` and `split_by_digits = TRUE` can't be combined.",
        "i" = "`split_by_digits` takes the decimal counts from the data, \
        whereas `digits` sets one for the whole plot."
      )
    }
    if (length(digits) != 1L) {
      abort_in_export(
        "!" = "`digits` must have length 1 (i.e., be a single number).",
        "x" = "It has length {length(digits)}."
      )
    } else if (!is_whole_number(digits)) {
      abort_in_export(
        "!" = "`digits` must be a whole number.",
        "x" = "It is {digits}."
      )
    }
  }

  # Transformations ----

  # A dummy value set lets the rest of the function run unchanged; it is never
  # drawn:
  if (!show_data) {
    data <- tibble::new_tibble(
      x = list(
        x = 0,
        n = 1,
        items_averaged = 1,
        digits_x = 2L,
        consistency = TRUE
      ),
      nrow = 1L
    )
    attr(data, "scrutiny") <- list(
      settings = new_settings(list(rounding = rounding), list(), "grim_map")
    )
  }

  is_percent <- isTRUE(scrutiny_args(data)$percent)

  # Zero-row mapper output is an ordinary result of `dplyr::filter()`; without
  # this check, `digits` would come out `NA` and fail further down:
  if (nrow(data) == 0L) {
    abort_in_export(
      "`data` has no rows, so there is nothing to plot.",
      "i" = "A GRIM plot needs at least one value set."
    )
  }

  # An undecided case has no color, so ggplot2 would drop it anyway, with a
  # vague "Removed 1 row containing missing values":
  if (show_data && anyNA(data$consistency)) {
    n_undecided <- sum(is.na(data$consistency))
    if (n_undecided == nrow(data)) {
      abort_in_export(
        "No value set in `data` could be decided.",
        "x" = "All {nrow(data)} `consistency` value{?s} {?is/are} `NA`.",
        "i" = "A tile is colored by its verdict, so there is nothing to draw.",
        "i" = "Set `show_data = FALSE` for the background raster on its own."
      )
    }
    cli::cli_warn(c(
      "!" = "Dropping {n_undecided} undecidable value set{?s} \\
      from the plot.",
      "i" = "Their `consistency` is `NA`, and a tile is colored by its \\
      verdict, so they cannot be drawn.",
      "i" = "The remaining {nrow(data) - n_undecided} value set{?s} \\
      {?is/are} plotted."
    ))
    data <- data[!is.na(data$consistency), ]
  }

  if (is.null(digits)) {
    # `decimal_places()` on numeric `x` is unreliable because trailing zeros
    # are lost (`5.00` reads as 0 decimal places), so it is only a fallback for
    # data built by hand, without the `digits_x` column every mapper adds.
    has_digits_x_col <- "digits_x" %in% colnames(data)

    digits_x <- if (has_digits_x_col) {
      data$digits_x
    } else {
      decimal_places(data$x)
    }

    # Always a list, even with a single decimal count, so that the return type
    # doesn't depend on the data:
    if (split_by_digits) {
      unique_digits <- digits_x |>
        unique() |>
        sort()

      # A mean with no decimal places has a fractional portion of zero, so
      # the raster says nothing about it. Percentages are exempt: their
      # decimal count is raised by 2 below.
      if (is_percent) {
        n_dropped <- 0L
      } else {
        n_dropped <- sum(digits_x == 0L)
        unique_digits <- unique_digits[unique_digits != 0L]
      }

      if (length(unique_digits) == 0L) {
        abort_in_export(
          "Every value set in `data` has `digits_x = 0`.",
          "i" = "A mean reported with no decimal places has a fractional \\
          portion of zero, so there is no GRIM plot to split by decimal \\
          places.",
          "i" = "Set `digits` explicitly (and `split_by_digits = FALSE`) if \
          you want a plot anyway."
        )
      }

      if (n_dropped > 0L) {
        cli::cli_warn(c(
          "!" = "Leaving out {n_dropped} value set{?s} with \\
          `digits_x = 0`.",
          "i" = "A mean reported with no decimal places has a fractional \\
          portion of zero, so the background raster says nothing about it."
        ))
      }

      plots <- lapply(unique_digits, function(d) {
        grim_plot_grid(
          data[digits_x == d, ],
          show_data = show_data,
          show_raster = show_raster,
          show_gradient = show_gradient,
          n = n,
          digits = d,
          rounding = rounding,
          tile_size = tile_size,
          raster_color = raster_color
        )
      })
      names(plots) <- paste0("digits_", unique_digits)
      # Auto-printing would show a list rather than draw the plots:
      for (p_split in plots) {
        print(p_split)
      }
      if (length(plots) > 1L) {
        cli::cli_alert_success(
          "Created {length(plots)} GRIM plots, one for each number of \\
          decimal places: {unique_digits}."
        )
      }
      return(invisible(plots))
    }

    if (show_raster) {
      if (!all(digits_x[1L] == digits_x)) {
        # One call returns one plot, and a raster fits only one decimal count.
        means_percentages <- dplyr::if_else(
          is_percent,
          "Percentages",
          "Means"
        )
        dp_unique <- unique(digits_x)
        if (length(dp_unique) <= 3L) {
          dp_unique_presented <- sort(dp_unique)
          msg_starting_with <- ":"
        } else {
          dp_unique_presented <- sort(dp_unique)[1:3]
          msg_starting_with <- ", starting with"
        }

        if (has_digits_x_col) {
          abort_in_export(
            "{means_percentages} have {length(dp_unique)} different \\
            numbers of decimal places{msg_starting_with} \\
            {dp_unique_presented}.",
            "i" = "Set `split_by_digits = TRUE` to get one plot per \\
            distinct number of decimal places instead of an error."
          )
        }

        abort_in_export(
          "{means_percentages} must have the same number of decimal places.",
          "x" = "There are {length(dp_unique)} unique numbers of decimal \\
          places in `x`{msg_starting_with} {dp_unique_presented}.",
          "i" = "The background raster is only informative if the number of \\
          decimal places is consistent across the \\
          {tolower(means_percentages)}.",
          "i" = "Avoid this error by plotting {tolower(means_percentages)} \\
          separately for each number of decimal places. (Alternatively, you \\
          can specify `digits` as the number of decimal places for which \\
          the plot should be shown. Be aware that this will not be sensible \\
          with regard to all {tolower(means_percentages)}.)"
        )
      }
    }

    digits <- digits_x[1L]
  }

  # GRIM's granularity is `n * items_averaged`. After the `split_by_digits`
  # branch, whose recursive calls do this themselves. As a double, since two
  # integers can multiply to more than the integer range:
  if ("items_averaged" %in% colnames(data)) {
    data$n <- as.double(data$n) * data$items_averaged
  }

  # Mirror `grim_scalar()`, which tests a percentage as a proportion with two
  # more decimal places: the raster must match the value that was tested. A
  # `digits` given by the caller counts the percentage as reported, like
  # `digits_x`.
  if (is_percent) {
    digits <- digits + 2L
    data$x <- data$x / 100
  }

  # The fractional portion of a mean with no decimal places is always zero, and
  # there is no raster for it.
  if (digits < 1L) {
    abort_in_export(
      "`digits` must be at least 1, but it is {digits}.",
      "i" = "The y-axis is the fractional portion of the mean, which is zero \\
      for a mean reported with no decimal places.",
      "i" = "With `percent = TRUE` in the `grim_map()` call, a whole-number \\
      percentage does have a fractional portion as a decimal number."
    )
  }

  # Preparations ----

  p10 <- 10^digits

  # Only a user-supplied `n` bounds the x-axis with a raster; see below.
  n_given <- !is.null(n)
  if (
    n_given && (length(n) != 1L || !is.numeric(n) || !is.finite(n) || n < 1)
  ) {
    abort_in_export(
      "`n` must be a single finite number of at least 1, or `NULL`.",
      "x" = "It is {wrap_in_quotes_or_backticks(n)}."
    )
  }
  if (!n_given) {
    n <- max(p10, if (show_data) data$n, na.rm = TRUE)
  }

  frac_unit <- 1 / p10

  if (show_raster) {
    if (digits <= 2L) {
      # Data built by hand has no recorded rounding method:
      rounding_id <- scrutiny_args(data)$rounding
      if (is.null(rounding_id)) {
        rounding_id <- rounding
      }

      # The rasters are keyed by the basic `rounding` names, so resolve the
      # `"ties_*"` methods to those. `symmetric = FALSE` because the axis is
      # non-negative: `"ties_away"` and `"ties_up"` share the `"up"` raster,
      # `"ties_zero"` and `"ties_down"` the `"down"` one. See the "Negative
      # means" section of the docs.
      rounding_id <- resolve_ties_rounding(rounding_id, FALSE)$rounding

      # Precomputed in data-raw/data-gen.R, stored in R/sysdata.rda. The
      # `"up_from"` family has none: with a `threshold`, there is no single
      # raster to precompute.
      df_plot <- GRIM_RASTERS[[paste(digits, rounding_id, sep = "_")]]

      if (is.null(df_plot)) {
        abort_in_export(
          "No background raster available for `rounding = {rounding_id}`",
          "i" = "Use a different `rounding` specification within the \\
          `grim_map()` call or set `show_raster` to `FALSE` within the \\
          `grim_plot_grid()` call."
        )
      }
    } else {
      # No raster: the tiles would be too small, so a gradient is drawn
      # further down. The tile layer is still added pro forma and needs data.
      df_plot <- TIBBLE_FRAC_N_ZERO
    }
  }

  # The fractional portion of the *absolute* value: `x - trunc(x)` would put a
  # negative `x` below the axis. GRIM's granularity is symmetric around zero
  # (achievable means are `k / n`), so `-2.51` sits on the same grid as `2.51`.
  data_emp <- data |>
    dplyr::mutate(x = abs(x) - trunc(abs(x))) |>
    dplyr::rename(frac = x)

  mean_percent_label <- if (is_percent) {
    "% (as decimal)"
  } else {
    "mean"
  }

  # The plot itself ----

  if (show_raster) {
    p <- ggplot2::ggplot(data = df_plot) +
      ggplot2::geom_tile(
        mapping = ggplot2::aes(
          x = .data$n,
          y = .data$frac
        ),
        fill = raster_color
      ) +
      ggplot2::theme(
        panel.border = ggplot2::element_rect(fill = NA, colour = "grey50"),
        panel.background = ggplot2::element_rect(fill = "white", colour = NA),
        panel.grid = ggplot2::element_blank()
      )
    if (digits > 2L) {
      if (show_gradient) {
        gradient <-
          grDevices::colorRampPalette(c(raster_color, "white"))(10000)
        grob_gradient <- gradient |>
          t() |>
          grid::rasterGrob(
            width = grid::unit(1, "npc"),
            height = grid::unit(1, "npc")
          )
        p <- p +
          ggplot2::geom_tile(
            data = df_plot,
            mapping = ggplot2::aes(
              x = .data$n,
              y = .data$frac
            )
          ) +
          # The gradient runs from 0 to `p10`, where the probability of a GRIM
          # inconsistency reaches 0, whatever the extent of the x-axis:
          ggplot2::annotation_custom(grob_gradient, xmin = 0, xmax = p10)
      }
      # The bound itself is set on the coordinate system further down.
      p <- p +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(add = c(0, 0.01))
        )
    }
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::theme(
        panel.border = ggplot2::element_rect(fill = NA, colour = "grey50"),
        panel.background = ggplot2::element_rect(fill = "white", colour = NA),
        panel.grid = ggplot2::element_blank()
      )
  }
  # The value sets' colors are mapped rather than set, so that adding a scale
  # changes them. The raster's fill is set, so the scale doesn't reach it:
  if (show_data) {
    p <- p +
      ggplot2::geom_tile(
        data = data_emp,
        mapping = ggplot2::aes(
          x = .data$n,
          y = .data$frac,
          colour = .data$consistency,
          fill = .data$consistency
        ),
        linewidth = 1,
        width = tile_size / 2,
        height = (frac_unit * tile_size) / 2
      ) +
      ggplot2::scale_colour_manual(
        values = c("TRUE" = "royalblue1", "FALSE" = "red"),
        aesthetics = c("colour", "fill"),
        guide = "none"
      )
  }

  # Bound the axes on the coordinate system, not on the scales. A scale limit
  # discards a tile as soon as one edge is outside, e.g. a tile at a fractional
  # portion of 0, which reaches below it. A coordinate limit clips it instead.
  if (digits <= 2L) {
    p <- p +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank()
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(from = 0, to = 1, by = max(0.2, frac_unit)),
        expand = ggplot2::expansion(add = c(0.01, 0))
      ) +
      # No lower x-axis bound with a raster: it would add space between the
      # raster and the y-axis. The upper bound is set only if the user chose
      # `n`; otherwise, the raster's and the data's extent is kept as it is.
      ggplot2::scale_x_continuous(
        breaks = pretty(c(0, n)),
        expand = ggplot2::expansion(mult = c(0, 0.01))
      ) +
      ggplot2::coord_cartesian(
        xlim = if (n_given) c(NA, n),
        ylim = c(0, 1)
      )
  } else {
    # Bound the x-axis too, so that it runs the full width of the gradient:
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = pretty(c(0, n)),
        expand = ggplot2::expansion(mult = c(0, 0.01))
      ) +
      ggplot2::coord_cartesian(xlim = c(0, n), ylim = c(0, 1))
  }

  # Returned, not printed: `print()` would draw whenever the result is
  # assigned, added to, or composed.
  p +
    ggplot2::labs(
      x = "Sample size",
      y = paste("Fractional portion of", mean_percent_label)
    ) +
    ggplot2::theme(aspect.ratio = 1)
}


#' Deprecated name of `grim_plot_grid()`
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#'   `grim_plot()` was renamed to [`grim_plot_grid()`] in scrutiny 1.0.0.
#'
#' @param ... Passed on to [`grim_plot_grid()`].
#'
#' @keywords internal
#' @export

grim_plot <- function(...) {
  lifecycle::deprecate_soft("1.0.0", "grim_plot()", "grim_plot_grid()")
  grim_plot_grid(...)
}
