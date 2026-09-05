#' Visualize GRIM test results
#'
#' @description `grim_plot()` visualizes summary data and their mutual GRIM
#'   consistency. Call this function only on a data frame that resulted from a
#'   call to [`grim_map()`].
#'
#'   Consistent and inconsistent value pairs from the input data frame are shown
#'   in distinctive colors. By default, consistent value pairs are blue and
#'   inconsistent ones are red. These and other parameters of the underlying
#'   geoms can be controlled via arguments.
#'
#'   The background raster follows the `rounding` argument from the `grim_map()`
#'   call (unless any of the plotted mean or proportion values has more than 2
#'   decimal places, in which case a gradient is shown, not a raster).
#'
#' @section Background raster: The background raster shows the probability of
#'   GRIM-inconsistency for random means or proportions, from 0 (all
#'   inconsistent) to the greatest number on the x-axis (all consistent). If the
#'   number of decimal places in the inputs -- means or percentages -- is 3 or
#'   greater, individual points would be too small to display. In these cases,
#'   there will not be a raster but a gradient, showing the overall trend.
#'
#'   As any raster only makes sense with respect to one specific number of
#'   decimal places, the function will throw an error if these numbers differ
#'   among input `x` values (and `show_raster` is `TRUE`). You can avoid the
#'   error and force plotting by specifying `digits` as the number of decimal
#'   places for which the raster or gradient should be displayed.
#'
#'   For 1 or 2 decimal places, the raster will be specific to the rounding
#'   procedure. As the raster varies by rounding procedure, it will
#'   automatically correspond to the `rounding` argument specified in the
#'   preceding [`grim_map()`] call. This works fast because the raster is based
#'   on data saved in the package itself, so these data don't need to be
#'   generated anew every time the function is called. Inconsistent value sets
#'   are marked with dark boxes. All other places in the raster denote
#'   consistent value sets. The raster is independent of the data -- it only
#'   follows the `rounding` specification in the [`grim_map()`] call and the
#'   `digits` argument in `grim_plot()`.
#'
#'   Display an "empty" plot, one without empirical test results, by setting
#'   `show_data` to `FALSE`. You can then control key parameters of the plot
#'   with `digits` and `rounding`.
#'
#'   With [`grim_map()`]'s default for `rounding`, `"up_or_down"`, strikingly
#'   few values are flagged as inconsistent for sample sizes 40 and 80 (or 4 and
#'   8). This effect disappears if `rounding` is set to any other value (see
#'   `vignette("rounding-options")`).
#'
#'   The 4/8 leniency effect arises because accepting values rounded either up
#'   or down is more careful and conservative than any other rounding procedure.
#'   In any case, `grim_plot()` doesn't cause this effect --- it only reveals
#'   it.
#'
#' @section Negative means: The y-axis is the fractional portion of the
#'   *absolute value* of `x`, so a mean of `-2.51` is drawn at `0.51`, in the
#'   same place as `2.51`. GRIM's granularity is symmetric around zero -- the
#'   achievable means of `n` integers are `k / n` for every whole number `k`,
#'   whatever its sign -- so this puts each value set at its correct point on
#'   the granularity grid rather than off the axis.
#'
#'   The background raster is the one place where the sign can still matter,
#'   and only for the one-directional rounding methods. `rounding = "up"`, for
#'   instance, breaks a tie in a negative number toward zero rather than away
#'   from it (unless `symmetric` is `TRUE`), so the raster drawn for `"up"`
#'   describes the mirror image of what such a value set was tested against.
#'   That concerns the backdrop only: every tile is colored from the
#'   `consistency` column, which [`grim_map()`] computed for the value as
#'   reported, sign included.
#'
#' @param data Data frame. Result of a call to [`grim_map()`].
#' @param show_data Logical. If set to `FALSE`, test results from the data are
#'   not displayed. Choose this if you only want to show the background raster.
#'   You can then control plot parameters directly via the `n`, `digits`, and
#'   `rounding` arguments. Default is `TRUE`.
#' @param show_gradient Logical. If the number of decimal places is 3 or
#'   greater, should a gradient be shown to signal the overall probability of
#'   GRIM inconsistency? Default is `TRUE`.
#' @param split_by_digits Logical (length 1). Set to `TRUE` to create a separate
#'   plot for each number of decimal places, stored in a named list, instead of
#'   throwing an error. Default is `FALSE`.
#' @param digits Integer. Number of decimal places for which the background
#'   raster will be generated. Default is `NULL`, in which case it is read from
#'   the `digits_x` column of `data` (or, failing that, inferred from the `x`
#'   values).
#' @param n Integer. Maximal value on the x-axis. Default is `NULL`, in which
#'   case `n` becomes `10 ^ digits` (e.g., `100` if `digits` is `2`).
#' @param rounding String. Only relevant if `show_data` is set to `FALSE`. The
#'   plot will then be constructed as it would be for data rounded in this
#'   particular way. Default is `"up_or_down"`.
#' @param show_raster Logical. If `TRUE` (the default), the plot has a
#'   background raster.
#' @param color_cons,color_incons Strings. Fill colors of the consistent and
#'   inconsistent scatter points. Defaults are `"royalblue1"` (consistent) and
#'   `"red"` (inconsistent).
#' @param tile_alpha,tile_size Numeric. Further parameters of the scatter
#'   points: opacity and, indirectly, size. Defaults are `1` and `1.5`.
#' @param raster_alpha,raster_color Numeric and string, respectively. Parameters
#'   of the background raster: opacity and fill color. Defaults are `1` and
#'   `"grey75"`.
#'
#' @include utils.R seq-decimal.R
#'
#' @references Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A
#'   Simple Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://journals.sagepub.com/doi/10.1177/1948550616673876

#' @return A ggplot object, returned the ordinary way: at the console,
#'   auto-printing draws it, and it can be added to, saved, or composed with
#'   other plots without a stray canvas appearing.
#'
#'   The exception is `split_by_digits = TRUE`, which returns a named list of
#'   ggplot objects, one per distinct non-zero number of decimal places (e.g.,
#'   `list(digits_1 = ..., digits_2 = ...)`). A list is not something
#'   auto-printing can draw, so that branch prints each plot itself and returns
#'   the list invisibly.

#' @export
#'
#' @examples
#' # Call `grim_plot()` following `grim_map()`. No need to pass `digits`
#' # along: `grim_map()` stores the `digits_x` it was given in a `digits_x`
#' # column, and `grim_plot()` reads the decimal count from there:
#' pigs1 |>
#'   grim_map(digits_x = 2) |>
#'   grim_plot()
#'
#' # If you change the rounding procedure
#' # in `grim_map()`, the plot will
#' # follow automatically if there is
#' # a difference:
#' pigs1 |>
#'   grim_map(digits_x = 2, rounding = "ceiling") |>
#'   grim_plot()
#'
#' # For percentages, the y-axis
#' # label also changes automatically:
#' pigs2 |>
#'   grim_map(digits_x = 1, percent = TRUE) |>
#'   grim_plot()

grim_plot <- function(
  data = NULL,
  show_data = TRUE,
  show_raster = TRUE,
  show_gradient = TRUE,
  split_by_digits = FALSE,
  n = NULL,
  digits = NULL,
  rounding = "up_or_down",
  color_cons = "royalblue1",
  color_incons = "red",
  tile_alpha = 1,
  tile_size = 1.5,
  raster_alpha = 1,
  raster_color = "grey75"
) {
  # Checks ----

  inherits_grim <- inherits(data, "scrutiny_grim_map")
  inherits_grimmer <- inherits(data, "scrutiny_grimmer_map")

  if (!inherits_grim) {
    # Issue an alert if any GRIMMER inconsistencies were found in `data`:
    if (inherits_grimmer) {
      reason <- data$reason[!is.na(data$reason)]
      n_grimmer_cases <- stringr::str_detect(reason, "GRIMMER")
      n_grimmer_cases <- length(which(n_grimmer_cases))
      if (n_grimmer_cases > 0L) {
        if (n_grimmer_cases == 1L) {
          msg_case_s <- "case was"
          msg_incons <- "inconsistency"
        } else {
          msg_case_s <- "cases were"
          msg_incons <- "inconsistencies"
        }
        if (n_grimmer_cases < length(reason)) {
          msg_viz <- "Also visualizing"
        } else {
          msg_viz <- "Visualizing"
        }
        cli::cli_alert("{msg_viz} {n_grimmer_cases} GRIMMER {msg_incons}.")
      }
    } else if (show_data) {
      cli::cli_abort(c(
        "!" = "`grim_plot()` needs GRIM or GRIMMER test results.",
        "x" = "`data` is not the output of `grim_map()`, `grim_map_seq()`, \\
        or `grim_map_total_n()`; or of the respective `grimmer_*()` functions.",
        "i" = "The only exception is an \"empty\" plot that shows the \\
        background raster but no empirical test results. Create such a plot \\
        with `show_data = FALSE`."
      ))
    }
  }

  # Warn the user who passed suitable data to `grim_plot()` but also set
  # `show_data` to `FALSE`, thereby defeating the data's purpose here:
  if (!show_data && any(inherits_grim, inherits_grimmer)) {
    msg_grimmer <- if (inherits_grimmer) {
      " and GRIMMER"
    } else {
      ""
    }
    cli::cli_warn(c(
      "Test results are not visualized.",
      "!" = "You set `show_data` to `FALSE`, but still passed \\
      GRIM{msg_grimmer} test results to `grim_plot()`.",
      "!" = "Only the background raster or gradient will be shown, not the \\
      tested data."
    ))
  }

  # The `digits` argument, if specified, must be a single integer-like number
  # because it controls the number of decimal places for which the plot will be
  # constructed:
  if (!is.null(digits)) {
    if (length(digits) != 1L) {
      cli::cli_abort(c(
        "!" = "`digits` must have length 1 (i.e., be a single number).",
        "x" = "It has length {length(digits)}."
      ))
    } else if (!is_whole_number(digits)) {
      cli::cli_abort(c(
        "!" = "`digits` must be a whole number.",
        "x" = "It is {digits}."
      ))
    }
  }

  # Transformations ----

  # In case the user set `show_data` to `FALSE`, a plot without empirical test
  # results (blue and/or red dots) will be shown. To this end, the function must
  # completely bypass the `data` argument. It does so via creating a dummy
  # object by that name:
  if (!show_data) {
    data <- tibble::new_tibble(
      x = list(
        x = "0.00",
        n = 1,
        items = 1,
        consistency = TRUE
      ),
      nrow = 1L,
      class = paste0("scrutiny_rounding_", rounding)
    )
  }

  # Nothing to read a decimal count off, no raster to choose, no data to draw.
  # A zero-row mapper output is an ordinary result of `dplyr::filter()`, so say
  # so plainly rather than let `digits` come out `NA` and fail further down:
  if (nrow(data) == 0L) {
    cli::cli_abort(c(
      "`data` has no rows, so there is nothing to plot.",
      "i" = "A GRIM plot needs at least one value set."
    ))
  }

  # A tile's color comes from the `consistency` column, so an undecided case has
  # no color and ggplot2 drops it. Dropping such rows here is the same outcome,
  # but said out loud rather than as "Removed 1 row containing missing values":
  if (show_data && anyNA(data$consistency)) {
    n_undecided <- sum(is.na(data$consistency))
    if (n_undecided == nrow(data)) {
      cli::cli_abort(c(
        "No value set in `data` could be decided.",
        "x" = "All {nrow(data)} `consistency` value{?s} {?is/are} `NA`.",
        "i" = "A tile is colored by its verdict, so there is nothing to draw.",
        "i" = "Set `show_data = FALSE` for the background raster on its own."
      ))
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
    # Every mapper stores its `digits_x` argument in a `digits_x` column, which
    # is a reliable source of the decimal count. `decimal_places()` on the
    # numeric `x` column is not: trailing zeros are lost, so `5.00` reads back
    # as 0 decimal places. Guess from `x` only if there is no such column, e.g.
    # because `data` was built by hand.
    has_digits_x_col <- "digits_x" %in% colnames(data)

    digits_x <- if (has_digits_x_col) {
      data$digits_x
    } else {
      decimal_places(data$x) # used to be wrapped in `max()`
    }

    if (show_raster) {
      if (!all(digits_x[1L] == digits_x)) {
        # A single call to `grim_plot()` always returns one plot, so a genuine
        # mix of decimal places is an error by default. Users can opt into one
        # plot per distinct decimal count via `split_by_digits`, which requires
        # a `digits_x` column to split on:
        if (split_by_digits && has_digits_x_col) {
          unique_digits <- sort(unique(data$digits_x))

          # A mean with no decimal places has a fractional portion of zero, so
          # its group would be a line of tiles along the x-axis against a raster
          # that says nothing about it. Leave those rows out, but say so:
          n_dropped <- sum(data$digits_x == 0L)
          unique_digits <- unique_digits[unique_digits != 0L]

          if (length(unique_digits) == 0L) {
            cli::cli_abort(c(
              "Every value set in `data` has `digits_x = 0`.",
              "i" = "A mean reported with no decimal places has a fractional \\
              portion of zero, so there is no GRIM plot to split by decimal \\
              places.",
              "i" = "Set `digits` explicitly if you want a plot anyway."
            ))
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
            grim_plot(
              data[data$digits_x == d, ],
              show_data = show_data,
              show_raster = show_raster,
              show_gradient = show_gradient,
              n = n,
              digits = d,
              rounding = rounding,
              color_cons = color_cons,
              color_incons = color_incons,
              tile_alpha = tile_alpha,
              tile_size = tile_size,
              raster_alpha = raster_alpha,
              raster_color = raster_color
            )
          })
          names(plots) <- paste0("digits_", unique_digits)
          # Auto-printing would render this branch's list as a list rather than
          # draw it, so print the plots here. The single-plot return below is a
          # plain ggplot object and needs no help:
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

        means_percentages <- dplyr::if_else(
          inherits(data, "scrutiny_percent_true"),
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
          cli::cli_abort(c(
            "{means_percentages} have {length(dp_unique)} different \\
            numbers of decimal places{msg_starting_with} \\
            {dp_unique_presented}.",
            "i" = "Set `split_by_digits = TRUE` to get one plot per \\
            distinct number of decimal places instead of an error."
          ))
        }

        cli::cli_abort(c(
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
        ))
      }
    }

    # The call will only pass the above test if all `x` values have the same
    # number of decimal places. Therefore, `digits` can now be determined simply
    # by taking the first element; or indeed any other element there might be:
    digits <- digits_x[1L]
  }

  # `percent = TRUE` in the `grim_map()` call means `x` is a proportion of 100,
  # which `grim_scalar()` tests by dividing by 100 and raising the decimal count
  # by 2. The plot must do the same, since the raster is indexed by the
  # granularity of the value that was tested: otherwise a percentage of `67.4`
  # is drawn against the one-decimal raster while the verdict coloring its tile
  # was reached at three. A `digits` given by the caller is the decimal count of
  # the percentage as reported, exactly like `digits_x`.
  is_percent <- inherits(data, "scrutiny_percent_true")

  if (is_percent) {
    digits <- digits + 2L
    data$x <- as.numeric(data$x) / 100
  }

  # The y-axis is the fractional portion of the mean, which is zero throughout
  # for values reported with no decimal places -- no plot to be had, and no
  # precomputed raster either. The `split_by_digits` branch above leaves such
  # rows out for the same reason.
  if (digits < 1L) {
    cli::cli_abort(c(
      "`digits` must be at least 1, but it is {digits}.",
      "i" = "The y-axis is the fractional portion of the mean, which is zero \\
      for a mean reported with no decimal places.",
      "i" = "With `percent = TRUE` in the `grim_map()` call, a whole-number \\
      percentage does have a fractional portion as a decimal number."
    ))
  }

  data$x <- as.numeric(data$x)

  # Preparations ----

  p10 <- 10^digits

  if (is.null(n)) {
    n <- p10
  }

  frac_unit <- 1 / p10

  # By default, a background raster is displayed in the plot:
  if (show_raster) {
    # For 1 or 2 decimal places, the function selects the appropriate raster
    # from among those saved within the package itself:
    if (digits <= 2L) {
      # Check the way `x` values were rounded in the preceding `grim_map()` call
      # to prepare selecting the plot background raster:
      dc <- class(data)
      rounding_id <- dc[stringr::str_detect(dc, "^scrutiny_rounding_")]
      rounding_id <- stringr::str_remove(rounding_id, "^scrutiny_rounding_")

      # The rasters are precomputed under the older `rounding` names, so resolve
      # the `"ties_*"` methods back to those. `symmetric = FALSE` because the
      # raster is drawn over a non-negative axis: `"ties_away"` and `"ties_up"`
      # share the `"up"` raster, `"ties_zero"` and `"ties_down"` the `"down"`
      # one. For a negative `x` this is the mirrored method's raster, since the
      # y-axis folds the value onto `abs(x)` while the rounding did not -- the
      # backdrop is mirrored, never the verdict. See the `Negative means`
      # section of this function's documentation.
      rounding_id <- resolve_ties_rounding(rounding_id, FALSE)$rounding

      # The rasters are precomputed in data-raw/data-gen.R and stored in
      # R/sysdata.rda, keyed by decimal count and rounding procedure. The
      # `"up_from"` family has no key: those methods take a `threshold`, so
      # there is no single raster to precompute.
      df_plot <- GRIM_RASTERS[[paste(digits, rounding_id, sep = "_")]]

      if (is.null(df_plot)) {
        cli::cli_abort(c(
          "No background raster available for `rounding = {rounding_id}`",
          "i" = "Use a different `rounding` specification within the \\
          `grim_map()` call or set `show_raster` to `FALSE` within the \\
          `grim_plot()` call."
        ))
      }
    } else {
      # Above 2 decimal places there is no raster -- the tiles would be too
      # small to see, and a gradient is drawn instead further down -- but the
      # tile layer is still added pro forma, so it needs a data frame:
      df_plot <- TIBBLE_FRAC_N_ZERO
    }
  }

  # Reduce `x` to the fractional portion of its *absolute* value: the y-axis
  # runs from 0 to 1, and `x - trunc(x)` would put a negative `x` outside it.
  # GRIM's granularity is the same on both sides of zero -- the achievable means
  # of `n` integers are `k / n` for every whole number `k` -- so `-2.51` sits on
  # the same grid as `2.51`. Only the background raster can come apart from
  # this, and only for the one-directional rounding methods; that affects the
  # backdrop, never the verdict, which is colored from `consistency`.
  data_emp <- data |>
    dplyr::mutate(x = abs(x) - trunc(abs(x))) |>
    dplyr::rename(frac = x)

  if (!show_data) {
    data_emp <- data_emp |>
      dplyr::mutate(dplyr::across(everything(), function(x) 0L))
  }

  # If `percent = TRUE` in the underlying `grim_map()` call, the y-axis label is
  # automatically adjusted to reflect the fact that the fractional values are
  # percentages (converted to decimal numbers), not means:
  mean_percent_label <- if (is_percent) {
    "% (as decimal)"
  } else {
    "mean"
  }

  # Automatically color the boxes of value pairs by whether they are
  # GRIM-consistent or not:
  color_by_consistency <- dplyr::if_else(
    data$consistency,
    color_cons,
    color_incons
  )

  # The plot itself ----

  # Background raster / gradient:
  if (show_raster) {
    # With 1 or 2 digits, the function provides a background raster...
    p <- ggplot2::ggplot(data = df_plot) +
      ggplot2::geom_tile(
        mapping = ggplot2::aes(
          x = .data$n,
          y = .data$frac
        ),
        alpha = raster_alpha,
        fill = raster_color
      ) +
      ggplot2::theme(
        panel.border = ggplot2::element_rect(fill = NA, colour = "grey50"),
        panel.background = ggplot2::element_rect(fill = "white", colour = NA),
        panel.grid = ggplot2::element_blank()
      )
    # ... but with more decimal places, individual boxes would be too small to
    # display, so we need a gradient instead to simply show the overall trend.
    # Boxes are still added pro forma; the call to `geom_tile()` is the same as
    # above (except for the `alpha` and `fill` specifications):
    if (digits > 2L) {
      if (show_gradient) {
        gradient <-
          grDevices::colorRampPalette(c(raster_color, "white"))(10000)
        p <- p +
          ggplot2::geom_tile(
            data = df_plot,
            mapping = ggplot2::aes(
              x = .data$n,
              y = .data$frac
            )
          ) +
          ggplot2::annotation_custom(grid::rasterGrob(
            t(gradient),
            width = grid::unit(1, "npc"),
            height = grid::unit(1, "npc")
          ))
      }
      # Keep the y-axis ranging from 0 to 1 with the gradient in place. The
      # bound itself is set on the coordinate system further down; see there.
      p <- p +
        ggplot2::scale_y_continuous(
          expand = ggplot2::expansion(add = c(0, 0.01))
        )
    }
  } else {
    # Without a raster there is still a plot to draw the data on:
    p <- ggplot2::ggplot() +
      ggplot2::theme(
        panel.border = ggplot2::element_rect(fill = NA, colour = "grey50"),
        panel.background = ggplot2::element_rect(fill = "white", colour = NA),
        panel.grid = ggplot2::element_blank()
      )
  }
  if (show_data) {
    p <- p +
      ggplot2::geom_tile(
        data = data_emp,
        mapping = ggplot2::aes(
          x = .data$n,
          y = .data$frac
        ),
        alpha = tile_alpha,
        linewidth = 1,
        color = color_by_consistency,
        fill = color_by_consistency,
        width = tile_size / 2,
        height = (frac_unit * tile_size) / 2
      )
  }

  # Both axes are bounded on the coordinate system rather than on the scales. A
  # scale limit *discards* everything outside of it, and a tile is outside as
  # soon as one edge is: a mean of `5.00` has a fractional portion of exactly 0,
  # so its tile reaches from `-0.00375` to `0.00375` and was dropped whole. A
  # coordinate limit zooms instead, so the tile is drawn and clipped at the
  # panel edge.
  if (digits <= 2L) {
    p <- p +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank()
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(from = 0, to = 1, by = max(0.2, frac_unit)),
        expand = ggplot2::expansion(add = c(0.01, 0))
      ) +
      # No x-axis bound with a raster: it would add space between the raster and
      # the y-axis.
      ggplot2::scale_x_continuous(
        breaks = seq(from = 0, to = n, by = (n / 5)),
        expand = ggplot2::expansion(mult = c(0, 0.01))
      ) +
      ggplot2::coord_cartesian(ylim = c(0, 1))
  } else {
    # With a gradient rather than a raster, the x-axis is bounded as well, so
    # that it runs the full width of the gradient:
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = seq(from = 0, to = n, by = (n / 5)),
        expand = ggplot2::expansion(mult = c(0, 0.01))
      ) +
      ggplot2::coord_cartesian(xlim = c(0, n), ylim = c(0, 1))
  }

  # Return the plot with axis labels -- returned, not printed. Auto-printing
  # draws it at the console anyway, whereas an explicit `print()` here would
  # draw a canvas nobody asked for whenever the result is assigned, added to, or
  # composed. Only the `split_by_digits` branch, which returns a list, still
  # needs one. Nor is anything suppressed: the guards at the top of this
  # function report dropped value sets themselves.
  p +
    ggplot2::labs(
      x = "Sample size",
      y = paste("Fractional portion of", mean_percent_label)
    ) +
    ggplot2::theme(aspect.ratio = 1)
}
