#' Visualize GRIM test results
#'
#' @description `grim_plot_grid()` plots the output of [`grim_map()`] or
#'   [`grimmer_map()`]. Each value set is placed by its sample size and the
#'   fractional part of its mean, in blue if consistent and red if not, on a
#'   background raster that shows which combinations are GRIM-consistent at
#'   all.
#'
#' @section Background raster: For one or two decimal places, the raster
#'   shows every combination of sample size and fractional part that is
#'   GRIM-consistent in grey and every inconsistent one in white. It depends
#'   on the number of decimal places, which is read from the `digits_x` column
#'   of `data`, and on the rounding method, which is read from the
#'   `grim_map()` call. The white tiles are the cells of [`grim_grid()`] whose
#'   `consistency` is `FALSE`. With three or more
#'   decimal places, they would be too small, so a gradient shows the
#'   probability of inconsistency instead.
#'
#'   A raster only applies to one number of decimal places, so `grim_plot_grid()`
#'   errors if `digits_x` varies across rows. Use `split_by_digits = TRUE` to
#'   get one plot per number of decimal places instead, or `digits_x` to choose
#'   one.
#'
#'   With `x_meaning = "percentage"` in the `grim_map()` call, percentages are
#'   plotted
#'   as the proportions they were tested as, with two more decimal places.
#'
#'   Set `show_data = FALSE` to plot the raster alone, controlled by `digits_x`,
#'   `rounding`, and `n`.
#'
#'   Under the default `rounding = "up_or_down"`, fewer values are
#'   inconsistent at sample sizes 40 and 80 than at their neighbors with two
#'   decimal places (4 and 8 with one), and none at 80 (or 8), because many
#'   achievable means there are ties, which the default accepts in both
#'   directions.
#'
#' @section Negative means: The y-axis is the fractional part of the absolute
#'   value of `x`, so a mean of `-2.51` is drawn at `0.51`. GRIM's granularity
#'   is symmetric around zero, so this is the value set's place on the grid.
#'   A one-directional rounding method treats the absolute value of a negative
#'   number the other way: `"ceiling"` like `"floor"`, and `"up"` like `"down"`
#'   (unless `symmetric = TRUE`). If all means are negative, the raster is the
#'   one for this mirrored method, so that it agrees with the tiles. With
#'   positive and negative means under such a method, no single raster does;
#'   it is then the one for positive means, with a warning. The tile's color
#'   comes from the `consistency` column and is unaffected.
#'
#' @param data Data frame returned by [`grim_map()`] or [`grimmer_map()`],
#'   or by their `*_seq()` and `*_total_n()` variants.
#' @param show_data Logical. If `FALSE`, only the raster is drawn. Default is
#'   `TRUE`.
#' @param show_gradient Logical. With three or more decimal places, should a
#'   gradient be drawn? Default is `TRUE`.
#' @param split_by_digits Logical. If `TRUE`, return a named list with one
#'   plot per number of decimal places instead of erroring when they differ.
#'   Cannot be combined with `digits_x`. Default is `FALSE`.
#' @param digits_x Integer. Number of decimal places for the raster. Default is
#'   `NULL`, in which case it is read from the `digits_x` column that every
#'   mapper adds, or, if that was removed, inferred from `x`, which loses
#'   trailing zeros. For percentages, give the number of decimal places as
#'   reported; 2 are added.
#' @param n Integer. Maximal value on the x-axis. Default is `NULL`, in which
#'   case the axis spans the raster and the data.
#' @param rounding,threshold The same as in [`grim()`], for the raster. Only
#'   used if `show_data` is `FALSE` or if the record of the mapper call was
#'   removed from `data`: otherwise, the raster follows the settings of that
#'   call, and giving others is an error. Defaults are `"up_or_down"` and `5`.
#' @param show_raster Logical. Should the raster be drawn? Default is `TRUE`.
#' @param tile_size Numeric. Size of the tiles for value sets. Default is
#'   `1.5`.
#' @param color_raster String. Color of the consistent cells of the raster, and
#'   of the consistent end of the gradient that replaces it with three or more
#'   decimal places. Default is `"grey75"`.
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
#'   grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
#'   grim_plot_grid()
#'
#' # The raster follows the rounding method of the `grim_map()` call:
#' pigs1 |>
#'   grim_map(
#'     digits_x = 2,
#'     items_averaged = 1,
#'     data_type = "integer",
#'     rounding = "ceiling"
#'   ) |>
#'   grim_plot_grid()
#'
#' # Percentages are plotted as proportions:
#' pigs2 |>
#'   grim_map(
#'     digits_x = 1,
#'     items_averaged = 1,
#'     data_type = "integer",
#'     x_meaning = "percentage"
#'   ) |>
#'   grim_plot_grid()
#'
#' # Other colors for the value sets:
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
#'   grim_plot_grid() +
#'   ggplot2::scale_colour_manual(
#'     values = c("TRUE" = "#2f7d74", "FALSE" = "#c1522e"),
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
  digits_x = NULL,
  rounding = "up_or_down",
  threshold = 5,
  tile_size = 1.5,
  color_raster = "grey75"
) {
  # Checks ----

  # Called `digits` below, where it may also come from the data:
  digits <- digits_x

  check_flag(show_data)
  check_flag(show_raster)
  check_flag(show_gradient)
  check_flag(split_by_digits)

  inherits_grim <- inherits(data, "scrutiny_grim_map")
  inherits_grimmer <- inherits(data, "scrutiny_grimmer_map")

  # GRIMMER output is announced further down, once the value sets that are not
  # drawn have been dropped:
  if (!inherits_grim && !inherits_grimmer) {
    if (show_data) {
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

  # The raster follows the settings of the mapper call, so others given here
  # would be ignored:
  given <- list(
    rounding = if (!missing(rounding)) rounding,
    threshold = if (!missing(threshold)) threshold
  )
  if (show_data) {
    check_cols_needed(data, c("x", "n", "consistency"))
    args_call <- scrutiny_args(data)
    for (name in names(given)) {
      value <- given[[name]]
      recorded <- args_call[[name]]
      if (
        !is.null(value) &&
          !is.null(recorded) &&
          !isTRUE(all.equal(value, recorded, check.attributes = FALSE))
      ) {
        abort_in_export(
          "`{name}` disagrees with the mapper call behind `data`.",
          "x" = "It is {wrap_in_quotes_or_backticks(value)}, but the call \\
          used {wrap_in_quotes_or_backticks(recorded)}.",
          "i" = "The raster follows the call. Leave out `{name}`, or give it \\
          to the mapper instead."
        )
      }
    }
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
        "!" = "`digits_x` and `split_by_digits = TRUE` can't be combined.",
        "i" = "`split_by_digits` takes the decimal counts from the data, \
        whereas `digits_x` sets one for the whole plot."
      )
    }
    if (length(digits) != 1L) {
      abort_in_export(
        "!" = "`digits_x` must have length 1 (i.e., be a single number).",
        "x" = "It has length {length(digits)}."
      )
    }
    check_not_factor(digits, "digits_x")
    if (!is.numeric(digits) || !isTRUE(is_whole_number(digits))) {
      abort_in_export(
        "!" = "`digits_x` must be a whole number.",
        "x" = "It is {wrap_in_quotes_or_backticks(digits)}."
      )
    }
    # Compared and used as the whole number it is accepted as:
    digits <- round_whole(digits)
  }

  if (
    !is.numeric(tile_size) ||
      length(tile_size) != 1L ||
      !isTRUE(is.finite(tile_size) && tile_size > 0)
  ) {
    abort_in_export(
      "`tile_size` must be a single positive number.",
      "x" = "It is {wrap_in_quotes_or_backticks(tile_size)}."
    )
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

  is_percent <- identical(scrutiny_args(data)$x_meaning, "percentage")

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
        "i" = "A tile is colored by its consistency, so there is nothing to draw.",
        "i" = "Set `show_data = FALSE` for the background raster on its own."
      )
    }
    cli::cli_warn(c(
      "!" = "Dropping {n_undecided} undecidable value set{?s} \\
      from the plot.",
      "i" = "{cli::qty(n_undecided)}{?Its/Their} `consistency` {?value is/values \\
      are} `NA`, and a tile is colored by its consistency, so {?it/they} \\
      cannot be drawn."
    ))
    data <- data[!is.na(data$consistency), ]
  }

  # A value set can be decided with its mean, decimal places, or size
  # missing, as in GRIMMER if no mean goes with the SD, or in GRIM if a mean
  # passes with any `n`. It has no place on the grid, and the comparison of
  # decimal places below would fail on it:
  if (show_data) {
    cols_place <- intersect(c("x", "n", "items_averaged"), colnames(data))
    if (is.null(digits)) {
      cols_place <- intersect(c(cols_place, "digits_x"), colnames(data))
    }
    is_placeless <- Reduce(`|`, lapply(data[cols_place], is.na))
    n_placeless <- sum(is_placeless)
    if (n_placeless == nrow(data)) {
      msg_rows <- if (nrow(data) == 1L) {
        "the only row"
      } else {
        paste("each of the", nrow(data), "rows")
      }
      abort_in_export(
        "No value set in `data` has a place on the grid.",
        "x" = "{.or {.code {cols_place}}} is missing in {msg_rows}.",
        "i" = "Set `show_data = FALSE` for the background raster on its own."
      )
    }
    if (n_placeless > 0L) {
      cli::cli_warn(c(
        "!" = "Dropping {n_placeless} value set{?s} without a place on the \\
        grid from the plot.",
        "i" = "{.or {.code {cols_place}}} is missing there."
      ))
      data <- data[!is_placeless, ]
    }
  }

  # Continuous data pass whatever the raster says about them:
  if (show_data) {
    data <- drop_continuous(
      data,
      "GRIM",
      "i" = "Set `show_data = FALSE` for the background raster on its own."
    )
  }

  if (is.null(digits)) {
    # `decimal_places()` on numeric `x` is unreliable because trailing zeros
    # are lost (`5.00` reads as 0 decimal places), so it is only a fallback for
    # mapper output whose `digits_x` column was removed.
    has_digits_x_col <- "digits_x" %in% colnames(data)

    digits_data <- if (has_digits_x_col) {
      data$digits_x
    } else {
      decimal_places(data$x)
    }

    # Always a list, even with a single decimal count, so that the return type
    # doesn't depend on the data:
    if (split_by_digits) {
      unique_digits <- digits_data |>
        unique() |>
        sort()

      # A mean with no decimal places has a fractional portion of zero, so
      # the raster says nothing about it. Percentages are exempt: their
      # decimal count is raised by 2 below.
      if (is_percent) {
        n_dropped <- 0L
      } else {
        n_dropped <- sum(digits_data == 0L)
        unique_digits <- unique_digits[unique_digits != 0L]
      }

      if (length(unique_digits) == 0L) {
        abort_in_export(
          "Every value set in `data` has `digits_x = 0`.",
          "i" = "A mean reported with no decimal places has a fractional \\
          portion of zero, so there is no GRIM plot to split by decimal \\
          places.",
          "i" = "Set `digits_x` explicitly (and `split_by_digits = FALSE`) if \
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

      # Settings only if given, which the call checked against the mapper's:
      settings <- given[!vapply(given, is.null, logical(1L))]
      plots <- lapply(unique_digits, function(d) {
        args <- list(
          data = data[digits_data == d, ],
          show_data = show_data,
          show_raster = show_raster,
          show_gradient = show_gradient,
          n = n,
          digits_x = d,
          tile_size = tile_size,
          color_raster = color_raster
        )
        do.call(grim_plot_grid, c(args, settings))
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
      if (!all(digits_data[1L] == digits_data)) {
        # One call returns one plot, and a raster fits only one decimal count.
        means_percentages <- dplyr::if_else(
          is_percent,
          "Percentages",
          "Means"
        )
        dp_unique <- unique(digits_data)
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
          can specify `digits_x` as the number of decimal places for which \\
          the plot should be shown. Be aware that this will not be sensible \\
          with regard to all {tolower(means_percentages)}.)"
        )
      }
    }

    digits <- digits_data[1L]
  }

  # Not if the data are not drawn, nor before splitting them: each plot's own
  # call says what it shows. After the value sets that are not drawn have been
  # dropped:
  if (inherits_grimmer && show_data) {
    alert_grimmer_incon(data)
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
      "`digits_x` must be at least 1, but it is {digits}.",
      "i" = "The y-axis is the fractional portion of the mean, which is zero \\
      for a mean reported with no decimal places.",
      "i" = "With `x_meaning = \"percentage\"` in the `grim_map()` call, a \\
      whole-number percentage does have a fractional portion as a decimal \\
      number."
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
      # Unless the mapper's record was removed from the output, its settings
      # apply:
      args <- scrutiny_args(data)
      if (!is.null(args$rounding)) {
        rounding <- args$rounding
      }
      if (!is.null(args$threshold)) {
        threshold <- args$threshold
      }

      # The axis is the fractional part of the absolute value, where a
      # negative mean is rounded the other way: `"ceiling"` takes -2.575 up to
      # -2.57, which takes 2.575 down to 2.57, as `"floor"` does. So negative
      # means get the raster of the mirrored method. With both signs in the
      # data, one raster can't be right for all of them:
      mirrored <- mirror_rounding(rounding, threshold, isTRUE(args$symmetric))
      is_negative <- show_data & data$x < 0
      is_mirrored <- mirrored$rounding != rounding ||
        mirrored$threshold != threshold
      if (any(is_negative) && is_mirrored) {
        if (all(is_negative | data$x == 0)) {
          rounding <- mirrored$rounding
          threshold <- mirrored$threshold
        } else {
          cli::cli_warn(c(
            "!" = "The raster is the one for positive means.",
            "x" = "`data` has {sum(is_negative)} negative mean{?s}, which \\
            `rounding = \"{rounding}\"` rounds the other way, so the \\
            raster behind {cli::qty(sum(is_negative))}{?it/them} can \\
            disagree with the tile{?s}.",
            "i" = "Plot negative and positive means separately."
          ))
        }
      }

      df_plot <- grim_grid_inconsistent(digits, rounding, threshold)
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
        # The pro forma tile of three or more decimal places is not a cell:
        # without the gradient over it, it was a white box at the origin.
        fill = if (digits <= 2L) "white" else NA
      ) +
      # The panel is the consistent cells, so only inconsistent ones are drawn:
      ggplot2::theme(
        panel.border = ggplot2::element_rect(fill = NA, colour = "grey50"),
        # Without the gradient, a panel of three or more decimal places
        # shows no cells, so it is white, as without the raster: grey would
        # call every cell consistent.
        panel.background = ggplot2::element_rect(
          fill = if (digits > 2L && !show_gradient) "white" else color_raster,
          colour = NA
        ),
        panel.grid = ggplot2::element_blank()
      )
    if (digits > 2L) {
      if (show_gradient) {
        gradient <-
          grDevices::colorRampPalette(c("white", color_raster))(10000)
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
        values = COLORS_CONSISTENCY,
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


# Rows of continuous data, which pass any test, are left out of a plot with a
# warning, or an error if none are left. The dots add bullets to the error.
drop_continuous <- function(data, test, ...) {
  is_continuous <- data[["data_type"]] %in% "continuous"
  if (!any(is_continuous)) {
    return(data)
  }
  n_continuous <- sum(is_continuous)
  # DEBIT's whole numbers are 0 and 1:
  numbers <- if (test == "DEBIT") "binary data (0 and 1)" else "whole numbers"
  if (n_continuous == nrow(data)) {
    abort_in_export(
      "All value sets in `data` are continuous data.",
      "i" = "{test} only applies to {numbers}, so there is nothing to \\
      draw.",
      ...
    )
  }
  cli::cli_warn(c(
    "!" = "Dropping {n_continuous} value set{?s} of continuous data from the \\
    plot.",
    "i" = "{test} only applies to {numbers}."
  ))
  data[!is_continuous, ]
}


#' Deprecated name of `grim_plot_grid()`
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#'   `grim_plot()` was renamed to [`grim_plot_grid()`] in scrutiny 1.0.0. It
#'   keeps the signature it had in scrutiny 0.6.1, so that calls by position
#'   still work.
#'
#' @param data,show_data,show_raster,show_gradient,n,rounding,tile_size Passed
#'   on to [`grim_plot_grid()`].
#' @param digits,raster_color The old names of `grim_plot_grid()`'s `digits_x`
#'   and `color_raster`.
#' @param color_cons,color_incons,tile_alpha,raster_alpha No longer have any
#'   effect: `grim_plot_grid()` draws its tiles in fixed colors.
#' @param ... Other arguments of [`grim_plot_grid()`], such as
#'   `split_by_digits`.
#'
#' @keywords internal
#' @export

grim_plot <- function(
  data = NULL,
  show_data = TRUE,
  show_raster = TRUE,
  show_gradient = TRUE,
  n = NULL,
  digits = NULL,
  rounding = "up_or_down",
  color_cons = deprecated(),
  color_incons = deprecated(),
  tile_alpha = deprecated(),
  tile_size = 1.5,
  raster_alpha = deprecated(),
  raster_color = NULL,
  ...
) {
  lifecycle::deprecate_warn("1.0.0", "grim_plot()", "grim_plot_grid()")
  removed <- list(
    color_cons = color_cons,
    color_incons = color_incons,
    tile_alpha = tile_alpha,
    raster_alpha = raster_alpha
  )
  for (name in names(removed)) {
    if (lifecycle::is_present(removed[[name]])) {
      lifecycle::deprecate_warn(
        "1.0.0",
        paste0("grim_plot(", name, ")"),
        details = "It no longer has any effect."
      )
    }
  }
  args <- list(
    data = data,
    show_data = show_data,
    show_raster = show_raster,
    show_gradient = show_gradient,
    n = n,
    tile_size = tile_size
  )
  # Only if given, which `grim_plot_grid()` checks against the mapper call:
  if (!missing(rounding)) {
    args$rounding <- rounding
  }
  # The old names are passed on as the new ones, which the dots may not also
  # give:
  dots <- list(...)
  renames <- list(digits_x = digits, color_raster = raster_color)
  names_old <- c(digits_x = "digits", color_raster = "raster_color")
  for (new in names(renames)) {
    if (is.null(renames[[new]])) {
      next
    }
    if (new %in% names(dots)) {
      abort_renamed_both(names_old[[new]], new)
    }
    args[[new]] <- renames[[new]]
  }
  do.call(grim_plot_grid, c(args, dots))
}


# The rounding method that treats the absolute value of a negative number as
# `rounding` treats the number itself, with its threshold. A method that
# rounds a tie either way, or to an even digit, or by the absolute value in
# the first place is its own mirror image; `symmetric = TRUE` makes `"up"`,
# `"down"`, and the `"*_from"` methods such methods. The `"ties_*"` strings
# are resolved first, as `roundwork::bound_numerators()` resolves them.
mirror_rounding <- function(rounding, threshold, symmetric) {
  resolved <- roundwork::resolve_ties_rounding(
    rounding,
    # A `"ties_*"` string brings its own:
    if (startsWith(rounding, "ties_")) FALSE else symmetric
  )
  basic <- resolved$rounding
  out <- list(rounding = rounding, threshold = threshold)
  if (basic %in% c("ceiling", "floor")) {
    out$rounding <- if (basic == "ceiling") "floor" else "ceiling"
  } else if (!resolved$symmetric && basic %in% c("up", "down")) {
    out$rounding <- if (basic == "up") "down" else "up"
  } else if (
    !resolved$symmetric &&
      basic %in% c("up_from", "down_from", "up_from_or_down_from")
  ) {
    out$rounding <- switch(
      basic,
      up_from = "down_from",
      down_from = "up_from",
      basic
    )
    out$threshold <- 10 - threshold
  }
  out
}


# GRIMMER inconsistencies are drawn like GRIM ones, which the plot is about, so
# their number is announced. Without a `reason` column, they can't be told
# apart.
alert_grimmer_incon <- function(data) {
  reason <- data[["reason"]]
  reason <- reason[!is.na(reason)]
  n_grimmer_cases <- sum(stringr::str_detect(reason, "GRIMMER"))
  if (n_grimmer_cases == 0L) {
    return(invisible())
  }
  msg_incon <- if (n_grimmer_cases == 1L) "inconsistency" else "inconsistencies"
  # "Also" if there are GRIM inconsistencies as well; consistent value sets
  # are no inconsistencies of either kind:
  msg_viz <- if (any(stringr::str_detect(reason, "^GRIM inconsistent"))) {
    "Also visualizing"
  } else {
    "Visualizing"
  }
  cli::cli_alert("{msg_viz} {n_grimmer_cases} GRIMMER {msg_incon}.")
}
