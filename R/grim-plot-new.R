

#' Visualize GRIM test results
#'
#' @description `grim_plot()` visualizes summary data and their mutual GRIM
#'   consistency. Call this function only on a data frame that resulted from a
#'   call to `grim_map()`.
#'
#'   Consistent and inconsistent value pairs from the input data frame are shown
#'   in distinctive colors. By default, consistent value pairs are blue and
#'   inconsistent ones are red. These and other parameters of the underlying
#'   geoms can be controlled via arguments.
#'
#'   If none of the plotted mean or proportion values has more than 2 decimal
#'   places, the background raster follows the `rounding` argument from the
#'   `grim_map()` call.
#'
#' @section Background raster: The background raster shows the probability of
#'   GRIM-inconsistency for random means or proportions, from 0 (all
#'   inconsistent) to the greatest number on the x-axis (all inconsistent). By
#'   default, that number is determined by the greatest number of decimal places
#'   among the tested means or proportions, such that 1 decimal place leads to
#'   10, two lead to 100, etc. You can also manually specify the number of
#'   decimal places via the `decimals` argument.
#'
#'   For 1 or 2 decimal places, the raster will be specific to the rounding
#'   method. As the raster varies by rounding procedure, it will automatically
#'   correspond to the `rounding` argument specified in the preceding
#'   `grim_map()` call. This works fast because the raster is based on data
#'   saved in the package itself, so these data don't need to be generated anew
#'   every time the function is called. Inconsistent value sets are marked with
#'   dark boxes. All other places in the raster denote consistent value sets.
#'   The raster is independent of the data, but it follows the `rounding`
#'   specification in the `grim_map()` call.
#'
#'   With the default, `"up_or_down"`, strikingly few values are flagged as
#'   inconsistent for sample sizes 40 and 80 (or 4 and 8). This effect
#'   disappears if `rounding` is set to any other value. For a list of values
#'   that `rounding` can take, see documentation for `grim()`, section
#'   `Rounding`.
#'
#'   The 4/8 leniency effect arises because accepting values rounded either up
#'   or down is more careful and conservative than any other rounding procedure.
#'   In any case, `grim_plot()` doesn't cause this effect --- it only reveals
#'   it.
#'
#' @param data Data frame. Result of a call to `grim_map()`.
#' @param show_gradient Boolean. If the number of decimal places is 3 or
#'   greater, should a gradient be shown to signal the overall probability of
#'   GRIM inconsistency? Default is `TRUE`.
#' @param show_full_range Boolean. Should the full range of the plot always be
#'   shown, regardless of the empirical values? Default is `TRUE`.
#' @param decimals Integer. Number of decimal places for which the background
#'   raster will be generated. Default is `NULL`, in which case the greatest
#'   number of decimal places from the means or proportions is used.
#' @param n Integer. Maximal value on the x-axis. Default is `NULL`, in which
#'   case `n` becomes `10 ^ decimals` (e.g., `100` if `decimals` is `2`).
#' @param show_raster Boolean. If `TRUE` (the default), the plot has a
#'   background raster.
#' @param color_cons,color_incons Strings. Fill colors of the consistent and
#'   inconsistent scatter points. Defaults are `"royalblue1"` (consistent) and
#'   `"red"` (inconsistent).
#' @param tile_alpha,tile_size Numeric. Further parameters of the scatter
#'   points: opacity and, indirectly, size. Defaults are `1` and `1.5`.
#' @param raster_alpha,raster_color Numeric and string, respectively. Parameters
#'   of the background raster: opacity and fill color. Defaults are `0.5` and
#'   `"grey50"`.
#'
#' @include utils.R seq-decimal.R
#'
#' @export
#'
#' @examples
#' # Call `grim_plot()` following `grim_map()`:
#' pigs1 %>%
#'   grim_map() %>%
#'   grim_plot()
#'
#' # If you change the rounding method
#' # in `grim_map()`, the plot will
#' # follow automatically:
#' pigs1 %>%
#'   grim_map(rounding = "up") %>%
#'   grim_plot()
#'
#' # For percentages, the y-axis
#' # label also changes automatically:
#' pigs2 %>%
#'   grim_map(percent = TRUE) %>%
#'   grim_plot()



grim_plot <- function(data = NULL,
                      show_raster = TRUE,
                      show_gradient = TRUE,
                      show_full_range = TRUE,
                      n = NULL,
                      decimals = NULL,
                      color_cons = "royalblue1",
                      color_incons = "red",
                      tile_alpha = 1,
                      tile_size = 1.5,
                      raster_alpha = 0.5,
                      raster_color = "grey50") {


  # Checks ----

  if (!inherits(data, "scr_grim_map")) {
    cli::cli_abort(c(
      "`data` is not `grim_map()` output",
      "x" = "`grim_plot()` only works with GRIM test results."
    ))
  }

  # if (!inherits(data, "scr_adjust_x_1")) {
  #   cli::cli_abort(c(
  #     "`grim_plot()` needs `grim_map(adjust_x = 1)`.",
  #     ">" = "Set `adjust_x` to `1` in your `grim_map()` call."
  #   ))
  # }

  if (!is.null(decimals)) {
    if (!length(decimals) == 1) {
      cli::cli_abort(c(
        "`decimals` has length {length(decimals)}",
        "x" = "It needs to have length 1."
      ))
    }
    if (!is_whole_number(decimals)) {
      cli::cli_abort(
        "`decimals` must be a whole number."
      )
    }
  }

  # if (decimals > 2) {
  #   cli::cli_abort(c(
  #     "`decimals` was given as `{decimals}`",
  #     "x" = "Currently, `decimals` can only be `1` or `2`."
  #   ))
  # }


  # Transformations ----

  if (is.null(decimals)) {
    decimals <- max(decimal_places(data$x))
  }

  data <- data %>%
    dplyr::mutate(x = (as.numeric(x) * items))

  # if (inherits(data, "scr_percent_true")) {
  #   data <- data %>%
  #     dplyr::mutate(x = (x / 100))
  # }


  # Preparations ----

  p10 <- 10 ^ decimals

  if (is.null(n)) {
    n <- p10
  }

  frac_unit <- 1 / p10

  frac_sequence <- seq_endpoint(from = frac_unit, to = (1 - frac_unit))
  n_sequence <- 1:n

  # By default, a background raster is displayed in the plot:
  if (show_raster) {

    # For 1 or 2 decimal places, the function selects the appropriate raster
    # from among those saved within the package itself:
    if (!(decimals > 2)) {

      # Check the way `x` values were rounded in the preceding `grim_map()` call
      # to prepare selecting the plot background raster:
      dc <- class(data)
      rounding_id <- dc[stringr::str_detect(dc, "scr_rounding_")]
      rounding_id <- stringr::str_remove(rounding_id, "scr_rounding_")

      # Throw error if the specified rounding option is one of the few for which
      # no raster is available:
      if (any(
        rounding_id == c("up_from", "down_from", "up_from_or_down_from")
      )) {
        cli::cli_abort(c(
          "No background raster available for `rounding = {rounding_id}`",
          ">" = "Please use a different `rounding` specification within the \\
        `grim_map()` call or set `show_raster` to `FALSE` within the \\
        `grim_plot()` call."
        ))
      }

      # Assemble the names of the appropriate raster components for sample size
      # (x-axis) and the fractional portion of the mean or proportion (y-axis)
      # from the information provided by the number of decimal places and the
      # previously specified rounding procedure. First, create two strings by
      # those names...
      raster_n    <- glue::glue("grim_raster_{decimals}_{rounding_id}_n")
      raster_frac <- glue::glue("grim_raster_{decimals}_{rounding_id}_frac")

      # ...and second, parse and evaluate these strings, so that R will identify
      # the raster component object by the respective name from among the files
      # saved within R/sysdata.R:
      raster_n    <- eval(rlang::parse_expr(raster_n))
      raster_frac <- eval(rlang::parse_expr(raster_frac))

      # # In the raster vector, the fractional and sample size values alternate, so
      # # we tease them apart here:
      # raster_frac <- raster[seq(from = 1, to = length(raster), by = 2)]
      # raster_n    <- raster[seq(from = 2, to = length(raster), by = 2)]

    } else {

      # # Adjust the raster (and the x-axis labels) for numbers of decimal places
      # # greater than 2:
      # power_diff <- 10 ^ (decimals - 2)
      # raster_n <- as.numeric(raster_n) * power_diff
      # # FIX THE ABOVE: NO `raster_n` OBJECT PRESENT!

      # These are just pro forma:
      raster_n    <- 0  # grim_raster_1_ceiling_or_floor_n
      raster_frac <- 0  # grim_raster_1_ceiling_or_floor_frac

    }

    # This data frame will be used for the raster when building the plot:
    df_plot <- tibble::tibble(
      raster_n = as.numeric(raster_n),
      raster_frac = as.numeric(raster_frac)
    )

  }

  # Reduce `x` to its fractional portion:
  data_emp <- data %>%
    dplyr::mutate(x = x - trunc(x)) %>%
    dplyr::rename(frac = x)

  if (is.null(n)) {
    n <- max(data$n) + 10
  }

  # If `percent = TRUE` in the underlying `grim_map()` call, the y-axis label is
  # automatically adjusted to reflect the fact that the fractional values are
  # percentages, not means:
  mean_percent_label <- dplyr::if_else(
    inherits(data, "scr_percent_true"),
    "% (as decimal)",
    "mean"
  )

  # Automatically color the boxes of value pairs by whether they are
  # GRIM-consistent or not:
  color_by_consistency <- dplyr::if_else(
    data$consistency,
    color_cons,
    color_incons
  )


  # The plot itself ----

  p <- ggplot2::ggplot(data = data_emp, ggplot2::aes(
    x = n,
    y = frac
  ))

  # Background raster / gradient:
  if (show_raster) {

    # With 1 or 2 decimals, the function provides a background raster...
    if (!(decimals > 2)) {

      p <- p +
        ggplot2::geom_tile(data = df_plot, mapping = ggplot2::aes(
          x = raster_n,
          y = raster_frac
        ), alpha = raster_alpha, fill = raster_color) +
        ggplot2::theme(
          panel.border = ggplot2::element_rect(fill = NA, colour = "grey50"),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA),
          panel.grid = ggplot2::element_blank()
        )


      # ... but with more decimal places, individual boxes would be too small to
      # display, so we need a gradient instead to simply show the overall trend:
    } else if (show_gradient) {

      gradient <- grDevices::colorRampPalette(c(raster_color, "white"))(10000)

      p <- p +
        ggplot2::annotation_custom(grid::rasterGrob(
          t(gradient),
          width  = grid::unit(1, "npc"),
          height = grid::unit(1, "npc")
        ))

      # x_min <- min(data_emp$n) - 1
      # x_max <- max(data_emp$n) + 1
      #
      # # x_distance <- x_max / x_min
      # # x_min <- x_min - x_distance
      # # x_max <- x_max + x_distance
      #
      # p +
      #   ggforce::facet_zoom(xlim = c(x_min, x_max))
    }

  }



  # Empirical data:
  p <- p +
    ggplot2::geom_tile(
      data = data_emp,
      mapping = ggplot2::aes(
      x = n,
      y = frac
    ),
      alpha = tile_alpha,
      size = 1,
      color = color_by_consistency,
      fill = color_by_consistency,
      width = tile_size / 2,
      height = (frac_unit * tile_size) / 2
    ) +
    ggplot2::labs(
      x = "Sample size",
      y = glue::glue("Fractional portion of {mean_percent_label}")
    )


  # Further specifications:
  p <- p +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(from = 0, to = 1, by = max(0.2, frac_unit)),
      expand = ggplot2::expansion(mult = c(0.01, 0.01))
    )

  # Show the entire range up to `n`...
  if (show_full_range) {

    p <- p +
      ggplot2::scale_x_continuous(
        breaks = seq(from = 0, to = n, by = (n / 5)),
        expand = ggplot2::expansion(mult = c(0, 0.01)),
        limits = c(0, (n + (n / 100)))
      )

  } else {

    # ...or don't:
    p <- p +
      ggplot2::scale_x_continuous(
        # breaks = seq(0, n, (n / 10)),
        expand = ggplot2::expansion(mult = c(0, 0.01))
      )
  }


  # Finally, return the plot:
  p

}


