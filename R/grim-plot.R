

# Visualize GRIM test results
#
# @description Plot a distribution of summary data and their mutual GRIM
#   consistency. Call this function only on a data frame that resulted from a
#   call to `grim_map()`.
#
#   The background raster flags every inconsistent value pair in the space
#   defined by the `decimals` argument, independently of the data. The raster
#   is only informative for value pairs where the mean/percentage has exactly
#   that many decimal places.
#
#   Consistent and inconsistent value pairs from the input data frame are shown
#   in distinctive colors. Parameters of the underlying geoms can be controlled
#   via arguments.
#
#   Running this function might take some time.
#
# @details The number of tiles in the plot, including both the raster and the
#   points spared by it, is \eqn{(10 ^ decimals) ^ 2}. So it's 10.000 with the
#   default `decimals = 2`, and one million with `decimals = 3`. For each
#   greater `decimals` value (with \eqn{decimals >= 2}), the number of tiles is
#   100 times the former value!
#
#   At sample sizes 40 and 80 and with \eqn{decimals = 2}, the plot displays
#   bright vertical lines. This pattern generalizes to \eqn{4 * (10 ^ (decimals
#   - 1))} and \eqn{8 * (10 ^ (decimals - 1))}. The reasons are currently
#   unknown.
#
# @param data Data frame. Result of a call to `grim_map()`.
# @param decimals Integer. Number of decimal places for which the background
#   raster will be generated. Default is `2`. *Warning*: Setting `decimals`
#   even just to `3` will greatly increase processing time.
# @param n Integer. Maximal value on the x-axis. Default is `NULL`, in which
#   case `n` becomes `10 ^ decimals` (e.g., `100` if `decimals` is `2`).
# @param show_raster Boolean. If `TRUE` (the default), the plot has a
#   background raster.
# @param color_cons,color_incons Strings. Fill colors of the consistent and
#   inconsistent scatter points. Defaults are `"royalblue1"` (consistent) and
#   `"red"` (inconsistent).
# @param tile_alpha,tile_size Numeric. Further parameters of the scatter
#   points: opacity and, indirectly, size. Defaults are `1` and `1.5`.
# @param raster_alpha,raster_color Numeric and string, respectively. Parameters
#   of the background raster: opacity and fill color. Defaults are `0.5` and
#   `"gray50"`.
#
# @include seq-decimal.R



#grim_plot <- function(data,
#                      show_raster = TRUE,
#                      show_full_range = FALSE,
#                      n = NULL,
#                      decimals = 2,
#                      color_cons = "royalblue1",
#                      color_incons = "red",
#                      tile_alpha = 1,
#                      tile_size = 1.5,
#                      raster_alpha = 0.5,
#                      raster_color = "gray50",
#                      raster_blur = 5) {
#
#
#  # Checks ---
#
#  if (!inherits(data, "scr_grim_map")) {
#    cli::cli_abort(c(
#      "`data` is not `grim_map()` output",
#      "x" = "´grim_plot()` only works with GRIM test results."
#    ))
#  }
#
#  if (!inherits(data, "scr_adjust_x_out_1")) {
#    cli::cli_abort(c(
#      "`grim_plot()` needs `grim_map(adjust_x_out = 1)`.",
#      ">" = "Set `adjust_x_out` to `1` in your `grim_map()` call."
#    ))
#  }
#
#
#  # Preparations ---
#
#  p10 <- 10 ^ decimals
#
#  if (is.null(n)) {
#    n <- p10
#  }
#
#  frac_unit <- 1 / p10
#
#  frac_sequence <- seq_endpoint(from = frac_unit, to = (1 - frac_unit))
#  n_sequence <- 1:n
#
#  raster_df <- tibble::tibble(x = max(n_sequence), y = max(frac_sequence))
#
#
#  # By default, a background raster is displayed in the plot:
#  if (show_raster == TRUE) {
#
#    # How the raster vector for two decimal places was originally created...
#    # r <- purrr::cross2(frac_sequence, n_sequence, .filter = grim)
#    # r <- unlist(r)
#
#    # ... and how it's used now that it has been stored in scrutiny's
#    # sysdata.rda file (via `usethis::use_data()` with `internal = TRUE`), and
#    # therefore been made available (only) within the package:
#    r <- grim_plot_raster_data_2
#
#    # In the raster vector, the fractional and sample size values alternate, so
#    # we tease them apart here:
#    raster_frac <- r[seq(from = 1, to = length(r), by = 2)]
#    raster_n    <- r[seq(from = 2, to = length(r), by = 2)]
#
#    # This data frame will be used for the raster when building the plot:
#    df_plot <- tibble::tibble(
#      raster_n = as.numeric(raster_n),
#      raster_frac = as.numeric(raster_frac)
#    )
#
#  }
#
#
#  data$x <- as.numeric(data$x)
#
#  data_emp <- data %>%
#    dplyr::mutate(x = x - trunc(x)) %>%
#    dplyr::rename(frac = x)
#
#  if (is.null(n)) {
#    n <- max(data$n) + 10
#  }
#
#  mean_percent_label <- dplyr::if_else(
#    inherits(data, "scr_percent_true"),
#    "percentage",
#    "mean"
#  )
#
#  color_by_consistency <- dplyr::if_else(
#    data$consistency == TRUE,
#    color_cons,
#    color_incons
#  )
#
#
#  # The plot itself ---
#
#  # Empirical data:
#  p <- ggplot2::ggplot(data = data_emp, ggplot2::aes(
#    x = n,
#    y = frac
#  )) +
#    ggplot2::geom_tile(
#      alpha = tile_alpha,
#      size = 1,
#      color = color_by_consistency,
#      fill = color_by_consistency,
#      width = tile_size / 2,
#      height = (frac_unit * tile_size) / 2
#    )
#
#
#  # Background raster (optional, default is `TRUE`):
#    if (show_raster == TRUE) {
#
#      # With 1 or 2 decimals, the function has a raster at the ready...
#      if (decimals < 3) {
#        p <- p +
#          ggplot2::geom_tile(data = df_plot, ggplot2::aes(
#            x = raster_n,
#            y = raster_frac
#          ), alpha = raster_alpha, fill = raster_color)
#      } else {
#
#        # x_ratio <- raster_n / as.numeric(data_emp$n)
#        #
#        # df_plot <- dplyr::mutate(df_plot,
#        #   raster_n = raster_n * x_ratio,
#        #   raster_frac = raster_frac * x_ratio
#        # )
#
#        # ...but with 3 or more decimals, it needs to blur the 2-decimals raster
#        # to give some idea of the probability of inconsistency:
#        p <- p +
#          ggfx::with_blur(
#            ggplot2::geom_tile(data = df_plot, ggplot2::aes(
#              x = raster_n,
#              y = raster_frac
#            ), alpha = raster_alpha, fill = raster_color),
#            sigma = raster_blur, stack = FALSE)
#      }
#
#      # Further specifications that only make sense with a raster:
#      p <- p +
#        ggplot2::theme(
#          panel.grid = ggplot2::element_blank()
#        ) +
#        ggplot2::scale_y_continuous(
#          breaks = seq(0, 1, max(0.1, frac_unit)),
#          expand = ggplot2::expansion(mult = c(0.01, 0.01))
#        )
#
#      if (show_full_range == FALSE) {
#        p <- p +
#          ggplot2::scale_x_continuous(
#            breaks = seq(0, n, (n / 10)),
#            expand = ggplot2::expansion(mult = c(0, 0.01)),
#            limits = c(0, n)
#        )
#      } else {
#        p <- p +
#          ggplot2::scale_x_continuous(
#            breaks = seq(0, n, (n / 10)),
#            expand = ggplot2::expansion(mult = c(0, 0.01))
#        )
#      }
#    }
#
##  p <- p +
##    ggplot2::geom_rect(data = raster_df, ggplot2::aes(
##      xmin = 0, xmax = x,
##      ymin = 0, ymax = as.numeric(y)
##      # fill = "red",   # Try to change this into a continuum
##    ), fill = "blue", inherit.aes = FALSE)
#
#  # Final touches, then return plot:
#  p +
#    ggplot2::labs(
#      x = "Sample size",
#      y = glue::glue("Fractional portion of {mean_percent_label}")
#    )
#
#}
#
#
#
#
## New, for the raster if `decimals` is >= 3 -------------------------------
#
## From:
## https://stackoverflow.com/questions/53397131/gradient-fill-in-ggplot2/53397832
#
#
#n <- 1000
#val <- seq(0, 0.5, length.out = n)
#df22 <- tibble::tibble(val, n)
#
#
#  # p +
#    ggplot2::ggplot(data = df22, ggplot2::aes(x = n, y = val)) +
#    ggplot2::geom_ribbon(data = df22, ggplot2::aes(
#      x = n,
#      y = val,
#      ymax = val, ymin = 0,
#      xmax = n, xmin = 0
#    )) +
#    ggplot2::geom_col(data = df22, ggplot2::aes(x = n, y = val, fill = val),
#                      inherit.aes = FALSE) +
#    ggplot2::scale_fill_gradient(low = "black", high = "white") +
#    ggplot2::coord_flip() +
#    ggplot2::theme(legend.position = "none")






# Formerly ----------------------------------------------------------------

# This is unreasonable:

# ggplot2::ggsave("grim_plot_wansink_n1000_decimals3.png",
#                 width = 13 * 100, height = 9 * 100, units = "mm",
#                 limitsize = FALSE)




# For teasing out the two white lines:

# ggplot2::geom_hline(yintercept = seq(0.005, 1, 0.02), color = "royalblue1") +
# ggplot2::geom_vline(xintercept = c(40, 80), color = "red") +

# t30 <- seq_distance_df(0.01, n = 30, .length_out = 100) %>%
#   grim_map()
#
# t40 <- seq_distance_df(0.01, n = 40, .length_out = 100) %>%
#   grim_map()
#
# t50 <- seq_distance_df(0.01, n = 50, .length_out = 100) %>%
#   grim_map()
#
# tdf <- tibble::tibble(t30 = t30$consistency,
#                       t40 = t40$consistency,
#                       t50 = t50$consistency)

