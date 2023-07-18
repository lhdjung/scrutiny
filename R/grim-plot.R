

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
#'   preceding `grim_map()` call. This works fast because the raster is based on
#'   data saved in the package itself, so these data don't need to be generated
#'   anew every time the function is called. Inconsistent value sets are marked
#'   with dark boxes. All other places in the raster denote consistent value
#'   sets. The raster is independent of the data -- it only follows the
#'   `rounding` specification in the `grim_map()` call and the `digits` argument
#'   in `grim_plot()`.
#'
#'   Display an "empty" plot, one without empirical test results, by setting
#'   `show_data` to `FALSE`. You can then control key parameters of the plot
#'   with `digits` and `rounding`.
#'
#'   With `grim_map()`'s default for `rounding`, `"up_or_down"`, strikingly few
#'   values are flagged as inconsistent for sample sizes 40 and 80 (or 4 and 8).
#'   This effect disappears if `rounding` is set to any other value. For a list
#'   of values that `rounding` can take, see documentation for `grim()`, section
#'   `Rounding`.
#'
#'   The 4/8 leniency effect arises because accepting values rounded either up
#'   or down is more careful and conservative than any other rounding procedure.
#'   In any case, `grim_plot()` doesn't cause this effect --- it only reveals
#'   it.
#'
#' @param data Data frame. Result of a call to `grim_map()`.
#' @param show_data Boolean. If set to `FALSE`, test results from the data are
#'   not displayed. Choose this if you only want to show the background raster.
#'   You can then control plot parameters directly via the `n`, `digits`, and
#'   `rounding` arguments. Default is `TRUE`.
#' @param show_gradient Boolean. If the number of decimal places is 3 or
#'   greater, should a gradient be shown to signal the overall probability of
#'   GRIM inconsistency? Default is `TRUE`.
#' @param digits Integer. Number of decimal places for which the background
#'   raster will be generated. Default is `NULL`, in which case the greatest
#'   number of decimal places from the means or proportions is used.
#' @param n Integer. Maximal value on the x-axis. Default is `NULL`, in which
#'   case `n` becomes `10 ^ digits` (e.g., `100` if `digits` is `2`).
#' @param digits Integer. Only relevant if `show_data` is set to `FALSE`. The
#'   plot will then be constructed as it would be for data where all `x` values
#'   have this many decimal places. Default is `2`.
#' @param rounding String. Only relevant if `show_data` is set to `FALSE`. The
#'   plot will then be constructed as it would be for data rounded in this
#'   particular way. Default is `"up_or_down"`.
#' @param show_raster Boolean. If `TRUE` (the default), the plot has a
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

#' @return A ggplot object.

#' @export
#'
#' @examples
#' # Call `grim_plot()` following `grim_map()`:
#' pigs1 %>%
#'   grim_map() %>%
#'   grim_plot()
#'
#' # If you change the rounding procedure
#' # in `grim_map()`, the plot will
#' # follow automatically if there is
#' # a difference:
#' pigs1 %>%
#'   grim_map(rounding = "ceiling") %>%
#'   grim_plot()
#'
#' # For percentages, the y-axis
#' # label also changes automatically:
#' pigs2 %>%
#'   grim_map(percent = TRUE) %>%
#'   grim_plot()


grim_plot <- function(data = NULL,
                      show_data = TRUE,
                      show_raster = TRUE,
                      show_gradient = TRUE,
                      n = NULL,
                      digits = NULL,
                      rounding = "up_or_down",
                      color_cons = "royalblue1",
                      color_incons = "red",
                      tile_alpha = 1,
                      tile_size = 1.5,
                      raster_alpha = 1,
                      raster_color = "grey75") {


  # Checks ----

  inherits_grim    <- inherits(data, "scr_grim_map")
  inherits_grimmer <- inherits(data, "scr_grimmer_map")

  if (!inherits_grim) {
    # Issue an alert if any GRIMMER inconsistencies were found in `data`:
    if (inherits_grimmer) {
      reason <- data$reason[!is.na(data$reason)]
      grimmer_cases <- stringr::str_detect(reason, "GRIMMER")
      grimmer_cases <- length(grimmer_cases[grimmer_cases])
      if (grimmer_cases > 0L) {
        if (grimmer_cases == 1L) {
          msg_case_s <- "case was"
          msg_incons <- "inconsistency"
        } else {
          msg_case_s <- "cases were"
          msg_incons <- "inconsistencies"
        }
        if (grimmer_cases < length(reason)) {
          msg_viz <- "Also visualizing"
        } else {
          msg_viz <- "Visualizing"
        }
        cli::cli_alert("{msg_viz} {grimmer_cases} GRIMMER {msg_incons}.")
      }
    } else if (show_data) {
      cli::cli_abort(c(
        "!" = "`grim_plot()` needs GRIM or GRIMMER test results.",
        "x" = "`data` is not `grim_map()` or `grimmer_map()` output.",
        "i" = "The only exception is an \"empty\" plot that shows the \\
        background raster but no empirical test results. Create such a plot \\
        by setting `show_data` to `FALSE`."
      ))
    }
  }

  # Warn the user who passed suitable data to `grim_plot()` but also set
  # `show_data` to `FALSE`, thereby defeating the data's purpose here:
  if (!show_data && any(inherits_grim, inherits_grimmer)) {
    if (inherits_grimmer) {
      msg_grimmer <- " and GRIMMER"
    } else {
      msg_grimmer <- ""
    }
    cli::cli_warn(c(
      "Test results are not visualized.",
      "!" = "You set `show_data` to `FALSE`, but still passed \\
      GRIM{msg_grimmer} test results to `grim_plot()`.",
      ">" = "Only the background raster or gradient will be shown, not the \\
        tested data."
    ))
  }

  # The `digits` argument, if specified, must be a single integer-like
  # number because it controls the number of decimal places for which the plot
  # will be constructed:
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
  # results (blue and/or red dots) will be shown. To this end, the function
  # must completely bypass the `data` argument. It does so via creating a
  # dummy object by that name:
  if (!show_data) {
    data <- tibble::tibble(x = "0.00", n = 1, items = 1, consistency = TRUE) %>%
      add_class(paste0("scr_rounding_", rounding))
  }


  if (is.null(digits)) {
    digits_x <- decimal_places(data$x)    # used to be wrapped in `max()`

    if (show_raster) {
      if (!all(digits_x[1] == digits_x)) {
        means_percentages <- dplyr::if_else(
          inherits(data, "scr_percent_true"),
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
    # number of decimal places. Therefore, `digits` can now be determined
    # simply by taking the first element; or indeed any other element there
    # might be:
    digits <- digits_x[1]

  }

  data$x <- as.numeric(data$x)


  # Preparations ----

  p10 <- 10 ^ digits

  if (is.null(n)) {
    n <- p10
  }

  frac_unit <- 1 / p10

  # By default, a background raster is displayed in the plot:
  if (show_raster) {

    # For 1 or 2 decimal places, the function selects the appropriate raster
    # from among those saved within the package itself:
    if (!(digits > 2L)) {

      # Check the way `x` values were rounded in the preceding `grim_map()` call
      # to prepare selecting the plot background raster:
      dc <- class(data)
      rounding_id <- dc[stringr::str_detect(dc, "scr_rounding_")]
      rounding_id <- stringr::str_remove(rounding_id, "scr_rounding_")

      # Throw error if the specified rounding option is one of the few for which
      # no raster is available:
      rounding_is_bad <- any(
        rounding_id == c("up_from", "down_from", "up_from_or_down_from")
      )
      if (rounding_is_bad) {
        cli::cli_abort(c(
          "No background raster available for `rounding = {rounding_id}`",
          "i" = "Use a different `rounding` specification within the \\
          `grim_map()` call or set `show_raster` to `FALSE` within the \\
          `grim_plot()` call."
        ))
      }

      # Assemble the names of the appropriate raster components for sample size
      # (`raster_n`; x-axis) and the fractional portion of the mean or
      # proportion (`raster_frac`, y-axis) from the information provided by the
      # number of decimal places and the previously specified rounding
      # procedure. First, create two strings by those names...
      raster_n    <- glue::glue("grim_raster_{digits}_{rounding_id}_n")
      raster_frac <- glue::glue("grim_raster_{digits}_{rounding_id}_frac")

      # ...and second, parse and evaluate these strings, so that R will identify
      # the raster component object by the respective name from among the files
      # saved within R/sysdata.R:
      raster_n    <- eval(rlang::parse_expr(raster_n))
      raster_frac <- eval(rlang::parse_expr(raster_frac))

    } else {

      # For any number of decimal places greater than 2, these two objects are
      # assigned the value of zero. This is only pro forma. We still need
      # `raster_n` and `raster_frac` because they are referenced several times
      # further down to build the plot:
      raster_n    <- 0
      raster_frac <- 0

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

  if (!show_data) {
    data_emp <- data_emp %>%
      dplyr::mutate(dplyr::across(everything(), function(x) 0L))
  }

  # If `percent = TRUE` in the underlying `grim_map()` call, the y-axis label is
  # automatically adjusted to reflect the fact that the fractional values are
  # percentages (converted to decimal numbers), not means:
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

  # Background raster / gradient:
  if (show_raster) {

    # With 1 or 2 digits, the function provides a background raster...

      p <- ggplot2::ggplot(data = df_plot) +
        ggplot2::geom_tile(mapping = ggplot2::aes(
          x = .data$raster_n,
          y = .data$raster_frac
        ), alpha = raster_alpha, fill = raster_color) +
        ggplot2::theme(
          panel.border = ggplot2::element_rect(fill = NA, colour = "grey50"),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA),
          panel.grid = ggplot2::element_blank()
        )


      # ... but with more decimal places, individual boxes would be too small to
      # display, so we need a gradient instead to simply show the overall trend.
      # Boxes are still added pro forma; the call to `geom_tile()` is the same
      # as above (except for the `alpha` and `fill` specifications):

      if (digits > 2L) {

        if (show_gradient) {

          gradient <-
            grDevices::colorRampPalette(c(raster_color, "white"))(10000)

          p <- p +
            ggplot2::geom_tile(data = df_plot, mapping = ggplot2::aes(
              x = .data$raster_n,
              y = .data$raster_frac
            )) +
            ggplot2::annotation_custom(grid::rasterGrob(
              t(gradient),
              width  = grid::unit(1, "npc"),
              height = grid::unit(1, "npc")
            ))

        }

        # Keep the y-axis ranging from 0 to 1, even with the gradient in place:
        p <- p +
          ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(add = c(0, 0.01)),
            limits = c(0, 1)
          )

      }

  }


  if (show_data) {

    if (utils::packageVersion("ggplot2") >= "3.4") {
      p <- p +
        ggplot2::geom_tile(
          data = data_emp,
          mapping = ggplot2::aes(
            x = .data$n,
            y = .data$frac
          ),
          alpha = tile_alpha,
          # Replaced here:
          linewidth = 1,
          color = color_by_consistency,
          fill = color_by_consistency,
          width = tile_size / 2,
          height = (frac_unit * tile_size) / 2
        )
    } else {
      p <- p +
        ggplot2::geom_tile(
          data = data_emp,
          mapping = ggplot2::aes(
            x = .data$n,
            y = .data$frac
          ),
          alpha = tile_alpha,
          # Still here:
          size = 1,
          color = color_by_consistency,
          fill = color_by_consistency,
          width = tile_size / 2,
          height = (frac_unit * tile_size) / 2
        )
    }

  }


  if (!(digits > 2L)) {

    # Further specifications:
    p <- p +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank()
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(from = 0, to = 1, by = max(0.2, frac_unit)),
        expand = ggplot2::expansion(add = c(0.01, 0)),
        limits = c(0, 1)
      )

    # Make the exact x-axis scale specification dependent on whether the plot
    # will (by default) show a raster...
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = seq(from = 0, to = n, by = (n / 5)),
        expand = ggplot2::expansion(mult = c(0, 0.01))
      )


  } else {

    # ...or a gradient, in which case we need the x-axis must be forced to
    # run from 0 to 1 using `limits = c(0, n)`, which is omitted above to remove
    # the space between the raster and the y-axis:
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = seq(from = 0, to = n, by = (n / 5)),
        expand = ggplot2::expansion(mult = c(0, 0.01)),
        limits = c(0, n)
      )

  }



  # Finally, return the plot with axis labels, suppressing unnecessary ggplot2
  # warnings:
  suppressWarnings(print(
    p +
      ggplot2::labs(
        x = "Sample size",
        y = glue::glue("Fractional portion of {mean_percent_label}")
      )
  ))

}


