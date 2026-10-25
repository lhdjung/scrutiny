#' Compare GRIM-inconsistency rates to random means
#'
#' @description `grim_plot_rates()` plots one point per study: its observed rate
#'   of GRIM-inconsistent value sets against the rate that means with random
#'   decimal digits would show at the same sample sizes and numbers of decimal
#'   places. This helps spot both problematic studies and problems in GRIM
#'   usage. See *Details*.
#'
#'   Correctly reported means are always consistent. Thus, any study not on the
#'   dotted line at zero indicates some problem; either with the study itself or
#'   with the GRIM analysis. The dashed diagonal marks studies where
#'   GRIM-inconsistencies are as frequent as expected if the means' digits were
#'   random.
#'
#' @param data Data frame returned by [`grim_map()`].
#' @param study <[`tidy-select`][dplyr::dplyr_tidy_select]> Column of `data`
#'   that identifies the study of each value set.
#'
#' @details The expected rate of a study is the mean of the `probability`
#'   column, from [`grim_probability()`]. Value sets whose probability is zero
#'   or missing are left out because GRIM cannot test them. Value sets without
#'   a study are left out with a warning.
#'
#'   Point area is the number of GRIM-testable value sets. Color shows the ratio
#'   of the observed rate to the expected one: 0 means no inconsistencies, 1 as
#'   many as random means. Studies under the diagonal are above 1, in darker
#'   colors.
#'
#'   No systematic factor tends to increase the GRIM inconsistency rate above
#'   chance. Therefore, large points (= many value sets) clearly below the
#'   diagonal are a warning sign for errors in the GRIM analysis. However,
#'   studies with few value sets, represented by small dots, could have such
#'   high ratios by chance.
#'
#' @return A ggplot object.
#'
#' @seealso [`grim_binomial()`] for a test of the same comparison.
#'
#' @export
#'
#' @examplesIf rlang::is_installed("ggrepel")
#' # Assume each unique code indicates one study
#' codes <- c(
#'   "B", "B", "C", "C", "A", "C",
#'   "A", "D", "A", "A", "A", "A"
#' )
#'
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1) |>
#'   dplyr::mutate(study = codes) |>
#'   grim_plot_rates(study)

grim_plot_rates <- function(data, study) {
  rlang::check_installed("ggrepel", "for the labels in `grim_plot_rates()`.")

  check_grim_map_basic(data)

  check_cols_needed(data, c("consistency", "probability"))

  study <- names(tidyselect::eval_select(rlang::enquo(study), data))

  if (length(study) != 1L) {
    abort_in_export(
      "`study` must select exactly one column.",
      "x" = "It selects {length(study)}."
    )
  }

  # These are the names of the summary columns, which would overwrite `study`:
  if (study %in% c("observed", "expected", "value_sets", "ratio")) {
    abort_in_export(
      "`study` can't be named `{study}`.",
      "i" = "Rename the column first, e.g. with `dplyr::rename()`."
    )
  }

  study_missing <- is.na(data[[study]])
  if (any(study_missing)) {
    cli::cli_warn(c(
      "!" = "Dropping {sum(study_missing)} value set{?s} without a study.",
      "i" = "Their `{study}` is `NA`."
    ))
    data <- data[!study_missing, ]
  }

  rates <- data |>
    dplyr::filter(.data$probability > 0) |>
    dplyr::summarise(
      observed = mean(!.data$consistency),
      expected = mean(.data$probability),
      value_sets = dplyr::n(),
      ratio = .data$observed / .data$expected,
      .by = dplyr::all_of(study)
    )

  # Red marks 1, not the end of the scale, so that ratios above it remain
  # distinguishable:
  upper <- max(2, rates$ratio)
  as_percent <- function(x) paste0(100 * x, "%")

  ggplot2::ggplot(
    rates,
    ggplot2::aes(x = .data$observed, y = .data$expected, label = .data[[study]])
  ) +
    ggplot2::geom_abline(linetype = "dashed", colour = "grey50") +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dotted",
      colour = "grey50"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$value_sets, colour = .data$ratio),
      shape = 19,
      alpha = 0.7
    ) +
    ggrepel::geom_text_repel(size = 3, colour = "grey20", seed = 1) +
    ggplot2::scale_x_continuous(labels = as_percent) +
    ggplot2::scale_y_continuous(labels = as_percent) +
    ggplot2::scale_size_area(name = "Testable\nvalue sets") +
    ggplot2::scale_colour_gradientn(
      colours = c("#2166ac", "#92c5de", "#f4a582", "#b2182b", "#40004b"),
      values = c(0, 1 / 3, 2 / 3, 1, upper) / upper,
      limits = c(0, upper),
      name = "Observed /\nexpected"
    ) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Observed GRIM-inconsistency rate",
      y = "Rate expected for random means"
    ) +
    ggplot2::theme_minimal()
}
