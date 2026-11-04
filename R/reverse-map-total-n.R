#' Reconstruct the input of a total-n mapper
#'
#' @description `reverse_map_total_n()` takes the output of a function made by
#'   [`function_map_total_n()`] and reconstructs the data frame it was called
#'   on. [`audit_total_n()`] builds on it.
#'
#' @param data Data frame with the `"scrutiny_map_total_n"` class.
#'
#' @return A tibble with the reported columns, such as `x1`, `x2`, and `n`,
#'   one row per case.
#'
#' @export
#'
#' @examples
#' df <- tibble::tribble(
#'   ~x1,  ~x2,  ~n,
#'   3.43, 5.28, 90,
#'   2.97, 4.42, 103
#' )
#'
#' out <- grim_map_total_n(df, digits_x = 2, items_averaged = 1)
#'
#' reverse_map_total_n(out)

reverse_map_total_n <- function(data) {
  if (!inherits(data, "scrutiny_map_total_n")) {
    abort_in_export(
      "!" = "`data` must be the output of \\
      a function like `grim_map_total_n()`.",
      "x" = "It isn't.",
      "i" = "Such functions were created by `function_map_total_n()`."
    )
  }

  # The statistics reported for both groups, as recorded by the mapper. They
  # need not all come before `n`, which a custom basic mapper may put anywhere:
  colnames_reported <- attr(data, "scrutiny", exact = TRUE)$reported
  if (is.null(colnames_reported)) {
    abort_lost_settings()
  }

  check_cols_needed(data, c(colnames_reported, "n", "n_change", "case", "dir"))

  # The group sizes come in pairs: consecutive rows of the same case and
  # direction with the same absolute `n_change`. All pairs of a case add up to
  # the same total. Removed or reordered rows break this, and taking the first
  # two rows of a case would then silently pair group sizes from different
  # steps:
  first <- seq(1L, by = 2L, length.out = nrow(data) %/% 2L)
  second <- first + 1L
  sums <- data$n[first] + data$n[second]
  pairs_broken <-
    nrow(data) %% 2L != 0L ||
    any(
      data$case[first] != data$case[second] |
        data$dir[first] != data$dir[second] |
        abs(data$n_change[first]) != abs(data$n_change[second]) |
        sums != stats::ave(sums, data$case[first], FUN = function(x) x[1L])
    )
  if (pairs_broken) {
    abort_in_export(
      "`data` must hold the mapper's output in full and in order.",
      "x" = "Some rows have been removed or reordered.",
      "i" = "Each pair of consecutive rows is one split of the total."
    )
  }
  # A pair is a step of one case in one direction; the step is the first
  # group's `n_change`. Both rows of a step of 0 may be the same:
  check_rows_unique(data[first, ], c("case", "dir", "n_change"))

  # The two group sizes in any pair add up to the reported total, so the first
  # pair of each case will do:
  data_reduced <- data |>
    dplyr::group_by(case) |>
    dplyr::slice(1:2) |>
    dplyr::ungroup()

  nrow_data_reduced <- nrow(data_reduced)

  # `length.out` rather than `to`, which fails for 0-row output:
  locations1 <- seq(from = 1, by = 2, length.out = nrow_data_reduced %/% 2)
  locations2 <- locations1 + 1

  # A `"forth"` pair has the first group's values in its first row, but a
  # `"back"` pair has them in its second: the mapper swaps the groups for that
  # direction. If the first pair of a case is a back pair, as after
  # `filter(dir == "back")`, the rows change places:
  is_back <- data_reduced$dir[locations1] == "back"
  data1 <- data_reduced[dplyr::if_else(is_back, locations2, locations1), ]
  data2 <- data_reduced[dplyr::if_else(is_back, locations1, locations2), ]

  data_reported_1 <- data1[, colnames_reported]
  data_reported_2 <- data2[, colnames_reported]

  n <- data1$n + data2$n

  colnames(data_reported_1) <- paste0(colnames_reported, "1")
  colnames(data_reported_2) <- paste0(colnames_reported, "2")

  colnames_in_order <- colnames_reported |>
    rep(each = 2L) |>
    paste0(c("1", "2"))

  dplyr::bind_cols(data_reported_1, data_reported_2) |>
    dplyr::relocate(all_of(colnames_in_order)) |>
    dplyr::mutate(n)
}
