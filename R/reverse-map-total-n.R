#' Reconstruct the input of a total-n mapper
#'
#' @description `reverse_map_total_n()` takes the output of a function made by
#'   [`function_map_total_n()`] and reconstructs the data frame it was called
#'   on. [`audit_total_n()`] builds on it.
#'
#' @param data Data frame with the `"scrutiny_map_total_n"` class.
#'
#' @return The reconstructed tibble.
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
    cli::cli_abort(c(
      "!" = "`data` must be the output of \\
      a function like `grim_map_total_n()`.",
      "x" = "It isn't.",
      "i" = "Such functions were created by `function_map_total_n()`."
    ))
  }

  # The group sizes come in pairs, so a case with an odd number of rows has
  # lost some. Taking the first two rows of it used to pair up group sizes from
  # different steps without a word:
  rows_per_case <- table(data$case)
  if (any(rows_per_case %% 2L != 0L)) {
    abort_in_export(
      "`data` must hold the mapper's output in full.",
      "x" = "Some cases have an odd number of rows, so rows have been \\
      removed.",
      "i" = "Each pair of consecutive rows is one split of the total."
    )
  }

  # Take the first pair of rows of each original-`n` block. The two group sizes
  # in any such pair add up to the reported total, whatever the dispersion step
  # that led to them; the total used to be rebuilt as twice the second group
  # size, which only held for a first step of `0`:
  data_reduced <- data |>
    dplyr::group_by(case) |>
    dplyr::slice(1:2) |>
    dplyr::ungroup()

  nrow_data_reduced <- nrow(data_reduced)

  # `length.out` rather than `to`, which fails for 0-row output:
  locations1 <- seq(from = 1, by = 2, length.out = nrow_data_reduced %/% 2)
  locations2 <- locations1 + 1

  data1 <- data_reduced |> dplyr::slice(locations1)
  data2 <- data_reduced |> dplyr::slice(locations2)

  # Number of columns before `n` (i.e., the columns with hypothetical values
  # dispersed from the reported statistics):
  ncol_before_n <- match("n", colnames(data)) - 1L

  colnames_reported <- colnames(data_reduced)[1L:ncol_before_n]

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
