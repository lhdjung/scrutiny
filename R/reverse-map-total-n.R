#' Reverse the `*_map_total_n()` process
#'
#' @description `reverse_map_total_n()` takes the output of a function created
#'   by [`function_map_total_n()`] and reconstructs the original data frame.
#'
#'   See [`audit_total_n()`], which takes `reverse_map_total_n()` as a basis.
#'
#' @param data Data frame that inherits the `"scrutiny_map_total_n"` class.
#'
#' @return The reconstructed tibble (data frame) which a factory-made
#'   `*_map_total_n()` function took as its `data` argument.
#'
#' @export
#'
#' @examples
#' # Originally reported summary data...
#' df <- tibble::tribble(
#'   ~x1,  ~x2,  ~n,
#'   3.43, 5.28, 90,
#'   2.97, 4.42, 103
#' )
#' df
#'
#' # ...GRIM-tested with dispersed `n` values...
#' out <- grim_map_total_n(df, digits_x = 2)
#' out
#'
#' # ...and faithfully reconstructed:
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

  # Take the first pair of rows of each original-`n` block. The two group sizes
  # in any such pair add up to the reported total, whatever the dispersion step
  # that led to them; the total used to be rebuilt as twice the second group
  # size, which only held for a first step of `0`:
  data_reduced <- data |>
    dplyr::group_by(case) |>
    dplyr::slice(1:2) |>
    dplyr::ungroup()

  nrow_data_reduced <- nrow(data_reduced)

  locations1 <- seq(from = 1, to = nrow_data_reduced - 1L, by = 2)
  locations2 <- seq(from = 2, to = nrow_data_reduced, by = 2)

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
