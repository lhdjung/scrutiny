#' Print the explanations of mapper results
#'
#' @description `explain_print()` prints the explanations behind the verdicts
#'   of a mapper such as [`grim_map()`], one row after the other. It reads the
#'   `explanation` column that [`explain_map()`] adds, or calls `explain_map()`
#'   itself if there is none yet. That way, only the printed rows are
#'   explained.
#'
#' @param data Data frame returned by [`grim_map()`], [`grimmer_map()`], or
#'   [`debit_map()`], or by their sequence and total-n mappers, with or without
#'   an `explanation` column.
#' @param ... Optionally, the rows to print, as in [`dplyr::slice()`]: positive
#'   integers to keep rows, negative ones to drop them. Default is to print all
#'   rows.
#'
#' @return `data`, invisibly.
#'
#' @include explain-map.R
#'
#' @export
#'
#' @examples
#' pigs1 |>
#'   grim_map(digits_x = 2) |>
#'   explain_print(1:2)
#'
#' # Inconsistent rows only:
#' pigs1 |>
#'   grim_map(digits_x = 2) |>
#'   dplyr::filter(!consistency) |>
#'   explain_print()
explain_print <- function(data, ...) {
  check_explainable(data)

  # `dplyr::slice()` without any rows given returns none, not all:
  rows <- if (...length() == 0L) data else dplyr::slice(data, ...)

  if (!"explanation" %in% colnames(rows)) {
    rows <- explain_map(rows)
  }

  if (nrow(rows) == 0L) {
    cat("No rows to explain.\n")
  } else {
    cat(paste0(rows$explanation, "\n"), sep = "\n")
  }

  invisible(data)
}
