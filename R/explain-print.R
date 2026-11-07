#' Print the explanations of mapper results
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `explain_print()` prints the explanations behind the results
#'   of a mapper such as [`grim_map()`], one row after the other. It calls
#'   [`explain_map()`] on the printed rows only.
#'
#' @param data Data frame returned by [`grim_map()`], [`grimmer_map()`], or
#'   [`debit_map()`], or by their sequence and total-n mappers.
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
#'   grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
#'   explain_print(1:2)
#'
#' # Inconsistent rows only:
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
#'   dplyr::filter(!consistency) |>
#'   explain_print()
explain_print <- function(data, ...) {
  # `dplyr::slice()` drops the record of a grouped tibble, so `explain_map()`
  # would no longer say that grouping is the cause. It sees no rows here,
  # which costs nothing, and the class that `data` was given with:
  if (is.data.frame(data)) {
    explain_map(data[0L, , drop = FALSE])
  }
  data <- as_tibble_input(data)

  # `dplyr::slice()` without any rows given returns none, not all:
  rows <- if (...length() == 0L) data else dplyr::slice(data, ...)

  # Always, rather than reading an `explanation` column: one that `data` brought
  # along would be printed as if it were scrutiny's. `explain_map()` refuses it.
  rows <- explain_map(rows)

  if (nrow(rows) == 0L) {
    cat("No rows to explain.\n")
  } else {
    cat(paste0(rows$explanation, "\n"), sep = "\n")
  }

  invisible(data)
}
