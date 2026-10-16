#' Print the explanations of mapper results
#'
#' @description `explain_print()` prints the explanations behind the verdicts
#'   of a mapper such as [`grim_map()`], one row after the other. It reads the
#'   `explanation` column that [`explain_map()`] adds, or calls `explain_map()`
#'   itself if there is none yet. That way, only the printed rows are
#'   explained.
#'
#' @param data Data frame returned by a mapper, with or without an
#'   `explanation` column.
#' @param n Integer. If given, only the first `n` rows are printed. Default is
#'   `NULL`, which prints all rows.
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
#'   explain_print(n = 2)
#'
#' # Inconsistent rows only:
#' pigs1 |>
#'   grim_map(digits_x = 2) |>
#'   dplyr::filter(!consistency) |>
#'   explain_print()
explain_print <- function(data, n = NULL) {
  rows <- data

  if (!is.null(n)) {
    if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1 || n %% 1 != 0) {
      abort_in_export(
        "`n` must be a single whole number of at least 1.",
        "x" = "It is {.val {n}}."
      )
    }
    rows <- rows[seq_len(min(n, nrow(rows))), ]
  }

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
