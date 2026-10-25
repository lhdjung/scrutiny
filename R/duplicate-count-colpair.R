#' Count duplicate values by column pair
#'
#' `duplicate_count_colpair()` counts, for every pair of columns in a data
#' frame, the values that occur in both. Results are ordered by that count.
#'
#' @param data Data frame with at least two columns.
#' @param ignore Optionally, a vector of values not to count.
#' @param show_rates Logical. If `TRUE` (the default), add `rate_x` and
#'   `rate_y`; see *Value*. `FALSE` is faster.
#'
#' @return A tibble with these columns:
#' - `x`, `y`: a pair of columns of `data`, one pair per row.
#' - `count`: the number of values of `x` that also occur in `y`. Missing and
#'   ignored values are not counted.
#' - `total_x`, `total_y`: the number of values in `x` and `y` that are
#'   neither missing nor ignored.
#' - `rate_x`: the proportion of `x` values that occur in `y`, i.e., `count /
#'   total_x`. `rate_y`: the proportion of `y` values that occur in `x`. This
#'   differs from `count / total_y` if a value repeats within a column.
#'
#' @section Summaries with [`audit()`]: `audit()` on
#'   `duplicate_count_colpair()` output returns summary statistics for the
#'   numeric columns.
#'
#' @export
#'
#' @include utils.R
#'
#' @seealso
#' - [`duplicate_count()`] for a frequency table.
#' - [`duplicate_tally()`] for counts next to each value.
#' - [`janitor::get_dupes()`] for duplicate rows.
#'
#' @examples
#' mtcars |>
#'   duplicate_count_colpair()
#'
#' mtcars |>
#'   duplicate_count_colpair() |>
#'   audit()

duplicate_count_colpair <- function(data, ignore = NULL, show_rates = TRUE) {
  if (!is.data.frame(data)) {
    abort_in_export("`data` must be a data frame.")
  } else if (!tibble::is_tibble(data) || !rlang::is_named(data)) {
    data <- data |>
      tibble::as_tibble()
  }

  if (ncol(data) < 2L) {
    abort_in_export(
      "`data` must have at least two columns.",
      "x" = "It has {ncol(data)}.",
      "i" = "`duplicate_count_colpair()` compares columns to each other."
    )
  }

  # Values are compared as strings, as in the other `duplicate_*()` functions,
  # so that `0.1 + 0.2` duplicates `0.3` here as it does there:
  values <- data |>
    lapply(function(x) value_to_string(x[!is.na(x)]))

  if (!is.null(ignore)) {
    ignore <- value_to_string(ignore)
    values <- values |>
      lapply(function(x) x[!x %in% ignore])
  }

  # Column-major, so each column is paired with the later ones only:
  pairs <- utils::combn(names(values), 2L)

  count_x <- pairs |>
    ncol() |>
    seq_len() |>
    vapply(
      # For each element of `x`, this function counts how many are also found in
      # `y`. `%in%` and `==` coerce mixed types the same way, so this matches
      # the element-wise comparison it replaced, without the quadratic scan:
      function(i) {
        sum(values[[pairs[1L, i]]] %in% values[[pairs[2L, i]]])
      },
      integer(1L)
    )

  out <- tibble::tibble(x = pairs[1L, ], y = pairs[2L, ], count = count_x) |>
    dplyr::arrange(dplyr::desc(.data$count)) |>
    add_class("scrutiny_dup_count_colpair")

  if (!show_rates) {
    return(out)
  }

  total_values <- lengths(values)

  # `count` is counted from the `x` side, so the `y` side needs its own count:
  # with `x = c(1, 1, 1)` and `y = 1`, three `x` values are duplicated in `y`,
  # but only one `y` value is duplicated in `x`.
  count_y <- purrr::map2_int(
    out$x,
    out$y,
    function(x, y) sum(values[[y]] %in% values[[x]])
  )

  out |>
    dplyr::mutate(
      total_x = unname(total_values[.data$x]),
      total_y = unname(total_values[.data$y]),
      rate_x = .data$count / .data$total_x,
      rate_y = count_y / .data$total_y
    )
}
