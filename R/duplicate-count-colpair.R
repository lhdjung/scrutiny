# For each element of `x`, this helper counts how many are also found in `y`.
# `%in%` and `==` coerce mixed types the same way, so this matches the
# element-wise comparison it replaced, without the quadratic scan:

dup_count_pairwise <- function(x, y) {
  sum(x %in% y)
}


#' Count duplicate values by column
#'
#' `duplicate_count_colpair()` takes a data frame and checks each combination of
#' columns for duplicates. Results are presented in a tibble, ordered by the
#' number of duplicates.
#'
#' @param data Data frame.
#' @param ignore Optionally, a vector of values that should not be checked for
#'   duplicates.
#' @param show_rates Logical. If `TRUE` (the default), adds columns `rate_x` and
#'   `rate_y`. See value section. Set `show_rates` to `FALSE` for higher
#'   performance.

#' @return A tibble (data frame) with these columns –
#' - `x` and `y`: Each line contains a unique combination of `data`'s columns,
#'   stored in the `x` and `y` output columns.
#' - `count`: Number of "duplicates", i.e., values that are present in both `x`
#'   and `y`.
#' - `total_x`, `total_y`, `rate_x`, and `rate_y` (added by default): `total_x`
#'   is the number of non-missing values in the column named under `x`. Also,
#'   `rate_x` is the proportion of `x` values that are duplicated in `y`, i.e.,
#'   `count / total_x`. Likewise with `total_y` and `rate_y`. The two `rate_*`
#'   columns will be equal unless `NA` values are present.

#' @section Summaries with [`audit()`]: There is an S3 method for [`audit()`],
#'   so you can call [`audit()`] following `duplicate_count_colpair()`. It
#'   returns a tibble with summary statistics.
#'
#' @export
#'
#' @include utils.R
#'
#' @seealso
#' - [`duplicate_count()`] for a frequency table.
#' - [`duplicate_tally()`] to show instances of a value next to each instance.
#' - [`janitor::get_dupes()`] to search for duplicate rows.
#' - [`corrr::colpair_map()`] for pairwise column analysis in general.
#'
#' @examples
#' # Basic usage:
#' mtcars |>
#'   duplicate_count_colpair()
#'
#' # Summaries with `audit()`:
#' mtcars |>
#'   duplicate_count_colpair() |>
#'   audit()

duplicate_count_colpair <- function(data, ignore = NULL, show_rates = TRUE) {
  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data frame.")
  } else if (!tibble::is_tibble(data) || !rlang::is_named(data)) {
    data <- tibble::as_tibble(data)
  }

  if (ncol(data) < 2L) {
    cli::cli_abort(c(
      "`data` must have at least two columns.",
      "x" = "It has {ncol(data)}.",
      "i" = "`duplicate_count_colpair()` compares columns to each other."
    ))
  }

  if (!is.null(ignore)) {
    data <- lapply(data, function(x) x[!x %in% ignore])
  }

  values <- lapply(data, function(x) x[!is.na(x)])

  # Column-major, so each column is paired with the later ones only:
  pairs <- utils::combn(names(values), 2L)

  out <- tibble::tibble(
    x = pairs[1L, ],
    y = pairs[2L, ],
    count = vapply(
      seq_len(ncol(pairs)),
      function(i) {
        dup_count_pairwise(values[[pairs[1L, i]]], values[[pairs[2L, i]]])
      },
      integer(1L)
    )
  ) |>
    dplyr::arrange(dplyr::desc(.data$count)) |>
    add_class("scrutiny_dup_count_colpair")

  if (!show_rates) {
    return(out)
  }

  total_values <- lengths(values)

  dplyr::mutate(
    out,
    total_x = unname(total_values[.data$x]),
    total_y = unname(total_values[.data$y]),
    rate_x = .data$count / .data$total_x,
    rate_y = .data$count / .data$total_y
  )
}
