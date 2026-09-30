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
#' - `count`: Number of "duplicates", i.e., values in `x` that are also present
#'   in `y`. Missing and ignored values are not counted.
#' - `total_x`, `total_y`, `rate_x`, and `rate_y` (added by default): `total_x`
#'   is the number of values in the column named under `x` that are neither
#'   missing nor ignored. Also,
#'   `rate_x` is the proportion of `x` values that are duplicated in `y`, i.e.,
#'   `count / total_x`. Likewise with `total_y` and `rate_y`: the proportion of
#'   `y` values that are duplicated in `x`. This is counted from the `y` side,
#'   so it is not `count / total_y` if a value repeats within one of the
#'   columns.

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
    data <- data |>
      tibble::as_tibble()
  }

  if (ncol(data) < 2L) {
    cli::cli_abort(c(
      "`data` must have at least two columns.",
      "x" = "It has {ncol(data)}.",
      "i" = "`duplicate_count_colpair()` compares columns to each other."
    ))
  }

  if (!is.null(ignore)) {
    data <- data |>
      lapply(function(x) x[!x %in% ignore])
  }

  # Values are compared as strings, as in the other `duplicate_*()` functions,
  # so that `0.1 + 0.2` duplicates `0.3` here as it does there:
  values <- data |>
    lapply(function(x) as.character(x[!is.na(x)]))

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
  count_y <- mapply(
    function(x, y) sum(values[[y]] %in% values[[x]]),
    out$x,
    out$y,
    USE.NAMES = FALSE
  )

  out |>
    dplyr::mutate(
      total_x = unname(total_values[.data$x]),
      total_y = unname(total_values[.data$y]),
      rate_x = .data$count / .data$total_x,
      rate_y = count_y / .data$total_y
    )
}
