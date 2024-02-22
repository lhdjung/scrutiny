
# For each element of `x`, this helper determines if that element is also to be
# found in `y`. Then, it counts the number of times for which this test returned
# `TRUE`, i.e., the number of elements of `x` that are also elements of `y`.
# Indexing deeply into the inputs means that it should only be used inside of
# `duplicate_count_colpair()`, for which it is a tailor-made helper. Note that a
# single vector is mapped -- in purrr terms, this would be `map_lgl()` as
# opposed to `map2_lgl()`:

dup_count_pairwise <- function(x, y) {
  length(which(vapply(
    x[1L][[1L]], function(e1, e2) any(e1 == e2), logical(1L), y[1L][[1L]]
  )))
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
#' @param na.rm [[Deprecated]] Missing values are never counted in any case.

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
#' - [`corrr::colpair_map()`], a versatile tool for pairwise column analysis which
#' the present function wraps.
#'
#' @examples
#' # Basic usage:
#' mtcars %>%
#'   duplicate_count_colpair()
#'
#' # Summaries with `audit()`:
#' mtcars %>%
#'   duplicate_count_colpair() %>%
#'   audit()


# # Full example input:
# data <- df <- tibble::tibble(
#   a = c(1, 2, 3, NA, 5), b = c(NA, 3L, 4L, 5L, 6L), c = c(3L, 4L, NA, NA, NA)
# )
# na.rm <- TRUE
# ignore <- 3
# show_rates <- TRUE


duplicate_count_colpair <- function(data, ignore = NULL, show_rates = TRUE,
                                    na.rm = deprecated()) {

  if (lifecycle::is_present(na.rm)) {
    lifecycle::deprecate_warn(
      when = "0.3.0",
      what = "duplicate_count_colpair(na.rm)",
      details = "Missing values are never counted, so `na.rm` has no effect."
    )
  }

  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data frame.")
  } else if (!tibble::is_tibble(data) || !rlang::is_named(data)) {
    data <- tibble::as_tibble(data)
  }

  if (!is.null(ignore)) {
    data <- lapply(data, function(x) x[!x %in% ignore])
  }

  data <- data %>%
    lapply(function(x) list(x[!is.na(x)])) %>%
    tibble::as_tibble()

  out <- data %>%
    corrr::colpair_map(dup_count_pairwise) %>%
    corrr::shave() %>%
    corrr::stretch(na.rm = TRUE, remove.dups = FALSE) %>%
    dplyr::arrange(dplyr::desc(.data$r)) %>%
    dplyr::rename(count = "r") %>%
    add_class("scr_dup_count_colpair")

  if (!show_rates) {
    return(out)
  }

  total_values <- vapply(data, function(x) length(x[[1L]]), integer(1L))

  dplyr::mutate(
    out,
    total_x = unname(total_values[.data$x]),
    total_y = unname(total_values[.data$y]),
    rate_x  = .data$count / .data$total_x,
    rate_y  = .data$count / .data$total_y
  )
}

