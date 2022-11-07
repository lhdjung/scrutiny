

# NOTE: This file has been outcommented so that it won't generate documentation
# on the website before `duplicate_count_colpair()` is released. It will be
# released in the first minor or major version after 0.2.2.


# For each element of `x`, this helper determines if that element is also to be
# found in `y`. Then, it counts the number of times for which this test returned
# `TRUE`, i.e., the number of elements of `x` that are also elements of `y`.
# Note the use of `purrr::map_lgl()` as opposed to `purrr::map2_lgl()`:

duplicate_count_by_vec <- function(x, y, na.rm) {
  if (na.rm) {
    x <- remove_na(x)
    y <- remove_na(y)
  }
  count <- purrr::map_lgl(x, `%in%`, y)
  length(count[count])
}


rate_from_data <- function(data, x, y, count, na.rm) {
  x <- data[x]
  y <- data[y]
  if (na.rm) {
    x <- remove_na(x)
    y <- remove_na(y)
  }
  x_rate <- count / length(x)
  y_rate <- count / length(y)
  list(x_rate, y_rate)
}



#' Count duplicate values by column
#'
#' `duplicate_count_colpair()` takes a data frame and checks each combination of
#' columns for duplicates. Results are presented in a tibble, ordered by the
#' number of duplicates.
#'
#' @param data Data frame.
#' @param na.rm Boolean. If `TRUE` (the default), any `NA` values in `data`'s
#'   columns will be removed before checking for duplicates. This makes sure
#'   that `NA` values in different columns will not be counted as duplicates of
#'   each other.
#' @param show_rates Boolean. If `TRUE` (the default), adds columns `rate_x` and
#'   `rate_y`. See value section. Set `show_rates` to `FALSE` for higher
#'   performance.

#' @return A tibble (data frame) with these columns ---
#' - `x` and `y`: Each line contains a unique combination of `data`'s columns,
#' stored in the `x` and `y` output columns.
#' - `count`: Number of "duplicates", i.e., values that are present in both `x`
#' and `y`.
#' - `rate_x` and `rate_y` (added by default): `rate_x` is the proportion of `x`
#' values that are duplicated in `y`. Likewise, `rate_y` is the proportion of
#' `y` values that are duplicated in `x`. These two `rate_*` columns will be
#' equal unless `NA` values are present.

#' @section Summaries with `audit()`: There is an S3 method for `audit()`, so
#'   you can call `audit()` following `duplicate_count_colpair()` to get a
#'   summary of `duplicate_count_colpair()`'s results. It is a tibble with a
#'   single row and these columns --
#'
#'   - `n`: number of column pairs tested (index 1).
#'   - `count_min`, `count_max`, `count_mean`, `count_sd`, `count_median`:
#'   Summary statistics of the duplicate `count` column (index 2 to 6).
#'   - `rate_x_min`, `rate_x_max`, `rate_x_mean`, `rate_x_sd`, `rate_x_median`:
#'   Summary statistics of the `rate_x` column (index 7 to 11).
#'   - `rate_y_min`, `rate_y_max`, `rate_y_mean`, `rate_y_sd`, `rate_y_median`:
#'   Summary statistics of the `rate_y` column (index 12 to 16).
#'
#'   You may pipe the output of `audit()` into `as.list()` for better
#'   readability.

#' @export
#'
#' @include utils.R
#'
#' @seealso `corrr::colpair_map()`, a versatile tool for pairwise column
#'   analysis which the present function wraps.
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


duplicate_count_colpair <- function(data, na.rm = TRUE, show_rates = TRUE) {
  out <- data %>%
    corrr::colpair_map(duplicate_count_by_vec, na.rm = na.rm) %>%
    corrr::shave() %>%
    corrr::stretch(na.rm = TRUE, remove.dups = FALSE) %>%
    dplyr::arrange(dplyr::desc(.data$r)) %>%
    dplyr::rename(count = "r")

  if (show_rates) {
    # Calculate the duplication rates using a helper function:
    rates <- purrr::pmap(out, rate_from_data, data, na.rm)

    # Yet another workaround to replace `tidyr::unnest_wider()` -- compare to
    # `unnest_consistency_cols()`. After some time, replace the superseded
    # `purrr::flatten()` by `purrr::list_flatten()`.
    rate_x <- purrr::map(rates, `[`, 1) %>% purrr::flatten() %>% as.numeric()
    rate_y <- purrr::map(rates, `[`, 2) %>% purrr::flatten() %>% as.numeric()

    out <- dplyr::mutate(out, rate_x, rate_y)
  }

  add_class(out, "scr_dup_count_colpair")
}


