

#' Count duplicate values
#'
#' @description `duplicate_count()` returns a frequency table. When searching a
#'   data frame, it includes values from all columns for each frequency count.
#'
#'   This function is a blunt tool designed for initial data checking. Don't put
#'   too much weight on its results.
#'
#'   For summary statistics, call `audit()` on the results.
#'
#' @details `duplicate_count()` is a thin wrapper around `janitor::get_dupes()`.
#'   Use `get_dupes()` to search for duplicate rows.
#'
#'   The function is not too informative if the values have few characters.
#'
#' @param x Vector or data frame.
#' @param numeric_only Boolean. If `TRUE` (the default), and if `x` is a data
#'   frame, the function includes only numeric columns and string columns
#'   coercible to numeric. *Note*: Be careful when setting it to `FALSE`. This
#'   can lead to all kinds of coercion issues.


#' @return A tibble with two columns —
#'
#' - `value` includes all the values from `x`.
#' - `count` is the frequency of each value in `x`, in descending order.

#' The tibble has the `scr_dup_count` class, which is recognized by the
#' `audit()` generic.

#' @section Summaries with `audit()`: There is an S3 method for the `audit()`
#'   generic, so you can call `audit()` following `duplicate_count()` to get
#'   summary statistics. These are mostly self-explaining, but `count_max` and
#'   `count_min` only directly apply to `count` and display their respective
#'   `value` numbers, not the minimal and maximal `value` numbers.
#'
#' @seealso `duplicate_detect()` checks if values have duplicates.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # Count duplicate values...
#' BJsales %>%
#'   duplicate_count()
#'
#' # ...and compute summaries:
#' BJsales %>%
#'   duplicate_count() %>%
#'   audit()


duplicate_count <- function(x, numeric_only = TRUE) {

  # Deal with a vector or data frame:
  if (!is.data.frame(x)) {
    x <- tibble::as_tibble(x) %>%
      dplyr::rename(value = x)
    val <- x
  } else {
    # By default, coerce all columns to numeric with which that is possible, and
    # drop the rest. In any case, lump all of the data frame's values into a
    # single vector:
    if (numeric_only) {
      x <- tibble::as_tibble(x)
      x <- x %>%
        dplyr::select(where(is.numeric) | where(is.character)) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
        dplyr::select(where(~ !all(is.na(.)))) %>%
        suppressWarnings()
      val <- purrr::flatten_dbl(x)
    } else {
      val <- x %>%
        purrr::flatten() %>%
        unlist()
    }
  }

  # Count duplicate values via {janitor}:
  val %>%
    tibble::as_tibble() %>%
    janitor::get_dupes() %>%
    dplyr::distinct() %>%
    dplyr::rename(count = dupe_count) %>%
    dplyr::arrange(desc(.data$count)) %>%
    add_class("scr_dup_count") %>%
    suppressMessages()
}

