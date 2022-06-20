
#' Compute minimal `audit()` summaries
#'
#' @description Call `required_audit_cols()` within your `audit()` methods for
#'   the output of consistency test mapper functions such as `grim_map()`. It
#'   will create a tibble with the three minimal, required columns:
#'
#'   1. `incons_cases` counts the inconsistent cases, i.e., the number of rows
#'   in the mapper's output where `"consistency"` is `FALSE`.
#'
#'   2. `all_cases` is the total number of rows in the mapper's output.
#'
#'   3. `incons_rate` is the ratio of `incons_cases` to `all_cases`.
#'
#'   You can still add other columns to this tibble. Either way, make sure to
#'   name your method correctly. See examples.

#' @param .name_test String (length 1). Short, plain-text name of the
#'   consistency test, such as `"GRIM"`. Only needed for a potential alert.
#'
#' @export
#'
#' @include check-colnames-mapper.R
#'
#' @seealso For context, see `vignette("consistency-tests")`. In case you don't
#'   call `required_audit_cols()`, you should call `check_audit_special()`.
#'
#' @examples
#' # For a mapper function called `schlim_map()`
#' # that applies a test called SCHLIM and returns
#' # a data frame with the `"scr_schlim_map"` class:
#' audit.scr_schlim_map <- function(data) {
#'   required_audit_cols(data)
#' }
#'
#' # If you like, add other summary columns
#' # with `dplyr::mutate` or similar.


required_audit_cols <- function(data, name_test) {

  check_audit_special(data, name_test)

  # Compute the summary values of interest ---

  # 1. the number of inconsistent cases:
  incons_cases <- nrow(data[!data$consistency, ])

  # 2. the total number of cases:
  all_cases <- nrow(data)

  # 3. the proportion of inconsistent cases:
  incons_rate <- incons_cases / all_cases

  # Collect these values in a resulting tibble:
  out <- tibble::tibble(incons_cases, all_cases, incons_rate)

  return(out)
}


