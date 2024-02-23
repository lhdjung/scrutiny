
#' Compute minimal `audit()` summaries
#'
#' @description Call `audit_cols_minimal()` within your [`audit()`] methods for
#'   the output of consistency test mapper functions such as [`grim_map()`]. It
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

#' @param data Data frame returned by a mapper function, such as [`grim_map()`].
#' @param name_test String (length 1). Short, plain-text name of the consistency
#'   test, such as `"GRIM"`. Only needed for a potential alert.
#'
#' @export
#'
#' @return A tibble (data frame) with the columns listed above.
#'
#' @include mapper-function-helpers.R
#'
#' @seealso For context, see `vignette("consistency-tests-in-depth")`. In case
#'   you don't call `audit_cols_minimal()`, you should call
#'   [`check_audit_special()`].
#'
#' @examples
#' # For a mapper function called `schlim_map()`
#' # that applies a test called SCHLIM and returns
#' # a data frame with the `"scr_schlim_map"` class:
#' audit.scr_schlim_map <- function(data) {
#'   audit_cols_minimal(data, name_test = "SCHLIM")
#' }
#'
#' # If you like, add other summary columns
#' # with `dplyr::mutate()` or similar.


audit_cols_minimal <- function(data, name_test) {

  # Checks ---

  if (!is.data.frame(data)) {
    cli::cli_abort(c("!" = "`data` must be a data frame."))
  }

  check_audit_special(data, name_test)


  # Compute the summary values of interest ---

  # 1. the number of inconsistent cases:
  incons_cases <- nrow(data[!data$consistency, ])

  # 2. the total number of cases:
  all_cases <- nrow(data)

  # 3. the proportion of inconsistent cases:
  incons_rate <- incons_cases / all_cases

  # Collect these values in a resulting tibble:
  tibble::tibble(incons_cases, all_cases, incons_rate)
}


