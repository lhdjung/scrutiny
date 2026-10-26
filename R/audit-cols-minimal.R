#' Compute minimal `audit()` summaries
#'
#' @description Call `audit_cols_minimal()` within an [`audit()`] method for
#'   the output of a consistency test mapper such as [`grim_map()`]. It returns
#'   a tibble with the three columns every such method must have:
#'
#'   1. `incons_cases`: the number of rows where `consistency` is `FALSE`.
#'   A case the test could not decide is `NA` there and not counted.
#'   2. `all_cases`: the number of rows, including undecidable ones.
#'   3. `incons_rate`: `incons_cases / all_cases`.
#'
#'   Add further columns as needed. See *Examples* for how to name the method.
#'
#' @param data Data frame returned by a mapper function.
#'
#' @export
#'
#' @return A tibble with the three columns above.
#'
#' @include mapper-function-helpers.R
#'
#' @seealso `vignette("consistency-tests-in-depth")`.
#'
#' @examples
#' # For a mapper `schlim_map()` that applies a test called SCHLIM and
#' # returns a data frame with the class `"scrutiny_schlim_map"`:
#' audit.scrutiny_schlim_map <- function(data, ...) {
#'   audit_cols_minimal(data)
#' }

audit_cols_minimal <- function(data) {
  if (!is.data.frame(data)) {
    abort_in_export("!" = "`data` must be a data frame.")
  }

  check_cols_needed(data, "consistency")

  # An undecidable case, with `consistency` missing, is not an inconsistent one:
  incons_cases <- sum(!data$consistency, na.rm = TRUE)

  all_cases <- nrow(data)

  incons_rate <- incons_cases / all_cases

  tibble::new_tibble(
    x = list(
      incons_cases = incons_cases,
      all_cases = all_cases,
      incons_rate = incons_rate
    ),
    nrow = 1L,
    class = NULL
  )
}
