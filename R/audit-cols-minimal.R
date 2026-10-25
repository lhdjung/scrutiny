#' Compute minimal `audit()` summaries
#'
#' @description Call `audit_cols_minimal()` within an [`audit()`] method for
#'   the output of a consistency test mapper such as [`grim_map()`]. It returns
#'   a tibble with the three columns every such method must have:
#'
#'   1. `incons_cases`: the number of rows where the result column is `FALSE`.
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
  # Checks ---

  if (!is.data.frame(data)) {
    abort_in_export("!" = "`data` must be a data frame.")
  }

  check_cols_needed(data, "consistency")

  # Compute the summary values of interest ---

  # 1. the number of inconsistent cases. `consistency` is `NA` where the test
  # could not decide the case -- a missing value in a key column, or rounding
  # bounds that are undefined for the given method. Such a case is not an
  # inconsistent one, and counting it as one used to overstate `incons_cases`:
  # indexing rows by `NA` returns a row of `NA`s rather than no row at all:
  incons_cases <- sum(!data$consistency, na.rm = TRUE)

  # 2. the total number of cases:
  all_cases <- nrow(data)

  # 3. the proportion of inconsistent cases:
  incons_rate <- incons_cases / all_cases

  # Collect these values in a resulting tibble:
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
