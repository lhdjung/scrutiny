#' Read the settings a mapper recorded on its output
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Every mapper, such as [`grim_map()`], records the settings of
#'   the call that produced its output in a list attribute named `"scrutiny"`.
#'   `scrutiny_meta()` returns that list, so that a report can state under
#'   which rounding method or SD convention a verdict was reached. Classes are
#'   for method dispatch only and carry no settings.
#'
#'   The list has these elements:
#'
#' - `args`: the arguments that applied to the whole call, with defaults
#'   resolved, such as `rounding`, `percent`, or `sd_type`. Arguments that vary
#'   by row, such as `digits_x`, are columns of the output instead.
#' - `results_reported`, `name_key_result`, `var`, and `dispersion_linear`:
#'   added by sequence mappers, such as [`grim_map_seq()`]. `results_reported`
#'   is the verdict of every row of the input data, which [`audit_seq()`]
#'   reports; `var` names the dispersed variables.
#'
#'   Some dplyr verbs, such as `summarise()`, drop the attribute; `filter()`,
#'   `arrange()`, and `mutate()` keep it.
#'
#' @param x Output of a mapper function.
#'
#' @return A named list, or `NULL` if `x` has no `"scrutiny"` attribute.
#'
#' @export
#'
#' @examples
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1, rounding = "up") |>
#'   scrutiny_meta()
#'
#' # Just the rounding method:
#' out <- grim_map(pigs1, digits_x = 2, items_averaged = 1)
#' scrutiny_meta(out)$args$rounding
scrutiny_meta <- function(x) {
  attr(x, "scrutiny", exact = TRUE)
}
