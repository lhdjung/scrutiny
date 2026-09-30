#' Reverse the `*_map_seq()` process
#'
#' @description `reverse_map_seq()` takes the output of a function created by
#'   [`function_map_seq()`] and reconstructs the original data frame.
#'
#'   See [`audit_seq()`], which takes `reverse_map_seq()` as a basis.
#'
#' @param data Data frame that inherits the `"scrutiny_map_seq"` class.
#'
#' @include utils.R
#'
#' @export
#'
#' @return The reconstructed tibble (data frame) which a factory-made
#'   `*_map_seq()` function took as its `data` argument.
#'
#' @examples
#' # Originally reported summary data...
#' pigs1
#'
#' # ...GRIM-tested with varying inputs...
#' out <- grim_map_seq(pigs1, digits_x = 2, include_consistent = TRUE)
#'
#' # ...and faithfully reconstructed:
#' reverse_map_seq(out)

reverse_map_seq <- function(data) {
  # Check that `data` is a tibble returned by a function that had been
  # manufactured using `function_map_seq()`:
  if (!inherits(data, "scrutiny_map_seq")) {
    cli::cli_abort(c(
      "!" = "`data` must be the output of \\
      a function like `grim_map_seq()`.",
      "x" = "It isn't.",
      "i" = "Such functions were created by `function_map_seq()`."
    ))
  }

  check_dispersion_linear(data, "reverse_map_seq")

  # The tested columns are those left of the key result column, which the
  # sequence mapper records by name because `.name_key_result` may have renamed
  # it. Absent the attribute, it is `"consistency"`:
  name_key_result <- scrutiny_meta(data)$name_key_result
  if (is.null(name_key_result)) {
    name_key_result <- "consistency"
  }

  var <- data |>
    select_tested_cols(before = name_key_result) |>
    colnames()

  # With no inconsistent case, the sequence mapper returns no rows:
  if (nrow(data) == 0L) {
    return(tibble::new_tibble(as.list(data)[var], nrow = 0L))
  }

  # The step size of each variable's dispersion. For a variable with a
  # `digits_*` column -- every variable that the mapper takes decimal places for
  # -- it is one unit of the last decimal place, stated by the caller of the
  # mapper rather than guessed from the values. `n` is dispersed in whole
  # numbers. Anything else falls back to `index_case_from_diff()`'s own
  # inference from the sequence:
  step_by_var <- function(var_name) {
    if (var_name == "n") {
      return(1)
    }
    digits_col <- paste0("digits_", var_name)
    if (any(digits_col == colnames(data))) {
      return(1 / (10^data[[digits_col]][[1L]]))
    }
    NULL
  }

  # Every row of a case carries the reported values of all variables except the
  # one it disperses, which sits `diff_var` steps away from its reported value.
  # Each reported value is therefore read off the rows that leave it untouched,
  # and only recovered from its own dispersion when there are no such rows. This
  # keeps the cases aligned even if `out_min` or `out_max` clipped a variable's
  # dispersion to nothing for one case: pairing the groups of each variable by
  # position used to shift every later case's value in that column. A case
  # without any rows at all is not in the output, as in `audit_seq()`.
  cases <- split(data, data$case)

  cols <- lapply(var, function(v) {
    by <- step_by_var(v)
    cases |>
      lapply(function(d) {
        untouched <- d$var != v
        if (any(untouched)) {
          d[[v]][untouched][[1L]]
        } else {
          index_case_from_diff(x = d[[v]], diff_var = d$diff_var, by = by)
        }
      }) |>
      purrr::list_c()
  })

  cols |>
    rlang::set_names(var) |>
    tibble::as_tibble()
}
