#' Reconstruct the input of a sequence mapper
#'
#' @description `reverse_map_seq()` takes the output of a function made by
#'   [`function_map_seq()`] and reconstructs the data frame it was called on.
#'   [`audit_seq()`] builds on it.
#'
#' @param data Data frame with the `"scrutiny_map_seq"` class.
#'
#' @include utils.R
#'
#' @export
#'
#' @return A tibble with the tested columns, one row per case.
#'
#' @examples
#' pigs1
#'
#' out <- grim_map_seq(
#'   pigs1,
#'   digits_x = 2,
#'   items_averaged = 1,
#'   include_consistent = TRUE
#' )
#'
#' reverse_map_seq(out)

reverse_map_seq <- function(data) {
  if (!inherits(data, "scrutiny_map_seq")) {
    abort_in_export(
      "!" = "`data` must be the output of \\
      a function like `grim_map_seq()`.",
      "x" = "It isn't.",
      "i" = "Such functions were created by `function_map_seq()`."
    )
  }

  check_dispersion_linear(data, "reverse_map_seq")

  # The tested columns are those left of the key result column, whose name the
  # sequence mapper records:
  name_key_result <- attr(data, "scrutiny", exact = TRUE)$name_key_result
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

  # The step size of each variable's dispersion: one unit of the last decimal
  # place as given by a `digits_*` column, not guessed from the values. `n`
  # steps by whole numbers. Anything else falls back to
  # `index_case_from_diff()`'s inference from the sequence:
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
  # one it disperses. Each reported value is therefore read off the rows that
  # leave it untouched, and only recovered from its own dispersion when there
  # are none. This keeps the cases aligned even if `out_min` or `out_max`
  # clipped a variable's dispersion to nothing for one case. A case without
  # any rows is not in the output, as in `audit_seq()`.
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
