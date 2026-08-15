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

  check_dispersion_linear(data)

  # The tested columns are those left of the key result column, which the
  # sequence mapper records by name because `.name_key_result` may have renamed
  # it. Absent the attribute -- e.g. after subsetting -- it is `"consistency"`:
  name_key_result <- attr(data, "scrutiny_name_key_result", exact = TRUE)
  if (is.null(name_key_result)) {
    name_key_result <- "consistency"
  }

  var <- data |>
    select_tested_cols(before = name_key_result) |>
    colnames()

  var_unique <- var

  if (length(var_unique) == 1L) {
    data_var <- list(data)
    data_var <- append(data_var, data_var)
    names(data_var) <- c(var_unique, "scrutiny_split_dummy")
  } else {
    data_var <- split(data, list(data$var))
    data_var <- data_var[var_unique] # order by `var`
    if (length(unique(data$var)) < length(data_var)) {
      length_diff <- length(data_var) - length(unique(data$var))
      data_var_fill <- rep(data_var[1L], length_diff)
      data_var <- append(data_var, data_var_fill)
      data_var <- Filter(length, data_var)
    }
  }

  data_nested <- data |>
    dplyr::nest_by(case, var) |>
    dplyr::arrange(var)

  data_nested <- split(data_nested, data_nested$var)[var]
  data_nested <- purrr::list_rbind(data_nested)

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

  # The reported value is recovered from `diff_var`, which records how many
  # steps each dispersed row sits from it. That is exact whether or not the
  # sequence is complete -- unlike inferring it from the shape of the sequence,
  # which silently returned the wrong value once `out_min` or `out_max` had
  # truncated one side of it:
  data_index_case <- data_nested |>
    dplyr::mutate(
      scrutiny_index_case = list(
        index_case_from_diff(
          x = data[var][[1L]],
          diff_var = data$diff_var,
          by = step_by_var(var)
        )
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(var, scrutiny_index_case)

  data_index_case |>
    tidyr::pivot_wider(
      names_from = var,
      values_from = scrutiny_index_case,
      values_fn = list
    ) |>
    tidyr::unnest(cols = everything()) |>
    tidyr::unnest(cols = everything()) # yes, this is weird
}
