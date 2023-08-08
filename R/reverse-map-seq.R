
# Helper functions to be mapped in the other helper functions below...
min_distance_abs_scalar <- function(x) {
  if (any(!is.numeric(x))) {
    return(NA_real_)
  }
  min(abs(x))
}

min_distance_pos_scalar <- function(x) {
  if (any(!is.numeric(x))) {
    return(NA_real_)
  }
  min(x[x > 0])
}

min_distance_neg_scalar <- function(x) {
  if (any(!is.numeric(x))) {
    return(NA_real_)
  }
  max(x[x < 0])
}

# ...namely these ones:
# min_distance_abs <- function(x) purrr::map_dbl(x, min_distance_abs_scalar)
# min_distance_pos <- function(x) purrr::map_dbl(x, min_distance_pos_scalar)
# min_distance_neg <- function(x) purrr::map_dbl(x, min_distance_neg_scalar)

min_distance_abs <- function(x) vapply(x, min_distance_abs_scalar, double(1L))
min_distance_pos <- function(x) vapply(x, min_distance_pos_scalar, double(1L))
min_distance_neg <- function(x) vapply(x, min_distance_neg_scalar, double(1L))



# # Example data:
# data <- grim_map_seq(pigs1)



#' Reverse the `*_map_seq()` process
#'
#' @description `reverse_map_seq()` takes the output of a function created by
#'   `function_map_seq()` and reconstructs the original data frame.
#'
#'   See `audit_seq()`, which takes `reverse_map_seq()` as a basis.
#'
#' @param data Data frame that inherits the `"scr_map_seq"` class.
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
#' out <- grim_map_seq(pigs1, include_consistent = TRUE)
#'
#' # ...and faithfully reconstructed:
#' reverse_map_seq(out)


reverse_map_seq <- function(data) {

  # Check that `data` is a tibble returned by a function that had been
  # manufactured using `function_map_seq()`:
  if (!inherits(data, "scr_map_seq")) {
    cli::cli_abort(c(
      "!" = "`data` must be the output of \\
      a function like `grim_map_seq()`.",
      "x" = "It isn't.",
      "i" = "Such functions were created by `function_map_seq()`."
    ))
  }

  var <- data %>%
    select_tested_cols() %>%
    colnames()

  var_unique <- var

  if (length(var_unique) == 1L) {
    data_var <- list(data)
    data_var <- append(data_var, data_var)
    names(data_var) <- c(var_unique, "scr_split_dummy")
  } else {
    data_var <- split(data, list(data$var))
    data_var <- data_var[var_unique]  # order by `var`
    if (length(unique(data$var)) < length(data_var)) {
      length_diff <- length(data_var) - length(unique(data$var))
      data_var_fill <- rep(data_var[1], length_diff)
      data_var <- append(data_var, data_var_fill)
      data_var <- Filter(length, data_var)
    }
  }

  data_nested <- data %>%
    dplyr::nest_by(case, var) %>%
    dplyr::arrange(var)

  data_nested <- split(data_nested, data_nested$var)[var]
  data_nested <- dplyr::bind_rows(data_nested)

  data_index_case <- data_nested %>%
    dplyr::mutate(
      scr_index_case = list(data[var]),
      scr_index_case = list(index_case_interpolate(scr_index_case[[1]]))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(var, scr_index_case)

  data_index_case %>%
    tidyr::pivot_wider(
      names_from  = var,
      values_from = scr_index_case,
      values_fn   = list
    ) %>%
    tidyr::unnest(cols = everything()) %>%
    tidyr::unnest(cols = everything()) # yes, this is weird
}


