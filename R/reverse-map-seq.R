
# Helper functions to be mapped in the other helper functions below...
min_distance_abs_scalar <- function(x) {
  if (is.null(x)) {
    return(NA)
  }
  min(abs(x))
}

min_distance_pos_scalar <- function(x) {
  if (is.null(x)) {
    return(NA)
  }
  min(x[x > 0])
}

min_distance_neg_scalar <- function(x) {
  if (is.null(x)) {
    return(NA)
  }
  max(x[x < 0])
}

# ...namely these ones:
min_distance_abs <- function(x) purrr::map_int(x, min_distance_abs_scalar)
min_distance_pos <- function(x) purrr::map_int(x, min_distance_pos_scalar)
min_distance_neg <- function(x) purrr::map_int(x, min_distance_neg_scalar)




# # Example data:
# data <- pigs1 %>%
#   grim_map_seq()


#' Reverse the `*_map_seq()` process
#'
#' `reverse_map_seq()` takes the output of a function created by
#' `function_map_seq()` and reconstructs the original data frame.
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples


reverse_map_seq <- function(data) {

  # Check that `data` is a tibble returned by a function that had been
  # manufactured using `function_map_seq()`:
  if (!inherits(data, "scr_map_seq")) {
    cli::cli_abort(c(
      "Invalid `data` argument.",
      "x" = "It needs to be the output of a function like `grim_map_seq()`.",
      ">" = "Such functions were created by `function_map_seq()`."
    ))
  }

  # var <- unique(data$var)

  var <- data %>%
    select_key_columns() %>%
    colnames()

  # var_unique <- unique(data$var)

  var_unique <- var

  if (length(var_unique) == 1) {
    data_var <- list(data)
    # names(data_var) <- var_unique
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

    # tibble::tibble(.name_repair = ~ "df") %>%
    # dplyr::arrange(var) %>%
    # dplyr::pull(df)

  data_var_with_index_case <- data_var %>%
    purrr::map(split, data$case) %>%
    tibble::tibble(.name_repair = ~ "df") %>%
    dplyr::mutate(
      var,
      df = purrr::modify_depth(df, .depth = 2, select_key_columns),  # dplyr::select, all_of(var)),
      df = purrr::map2(df, var, ~ purrr::map2(.x, .y, `[`))
    ) %>%
    dplyr::pull(df) %>%
    purrr::flatten() %>%
    purrr::map(`[[`, 1) %>%
    purrr::map(index_case_interpolate) %>%
    suppressWarnings()

  var_group_size <- length(data_var_with_index_case) / length(var)

  data_var_with_index_case <- data_var_with_index_case %>%
    split_into_groups(group_size = var_group_size)

  out <- data_var_with_index_case %>%
    tibble::tibble(.name_repair = ~ "df") %>%
    dplyr::mutate(var) %>%
    tidyr::pivot_wider(names_from = var, values_from = df) %>%
    tidyr::unnest(cols = everything()) %>%
    tidyr::unnest(cols = everything()) # yes, this is weird

  return(out)
}


