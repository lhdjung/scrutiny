
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


reverse_map_seq <- function(data) {
  var <- unique(data$var)

  data_var <- data %>%
    split(list(data$var)) %>%
    tibble::tibble(.name_repair = ~ "df") %>%
    dplyr::arrange(var) %>%
    dplyr::pull(df)

  data_var_with_index_case <- data_var %>%
    purrr::map(split, data$case) %>%
    tibble::tibble(.name_repair = ~ "df") %>%
    dplyr::mutate(
      var,
      df = purrr::modify_depth(df, .depth = 2, dplyr::select, all_of(var)),
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
    dplyr:::mutate(var) %>%
    tidyr::pivot_wider(names_from = var, values_from = df) %>%
    tidyr::unnest(cols = everything()) %>%
    tidyr::unnest(cols = everything()) # yes, this is weird

  return(out)
}



