

recover_index_case <- function(x) {
  # Indexing right below is a pragmatic choice because of the place in
  # `reverse_map_seq()` where the present function is mapped:
  x <- x[[1]]
  lx <- length(x)
  if (is_even(lx)) {
    from <- x[lx / 2]
  } else {
    # Zero is just a placeholder in the absence of knowledge about the index
    # case in sequences that are of odd length, and hence asymmetrical:
    from <- 0
  }
  seq_distance(
    from = from, length_out = 1, offset_from = 1, string_output = "auto"
  )
}



# # Example data:
# data <- pigs1 %>%
#   grim_map() %>%
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
    purrr::map(recover_index_case) %>%
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




summarize_map_seq <- function(data) {
  df_list <- split(data, data$case)

  df_list_hits <- df_list %>%
    purrr::map(dplyr::filter, consistency)

  hits <- df_list_hits %>%
    purrr::map_int(nrow) %>%
    unname()

  out <- data %>%
    reverse_map_seq() %>%
    dplyr::mutate(hits)

  return(out)
}

