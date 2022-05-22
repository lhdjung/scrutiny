
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




summarize_map_seq <- function(data) {
  df_list <- split(data, data$case)

  # Might be useful later: `purrr::map(df_list, split, ~ var)`

  df_list_hits <- df_list %>%
    purrr::map(dplyr::filter, consistency)

  hits <- df_list_hits %>%
    purrr::map_int(nrow) %>%
    unname()

  hits_positions <- df_list %>%
    purrr::map(~ which(.$consistency))

  var_names <- unique(df_list[[1]]$var)

  index_hit_distance <- function(df, var_order = var_names) {
    df_by_var <- split(df, df$var)
    out <- purrr::map(df_by_var, index_case_diff)
    out <- purrr::map(out, ~ .[.$consistency, ])
    # out <- purrr::map(df_by_var, ~ which(.$consistency))
    out <- out[order(var_order)]
    purrr::map(out, ~ .$index_diff)
  }

  # # HERE, I NEED TO USE (APPLY?) A FUNCTION THAT REALLY JUST GETS ME THE INDEX,
  # # NOT THE INDEX CASE; BUT THAT'S OTHERWISE JUST LIKE `index_case_recover()`...
  # df_by_var %>%
  #   tibble::tibble(.name_repair = ~"df") %>%
  #   dplyr::mutate(name = names(df_by_var)) %>%
  #   dplyr::mutate(df = dplyr::mutate(df, THATS_RIGHT_HERE))
  #
  # # ... HERE'S HOW TO DO THAT FOR A SINGLE DATA FRAME:
  # df <- df_list[[1]] %>%
  #   dplyr::filter(var == "n")



  # min_distance_or_na <- function(x) {
  #   # x <- x[[1]]
  #   if (is.null(x)) {
  #     return(NA)
  #   } else {
  #     x_ma <- min(abs(x))
  #     out <- x[x == -x_ma | x == x_ma]
  #     # In case of a tie (i.e., same distance to 0 on the positive and the
  #     # negative side), the positive number is returned. This is a pragmatic
  #     # decision without any special meaning. Output length has to be 1, because
  #     # the function will be mapped within `purrr::map_int()` below; so every
  #     # single output will be an element of an atomic vector:
  #     if (length(out) > 1) {
  #       return(max(out))
  #     } else {
  #       return(out)
  #     }
  #     # min(abs(x))
  #     # out <- x[x == min(abs(x))]
  #     # if (length(out) == 0) {
  #     #   return(NA)
  #     # }
  #     # return(out)
  #     # min_abs_index <- match(x[x == min(abs(x))], x)
  #     # min_abs_index_sign <- sign(min_abs_index)
  #     # return(x[min_abs_index])
  #     # return(x[index])
  #   }
  # }


  fn_names <- rep(c("", "_up", "_down"), length(var_names))

  # TO DO: COMPUTE DISTANCE OF FIRST HIT FROM INDEX CASE; IN PARTICULAR, MAKE
  # SURE IT'S ACTUALLY THAT DISTANCE, NOT SIMPLY THE INDEX! GOOD START BELOW

  diff_cols <- df_list %>%
    purrr::map(index_hit_distance) %>%
    tibble::tibble(.name_repair = ~ "distance") %>%
    tidyr::unnest_wider(col = distance) %>%
    dplyr::mutate(dplyr::across(
      .cols = everything(),
      .fns = list(min_distance_abs, min_distance_pos, min_distance_neg),
      .names = "diff_{.col}{fn_names}"
    )) %>%
    dplyr::select(-(1:length(var_names)))

    # dplyr::mutate(dplyr::across(
    #   everything(), purrr::map_int, min_distance_or_na  # , 1
    # ))


  hits_positions_means <- hits_positions %>%
    purrr::map_dbl(mean) %>%
    unname()

  dc <- class(data)

  rounding <- dc[stringr::str_detect(dc, "scr_rounding_")]
  rounding <- stringr::str_remove(rounding, "scr_rounding_")

  fun_test <-
    dc[stringr::str_detect(dc, "^scr_") & stringr::str_detect(dc, "_map$")]
  fun_test <- stringr::str_remove(fun_test, "scr_")
  fun_test <- rlang::eval_bare(rlang::parse_expr(fun_test))

  data_rev <- reverse_map_seq(data)
  data_rev_tested <- fun_test(data_rev, rounding = rounding)
  consistency <- data_rev_tested$consistency

  out <- data_rev %>%
    dplyr::mutate(consistency, hits) %>%
    dplyr::bind_cols(diff_cols)

  # out <- data %>%
  #   reverse_map_seq() %>%
  #   dplyr::mutate(hits) %>%
  #   dplyr::bind_cols(diff_cols)

  return(out)
}

