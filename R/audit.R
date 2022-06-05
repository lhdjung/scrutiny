
#' Summaries of scrutiny objects
#'
#' @description `audit()` is an S3 generic to follow up on those scrutiny
#'   functions that perform tests on a data frame. It summarizes results of
#'   those tests and presents the summaries in a tibble. `audit_seq()` and
#'   `audit_total_n()` summarize the results of functions that end on `_seq` and
#'   `_total_n`, respectively.
#'
#'   Below is a list of functions that return objects with classes for which
#'   there are `audit()` methods. This means you can run `audit()` on the output
#'   returned by any of these functions. The same is true for `audit_seq()` and
#'   `audit_total_n()`.
#'
#'   Go to the documentation of any function below to learn about its particular
#'   `audit()` method.

#   @section `audit()`: | \strong{Function}            | \strong{Class}            |
#   | ---                          | ---                       |
#   | `grim_map()`                 | `scr_grim_map`            |
#   | `grim_map_total_n()`         | `scr_grim_map_total_n`    |
#   | `debit_map()`                | `scr_debit_map`           |
#   | `debit_map_total_n()`        | `scr_debit_map_total_n`   |
#   | `duplicate_count()`          | `scr_dup_count`           |
#   | `duplicate_detect()`         | `scr_dup_detect`          |

#' @param data A data frame with one of the classes named below.

#' @section `audit()`:
#'   | \strong{Function}            | \strong{Class}            |
#'   | ---                          | ---                       |
#'   | `grim_map()`                 | `scr_grim_map`            |
#'   | `debit_map()`                | `scr_debit_map`           |
#'   | `duplicate_count()`          | `scr_dup_count`           |
#'   | `duplicate_detect()`         | `scr_dup_detect`          |

#' @section `audit_seq()`:
#'   | \strong{Function}            | \strong{Class}            |
#'   | ---                          | ---                       |
#'   | `grim_map_seq()`             | `scr_grim_map_seq`    |
#'   | `debit_map_seq()`            | `scr_debit_map_seq`   |

#' @section `audit_total_n()`:
#'   | \strong{Function}            | \strong{Class}            |
#'   | ---                          | ---                       |
#'   | `grim_map_total_n()`         | `scr_grim_map_total_n`    |
#'   | `debit_map_total_n()`        | `scr_debit_map_total_n`   |


#'
#' @return A tibble (data frame) with test summary statistics.
#' @export
#'
#' @examples
#' # For GRIM-testing:
#' pigs1 %>%
#'   grim_map() %>%
#'   audit()
#'
#' # For detecting duplicates:
#' pigs4 %>%
#'   duplicate_detect() %>%
#'   audit()


audit <- function(data) {
  UseMethod("audit")
}


#' @rdname audit
#' @export


audit_seq <- function(data) {

  if (!inherits(data, "scr_map_seq")) {
    cli::cli_abort(c(
      "Invalid `data` argument.",
      "It needs to be the output of a `*_map_seq()` function, \\
      such as `grim_map_seq()`."
    ))
  }

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

  inf_to_na <- function(x) {
    x[is.infinite(x)] <- NA
    x
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
    dplyr::select(-(1:length(var_names))) %>%
    dplyr::mutate(dplyr::across(
      .cols = everything(),
      .fns = inf_to_na
    )) %>%
    suppressWarnings()

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

  # class_reported %>%
  #   stringr::str_remove("scr_reported_") %>%
  #   stringr::str_split("_SCR_STOP_") %>%
  #   unlist()

  data_rev <- reverse_map_seq(data)

  if (length(rounding) > 0) {
    data_rev_tested <- fun_test(data_rev, rounding = rounding)
  } else {
    data_rev_tested <- fun_test(data_rev)
  }

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




# audit_seq <- function(data) {
#   UseMethod("audit_seq")
# }


#' @rdname audit
#' @export

audit_total_n <- function(data) {

  if (!inherits(data, "scr_map_total_n")) {
    cli::cli_abort(c(
      "Invalid `data` argument.",
      "x" = "It needs to be the output of a `*map_total_n()` function, \\
      such as `grim_map_total_n()`."
    ))
  }

  df_list <- split(data, data$case)

  df_list_hits <- df_list %>%
    purrr::map(dplyr::filter, both_consistent)

  hits_forth <- df_list_hits %>%
    purrr::map(dplyr::filter, dir == "forth") %>%
    purrr::map_int(nrow) %>%
    `/`(2)

  hits_back <- df_list_hits %>%
    purrr::map(dplyr::filter, dir == "back") %>%
    purrr::map_int(nrow) %>%
    `/`(2)

  hits_total <- hits_forth + hits_back

  scenarios_total <- df_list %>%
    purrr::map_int(nrow) %>%
    `/`(2)

  hit_rate <- hits_total / scenarios_total

  data_rec <- reverse_map_total_n(data)

  out <- data_rec %>%
    dplyr::mutate(hits_forth, hits_back, hits_total, scenarios_total, hit_rate)

  return(out)
}

