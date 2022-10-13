
#' Summarize scrutiny objects
#'
#' @description `audit()` is an S3 generic to follow up on those scrutiny
#'   functions that perform tests on data frames. It summarizes results of those
#'   tests and presents the summaries in a tibble.
#'
#'   `audit_seq()` and `audit_total_n()` summarize the results of functions that
#'   end on `_seq` and `_total_n`, respectively.
#'
#'   Below is a list of functions that return objects with classes for which
#'   there are `audit()` methods. This means you can run `audit()` on the output
#'   returned by any of these functions. The same is true for `audit_seq()` and
#'   `audit_total_n()`.
#'
#'   Go to the documentation of any function named below to learn about its
#'   `audit()` method, or about the way its output is processed by `audit_seq()`
#'   or `audit_total_n()`.

#' @param data A data frame that inherits one of the classes named below.

#' @section `audit()`:
#'   | \strong{Function}            | \strong{Class}              |
#'   | ---                          | ---                         |
#'   | `grim_map()`                 | `"scr_grim_map"`            |
#'   | `grimmer_map()`              | `"scr_grimmer_map"`         |
#'   | `debit_map()`                | `"scr_debit_map"`           |
#'   | `duplicate_count()`          | `"scr_dup_count"`           |
#'   | `duplicate_detect()`         | `"scr_dup_detect"`          |

#' @section `audit_seq()`:
#'   | \strong{Function}            | \strong{Class}              |
#'   | ---                          | ---                         |
#'   | `grim_map_seq()`             | `"scr_grim_map_seq"`        |
#'   | `grimmer_map_seq()`          | `"scr_grimmer_map_seq"`     |
#'   | `debit_map_seq()`            | `"scr_debit_map_seq"`       |

#' @section `audit_total_n()`:
#'   | \strong{Function}            | \strong{Class}              |
#'   | ---                          | ---                         |
#'   | `grim_map_total_n()`         | `"scr_grim_map_total_n"`    |
#'   | `grimmer_map_total_n()`      | `"scr_grimmer_map_total_n"` |
#'   | `debit_map_total_n()`        | `"scr_debit_map_total_n"`   |

#' @return A tibble (data frame) with test summary statistics.
#' @export
#'
#' @examples
#' # For basic GRIM-testing:
#' pigs1 %>%
#'   grim_map() %>%
#'   audit()
#'
#' # For GRIM-testing with
#' # dispersed inputs:
#' pigs1 %>%
#'   grim_map_seq() %>%
#'   audit_seq()
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
      "x" = "It needs to be the output of a `*_map_seq()` function, \\
      such as `grim_map_seq()`."
    ))
  }

  df_list <- split(data, data$case)

  df_list_hits <- df_list %>%
    purrr::map(dplyr::filter, consistency)

  hits_total <- df_list_hits %>%
    purrr::map_int(nrow) %>%
    unname()

  hits_positions <- df_list %>%
    purrr::map(~ which(.$consistency))

  if (is.null(dim(data))) {
    fun <- class(data)[stringr::str_detect(class(data), "_map_seq")]
    fun <- fun[fun != "scr_map_seq"]
    fun <- stringr::str_remove(fun, "scr_")
    fun <- eval(rlang::parse_expr(fun))
    msg_error <- "No values could be tested with the current parameters."
    if ("items" %in% names(formals(fun))) {
      fun_name <- deparse(substitute(fun))
      msg_items <- list(
        ">" = "Did you specify the `items` argument in {fun_name} \\
        as an unreasonably high number?"
      )
      msg_error <- append(msg_error, msg_items)
    }
    cli::cli_abort(msg_error)
  }

  var_names <- unique(df_list[[1]]$var)

  # Define some helper functions to be mapped below:
  index_hit_distance <- function(df, var_order = var_names) {
    df_by_var <- split(df, df$var)
    out <- purrr::map(df_by_var, index_case_diff)
    out <- purrr::map(out, ~ .[.$consistency, ])
    out <- out[order(var_order)]
    purrr::map(out, ~ .$index_diff)
  }

  inf_to_na <- function(x) {
    x[is.infinite(x)] <- NA
    x
  }

  map_to_length <- function(x) {
    purrr::map(x, length)
  }

  # Prepare endings of the `diff_*` columns:
  fn_names <- c("", "_up", "_down")
  fn_names <- rep(fn_names, length(var_names))

  df_nested <- df_list %>%
    purrr::map(index_hit_distance) %>%
    tibble::tibble(.name_repair = ~ "distance") %>%
    tidyr::unnest_wider(col = distance)

  cols_hits <- df_nested %>%
    dplyr::mutate(dplyr::across(
      .cols = everything(),
      .fns = map_to_length,
      .names = "hits_{.col}"
    )) %>%
    dplyr::select(-all_of(colnames(df_nested))) %>%
    tidyr::unnest(cols = everything())

  cols_diff <- df_nested %>%
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

  if (length(rounding) > 0) {
    data_rev_tested <- fun_test(data_rev, rounding = rounding)
  } else {
    data_rev_tested <- fun_test(data_rev)
  }

  consistency <- data_rev_tested$consistency

  cols_hits <- dplyr::mutate(cols_hits, dplyr::across(
    .cols = where(is.character),
    .fns = as.numeric
  ))

  cols_diff <- dplyr::mutate(cols_diff, dplyr::across(
    .cols = where(is.character),
    .fns = as.numeric
  ))

  out <- data_rev %>%
    dplyr::mutate(consistency, hits_total) %>%
    dplyr::bind_cols(cols_hits, cols_diff) %>%
    add_class("scr_audit_seq")

  return(out)
}




#' @rdname audit
#' @export

audit_total_n <- function(data) {

  if (!inherits(data, "scr_map_total_n")) {
    cli::cli_abort(c(
      "Invalid `data` argument.",
      "x" = "It needs to be the output of a `*_map_total_n()` function, \\
      such as `grim_map_total_n()`."
    ))
  }

  df_list <- split(data, data$case)

  df_list_hits <- df_list %>%
    purrr::map(dplyr::filter, both_consistent)

  map_nrow_half <- function(x) {
    out <- purrr::map_int(x, nrow)
    out <- out / 2
    out
  }

  hits_forth <- df_list_hits %>%
    purrr::map(dplyr::filter, dir == "forth") %>%
    map_nrow_half()

  hits_back <- df_list_hits %>%
    purrr::map(dplyr::filter, dir == "back") %>%
    map_nrow_half()

  hits_total <- hits_forth + hits_back
  scenarios_total <- map_nrow_half(df_list)
  hit_rate <- hits_total / scenarios_total

  out <- data %>%
    reverse_map_total_n() %>%
    dplyr::mutate(
      hits_total, hits_forth, hits_back, scenarios_total, hit_rate
    ) %>%
    add_class("scr_audit_total_n")

  return(out)
}

