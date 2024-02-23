
#' Summarize scrutiny objects
#'
#' @description `audit()` summarizes the results of scrutiny functions like
#'   [`grim_map()`] that perform tests on data frames.
#'
#'   See below for a record of such functions. Go to the documentation of any of
#'   them to learn about its `audit()` method.

#' @param data A data frame that inherits one of the classes named below.

#' @details `audit()` is an S3 generic. It looks up the (invisible) scrutiny
#'   class of a tibble returned by any function named below. You don't need to
#'   deal with the classes directly. Behind the scenes, they mediate between
#'   these functions and their associated summary statistics.

#' @section Run before `audit()`:
#'   | \strong{Function}              | \strong{Class}              |
#'   | ---                            | ---                         |
#'   | [`grim_map()`]                 | `"scr_grim_map"`            |
#'   | [`grimmer_map()`]              | `"scr_grimmer_map"`         |
#'   | [`debit_map()`]                | `"scr_debit_map"`           |
#'   | [`duplicate_count()`]          | `"scr_dup_count"`           |
#'   | [`duplicate_count_colpair()`]  | `"scr_dup_count_colpair"`   |
#'   | [`duplicate_tally()`]          | `"scr_dup_tally"`           |
#'   | [`duplicate_detect()`]         | `"scr_dup_detect"`          |
#'   | [`audit_seq()`]                | `"scr_audit_seq"`           |
#'   | [`audit_total_n()`]            | `"scr_audit_total_n"`       |

#' @return A tibble (data frame) with test summary statistics.
#' @export
#'
#' @examples
#' # For basic GRIM-testing:
#' pigs1 %>%
#'   grim_map() %>%
#'   audit()
#'
#' # For duplicate detection:
#' pigs4 %>%
#'   duplicate_count() %>%
#'   audit()

audit <- function(data) {
  UseMethod("audit")
}



#' Summaries in list form
#' @description `r lifecycle::badge("deprecated")`
#'
#'   `audit_list()` is deprecated. Use `audit()` instead.
#'
#'   It was meant to be used when `audit()` would have returned tibbles that
#'   were too wide to be read. However, the output format for `audit()` has now
#'   been overhauled, there is no longer a need for `audit_list()`.
#'
#' @return Named list of `audit()`'s results.
#'
#' @keywords internal
#'
#' @export
#'
#' @examples
#' # Only use `audit()` instead:
#' pigs1 %>%
#'   grim_map() %>%
#'   audit()


audit_list <- function(data) {
  lifecycle::deprecate_warn(
    when = "0.3.0",
    what = "audit_list()",
    with = "audit()"
  )
  as.list(audit(data))
}



#' Summarize output of sequence mappers and total-n mappers
#'
#' @description `audit_seq()` and `audit_total_n()` summarize the results of
#'   functions that end on `_seq` and `_total_n`, respectively.
#'
#'   See below for a record of such functions. Go to the documentation of any of
#'   them to learn about the way its output is processed by `audit_seq()` or
#'   `audit_total_n()`.

#' @details All functions named below that end on `_seq` were made by
#'   `function_map_seq()`. All that end on `_total_n` were made by
#'   `function_map_total_n()`.

#' @param data A data frame that inherits one of the classes named below.

#' @name audit-special

#' @section Before `audit_seq()`:
#'   | \strong{Function}            | \strong{Class}              |
#'   | ---                          | ---                         |
#'   | `grim_map_seq()`             | `"scr_grim_map_seq"`        |
#'   | `grimmer_map_seq()`          | `"scr_grimmer_map_seq"`     |
#'   | `debit_map_seq()`            | `"scr_debit_map_seq"`       |

#' @section Before `audit_total_n()`:
#'   | \strong{Function}            | \strong{Class}              |
#'   | ---                          | ---                         |
#'   | `grim_map_total_n()`         | `"scr_grim_map_total_n"`    |
#'   | `grimmer_map_total_n()`      | `"scr_grimmer_map_total_n"` |
#'   | `debit_map_total_n()`        | `"scr_debit_map_total_n"`   |

#' @return A tibble (data frame) with test summary statistics.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' # For GRIM-testing with dispersed inputs:
#' out <- pigs1 %>%
#'   grim_map_seq() %>%
#'   audit_seq()
#' out
#'
#' # Follow up on `audit_seq()` or
#' # `audit_total_n()` with `audit()`:
#' audit(out)


audit_seq <- function(data) {

  if (!inherits(data, "scr_map_seq")) {
    cli::cli_abort(c(
      "Invalid `data` argument.",
      "!" = "It must be the output of a `*_map_seq()` function, \\
      such as `grim_map_seq()`."
    ))
  }

  check_dispersion_linear(data)

  df_list <- split(data, data$case)

  df_list_hits <- df_list %>%
    purrr::map(dplyr::filter, consistency)

  hits_total <- df_list_hits %>%
    vapply(nrow, integer(1L), USE.NAMES = FALSE) %>%
    unname()

  hits_positions <- df_list %>%
    purrr::map(function(x) which(x$consistency))

  if (is.null(dim(data))) {
    fun <- class(data)[stringr::str_detect(class(data), "_map_seq$")]
    fun <- fun[fun != "scr_map_seq"]
    fun <- stringr::str_remove(fun, "^scr_")
    fun <- eval(rlang::parse_expr(fun))
    msg_error <-
      c("!" = "No values could be tested.")
    if (any(names(formals(fun)) == "items")) {
      fun_name <- deparse(substitute(fun))
      msg_items <- c(
        "x" = "Did you specify the `items` argument in {fun_name} \\
        as an unreasonably large number?"
      )
      msg_error <- append(msg_error, msg_items)
    }
    cli::cli_abort(msg_error)
  }

  var_names <- unique(df_list[[1L]]$var)

  # Define some helper functions to be mapped below:
  index_hit_distance <- function(df, var_order = var_names) {
    out <- df %>%
      split(df$var) %>%
      purrr::map(function(x) x[x$consistency, ])
    out[order(var_order)] %>%
      purrr::map(function(x) x$diff_var)
  }

  length_unless_na <- function(x) {
    if (length(x) == 1L && is.na(x)) {
      0L
    } else {
      length(x)
    }
  }

  # Prepare endings of the `diff_*` columns:
  fun_names <- c("", "_up", "_down")
  fun_names <- rep(fun_names, length(var_names))

  df_nested <- df_list %>%
    purrr::map(index_hit_distance) %>%
    tibble::tibble(.name_repair = function(x) "distance") %>%
    tidyr::unnest_wider(col = distance)

  cols_hits <- df_nested %>%
    dplyr::mutate(dplyr::across(
      .cols = everything(),
      .fns = function(x) {
        vapply(x, length_unless_na, integer(1L), USE.NAMES = FALSE)
      },
      .names = "hits_{.col}"
    ), .keep = "none") %>%
    tidyr::unnest(cols = everything())

  # Go to utils.R to see the `list_min_distance_functions` object.
  cols_diff <- df_nested %>%
    dplyr::mutate(dplyr::across(
      .cols = everything(),
      .fns = list_min_distance_functions,
      .names = "diff_{.col}{fun_names}"
    ), .keep = "none") %>%
    dplyr::mutate(dplyr::across(
      .cols = everything(),
      .fns = function(x) {
        x[is.infinite(x)] <- NA
        as.integer(x)
      }
    )) %>%
    suppressWarnings()

  dc <- class(data)
  rounding <- dc[stringr::str_detect(dc, "^scr_rounding_")]
  rounding <- stringr::str_remove(rounding, "^scr_rounding_")

  fun_test <- dc[stringr::str_detect(dc, "^scr_.*map$")]
  fun_test <- stringr::str_remove(fun_test, "^scr_")
  fun_test <- rlang::eval_bare(rlang::parse_expr(fun_test))

  data_rev <- reverse_map_seq(data)

  if (length(rounding) > 0L) {
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

  data_rev %>%
    dplyr::mutate(consistency, hits_total) %>%
    dplyr::bind_cols(cols_hits, cols_diff) %>%
    add_class("scr_audit_seq")
}




#' @rdname audit-special
#' @export

audit_total_n <- function(data) {

  if (!inherits(data, "scr_map_total_n")) {
    cli::cli_abort(c(
      "Invalid `data` argument.",
      "!" = "It must be the output of a `*_map_total_n()` function, \\
      such as `grim_map_total_n()`."
    ))
  }

  df_list <- split(data, data$case)

  df_list_hits <- df_list %>%
    purrr::map(dplyr::filter, both_consistent)

  map_nrow_half <- function(x) {
    vapply(x, nrow, integer(1L), USE.NAMES = FALSE) / 2L
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

  data %>%
    reverse_map_total_n() %>%
    dplyr::mutate(
      hits_total, hits_forth, hits_back, scenarios_total, hit_rate,
      dplyr::across(
        .cols = c("n", starts_with("hits"), "scenarios_total"),
        .fns  = as.integer
      )
    ) %>%
    add_class("scr_audit_total_n")
}

