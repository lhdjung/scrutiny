
# Internal helpers; not exported ------------------------------------------

check_key_args_in_colnames <- function(data, reported) {
  offenders <- reported[!reported %in% colnames(data)]
  if (length(offenders) > 0L) {
    if (length(offenders) == 1L) {
      msg_cols <- "Column"
      msg_it_they <- "It's"
    } else {
      msg_cols <- "Columns"
      msg_it_they <- "They are"
    }
    if (length(offenders) > 1L) {
      msg_each_other <- "each other"
    } else {
      msg_each_other <- ""
    }
    non_offenders <- reported[!reported %in% offenders]
    if (length(non_offenders) > 0L) {
      if (length(offenders) > 1L) {
        msg_each_other <- paste0(msg_each_other, " and with ")
      }
      non_offenders <- wrap_in_backticks(non_offenders)
    }
    offenders <- wrap_in_backticks(offenders)
    cli::cli_abort(c(
      "{msg_cols} {offenders} must be in `data`.",
      "{msg_it_they} meant to be tested for consistency with \\
      {msg_each_other}{non_offenders}."
    ))
  }
}


check_consistency_not_in_colnames <- function(data, name_test) {
  if (any("consistency" == colnames(data))) {
    dc <- class(data)
    class_basic   <- dc[stringr::str_detect(dc, "_map$")]
    class_seq     <- dc[stringr::str_detect(dc, "_map_seq$")]
    class_total_n <- dc[stringr::str_detect(dc, "_map_total_n$")]
    if (length(class_basic) > 0L) {
      fun_name_basic <- stringr::str_remove(class_basic, "^scr_")
    } else {
      fun_name_basic <- NULL
    }
    if (length(class_seq) > 0L) {
      fun_name_basic <- NULL
      fun_name_seq <- stringr::str_remove(class_seq, "^scr_")
    } else {
      fun_name_seq <- NULL
    }
    if (length(class_total_n) > 0L) {
      fun_name_basic <- NULL
      fun_name_total_n <- stringr::str_remove(class_total_n, "^scr_")
    } else {
      fun_name_total_n <- NULL
    }
    non_fun_name_classes <- c("map_seq", "map_total_n")
    fun_name_all <- c(fun_name_basic, fun_name_seq, fun_name_total_n)
    fun_name_all <- fun_name_all[!fun_name_all %in% non_fun_name_classes]
    fun_name_all <- fun_name_all[length(fun_name_all) > 0L]
    if (length(fun_name_all) == 0L) {
      fun_name_all <- ""
    }
    if (stringr::str_detect(fun_name_all, "_seq$")) {
      msg_special <- "sequence "
    } else if (stringr::str_detect(fun_name_all, "_total_n$")) {
      msg_special <- "total-n "
    } else {
      msg_special <- ""
    }
    if (length(fun_name_all) > 0L) {
      msg_fun_name <- paste0(", `", fun_name_all, "()`,")
    } else {
      msg_fun_name <- ""
    }
    cli::cli_abort(c(
      "`data` already includes a \"consistency\" column.",
      "x" = "This shouldn't be the case before {name_test}-testing.",
      "i" = "Did you use the output of a consistency test \\
      {msg_special}mapper function for {name_test}{msg_fun_name} \\
      as an input here?"
    ))
  }
}



#' Check that a mapper's input has correct column names
#'
#' @description When called within a consistency test mapper function,
#'   `check_mapper_input_colnames()` makes sure that the input data frame has
#'   correct column names:

#'  - They include all the key columns corresponding to the test applied by the
#'  mapper.
#'  - They don't already include `"consistency"`.
#'
#'   If either check fails, the function throws an informative error.

#' @param data Data frame. Input to the mapper function.
#' @param reported String vector of the "key" column names that `data` must
#'   have, such as `c("x", "n")` for `grim_map()`.
#' @param name_test String (length 1). Short, plain-text name of the consistency
#'   test that the mapper function applies, such as `"GRIM"`.
#'
#' @include utils.R
#'
#' @export
#'
#' @return No return value. Might throw an error.
#'
#' @seealso `vignette("consistency-tests-in-depth")`, for context and the "key
#'   columns" terminology.


check_mapper_input_colnames <- function(data, reported, name_test) {
  check_key_args_in_colnames(data, reported)
  check_consistency_not_in_colnames(data, name_test)
}



#' Alert user if more specific `audit_*()` summaries are available
#'
#' @description (Note: Ignore this function if your `audit()` method calls
#'   `audit_cols_minimal()`.)
#'
#'   Call `check_audit_special()` within an `audit()` method for a consistency
#'   test mapper function, such as `audit.scr_grim_map()`. It checks if the
#'   input data frame was the product of a function produced by
#'   `function_map_seq()` or `function_map_total_n()`.
#'
#'   If so, the function issues a gentle alert to the user that points to
#'   `audit_seq()` or `audit_total_n()`, respectively.
#'
#' @param data The `audit()` method's input data frame.
#' @param name_test String (length 1). Short, plain-text name of the consistency
#'   test, such as `"GRIM"`.
#'
#' @seealso `vignette("consistency-tests-in-depth")`, for context.
#'
#' @export
#'
#' @return No return value. Might print an alert.


check_audit_special <- function(data, name_test) {

  class_name_root <- paste0("scr_", tolower(name_test), "_map_")

  class_seq     <- paste0(class_name_root, "seq")
  class_total_n <- paste0(class_name_root, "total_n")

  # If `data` is the output of a function like `grim_map_seq()`, point the user
  # to the dedicated summary function for such output, `audit_seq()`:
  if (inherits(data, class_seq)) {
    cli::cli_alert_info(
      "More specialized {name_test} summaries available with `audit_seq()`."
    )
  }

  # Likewise, if `data` is the output of a function like `grim_map_total_n()`,
  # point the user to `audit_total_n()`:
  if (inherits(data, class_total_n)) {
    cli::cli_alert_info(
      "More specialized {name_test} summaries available with `audit_total_n()`."
    )
  }

}



#' Helper column operations
#'
#' @description If your consistency test mapper function supports helper
#'   columns, call `manage_helper_col()` internally; once for every such column.
#'   It will check whether a helper column is compatible with its eponymous
#'   argument, i.e., if the argument was not specified by the user but has its
#'   default value.
#'
#'   By default (`affix = TRUE`), the function will add the column to the
#'   mapper's input data frame. It returns the input data frame, so reassign its
#'   output to that variable.
#'
#'   All of this only works in mapper functions that were "handwritten" using
#'   `function()`, as opposed to those produced by `function_map()`. See
#'   `vignette("consistency-tests-in-depth")`, section *Writing mappers
#'   manually*.
#'
#' @param data The data frame that is the mapper function's first argument.
#' @param var_arg The argument to the mapper function that has the same name as
#'   the helper column you want to manage.
#' @param default The default for the argument that was specified in `var_arg`.
#' @param affix Logical (length 1). If `data` doesn't include the helper column
#'   already, should `var_arg` be added to `data`, bearing its proper name?
#'   Default is `TRUE`.
#'
#' @return `data`, possibly modified (see `affix` argument).
#'
#' @export


manage_helper_col <- function(data, var_arg, default, affix = TRUE) {

  # Retrieve the variable's name:
  var_name <- deparse(substitute(var_arg))

  # Throw error if the argument in question was specified in a way that
  # contradicts `data`:
  if (any(var_name == colnames(data))) {
    # Determine whether or not the argument in question was specified by the
    # user; i.e., whether it's different from the default. For numeric values,
    # strict equality as tested by `identical()` would be asking too much, so
    # `all(dplyr::near())` is used instead. This works via a helper from
    # utils.R, which is also the source of all the other helpers here:
    if (!about_equal(var_arg, default)) {
      data_name <- deparse(substitute(data))
      data_name <- wrap_in_backticks(data_name)
      var_name_as_arg <- wrap_in_backticks(var_name)
      var_name <- wrap_in_quotes(var_name)
      var_arg  <- wrap_in_quotes_or_backticks(var_arg)
      fun_name <- name_caller_call(n = 2L)
      default  <- wrap_in_quotes_or_backticks(default)
      cli::cli_abort(c(
        "Column {var_name} already in {data_name}.",
        "x" = "The {var_name_as_arg} argument in {fun_name} \\
        was specified as {var_arg} (default: {default}).",
        "x" = "This conflicts with the {var_name} column in {data_name}."
      ))
    }
  } else if (affix) {
    # If a column by that name is not yet present in `data`, supply it from the
    # respective argument:
    return(dplyr::mutate(data, {{ var_name }} := var_arg))
  }

  data
}





#' Enable name-independent key column identification
#'
#' @description A handwritten mapper function for consistency tests, such as
#'   `grim_map()`, may include arguments named after the key columns in its
#'   input data frame. When such an argument is specified by the user as a
#'   column name of the input data frame, it identifies a differently-named
#'   column as that key column.
#'
#'   Create such functionality in three steps:
#'
#'   1. Add arguments to your mapper function named after the respective key
#'   columns. They should be `NULL` by default; e.g., `x = NULL, n = NULL`.
#'
#'   2. Within the mapper, capture the user input by quoting it using
#'   `rlang::enexpr()`. Reassign these values to the argument variables; e.g.,
#'   `x <- rlang::enexpr(x)` and `n <- rlang::enexpr(n)`.

#'   3. For every such argument, call `manage_key_colnames()` and reassign its
#'   value to the input data frame variable, adding a short description;
#'   e.g.,`data <- manage_key_colnames(data, x, "mean/proportion")` and `data <-
#'   manage_key_colnames(data, n, "sample size")`.
#'
#' @param data The mapper function's input data frame.
#' @param arg Symbol. The quoted input variable, captured by `rlang::enexpr()`.
#' @param description String (length 1). Short description of the column in
#'   question, to be inserted into an error message.
#'
#' @return The input data frame, `data`, possibly modified.
#'
#' @seealso `vignette("consistency-tests-in-depth")`, for context.
#'
#' @export


manage_key_colnames <- function(data, arg, description = NULL) {
  arg_name <- deparse(substitute(arg))
  if (!is.null(arg)) {
    # data <- dplyr::rename(data, "scr_temp_placeholder" := arg)  # {{ arg_name }} := arg
    data <- dplyr::rename(data, {{ arg_name }} := all_of(arg))
  } else if (!any(arg_name == colnames(data))) {
    if (is.null(description)) {
      msg_this_col <- "One"
    } else {
      msg_this_col <- paste("The ", description)
    }
    cli::cli_abort(c(
      "Column `{arg_name}` missing.",
      "i" = "{msg_this_col} column in `data` must be named \\
      `{arg_name}`. Alternatively, specify the `{arg_name}` argument \\
      as the name of that column."
    ))
  }
  data
}



manage_key_colnames_list_el <- function(data, key_arg) {
  if (is.null(key_arg)) {
    data
  } else {
    dplyr::rename(data, {{ names(key_arg) }} := key_arg)
  }
}




#' Unnest a test result column
#'
#' @description Within a consistency test mapper function, it may become
#'   necessary to unpack a column resulting from a basic `*_scalar()` testing
#'   function. That will be the case if a `show_*` argument of the mapper
#'   function like `show_rec` in `grim_map()` is `TRUE`, and the `*_scalar()`
#'   function returns a list of values, not just a single value.
#'
#'   At the point where such as list is stored in a data frame column (most
#'   likely `"consistency"`), call `unnest_consistency_cols()` to unnest the
#'   results into multiple columns.
#'
#' @param results Data frame containing a list-column by the name passed to
#'   `col`.
#' @param col_names String vector of new names for the unnested columns. It
#'   should start with the same string that was given for `col`.
#' @param index Logical. Should the list-column be indexed into? Default is
#'   `FALSE`.
#' @param col String (length 1). Name of the list-column within `results` to
#'   operate on. Default is `"consistency"`.
#'
#' @details This function is a custom workaround in place of
#'   `tidyr::unnest_wider()`, mirroring some of the latter's functionality. It
#'   was created because `unnest_wider()` can be too slow for use as a helper
#'   function.
#'
#' @return Data frame. The column names are determined by `col_names`.
#'
#' @seealso `vignette("consistency-tests-in-depth")`, for context.
#'
#' @export


unnest_consistency_cols <- function(results, col_names, index = FALSE,
                                    col = "consistency") {

  # The difference between the two conditions lies only in the
  # `purrr::map_depth()` call:
  if (index) {
    consistency_list <- results[col][[1L]] %>%
      purrr::map_depth(.depth = 2L, .f =  `[`, 1) %>%
      purrr::map(function(x) unlist(x, use.names = FALSE))
  } else {
    consistency_list <- purrr::map(
      results[col][[1L]], function(x) unlist(x, use.names = FALSE)
    )
  }

  consistency_df <- consistency_list %>%
    tibble::as_tibble(.name_repair = "minimal") %>%
    t() %>%
    tibble::as_tibble(.name_repair = function(x) {
      paste0("V", seq_along(col_names))
    }) %>%
    dplyr::mutate("V1" = as.logical(.data$V1))

  colnames(consistency_df) <- col_names

  results %>%
    dplyr::select(- {{ col }}) %>%
    dplyr::bind_cols(consistency_df)
}

