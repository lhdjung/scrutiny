# Internal helpers; not exported ------------------------------------------

#' Split a list of per-row results into columns
#'
#' A `*_scalar()` function that has been asked to show its reconstructed values
#' returns one list per row. `split_result_cols()` turns a list of such lists
#' into a named list of columns, ready to be used as tibble columns.
#'
#' Unlike `unnest_consistency_cols()`, this preserves the type of every column:
#' it collects each result across rows before unlisting, so a logical column
#' stays logical even if the row next to it is a string. (The exported function
#' unlists row-wise, which coerces every value in a row to the most general type
#' among them.)
#'
#' @param results List of lists; one inner list per row, all of the same length
#'   as `col_names`.
#' @param col_names String vector of names for the resulting columns.
#'
#' @return Named list of vectors, each as long as `results`.
#'
#' @noRd
split_result_cols <- function(results, col_names) {
  out <- lapply(
    seq_along(col_names),
    function(i) {
      results |>
        lapply(function(x) x[[i]]) |>
        unlist(use.names = FALSE)
    }
  )
  names(out) <- col_names
  out
}


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
    abort_in_export(
      "{msg_cols} {offenders} must be in `data`.",
      "{msg_it_they} meant to be tested for consistency with \\
      {msg_each_other}{non_offenders}."
    )
  }
}


check_consistency_not_in_colnames <- function(
  data,
  name_test,
  name_key_result = "consistency"
) {
  # `function_map()` names the result column `"consistency"` before renaming
  # it, so a column of that name would be lost, too:
  names_taken <- intersect(c(name_key_result, "consistency"), colnames(data))
  if (length(names_taken) > 0L) {
    # If `data` carries a scrutiny mapper class, name the mapper that most
    # likely produced it. The most specific tier wins: a `*_map_seq()` output
    # also inherits the basic `*_map` class, but it was the sequence mapper that
    # the user called. Generic infrastructure classes such as
    # `"scrutiny_map_seq"` carry no function name and are filtered out.
    dc <- class(data)
    class_basic <- dc[stringr::str_detect(dc, "_map$")]
    class_seq <- dc[stringr::str_detect(dc, "_map_seq$")]
    class_total_n <- dc[stringr::str_detect(dc, "_map_total_n$")]
    class_best <- if (length(class_seq) > 0L) {
      class_seq
    } else if (length(class_total_n) > 0L) {
      class_total_n
    } else {
      class_basic
    }
    fun_name_all <- class_best |>
      stringr::str_remove("^scrutiny_") |>
      call_on(\(x) x[!x %in% c("map_seq", "map_total_n")])
    # Guard against multiple matches, which have no single function to name:
    if (length(fun_name_all) != 1L) {
      fun_name_all <- character(0L)
    }
    if (length(fun_name_all) == 1L && grepl("_seq$", fun_name_all)) {
      msg_special <- "sequence "
    } else if (length(fun_name_all) == 1L && grepl("_total_n$", fun_name_all)) {
      msg_special <- "total-n "
    } else {
      msg_special <- ""
    }
    # Without a function name, the message simply omits it. (It used to render
    # an empty name as "`()`".)
    msg_fun_name <- if (length(fun_name_all) == 1L) {
      paste0(", `", fun_name_all, "()`,")
    } else {
      ""
    }
    abort_in_export(
      "`data` already includes a \"{names_taken[1L]}\" column.",
      "x" = "This shouldn't be the case before {name_test}-testing.",
      "i" = "Did you use the output of a consistency test \\
      {msg_special}mapper function for {name_test}{msg_fun_name} \\
      as an input here?"
    )
  }
}


#' Check that a mapper's input has the right column names
#'
#' @description `check_mapper_input_colnames()` is called within consistency
#'   test mapper functions. It errors if `data` lacks a key column, or if it
#'   already has a column named like the result column (`"consistency"` by
#'   default).
#'
#' @param data Data frame. Input to the mapper function.
#' @param reported String. Names of the key columns that `data` must have,
#'   such as `c("x", "n")` for `grim_map()`.
#' @param name_test String (length 1). Plain-text name of the test, such as
#'   `"GRIM"`, for the error message.
#' @param name_key_result String (length 1). Name of the result column that
#'   the mapper adds. Default is `"consistency"`.
#'
#' @include utils.R
#'
#' @export
#'
#' @return No return value; might throw an error.
#'
#' @seealso `vignette("consistency-tests-in-depth")`.

check_mapper_input_colnames <- function(
  data,
  reported,
  name_test,
  name_key_result = "consistency"
) {
  check_key_args_in_colnames(data, reported)
  check_consistency_not_in_colnames(data, name_test, name_key_result)
}


#' Check that a mapper returned the key result column that was expected
#'
#' @description The seq and total-n factories build on a basic mapper's output,
#'   and they read its test results off a column they know only by name. That
#'   name is each factory's `.name_key_result`, so the two have to agree: a
#'   mapper created with `.name_key_result = "verdict"` must be wrapped by a
#'   sequence mapper created the same way.
#'
#'   When they disagree, the column lookup returns `NULL` and fails much later
#'   with an error from base R that says nothing about the cause. This check
#'   turns that into a message naming both the column that is missing and the
#'   argument that decides it.
#'
#' @param data Data frame returned by the basic mapper, `fun()`.
#' @param name_key_result String (length 1). The expected column name.
#' @param name_fun String (length 1). Name of the basic mapper, for the message.
#'
#' @return No return value. Might throw an error.
#'
#' @noRd
check_key_result_col <- function(data, name_key_result, name_fun) {
  if (!any(name_key_result == colnames(data))) {
    abort_in_export(
      "`{name_fun}()` did not return a \"{name_key_result}\" column.",
      "x" = "Its results columns are {wrap_in_backticks(colnames(data))}.",
      "i" = "The two functions must have been created with the same \\
      `.name_key_result` value."
    )
  }
}


#' Alert the user if more specific `audit_*()` summaries are available
#'
#' @description Call `check_audit_special()` within an `audit()` method for a
#'   consistency test mapper, unless the method calls [`audit_cols_minimal()`],
#'   which does so itself. If the input was produced by a function made by
#'   [`function_map_seq()`] or [`function_map_total_n()`], it prints a message
#'   pointing to [`audit_seq()`] or [`audit_total_n()`].
#'
#' @param data The `audit()` method's input data frame.
#' @param name_test String (length 1). Plain-text name of the test, such as
#'   `"GRIM"`.
#'
#' @seealso `vignette("consistency-tests-in-depth")`.
#'
#' @export
#'
#' @return No return value; might print a message.

check_audit_special <- function(data, name_test) {
  class_name_root <- paste0("scrutiny_", tolower(name_test), "_map_")

  class_seq <- paste0(class_name_root, "seq")
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


#' Manage a helper column
#'
#' @description A helper column is a column of a mapper's input that is not
#'   itself tested but enters the test, such as `items` in [`grim_map()`]. It
#'   can be given either as a column of `data` or as an argument of the same
#'   name. `manage_helper_col()` checks that the two are not both given, and
#'   by default (`affix = TRUE`) adds the argument's value to `data` as a
#'   column if `data` has none.
#'
#'   Mappers made by [`function_map()`] call it for every `.cols_helper`
#'   argument. Call it yourself in a handwritten mapper, once per helper
#'   column, and reassign the result to `data`. See
#'   `vignette("consistency-tests-in-depth")`, section *Writing mappers
#'   manually*.
#'
#' @param data The mapper's input data frame.
#' @param var_arg The mapper argument named like the helper column.
#' @param default The default of that argument.
#' @param affix Logical. If `data` has no such column, should one be added
#'   from `var_arg`? Default is `TRUE`.
#'
#' @return `data`, possibly with the helper column added.
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
      var_arg <- wrap_in_quotes_or_backticks(var_arg)
      fun_name <- name_last_export_code()
      default <- wrap_in_quotes_or_backticks(default)
      abort_in_export(
        "Column {var_name} already in {data_name}.",
        "x" = "The {var_name_as_arg} argument in {fun_name} \\
        was specified as {var_arg} (default: {default}).",
        "x" = "This conflicts with the {var_name} column in {data_name}."
      )
    }
  } else if (affix) {
    # If a column by that name is not yet present in `data`, supply it from the
    # respective argument:
    data <- data |>
      dplyr::mutate({{ var_name }} := var_arg)
  }

  data
}


#' Rename a key column in a handwritten mapper
#'
#' @description A mapper made by [`function_map()`] lets the user pass the name
#'   of a column of `data` through an argument named like the key column, as in
#'   `grim_map(df, x = mean)`. `manage_key_colnames()` provides this for a
#'   handwritten mapper. Three steps:
#'
#'   1. Add an argument per key column, with default `NULL`; e.g., `x = NULL,
#'   n = NULL`.
#'
#'   2. Capture each with `rlang::enexpr()`.
#'
#'   3. If it was specified, call `manage_key_colnames()` and reassign the
#'   result to `data`:
#'
#' \preformatted{
#'   if (!missing(x)) {
#'     x <- rlang::enexpr(x)
#'     data <- manage_key_colnames(data, x, "mean/proportion")
#'   }
#' }
#'
#' @param data The mapper's input data frame.
#' @param arg Symbol. The argument, captured by `rlang::enexpr()`.
#' @param description String (length 1). Short description of the column, for
#'   the error message.
#'
#' @return `data`, possibly with a column renamed.
#'
#' @seealso `vignette("consistency-tests-in-depth")`.
#'
#' @export

manage_key_colnames <- function(data, arg, description = NULL) {
  arg_name <- deparse(substitute(arg))

  if (is.null(arg)) {
    # Throw an error if the argument name is not found among the column names
    if (!(arg_name %in% colnames(data))) {
      msg_this_col <- if (is.null(description)) {
        "One"
      } else {
        paste("The ", description)
      }
      abort_in_export(
        "Column `{arg_name}` missing.",
        "i" = "{msg_this_col} column in `data` must be named \\
      `{arg_name}`. Alternatively, specify the `{arg_name}` argument \\
      as the name of that column."
      )
    }
    # Otherwise, there are no columns to rename
    return(data)
  }

  # Splice `arg_name`, the target new column name, into dplyr's renaming syntax
  data |>
    dplyr::rename({{ arg_name }} := all_of(arg))
}


#' Unnest a test result column
#'
#' @description If a `*_scalar()` function returns a list of values rather
#'   than a single one, as `grim_scalar()` does with `show_rec = TRUE`, a
#'   handwritten mapper ends up with a list-column. `unnest_consistency_cols()`
#'   splits it into one column per value.
#'
#' @param results Data frame with a list-column named `col`.
#' @param col_names String. Names of the new columns. The first should be
#'   `col`.
#' @param index Logical. Should the list-column be indexed into? Default is
#'   `FALSE`.
#' @param col String (length 1). Name of the list-column. Default is
#'   `"consistency"`.
#'
#' @details This is a faster, narrower replacement for
#'   [`tidyr::unnest_wider()`].
#'
#' @return Data frame with the columns named in `col_names`.
#'
#' @seealso `vignette("consistency-tests-in-depth")`.
#'
#' @export

unnest_consistency_cols <- function(
  results,
  col_names,
  index = FALSE,
  col = "consistency"
) {
  # The difference between the two conditions lies only in the
  # `purrr::map_depth()` call:
  if (index) {
    consistency_list <- results[col][[1L]] |>
      purrr::map_depth(.depth = 2L, .f = `[`, 1) |>
      purrr::map(function(x) unlist(x, use.names = FALSE))
  } else {
    consistency_list <- results[col][[1L]] |>
      purrr::map(function(x) unlist(x, use.names = FALSE))
  }

  consistency_df <- consistency_list |>
    tibble::as_tibble(.name_repair = "minimal") |>
    t() |>
    tibble::as_tibble(.name_repair = function(x) {
      paste0("V", seq_along(col_names))
    }) |>
    dplyr::mutate("V1" = as.logical(.data$V1))

  colnames(consistency_df) <- col_names

  results |>
    dplyr::select(-{{ col }}) |>
    dplyr::bind_cols(consistency_df)
}
