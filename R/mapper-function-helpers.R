# Internal helpers; not exported ------------------------------------------

#' Split a list of per-row results into columns
#'
#' A `*_scalar()` function told to show its reconstructed values returns one
#' list per row. `split_result_cols()` turns a list of such lists into a named
#' list of columns.
#'
#' It unlists each result across rows, not each row across results, so that a
#' logical column stays logical even if a value next to it is a string.
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
    # Name the mapper that most likely produced `data`. The most specific tier
    # wins: `*_map_seq()` output also has the basic `*_map` class. Generic
    # classes such as `"scrutiny_map_seq"` name no function and are dropped:
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
    # Several matches have no single function to name:
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
#' @description `check_mapper_input_colnames()` is called within mappers. It
#'   errors if `data` lacks a key column, or if it already has a column named
#'   like the key result column (`"consistency"` by default).
#'
#' @param data Data frame. Input to the mapper.
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
#' @description The seq and total-n factories read a basic mapper's test
#'   results off the column named by their own `.name_key_result`, so both must
#'   have been created with the same value. Otherwise, the column lookup returns
#'   `NULL` and fails much later with an unhelpful error from base R.
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


# A mapper's output keeps its class when columns are dropped, so functions
# that read its columns check for them rather than trusting the class.
check_cols_needed <- function(data, cols, ...) {
  cols_missing <- setdiff(cols, colnames(data))
  if (length(cols_missing) > 0L) {
    name_fun <- name_last_export()
    abort_in_export(
      "`data` lacks columns that `{name_fun}()` needs.",
      "x" = "Missing: {.code {cols_missing}}.",
      ...
    )
  }
}


#' Manage a helper column
#'
#' @description A helper column is a column of a mapper's input that is not
#'   itself tested but enters the test, such as `items_averaged` in
#'   [`grim_map()`]. It can be given either as a column of `data` or as an
#'   argument of the same name. `manage_helper_col()` checks that the two
#'   don't disagree if both are given and, by default, adds the argument's
#'   value to `data` as a column if `data` has none.
#'
#'   Mappers made by [`function_map()`] call it for every `.cols_helper`
#'   argument. Call it yourself in a handwritten mapper, once per helper
#'   column, and reassign the result to `data`. See
#'   `vignette("consistency-tests-in-depth")`, section *Writing mappers
#'   manually*.
#'
#' @param data The mapper's input data frame.
#' @param var_arg The mapper argument named like the helper column.
#' @param default The default of that argument. Leave it out if the argument
#'   has none, like `items_averaged`: `data` is then returned unchanged if the
#'   argument is missing, and the test function reports that.
#' @param affix Logical. If `data` has no such column, should one be added
#'   from `var_arg`? Default is `TRUE`.
#'
#' @return `data`, possibly with the helper column added.
#'
#' @export

manage_helper_col <- function(data, var_arg, default, affix = TRUE) {
  var_name <- deparse(substitute(var_arg))

  # Without a default, a missing argument is for the test function to report:
  has_default <- !missing(default)
  if (!has_default && missing(var_arg)) {
    return(data)
  }

  if (any(var_name == colnames(data))) {
    # Without a default, the argument was given, as checked above.
    # `about_equal()` because `identical()` is too strict for numbers. An
    # argument that agrees with the column, as when a mapper's output is tested
    # again, is no contradiction, and the column is kept:
    arg_given <- !has_default || !about_equal(var_arg, default)
    # Only a single value or one per row can agree; `rep_len()` would cut
    # off or recycle any other length into agreement:
    col_agrees <- length(var_arg) %in%
      c(1L, nrow(data)) &&
      isTRUE(about_equal(data[[var_name]], rep_len(var_arg, nrow(data))))
    if (arg_given && !col_agrees) {
      data_name <- deparse(substitute(data))
      data_name <- wrap_in_backticks(data_name)
      var_name_as_arg <- wrap_in_backticks(var_name)
      var_name <- wrap_in_quotes(var_name)
      var_arg <- wrap_in_quotes_or_backticks(var_arg)
      fun_name <- name_last_export_code()
      msg_default <- if (has_default) {
        paste0(" (default: ", wrap_in_quotes_or_backticks(default), ")")
      }
      abort_in_export(
        "Column {var_name} already in {data_name}.",
        "x" = "The {var_name_as_arg} argument in {fun_name} \\
        was specified as {var_arg}{msg_default}.",
        "x" = "This conflicts with the {var_name} column in {data_name}."
      )
    }
  } else if (affix) {
    data <- data |>
      dplyr::mutate({{ var_name }} := var_arg)
  }

  data
}


# Study-level GRIM statistics, such as `grim_binomial()` and
# `grim_plot_rates()`, count reported value sets. Sequence and total-n output
# inherits the basic mapper's class, too, but its rows are hypothetical value
# sets, most of them inconsistent by construction, so it is rejected here.
check_grim_map_basic <- function(data) {
  if (inherits(data, "scrutiny_grim_map") && is_map_basic_df(data)) {
    return(invisible(NULL))
  }
  msg_what_it_is <- if (is_map_seq_df(data) || is_map_total_n_df(data)) {
    c(
      "x" = "It is the output of a sequence or total-n mapper, whose rows \\
      are hypothetical value sets, not reported ones."
    )
  } else {
    c("x" = "It has class {.cls {class(data)}}.")
  }
  abort_in_export("`data` must be the output of `grim_map()`.", msg_what_it_is)
}


# `purrr::pmap()` wraps an error in an indexed-error context that names purrr's
# own anonymous function as the call ("In index: 14."). Re-signal the test's
# error itself, naming the row of `data`. The row is also kept as a field, so
# that a total-n mapper can name the row of its own `data` instead. An error
# not made by rlang has no body to add to, so it propagates as it is:
rethrow_row_error <- function(cnd) {
  signal_in_row(cnd$parent, cnd$location)
}

signal_in_row <- function(error, row) {
  if (!inherits(error, "rlang_error")) {
    return()
  }
  error$scrutiny_row <- row
  error$body <- c(error$body, "i" = paste0("In row ", row, " of `data`."))
  rlang::cnd_signal(error)
}

# Does `test_row(row)` run without an error? Its warnings are not muffled, for
# the reason given in `function_map()`:
passes_row <- function(test_row, row) {
  tryCatch(
    {
      test_row(row)
      TRUE
    },
    error = function(cnd) FALSE
  )
}
