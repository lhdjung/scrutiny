
# Two internal helper functions to check the validity of arguments supplied to
# the factory-made function. The first one concerns the input data frame in
# conjunction with the expressions provided to identify the "key" columns in
# `data` and makes sure that no values other than these column names have been
# provided. The second one checks that all key columns have been identified.

check_factory_key_args_values <- function(data, key_cols_call) {

  offenders <- key_cols_call[!key_cols_call %in% colnames(data)]

  # Error condition -- one or more key arguments have been specified with values
  # that are not actually column names of `data`:
  if (length(offenders) > 0) {
    offenders_names <- glue::as_glue(names(offenders))
    offenders_names <- wrap_in_backticks(offenders_names)
    offenders <- wrap_in_backticks(offenders)
    name_current_fn <- name_caller_call(n = 2)
    if (length(offenders) == 1) {
      msg_is_colname <- "is not a column name"
    } else {
      msg_is_colname <- "are not column names"
    }

    # Prepare an error message. It might be subsequently appended...
    msg_error <- c(
      "!" = "{offenders} {msg_is_colname} of `data`.",
      "x" = "The {offenders_names[1]} argument of \\
      {name_current_fn} was specified as {offenders[[1]]}, \\
      but there is no column in `data` called {offenders[[1]]}."
    )

    # ... to point out that more than one supplied value is flawed:
    if (length(offenders) > 1) {
      if (length(offenders) == 2) {
        msg_arg_s <- "argument"
        msg_a <- "a "
        msg_col <- "column"
      } else {
        msg_arg_s <- "arguments"
        msg_a <- ""
        msg_col <- "columns"
      }
      msg_error <- append(
        msg_error, c(
          "x" = "Same with the {offenders_names[-1]} {msg_arg_s}: \\
          `data` doesn't contain {msg_a}{offenders[-1]} {msg_col}."
        )
      )
    }

    # Throw the actual error:
    cli::cli_abort(msg_error)
  }

}



check_factory_key_args_names <- function(key_cols_missing,
                                         key_cols_call_names) {

  offenders <- key_cols_missing
  offenders <- offenders[!offenders %in% key_cols_call_names]

  # Error condition -- not all of the `reported` values that are not column
  # names of `data` have been supplied as values of the respective arguments:
  if (length(offenders) > 0) {
    offenders <- wrap_in_backticks(offenders)

    # Get the name of the current (i.e., factory-made) function using a helper
    # from the utils.R file that wraps `rlang::caller_call()`:
    msg_fn_name <- name_caller_call(n = 2)

    # Because either one or more arguments (or column names) may be missing, the
    # wording of the error message may be either singular or plural:
    if (length(offenders) == 1) {
      msg_missing <- "Column {offenders} is"
      msg_is_are <- "is"
      msg_needs_to_be <- "It should be a column"
      msg_names <- "the name of the equivalent column"
      msg_column_s <- "Column"
      msg_argument <- "argument"
    } else {
      msg_missing <- "Columns {offenders} are"
      msg_is_are <- "are"
      msg_needs_to_be <- "They should be columns"
      msg_names <- "the names of the equivalent columns"
      msg_it_them <- "them"
      msg_column_s <- "Columns"
      msg_argument <- "arguments"
    }

    # Throw the error:
    cli::cli_abort(c(
      "{msg_column_s} {offenders} {msg_is_are} \\
          missing from `data`.",
      "x" = "{msg_needs_to_be} of the input data frame.",
      "i" = "Alternatively, specify the {offenders} \\
          {msg_argument} of {msg_fn_name} as {msg_names}."
    ))
  }

}


# # Unclear if needed:
# check_factory_call_args <- function(key_args_call)


# Check that no argument specified via the dots, `...`, was misspelled. This
# function requires the following line in the prologue (i.e., the part of the
# function factory before the first version of the factory-made function is
# created): `fun_name <- deparse(substitute(.fun))`
check_factory_dots <- function(fun, fun_name_scalar, ...) {
  dots <- rlang::enexprs(...)
  dots_names <- names(dots)
  offenders <- dots_names[!dots_names %in% names(formals(fun))]
  if (length(offenders) > 0) {
    fun_name_mapper <- name_caller_call(n = 2)
    offenders <- wrap_in_backticks(offenders)
    if (length(offenders) == 1) {
      msg_arg <- "argument"
      msg_it_they <- "It's not an"
    } else {
      msg_arg <- "arguments"
      msg_it_they <- "They are not"
    }
    cli::cli_abort(c(
      "Invalid {msg_arg} {offenders}.",
      "x" = "{msg_it_they} {msg_arg} of {fun_name_mapper} \\
      or `{fun_name_scalar}()`."
    ))
  }
}




# Helper for the epilogue of function factories (i.e., the part after the first
# version of the factory-made function is created). Insert parameters named
# after the key columns into `fun()`, with `NULL` as the default for each.
# The key columns need to be present in the input data frame. They are expected
# to have the names specified in `.reported`. If they don't, however, the user
# can simply specify the key column arguments as the non-quoted names of the
# columns meant to fulfill these roles:
insert_key_args <- function(fun, reported, insert_after = 1) {
  key_args <- list(NULL)
  key_args <- rep(key_args, times = length(reported))
  names(key_args) <- reported
  formals(fun) <- append(formals(fun), key_args, after = insert_after)
  return(fun)
}




# If `insert_key_args()` is used in the epilogue of a function factory (i.e.,
# after the part that produces the factory-made function), `absorb_key_args()`
# needs to be used in the main part. Unlike the former, it transforms `data`,
# not `fun`, and should be reassigned to `data`. Rename key columns that have
# non-standard names, following user-supplied directions via the arguments
# automatically inserted below the function.
absorb_key_args <- function(data, reported, key_cols_call) {

  key_cols_missing <- reported[!reported %in% colnames(data)]
  key_cols_missing <- as.character(key_cols_missing)

  # No need to work with key arguments here if `data` has all of the expected
  # column names:
  if (length(key_cols_missing) == 0) {
    return(data)
  }

  names(key_cols_missing) <- key_cols_missing

  # Extract the expressions supplied by the factory-made function's user as
  # values of the arguments that are named after `reported`. Coerce them to
  # string because they will be needed as column names:
  key_cols_call <- as.list(rlang::caller_call())
  key_cols_call <- key_cols_call[names(key_cols_call) %in% key_cols_missing]
  key_cols_call_names <- names(key_cols_call)
  key_cols_call <- as.character(key_cols_call)
  names(key_cols_call) <- key_cols_call_names

  # Run specialized checks on the code supplied by the factory-made
  # function's user to the subsequently inserted key argument parameters:
  check_factory_key_args_values(data, key_cols_call)
  check_factory_key_args_names(key_cols_missing, key_cols_call_names)

  df_colnames <- tibble::tibble(
    data = list(data),
    name_missing = names(key_cols_missing),
    name_call = key_cols_call
  )

  replace_colname <- function(data, name_missing, name_call) {
    colnames(data)[colnames(data) == name_call] <- name_missing
    data[name_missing]
  }

  data_renamed <- purrr::pmap_dfc(df_colnames, replace_colname)
  data_not_renamed <- data[!colnames(data) %in% key_cols_call]

  data <- dplyr::bind_cols(data_renamed, data_not_renamed)
  return(data)
}


