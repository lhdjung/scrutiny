
#' Check that key columns perfectly map onto their identifiers
#'
#' Two helpers only called within `absorb_key_args()` to check the validity of
#' arguments supplied to the factory-made function:
#'
#' - `check_factory_key_args_values()` concerns the input data frame in
#'   conjunction with the expressions provided to identify the "key" columns in
#'   `data` and makes sure that no values other than these column names have
#'   been provided.
#' - `check_factory_key_args_names()` checks that all key columns have been
#'   identified.
#'
#' @param data Data frame passed to the factory-made function.
#' @param key_cols_call User-provided arguments named after one or more key
#'   columns.
#' @param key_cols_missing,key_cols_call_names String. Vectors with names of the
#'   key columns that are missing in `data` or that were provided by the user as
#'   arguments, respectively.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_factory_key_args_values <- function(data, key_cols_call) {

  offenders <- key_cols_call[!key_cols_call %in% colnames(data)]

  # Error condition -- one or more key arguments have been specified with values
  # that are not actually column names of `data`:
  if (length(offenders) > 0L) {
    offenders_names <- glue::as_glue(names(offenders))
    offenders_names <- wrap_in_backticks(offenders_names)
    offenders <- wrap_in_backticks(offenders)
    name_current_fn <- name_caller_call(n = 2L)
    if (length(offenders) == 1L) {
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
    if (length(offenders) > 1L) {
      if (length(offenders) == 2L) {
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
  if (length(offenders) > 0L) {
    offenders <- wrap_in_backticks(offenders)

    # Get the name of the current (i.e., factory-made) function using a helper
    # from the utils.R file that wraps `rlang::caller_call()`:
    msg_fn_name <- name_caller_call(n = 2L)

    # Because either one or more arguments (or column names) may be missing, the
    # wording of the error message may be either singular or plural:
    if (length(offenders) == 1L) {
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



#' Check that no dots-argument is misspelled
#'
#' `check_factory_dots()` is called within each main function factory:
#' `function_map()`, `function_map_seq()`, and `function_map_total_n()`.
#'
#' For the `fun_name_scalar` argument, the function requires the following line
#' in the entry area (i.e., the part of the function factory before the first
#' version of the factory-made function is created):
#' `fun_name <- deparse(substitute(.fun))`
#'
#' @param fun Function applied by the function factory.
#' @param fun_name_scalar String (length 1). Name of `fun`.
#' @param ... Arguments passed by the factory-made function's user to `fun`.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_factory_dots <- function(fun, fun_name_scalar, ...) {
  dots <- rlang::enexprs(...)
  dots_names <- names(dots)
  offenders <- dots_names[!dots_names %in% names(formals(fun))]
  if (length(offenders) > 0L) {
    fun_name_mapper <- name_caller_call(n = 2L)
    offenders <- wrap_in_backticks(offenders)
    if (length(offenders) == 1L) {
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



#' Get an `arg_list` object
#'
#' That is, a named list of arguments passed by the user who called function
#' within which `call_arg_list()` was called.
#'
#' @return Named list.
#'
#' @noRd
call_arg_list <- function() {
  out <- as.list(rlang::caller_call())
  out[-(1:2)]
}



#' Insert key arguments into the factory-made function
#'
#' `insert_key_args()` extends the list of the factory-made function's
#' parameters (i.e., its formal arguments) by the key arguments corresponding to
#' the particular consistency test which the factory-made function will apply.
#'
#' It must be used in concert with `absorb_key_args()`.
#'
#' @details The function is called in the exit area of function factories (i.e.,
#'   the part after the first version of the factory-made function is created).
#'   It inserts parameters named after the key columns into `fun`, with `NULL`
#'   as the default for each.
#'
#'   The key columns need to be present in the input data frame. They are
#'   expected to have the names specified in `.reported`. If they don't,
#'   however, the user can simply specify the key column arguments as the
#'   non-quoted names of the columns meant to fulfill these roles.
#'
#'   In its current form, the function is very hard to read, but this is for
#'   performance only. An equivalent but better-readable version is outcommented
#'   below the function.
#'
#' @param fun Factory-made function.
#' @param reported String. Names of the key arguments to be inserted into `fun`.
#' @param insert_after Integer. Index of the existing formal argument of `fun`
#'   after which the key arguments will be inserted. Default is `1L`. For
#'   convention's sake, this should hardly be changed.
#'
#' @return Function `fun` with new arguments, named after `reported`, with
#'   `NULL` as the default for each.
#'
#' @noRd
insert_key_args <- function(fun, reported, insert_after = 1L) {
  `formals<-`(fun, value = append(
    formals(fun),
    `names<-`(rep(list(NULL), times = length(reported)), value = reported),
    after = insert_after
  ))
}

# # Better readable version:
# insert_key_args <- function(fun, reported, insert_after = 1L) {
#   key_args <- rep(list(NULL), times = length(reported))
#   names(key_args) <- reported
#   formals(fun) <- append(formals(fun), key_args, after = insert_after)
#   fun
# }



#' Absorb key arguments from the user's call
#'
#' If `insert_key_args()` is called in the exit area of a function factory
#' (i.e., after the part that produces the factory-made function),
#' `absorb_key_args()` must be called in the main part. Unlike the former, it
#' transforms `data`, not `fun`, and should be reassigned to `data`.
#'
#' It renames key columns that have non-standard names, following user-supplied
#' directions via the arguments automatically inserted below the function.
#'
#' @param data User-supplied data frame.
#' @param reported String. Names of the key arguments.
#' @param key_cols_call User-provided arguments named after one or more key
#'   columns.
#'
#' @return Data frame `data`, possibly with one or more columns renamed.
#'   Remember reassigning the value to `data`!
#'
#' @noRd
absorb_key_args <- function(data, reported, key_cols_call) {

  key_cols_missing <- reported[!reported %in% colnames(data)]
  key_cols_missing <- as.character(key_cols_missing)

  # No need to work with key arguments here if `data` has all of the expected
  # column names:
  if (length(key_cols_missing) == 0L) {
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



#' Absorb other arguments (not used)
#'
#' `absorb_other_args()` is intended to work like `absorb_key_args()`, but for
#' any other arguments, and it must be reassigned to `fun`, not to `data`.
#'
#' The function is not currently used because I'm now very skeptical of its
#' utility and even its reliability. It uses on a style of function manipulation
#' that might just be too intricate to be feasible in practice.
#'
#' I originally attempted for `absorb_other_args()` to replace the dots on one
#' level, thereby enabling their use on another level, but this solution was
#' somewhat overengineered.
#'
#' @param fun Function to be applied.
#' @param reported String. Names of the key arguments.
#'
#' @return Function `fun` with modified arguments.
#'
#' @noRd
absorb_other_args <- function(fun, reported) {

  args_excluded <- c("data", "...", reported)

  arg_list <- formals(fun)
  arg_list <- arg_list[!(names(arg_list)) %in% args_excluded]

  # Capture any arguments specified by the user of the factory-made function:
  call_args <- as.list(rlang::caller_call())
  call_args <- call_args[names(call_args) != ""]

  # Prioritize mapper function defaults before scalar defaults...
  args_default <- formals(fun)[names(formals(fun)) %in% names(arg_list)]
  args_default <- names(args_default)

  formals(fun)[args_default] <- arg_list[args_default]

  # ... and user-specified arguments before mapper defaults:
  formals(fun)[names(formals(fun)) %in% names(call_args)] <-
    call_args[names(call_args) %in% names(formals(fun))]

  return(fun)
}



#' Check that disabled arguments are not specified
#'
#' If the user of the function factory specified its `.args_disabled` argument,
#' `check_args_disabled()` enforces this ban.
#'
#' More precisely, it throws an error if the user of the factory-made function
#' specified one or more arguments which the user of the function factory had
#' disabled via the latter's `.args_disabled` argument. The arguments would
#' otherwise be passed on to the function that is mapped within the factory-made
#' function.
#'
#' on the use of the certain arguments of the function applied by the
#' factory-made function.
#'
#' @param args_disabled String. One or more names of arguments of the function
#'   applied within the factory-made function.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_args_disabled <- function(args_disabled) {

  # Enforce argument disabling via `args_disabled`:
  if (!is.null(args_disabled)) {
    arg_names_caller_call <- rlang::frame_call(frame = parent.frame())
    arg_names_caller_call <- names(arg_names_caller_call)
    offenders <- args_disabled[args_disabled %in% arg_names_caller_call]
    if (length(offenders) > 0L) {
      fun_name <- name_caller_call(n = 2L)
      fun_name_bare <- name_caller_call(wrap = FALSE)
      fun_name_bare <- as.character(fun_name_bare)
      package_name <- utils::getAnywhere(fun_name_bare)$where
      package_name <- as.character(package_name[1])
      package_name <- sub("package:", "", package_name)
      if (length(offenders) > 3L) {
        offenders <- offenders[1:3]
        msg_among_others <- ", among others"
      } else {
        msg_among_others <- ""
      }
      if (length(offenders) > 1L) {
        msg_arg_s <- "Arguments"
        msg_is_are <- "are"
      } else {
        msg_arg_s <- "Argument"
        msg_is_are <- "is"
      }
      offenders <- wrap_in_backticks(offenders)
      cli::cli_abort(c(
        "{msg_arg_s} {offenders} {msg_is_are} \\
          disabled in {fun_name}{msg_among_others}.",
        "i" = "This is by design. When {fun_name} was created \\
          within {package_name} using `scrutiny::function_map()`, \\
          this function factory's `.args_disabled` argument was \\
          specified so as to include {offenders}.",
        "i" = "The purpose is to prevent hidden errors that \\
          might otherwise arise due to certain arguments not \\
          working properly within `scrutiny::function_map()`."
      ))
    }
    rm(arg_names_caller_call, offenders)
  }

}



#' Check that the vector of disabled arguments is unnamed
#'
#' `check_args_disabled_unnamed()` is a companion to `check_args_disabled()`. It
#' must be called in a function factory's entry area. The function will throw an
#' error if the factory's (!) user provided a named vector, rather than simply
#' the names of the arguments to be disabled.
#'
#' @param args_disabled The factory's `.args_disabled` argument, specified by
#'   the factory's user.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_args_disabled_unnamed <- function(args_disabled) {
  if (!is.null(names(args_disabled))) {
    name <- deparse(substitute(args_disabled))
    name_names <- wrap_in_backticks(names(args_disabled))
    name_fun <- name_caller_call(n = 2L)
    if (length(name_names) == 1L) {
      msg_names <- "this name"
    } else {
      msg_names <- "these names"
    }
    cli::cli_abort(c(
      "In {name_fun}, the `{name}` argument must be \\
      an unnamed string vector.",
      "x" = "It has {msg_names}: {name_names}."
    ))
  }
}



#' Inheritance tests
#'
#' These functions are used within `is_map_df()` and friends:
#' - `class_with()` returns the longest (or all) of an object's classes that
#' contain at least one user-specified substring.
#' - `inherits_class_with()` wraps `class_with()` and tests whether its output
#' is length 1 or more -- i.e., whether an object inherits at least 1 class with
#' one of the specified substrings. This is conceptually similar to
#' `inherits()`, but more flexible.
#'
#' @details `class_with()` loops through a string vector, `contains`, and tests
#'   whether any of its values are present within any of those of another string
#'   vector, the classes of `data`. Classes are ordered by number of characters
#'   before the looping, such that the "longest" classes come first by default
#'   (`order_decreasing = TRUE`).
#'
#'   The first class that fits the first `contains` value is returned. If the
#'   first `contains` value doesn't fit any of the classes, the second one is
#'   tested, etc.
#'
#'   All of this makes sense because scrutiny's classes differ in complexity
#'   (e.g., `"scr_grim_map_seq"` versus `"scr_map_seq"`), and the purpose here
#'   is to find the most complex class with the earliest in line of the
#'   `contains` strings. The latter should be ordered in descending order of
#'   desirability, so that the most "desired" string comes first.
#'
#' @param data Object to be tested for classes (most likely a data frame).
#' @param contains String. The presence of `contains` values in the classes of
#'   `data` is tested via `stringr::str_detect()`. Note that the order among
#'   `contains` values matters.
#' @param all_classes Boolean. If set to `TRUE`, returns all classes with a
#'   length-1 `contains` value. (Other lengths will then throw an error.)
#'   Default is `FALSE`.
#' @param order_decreasing Boolean. If `TRUE` (the default), longer classes are
#'   tested before shorter ones.
#'
#' @return
#' - For `class_with()`, a string vector.
#' - For `inherits_class_with()`, a length-1 Boolean vector.
#'
#' @noRd
class_with <- function(data, contains, all_classes = FALSE,
                       order_decreasing = TRUE) {
  cd <- class(data)

  if (all_classes) {
    if (length(contains) > 1L) {
      contains <- wrap_in_backticks(contains)
      cli::cli_abort(c(
        "`contains` has length {length(contains)}.",
        "x" = "With `all_classes` set to `TRUE`, `contains` \\
        cannot be longer than 1.",
        "i" = "(Its values are {contains}.)"
      ))
    }
    return(cd[stringr::str_detect(cd, contains)])
  }

  cd_lengths <- purrr::map_int(cd, stringr::str_length)
  cd <- cd[order(cd_lengths, decreasing = order_decreasing)]

  # Outer loop:
  for (i in seq_along(contains)) {

    # Inner loop:
    for (j in seq_along(cd)) {
      cd_contains_string <- stringr::str_detect(cd[j], contains[i])
      if (cd_contains_string) {
        return(cd[j])
      }
    }
    # End of inner loop

  }
  # End of outer loop

  character(0L)
}



inherits_class_with <- function(data, contains, all_classes = FALSE,
                                order_decreasing = TRUE) {
  length(class_with(
    data = data, contains = contains, all_classes = all_classes,
    order_decreasing = order_decreasing
  )) > 0L
}


