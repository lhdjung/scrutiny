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
#' @param key_cols_call Optionally, a named character vector that maps key
#'   argument names to the columns of `data` that they refer to, such as `c(x =
#'   "mean")`. By default, it is read from the key arguments of the function
#'   that calls `absorb_key_args()`.
#' @param key_cols_missing,key_cols_call_names String. Vectors with names of the
#'   key columns that are missing in `data` or that were provided by the user as
#'   arguments, respectively.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_factory_key_args_values <- function(data, key_cols_call) {
  offenders <- key_cols_call[!key_cols_call %in% colnames(data)]

  if (length(offenders) == 0L) {
    return(NULL)
  }

  # Error condition -- one or more key arguments have been specified with values
  # that are not actually column names of `data`:

  offenders_names <- offenders |>
    names() |>
    glue::as_glue() |>
    wrap_in_backticks()
  offenders <- wrap_in_backticks(offenders)
  name_current_fn <- name_last_export_code()
  if (length(offenders) == 1L) {
    msg_is_colname <- "is not a column name"
  } else {
    msg_is_colname <- "are not column names"
  }

  # Prepare an error message. It might be subsequently appended...
  msg_error <- c(
    "!" = "{offenders} {msg_is_colname} of `data`.",
    "x" = "The {offenders_names[1L]} argument of \\
      {name_current_fn} was specified as {offenders[[1L]]}, \\
      but there is no column in `data` called {offenders[[1L]]}."
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
      msg_error,
      c(
        "x" = "Same with the {offenders_names[-1L]} {msg_arg_s}: \\
          `data` doesn't contain {msg_a}{offenders[-1L]} {msg_col}."
      )
    )
  }

  # Throw the actual error:
  abort_in_export(msg_error)
}


check_factory_key_args_names <- function(
  key_cols_missing,
  key_cols_call_names
) {
  offenders <- key_cols_missing |>
    call_on(\(x) x[!x %in% key_cols_call_names])

  # Error condition -- not all of the `reported` values that are not column
  # names of `data` have been supplied as values of the respective arguments:
  if (length(offenders) == 0L) {
    return(NULL)
  }

  offenders <- wrap_in_backticks(offenders)

  msg_fun_name <- name_last_export_code()

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
  abort_in_export(
    "{msg_column_s} {offenders} {msg_is_are} \\
          missing from `data`.",
    "x" = "{msg_needs_to_be} of the input data frame.",
    "i" = "Alternatively, specify the {offenders} \\
          {msg_argument} of {msg_fun_name} as {msg_names}."
  )
}


# The settings of a mapper `fun`, such as `rounding`: its formals after the
# dots, minus those named in `exclude`. `function_map_seq()` and
# `function_map_total_n()` copy them, with `fun`'s defaults, so that they are
# arguments of the mappers they create, as in `function_map()`. Through the
# dots, they were invisible to `formals()`, tab completion, and the help page.
# A `fun` without dots has no settings to copy, and keeps getting them through
# the dots.
formals_settings <- function(fun, exclude) {
  formals_fun <- formals(fun)
  index_dots <- match("...", names(formals_fun))
  if (is.na(index_dots)) {
    return(list())
  }
  formals_after <- formals_fun[-seq_len(index_dots)]
  formals_after[setdiff(names(formals_after), exclude)]
}


# Is a formal's default a set of strings, as in `sd_type = c("sample",
# "population", "either")`? See `function_map()`.
is_enum_default <- function(default) {
  is.call(default) &&
    identical(default[[1L]], as.name("c")) &&
    length(default) > 2L &&
    all(vapply(as.list(default)[-1L], is.character, logical(1L)))
}


#' Check that no dots argument is misspelled
#'
#' `check_factory_dots()` is called within the mapper functions made by
#' [`function_map()`], [`function_map_seq()`], and [`function_map_total_n()`].
#' It errors if an argument passed through the dots is not an argument of the
#' function being mapped.
#'
#' @param fun Function applied by the factory-made function.
#' @param fun_name_scalar String (length 1). Name of `fun`.
#' @param ... The dots of the factory-made function.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_factory_dots <- function(fun, fun_name_scalar, ...) {
  dots <- rlang::enexprs(...)
  dots_names <- names(dots)
  offenders <- dots_names[!dots_names %in% names(formals(fun))]

  if (length(offenders) == 0L) {
    return(NULL)
  }

  fun_name_mapper <- name_last_export_code()

  # The dots come right after the key arguments, so an unnamed argument
  # after them lands here, with an empty name:
  n_unnamed <- sum(offenders == "")
  if (n_unnamed > 0L) {
    abort_in_export(
      "{n_unnamed} unnamed argument{?s} after the key arguments.",
      "x" = "Every argument after `...` must be named in full.",
      "i" = "The key arguments are the columns to test, such as `x` and `n`."
    )
  }

  # Arguments after the dots must be named in full, so an abbreviation such as
  # `items` for `items_averaged` lands here. `fun` has the arguments a sequence
  # or total-n mapper passes on, such as `rounding`:
  names_full <- union(names(formals(rlang::caller_fn())), names(formals(fun)))
  names_meant <- unique(unlist(lapply(offenders, function(offender) {
    names_full[startsWith(names_full, offender)]
  })))

  offenders <- paste0("`", offenders, "`")

  if (length(offenders) == 1L) {
    msg_arg <- "argument"
    msg_it_they <- "It's not an"
  } else {
    msg_arg <- "arguments"
    msg_it_they <- "They are not"
  }

  abort_in_export(
    "Invalid {msg_arg} {offenders}.",
    "x" = "{msg_it_they} {msg_arg} of {fun_name_mapper} \\
      or `{fun_name_scalar}()`.",
    "i" = if (length(names_meant) > 0L) {
      "Did you mean {wrap_in_backticks(names_meant)}? Arguments after \\
      `...` must be named in full."
    }
  )
}


#' Check that an argument names arguments of `.fun`
#'
#' Several arguments of `function_map()` -- `.reported`, `.args_by_row`, and
#' `.cols_helper` -- are string vectors that must name arguments of the
#' `*_scalar()` function passed as `.fun`. `check_factory_arg_names()` enforces
#' this in the factory's entry area, where the user of the factory can still be
#' told which of their specifications is at fault.
#'
#' @param names String. The values given for `arg_name`.
#' @param formals_fun Pairlist. The formal arguments of `.fun`.
#' @param fun_name String (length 1). Name of `.fun`.
#' @param arg_name String (length 1). Name of the function factory's argument
#'   that `names` was given for, such as `".reported"`.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_factory_arg_names <- function(names, formals_fun, fun_name, arg_name) {
  offenders <- names[!names %in% names(formals_fun)]

  if (length(offenders) == 0L) {
    return(NULL)
  }

  offenders <- wrap_in_backticks(offenders)

  if (length(offenders) == 1L) {
    msg_arg <- "argument"
    msg_it_they <- "It was"
  } else {
    msg_arg <- "arguments"
    msg_it_they <- "They were"
  }

  abort_in_export(
    "Function `{fun_name}()` lacks {msg_arg} {offenders}.",
    "i" = "{msg_it_they} given as `{arg_name}` in the \\
    `function_map()` call, where `.fun` was specified as `{fun_name}`."
  )
}


#' Turn a mapper's test results into output columns
#'
#' A `*_scalar()` function returns a single value per row, or -- if it was told
#' to show its reconstructed values, as via `show_rec` -- a list of values per
#' row. `write_result_cols()` covers both cases, and is called within
#' factory-made mapper functions.
#'
#' @param results List with one element per row of the mapper's input data
#'   frame, as returned by `purrr::pmap()`.
#' @param col_names String vector with the names of the columns that the
#'   `*_scalar()` function's list of values unpacks into, key result first. It
#'   is the `.col_names` argument of `function_map()`, and may be `NULL`.
#'
#' @return Named list of columns.
#'
#' @noRd
write_result_cols <- function(results, col_names) {
  # With no rows to test, there is no result to read the output's shape off, so
  # only the key result column is created -- the one column that is present
  # whatever the `*_scalar()` function was told to show. Without this,
  # `unlist()` below returns `NULL`, and the tibble ends up counting a column
  # that it doesn't have:
  if (length(results) == 0L) {
    out <- list(logical(0L))
    names(out) <- if (is.null(col_names)) "consistency" else col_names[1L]
    return(out)
  }

  lengths_results <- lengths(results)

  # The regular case: one value per row, so the key result column is all there
  # is:
  if (all(lengths_results == 1L)) {
    out <- list(unlist(results, use.names = FALSE))
    names(out) <- if (is.null(col_names)) "consistency" else col_names[1L]
    return(out)
  }

  # Without `.col_names`, there is nothing to unpack into, so the values stay in
  # a list-column, as before:
  if (is.null(col_names)) {
    return(list(consistency = results))
  }

  offenders <- unique(lengths_results[lengths_results != length(col_names)])

  if (length(offenders) > 0L) {
    abort_in_export(
      "The consistency test function returned {offenders[1L]} value{?s} \\
      for at least one row.",
      "x" = "It must return either a single value or one value per \\
      `.col_names` name, of which there are {length(col_names)}.",
      "i" = "`.col_names` is an argument of `function_map()`, specified \\
      when the present function was created."
    )
  }

  split_result_cols(results, col_names)
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
#' @param variadic String (length 1) or `NULL`, the default. Optionally, the
#'   name of a variadic key argument, which is inserted ahead of the `reported`
#'   ones and with no default rather than with `NULL`: it takes a tidyselect
#'   expression, and there is no set of columns that could be guessed at.
#'
#' @return Function `fun` with new arguments, named after `reported`, with
#'   `NULL` as the default for each.
#'
#' @noRd
insert_key_args <- function(fun, reported, insert_after = 1L, variadic = NULL) {
  key_args <- rep(list(NULL), times = length(reported))
  names(key_args) <- reported

  if (!is.null(variadic)) {
    # The empty symbol is what a formal without a default has; see `alist()`:
    arg_variadic <- rlang::missing_arg() |>
      list() |>
      rlang::set_names(variadic)
    key_args <- c(arg_variadic, key_args)
  }

  # Replace the arguments of `fun` by the result of the pipeline, which just
  # appends `key_args` after the specified position.
  `formals<-`(
    fun,
    value = fun |>
      formals() |>
      append(key_args, after = insert_after)
  )
}


#' Check that a variadic key argument was specified
#'
#' A factory-made function with a `.reported_variadic` argument has one formal
#' that takes a tidyselect expression, and that formal has no default: which
#' columns are tested is a property of the caller's data, so there is nothing
#' the factory could have guessed at. `check_variadic_arg()` turns the empty
#' quosure that results from leaving it out into a message that says so.
#'
#' @param quo Quosure captured from the variadic argument.
#' @param name String (length 1). Name of that argument.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_variadic_arg <- function(quo, name) {
  if (rlang::quo_is_missing(quo)) {
    fun_name <- name_last_export_code()
    abort_in_export(
      "The `{name}` argument of {fun_name} must be specified.",
      "x" = "It has no default: which columns are tested is up to the data, \\
      and testing all the remaining ones by default would quietly draw in \\
      any column that is not a key column for some other reason.",
      "i" = "Select them using tidyselect syntax, as in \\
      `{name} = c(a, b, c)` or `{name} = starts_with(\"item\")`."
    )
  }
}


#' Check what a variadic key argument selected
#'
#' `tidyselect::eval_select()` guarantees that the selected columns exist, but
#' not that they are usable as variadic key columns. Two ways they may not be:
#'
#' - The selection is empty, which leaves the test function with no values.
#'   `purrr::pmap()` would report this as a recycling failure over a variable
#'   the caller has never heard of.
#' - The selection overlaps the columns that already have a role in the test.
#'   Such a column would be both tested as one of many values and used in its
#'   own right, and it would appear twice in the output, giving a tibble with
#'   duplicate column names.
#'
#' @param index Integer. Column positions, as returned by
#'   `tidyselect::eval_select()`.
#' @param data The mapper's input data frame.
#' @param spoken_for String. Names of the columns that have a role already:
#'   the key columns and any helper columns.
#' @param name String (length 1). Name of the variadic argument.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_variadic_cols <- function(index, data, spoken_for, name) {
  fun_name <- name_last_export_code()

  if (length(index) == 0L) {
    abort_in_export(
      "The `{name}` argument of {fun_name} selected no columns.",
      "x" = "There would be no values to test."
    )
  }

  offenders <- intersect(colnames(data)[index], spoken_for)

  if (length(offenders) == 0L) {
    return(invisible(NULL))
  }

  name_first <- offenders[1L]
  offenders <- wrap_in_backticks(offenders)

  if (length(offenders) == 1L) {
    msg_that_column <- "That column has"
    msg_subject <- "it"
    msg_object <- "it"
    msg_one <- "one"
  } else {
    msg_that_column <- "Those columns have"
    msg_subject <- "they"
    msg_object <- "them"
    msg_one <- "some"
  }

  abort_in_export(
    "The `{name}` argument of {fun_name} selected {offenders}.",
    "x" = "{msg_that_column} a role in the test already, so {msg_subject} \\
    cannot also be tested as {msg_one} of the `{name}` values.",
    "i" = "Exclude {msg_object} from the selection, as in \\
    `{name} = !{name_first}`."
  )
}

#' Rename key columns according to the key arguments
#'
#' `absorb_key_args()` is called within mapper functions. If the user named a
#' key argument such as `x` after a column of `data`, it renames that column to
#' the key argument's name, so that the rest of the mapper finds it under the
#' expected name.
#'
#' @param data Data frame passed to the mapper.
#' @param reported String. Names of the key arguments.
#' @param key_cols_call Optionally, a named character vector that maps key
#'   argument names to the columns of `data` that they refer to, such as `c(x =
#'   "mean")`. By default, it is read from the key arguments of the calling
#'   mapper.
#'
#' @export
#'
#' @return `data`, possibly with columns renamed. Reassign the result to
#'   `data`.
#'
#' @examples
#' df <- tibble::tibble(mean = c(5.19, 4.56), n = c(28, 30))
#' absorb_key_args(df, c("x", "n"), key_cols_call = c(x = "mean"))
absorb_key_args <- function(data, reported, key_cols_call = NULL) {
  # The values of the key arguments are read in the frame of the factory-made
  # function, not off its call: the call is `FUN(X[[i]], ...)` if the function
  # was reached through `lapply()`, and names a variable rather than its value
  # if it was called from within another function.
  if (is.null(key_cols_call)) {
    fn_caller <- rlang::caller_fn()
    env <- rlang::caller_env()
    names_formals <- names(formals(fn_caller))
    quos <- list()
    for (name in intersect(reported, names_formals)) {
      quos[[name]] <- eval(rlang::call2(rlang::enquo, rlang::sym(name)), env)
    }
    # A handwritten caller may pass the key arguments on through its dots:
    if (any(names_formals == "...")) {
      quos_dots <- eval(quote(rlang::enquos(...)), env)
      quos <- c(quos, quos_dots[intersect(reported, names(quos_dots))])
    }
    key_cols_call <- capture_key_args(data, quos)
  }

  # A key argument pointing at the column that already has its name is a no-op:
  key_cols_call <- key_cols_call[key_cols_call != names(key_cols_call)]

  # A column by the key argument's own name would be tested instead of the one
  # the argument points to, which is not what the user asked for:
  key_cols_clash <- names(key_cols_call)[
    names(key_cols_call) %in% colnames(data)
  ]
  if (length(key_cols_clash) > 0L) {
    name_arg <- key_cols_clash[[1L]]
    name_col <- key_cols_call[[name_arg]]
    abort_in_export(
      "`{name_arg}` was specified as {.val {name_col}}, but `data` \\
      already has a `{name_arg}` column.",
      "x" = "It is unclear which of the two columns should be tested.",
      "i" = "Rename or remove the `{name_arg}` column first."
    )
  }

  key_cols_missing <- reported |>
    call_on(\(x) x[!x %in% colnames(data)]) |>
    as.character()

  # No need to work with key arguments here if `data` has all of the expected
  # column names:
  if (length(key_cols_missing) == 0L) {
    return(data)
  }

  names(key_cols_missing) <- key_cols_missing
  key_cols_call <- key_cols_call[names(key_cols_call) %in% key_cols_missing]
  key_cols_call_names <- names(key_cols_call)

  # Run specialized checks on the code supplied by the factory-made
  # function's user to the subsequently inserted key argument parameters:
  check_factory_key_args_values(data, key_cols_call)
  check_factory_key_args_names(key_cols_missing, key_cols_call_names)

  # One column cannot stand in for two key columns:
  if (anyDuplicated(key_cols_call) > 0L) {
    name_col <- key_cols_call[duplicated(key_cols_call)][1L]
    names_args <- key_cols_call_names[key_cols_call == name_col]
    abort_in_export(
      "{wrap_in_backticks(names_args)} must name different columns.",
      "x" = "They all name `{name_col}`."
    )
  }

  # Replace the actual column names by the missing names for which they stand
  # in, then move the renamed columns to the front, as they are key columns:
  index <- match(key_cols_call, colnames(data))
  colnames(data)[index] <- key_cols_call_names
  data |>
    dplyr::relocate(dplyr::all_of(unname(key_cols_missing)))
}


# Read the key arguments, captured as named quosures, as a named character
# vector of column names. An argument that is `NULL`, i.e., not
# specified, is left out. A bare name is taken as a column name if `data` has
# such a column; otherwise, if it is a variable holding a string, as that
# string. Any other expression must evaluate to a single string.
capture_key_args <- function(data, quos) {
  out <- character()
  for (name in names(quos)) {
    quo <- quos[[name]]
    if (rlang::quo_is_null(quo)) {
      next
    }
    expr <- rlang::quo_get_expr(quo)
    value <- if (rlang::is_symbol(expr)) {
      value_sym <- rlang::as_string(expr)
      value_var <- rlang::env_get(
        rlang::quo_get_env(quo),
        value_sym,
        default = NULL,
        inherit = TRUE
      )
      if (!any(value_sym == colnames(data)) && rlang::is_string(value_var)) {
        value_var
      } else {
        value_sym
      }
    } else {
      rlang::eval_tidy(quo)
    }
    if (!rlang::is_string(value)) {
      abort_in_export(
        "The `{name}` argument must be a column name.",
        "x" = "It is {.obj_type_friendly {value}}.",
        "i" = "Specify it as a string, like `{name} = \"my_col\"`, or as a \\
        bare column name."
      )
    }
    out[[name]] <- value
  }
  out
}


#' Check that disabled arguments are not specified
#'
#' `check_args_disabled()` is called within functions made by
#' [`function_map()`] or [`function_map_seq()`]. It errors if the user of the
#' factory-made function specified an argument that the factory's user disabled
#' via `.args_disabled` or fixed via `.args_fixed`.
#'
#' @param args_disabled String. Names of the disabled arguments.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_args_disabled <- function(args_disabled) {
  if (is.null(args_disabled)) {
    return(NULL)
  }
  # Disabled arguments arrive through the caller's dots, or they are formals
  # of the caller, as `dispersion` is in a sequence mapper. The names of the
  # dots are read off the dots themselves, not off the call, which through
  # `lapply()` is `FUN(X[[i]], ...)` and names nothing; a formal is given if
  # `missing()` says so in the caller's frame:
  env_caller <- parent.frame()
  names_dots <- if (exists("...", envir = env_caller, inherits = FALSE)) {
    eval(quote(...names()), env_caller)
  }
  names_formals <- names(formals(sys.function(sys.parent())))
  is_given <- vapply(
    args_disabled,
    function(arg) {
      (arg %in% names_dots) ||
        ((arg %in% names_formals) &&
          !eval(call("missing", as.name(arg)), env_caller))
    },
    logical(1L),
    USE.NAMES = FALSE
  )
  offenders <- args_disabled[is_given]
  if (length(offenders) > 0L) {
    fun_name <- name_last_export_code()
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
    abort_in_export(
      "{msg_arg_s} {offenders} {msg_is_are} \\
          disabled in {fun_name}{msg_among_others}."
    )
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
    name_fun <- name_last_export_code()
    if (length(name_names) == 1L) {
      msg_names <- "this name"
    } else {
      msg_names <- "these names"
    }
    abort_in_export(
      "In {name_fun}, the `{name}` argument must be \\
      an unnamed string vector.",
      "x" = "It has {msg_names}: {name_names}."
    )
  }
}


#' Check what a test function returned
#'
#' `check_fun_result()` is called on the result of applying `.fun` to the
#' first row. The key result column is logical, so `.fun` must return a single
#' logical value, or a list that starts with one if it shows its
#' reconstructed values.
#'
#' @param result What `.fun` returned.
#' @param fun_name String. Name of `.fun`, for the message.
#'
#' @return `result`, invisibly; might throw an error.
#'
#' @noRd
check_fun_result <- function(result, fun_name) {
  key <- if (is.list(result) && length(result) > 0L) result[[1L]] else result
  if (!is.logical(key) || length(key) != 1L) {
    cli::cli_abort(c(
      "`{fun_name}()` must return a single logical value, or a list \\
      starting with one.",
      "x" = "It returned {.obj_type_friendly {result}}."
    ))
  }
  invisible(result)
}
