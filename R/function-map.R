
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




#' Create new `*_map()` functions
#'
#' @description `function_map()` creates new basic mapper functions for
#'   consistency tests, such as `grim_map()` or `debit_map()`. For context, see
#'   `vignette("consistency-tests")`.
#'
#' @param .fun Single-case consistency testing function that will be applied to
#'   each row in a data frame, such as the (non-exported) scrutiny functions
#'   `grim_scalar()` and `debit_scalar()`. It needs to return a Boolean value of
#'   length 1, i.e., `TRUE` or `FALSE`.
#' @param .reported String. Names of the columns to be tested.
#' @param .name_test String (length 1). Plain-text name of the consistency test,
#'   such as `"GRIM"`.
#' @param .name_class String. One or more classes to be added to the output data
#'   frame. Default is `NULL`, i.e., no extra class (but see *Details*).

#' @details The output tibble returned by the factory-made function will inherit
#'   one or two classes independently of the `.name_class` argument:
#' - It will inherit a class named `"scr_{tolower(.name_test)}_map"`; for
#'   example, `"scr_grim_map"` if `.name_test` is `"GRIM"`.
#' - If a `rounding` argument is specified via `...`, or else if `.fun` has a
#'   `rounding` argument with a default, the output tibble will inherit a class
#'   named `"scr_rounding_{rounding}"`; for example,
#'   `"scr_rounding_up_or_down"`.

#' @return A factory-made function with these arguments:
#' - `data`: Data frame with all the columns named in `.reported`. It needs to
#'   have columns named after the key arguments in `.fun`. Other columns are
#'   permitted.
#' - Arguments named after the `.reported` values. They can be specified as the
#'   names of `data` columns so that the function will rename that column using
#'   the `.reported` name.
#' - `reported`, `fun`, `name_class`: Same as when calling `function_map()` but
#'   spelled without dots. You can override these defaults when calling the
#'   factory-made function.
#' - `...`: Arguments passed down to `.fun`. This does not include the
#'   column-identifying arguments derived from `.reported`.

#' @section Value returned by the factory-made function: A tibble that includes
#'   `"consistency"`: a Boolean column showing whether the values to its left
#'   are mutually consistent (`TRUE`) or not (`FALSE`).

#' @export

#' @examples
#' # Basic test implementation for "SCHLIM",
#' # a mock test with no real significance:
#' schlim_scalar <- function(y, n) {
#'   (y / 3) > n
#' }
#'
#' # Let the function factory produce
#' # a mapper function for SCHLIM:
#' schlim_map <- function_map(
#'   .fun = schlim_scalar,
#'   .reported = c("y", "n"),
#'   .name_test = "SCHLIM"
#' )
#'
#' # Example data:
#' df1 <- tibble::tibble(y = 16:25, n = 3:12)
#'
#' # Call the "factory-made" function:
#' schlim_map(df1)
#'
#'
#' # Advice on exporting factory-made functions ----------------
#'
#' # (The guidelines below were adapted from purrr:
#' # https://purrr.tidyverse.org/reference/faq-adverbs-export.html)
#'
#' # If you want to export a function produced
#' # by `function_map()` from your own package,
#' # follow this pattern, except for the `if`-wrapping:
#'
#' if (FALSE) {
#'
#'   schlim_map <- function(...) "dummy"
#'
#'   .onLoad <- function(lib, pkg) {
#'     schlim_map <<- function_map(
#'       .fun = schlim_scalar,
#'       .reported = c("y", "n"),
#'       .name_test = "SCHLIM"
#'     )
#'   }
#'
#' }
#'
#' # The same applies to functions
#' # produced with scrutiny's other
#' # function factories, `function_map_seq()`
#' # and `function_map_total_n()`.




# # Example data:
# data <- pigs1
# reported <- c("x", "n")
# fun <- grim_scalar
# name_test <- "GRIM"


function_map <- function(.fun, .reported, .name_test, .name_class = NULL) {

  # Checks ---

  fun_name <- deparse(substitute(.fun))
  fun_args <- as.list(args(.fun))
  offenders <- .reported[!.reported %in% names(fun_args)]

  if (length(offenders) > 0) {
    offenders <- wrap_in_backticks(offenders)
    fun_name <- deparse(substitute(.fun))
    if (length(offenders) == 1) {
      msg_arg <- "argument"
      msg_it_they <- "It was"
    } else {
      msg_arg <- "arguments"
      msg_it_they <- "They were"
    }
    cli::cli_abort(c(
      "Function `{fun_name}()` lacks {msg_arg} {offenders}.",
      "x" = "{msg_it_they} stated as `.reported` in the \\
      `function_map()` call, where `.fun` was specified as `{fun_name}`."
    ))
  }


  # --- Start of the factory-made function, `fn_out()` ---

  fn_out <- function(data, ...) {

    fun <- .fun
    reported <- .reported
    name_test <- .name_test
    name_class <- .name_class

    # Manage key columns in `data` ---

    key_cols_missing <- reported[!reported %in% colnames(data)]
    key_cols_missing <- as.character(key_cols_missing)

    # Rename key columns that have non-standard names, following user-supplied
    # directions via the arguments automatically inserted below the function:
    if (length(key_cols_missing) > 0) {
      names(key_cols_missing) <- key_cols_missing

      # Extract the expressions supplied by the factory-made function's user as
      # values of the arguments that are named after `reported`. Coerce them to
      # string because they will be needed as column names:
      key_cols_call <- as.list(rlang::call_match())
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

      # Extract the renamed columns from the tibble returned below and put them
      # together with each other and the non-renamed columns! MAYBE REWRITE THIS
      # WITH purrr? THAT MIGHT BE `purrr::pmap_df()` OR SO.
      df_key_cols_renamed <- df_colnames %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          data = list(replace_colname(data, name_missing, name_call))
        )

      data_renamed <- dplyr::bind_cols(df_key_cols_renamed$data)
      data_not_renamed <- data[!colnames(data) %in% df_colnames$name_call]

      data <- dplyr::bind_cols(data_renamed, data_not_renamed)
    }


    # Checks ---

    check_mapper_input_colnames(data, reported, name_test)

    # Check that no argument specified via the dots, `...`, was misspelled:
    dots <- rlang::enexprs(...)
    dots_names <- names(dots)
    offenders <- dots_names[!dots_names %in% names(fun_args)]
    if (length(offenders) > 0) {
      offenders <- wrap_in_backticks(offenders)
      if (length(offenders) == 1) {
        msg_arg <- "argument"
        msg_it_they <- "It's not an"
      } else {
        msg_arg <- "arguments"
        msg_it_they <- "They are not"
      }
      cli::cli_abort(c(
        "Unknown {msg_arg} {offenders}.",
        "x" = "{msg_it_they} {msg_arg} of `{fun_name}`."
      ))
    }


    # Main part ---

    rounding_dots <- dots$rounding
    rounding_args <- fun_args$rounding

    if (length(rounding_dots) > 0) {
      rounding <- rounding_dots
    } else if (length(rounding_args) > 0) {
      rounding <- rounding_args
    } else {
      rounding <- NULL
    }

    if (length(rounding) > 0) {
      rounding_class <- paste0("scr_rounding_", rounding)
      name_class <- append(name_class, rounding_class)
    }

    # This says `all`...
    all_classes <- paste0("scr_", tolower(name_test), "_map")

    # ...because more values might be added to it:
    if (!is.null(name_class)) {
      all_classes <- c(all_classes, name_class)
    }

    # Divide the data into tested and non-tested columns, going by the key
    # column names expected from the `reported` argument:
    data_tested <- data[, reported]
    data_non_tested <- data[!colnames(data) %in% reported]

    # Test for consistency:
    consistency <- purrr::pmap_lgl(data_tested, fun, ...)

    # Following scrutiny's requirements for mapper functions, `"consistency"`
    # goes immediately to the right of the key columns (which here are identical
    # to the tested columns). Any other columns from the input go to the right
    # of `"consistency"`:
    out <- tibble::tibble(data_tested, consistency, data_non_tested)
    out <- add_class(out, all_classes)

    return(out)
  }

  # --- End of the factory-made function, `fn_out()` ---


  # Insert parameters named after the key columns into `fn_out()`, with `NULL`
  # as the default for each. The key columns need to be present in the input
  # data frame. They are expected to have the names specified in `.reported`. If
  # they don't, however, the user can simply specify the key column arguments as
  # the non-quoted names of the columns meant to fulfill these roles:
  key_args <- list(NULL)
  key_args <- rep(key_args, times = length(.reported))
  names(key_args) <- .reported
  formals(fn_out) <- append(formals(fn_out), key_args, after = 1)

  return(fn_out)
}




# Example factory-made functions:

grim_map_alt <- function_map(
  .fun = grim_scalar,
  .reported = c("x", "n"),
  .name_test = "GRIM"
)

debit_map_alt <- function_map(
  .fun = debit_scalar,
  .reported = c("x", "sd", "n"),
  .name_test = "DEBIT"
)




