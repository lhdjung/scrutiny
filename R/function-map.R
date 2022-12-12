
#' Create new `*_map()` functions
#'
#' @description `function_map()` creates new basic mapper functions for
#'   consistency tests, such as `grim_map()` or `debit_map()`.
#'
#'   For context, see `vignette("consistency-tests")`, section *Creating mappers
#'   with `function_map()`*.
#'
#' @param .fun Single-case consistency testing function that will be applied to
#'   each row in a data frame, such as the (non-exported) scrutiny functions
#'   `grim_scalar()` and `debit_scalar()`. It needs to return a Boolean value of
#'   length 1, i.e., `TRUE` or `FALSE`.
#' @param .reported String. Names of the columns to be tested.
#' @param .name_test String (length 1). Plain-text name of the consistency test,
#'   such as `"GRIM"`.
#' @param .name_class String. Optionally, one or more classes to be added to the
#'   output data frame. Default is `NULL`, i.e., no extra class (but see
#'   *Details*).
#' @param .args_disabled Optionally, a string vector with names of arguments of
#'   the `*_scalar()` function that don't work with the factory-made function.
#'   If the user  tries to specify these arguments, an informative error will be
#'   thrown.
#' @param .col_names (Experimental) Optionally, a string vector with the names
#'   of additional columns that are derived from the `*_scalar()` function.
#'   Requires `.col_control` and `.col_filler` specifications.
#' @param .col_control (Experimental) Optionally, a single string with the name
#'   of the `*_scalar()` function's Boolean argument that controls if the
#'   columns named in `.col_names` will be displayed.
#' @param .col_filler (Experimental) Optionally, a vector specifying the values
#'   of `.col_names` columns in rows where the `*_scalar()` function only
#'   returned the `consistency` value.

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

#' @include grim.R debit.R function-factory-helpers.R

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



# # Example data:
# data <- pigs1
# reported <- c("x", "n")
# fun <- grim_scalar
# name_test <- "GRIM"


function_map <- function(.fun, .reported, .name_test, .name_class = NULL,
                         .args_disabled = NULL, .col_names = NULL,
                         .col_control = NULL, .col_filler = NULL) {

  force(.fun)
  force(.reported)
  force(.name_test)
  force(.name_class)
  force(.args_disabled)
  force(.col_names)
  force(.col_control)
  force(.col_filler)

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

    # Manage key columns in `data`:
    data <- absorb_key_args(data, reported)


    # Checks ---

    check_args_disabled(.args_disabled)
    check_factory_dots(fun, fun_name, ...)
    check_mapper_input_colnames(data, reported, name_test)


    # Main part ---

    # Divide the data into tested and non-tested columns, going by the key
    # column names expected from the `reported` argument:
    data_tested <- data[, reported]
    data_non_tested <- data[!colnames(data) %in% reported]

    # Test for consistency:
    consistency <- purrr::pmap(data_tested, fun, ...)

    # Support rounding classes:
    if ("rounding" %in% names(formals(fun))) {
      dots <- rlang::enexprs(...)

      if ("rounding" %in% names(dots)) {
        rounding_class <- dots$rounding
      } else {
        rounding_class <- formals(fun)$rounding
      }

      rounding_class <- paste0("scr_rounding_", rounding_class)
      name_class <- c(name_class, rounding_class)
    }

    # This says `all`...
    all_classes <- paste0("scr_", tolower(name_test), "_map")

    # ...because more values might be added to it:
    if (!is.null(name_class)) {
      all_classes <- c(all_classes, name_class)
    }

    # Following scrutiny's requirements for mapper functions, `"consistency"`
    # goes immediately to the right of the key columns (which here are identical
    # to the tested columns). Any other columns from the input go to the right
    # of `"consistency"`:
    out <- tibble::tibble(data_tested, consistency, data_non_tested)
    out <- add_class(out, all_classes)

    # The idea here is that `.col_control` might have been specified as a string
    # that is the name of a Boolean argument which controls whether or not
    # additional columns beyond `"consistency"` are shown. They would have to be
    # extracted from the `*_scalar()` function and initially stored in a
    # `"consistency"` list-column, together with the actual `consistency` value:
    if (!is.null(.col_control)) {
      .col_control <- eval(rlang::parse_expr(.col_control))
      # return(.col_control)
      lengths_consistency <- vapply(consistency, length, integer(1))
      lengths_consistency_all1 <- all(lengths_consistency == 1)
      if (.col_control & !lengths_consistency_all1) {
        extend_if_length1 <- function(x, value_if_length1) {
          if (length(x) == 1) {
            list(list(x, value_if_length1))
          } else {
            x
          }
        }
        out$consistency <- purrr::map(
          out$consistency, extend_if_length1, value_if_length1 = .col_filler
        )
        out <- unnest_consistency_cols(
          out, col_names = c("consistency", .col_names), index = FALSE
        )
      } else {
        out <- tidyr::unnest(out, cols = consistency)
      }
    }

    if (is.list(out$consistency)) {
      out$consistency <- unlist(out$consistency)
    }

    return(out)
  }

  # --- End of the factory-made function, `fn_out()` ---


  # Insert parameters named after the key columns into `fn_out()`, with `NULL`
  # as the default for each. The key columns need to be present in the input
  # data frame. They are expected to have the names specified in `.reported`. If
  # they don't, however, the user can simply specify the key column arguments as
  # the non-quoted names of the columns meant to fulfill these roles:
  fn_out <- insert_key_args(fun = fn_out, reported = .reported)

  return(fn_out)
}


