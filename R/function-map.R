
#' Create new `*_map()` functions
#'
#' @description `function_map()` creates new basic mapper functions for
#'   consistency tests, such as `grim_map()` or `debit_map()`.
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

#' @details The output tibble returned by the manufactured function will inherit
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
#' - `reported`, `fun`, `name_class`: Same as when calling `function_map()` but
#'   spelled without dots. You can override these defaults when calling the
#'   manufactured function.
#' - `...`: Arguments passed down to `.fun`.
#'
#' The manufactured function should then return a tibble that includes
#' `"consistency"`: a Boolean column that shows whether the values to its left
#' are mutually consistent (`TRUE`) or not (`FALSE`).

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
#' # Call the "manufactured" function:
#' schlim_map(df1)
#'
#'
#' # Advice on exporting manufactured functions ----------------
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
    offenders <- backticks(offenders)
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


  # --- Start of the manufactured function ---

  function(data, fun = .fun, reported = .reported, name_test = .name_test,
           name_class = .name_class, ...) {

    # Checks ---

    check_mapper_input_colnames(data, reported, name_test)

    # Check that no argument specified via the dots, `...`, was misspelled:
    dots <- rlang::enexprs(...)
    dots_names <- names(dots)
    offenders <- dots_names[!dots_names %in% names(fun_args)]
    if (length(offenders) > 0) {
      offenders <- backticks(offenders)
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

    # all_classes <- stringr::str_remove(fun_name, "_scalar")
    # all_classes <- paste0("scr_", all_classes, "_map")

    # This ends on `_all`...
    all_classes <- paste0("scr_", tolower(name_test), "_map")

    # ...because more values might be added to it:
    if (!is.null(name_class)) {
      all_classes <- c(all_classes, name_class)
    }

    # Separate the data into tested and non-tested columns, going by the column
    # names expected from the `reported` argument:
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

  # --- End of the manufactured function ---

}




# grim_map_alt <- function_map(
#   .fun = grim_scalar,
#   .reported = c("x", "n"),
#   .name_test = "GRIM"
# )
#
#
# debit_map_alt <- function_map(
#   .fun = debit_scalar,
#   .reported = c("x", "sd", "n"),
#   .name_test = "DEBIT"
# )




