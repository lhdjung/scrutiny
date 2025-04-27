
#' Create new `*_map()` functions
#'
#' @description `function_map()` creates new basic mapper functions for
#'   consistency tests, such as [`grim_map()`] or [`debit_map()`].
#'
#'   For context, see [*Creating basic mappers with `function_map()`*](
#'   https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#creating-basic-mappers-with-function_map
#'   ).
#'
#' @param .fun Single-case consistency testing function that will be applied to
#'   each row in a data frame. The function must return a single logical value,
#'   i.e., `TRUE`, `FALSE`, or `NA`.
#' @param .reported String. Names of the columns to be tested.
#' @param .name_test String (length 1). Plain-text name of the consistency test,
#'   such as `"GRIM"`.
#' @param .name_key_result (Experimental) Optionally, a single string that will
#'   be the name of the key result column in the output. Default is
#'   `"consistency"`.
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
#'   of the `*_scalar()` function's logical argument that controls if the
#'   columns named in `.col_names` will be displayed.
#' @param .col_filler (Experimental) Optionally, a vector specifying the values
#'   of `.col_names` columns in rows where the `*_scalar()` function only
#'   returned the `consistency` value.
#' @param ... These dots must be empty.

#' @details The output tibble returned by the factory-made function will inherit
#'   one or two classes independently of the `.name_class` argument:
#' - It will inherit a class named `"scr_{tolower(.name_test)}_map"`; for
#'   example, the class is `"scr_grim_map"` if `.name_test` is `"GRIM"`.
#' - If a `rounding` argument is specified via `...`, or else if `.fun` has a
#'   `rounding` argument with a default, the output tibble will inherit a class
#'   named `"scr_rounding_{rounding}"`; for example,
#'   `"scr_rounding_up_or_down"`.

#' @return A factory-made function with these arguments:
#' - `data`: Data frame with all the columns named in `.reported`. It must
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
#'   `"consistency"`: a logical column showing whether the values to its left
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


# # Example run:
# grim_map2 <- function_map(
#   .fun = grim_scalar,
#   .reported = c("x", "n"),
#   .name_test = "GRIM",
#   .col_names = paste0("V", 1:8),
#   .col_control = "show_rec",
#   .col_filler = NA
# )


function_map <- function(.fun, .reported, .name_test,
                         .name_key_result = "consistency", .name_class = NULL,
                         .args_disabled = NULL, .col_names = NULL,
                         .col_control = NULL, .col_filler = NULL, ...) {

  force(.fun)
  force(.reported)
  force(.name_test)
  force(.name_class)
  force(.name_key_result)
  force(.args_disabled)
  force(.col_names)
  force(.col_control)
  force(.col_filler)

  # Checks ---

  # The dots are only included to prevent a false-positive CRAN warning, so they
  # must not be used:
  rlang::check_dots_empty()

  fun_name <- deparse(substitute(.fun))

  # Check that all `.reported` values are names of arguments in `.fun`:
  offenders <- .reported[!.reported %in% names(formals(.fun))]
  if (length(offenders) > 0L) {
    offenders <- paste0("`", offenders, "`")
    if (length(offenders) == 1L) {
      msg_arg <- "argument"
      msg_it_they <- "It was"
    } else {
      msg_arg <- "arguments"
      msg_it_they <- "They were"
    }
    cli::cli_abort(c(
      "Function `{fun_name}()` lacks {msg_arg} {offenders}.",
      "i" = "{msg_it_they} given as `.reported` in the \\
      `function_map()` call, where `.fun` was specified as `{fun_name}`."
    ))
  }

  code_key_arg_checks <- paste0("!missing(", .reported, ")", collapse = " || ")
  code_key_arg_checks <- rlang::expr({
    if (`!!`(rlang::parse_expr(code_key_arg_checks))) {
      data <- scrutiny::absorb_key_args(data, `!!`(.reported))
    }
  })

  all_classes <- c(paste0("scr_", tolower(.name_test), "_map"), .name_class)

  code_rounding_class <-
    if (any(names(formals(.fun)) == "rounding")) {
      rlang::expr({
        dots <- rlang::enexprs(...)
        if (any(names(dots) == "rounding")) {
          rounding_class <- dots$rounding
        } else {
          rounding_class <- formals(fun)$rounding
        }
        rounding_class <- paste0("scr_rounding_", rounding_class)
      })
    } else {
      rlang::expr({
        rounding_class <- NULL
      })
    }

  # TODO: DEBUG THE `code_col_control` STUFF! TEST IT WITH:
  if (missing(.col_names)) {
    # No unnesting code will be created if this argument is not specified:
    code_col_control <- NULL
    if (!missing(.col_control) || !missing(.col_filler)) {
      cli::cli_warn(c(
        "`.col_control` and `.col_filler` have no effect.",
        "i" = "That's because `.col_names` is not specified."
      ))
    }
  } else {
    # Check that both of the other arguments needed for column unnesting are
    # present:
    if (missing(.col_control) || missing(.col_filler)) {
      cli::cli_abort(
        "`.col_control` and `.col_filler` must be specified \\
        if `.col_names` is."
      )
    }
    check_length(.name_key_result, 1L)
    check_length(.col_control, 1L)
    check_length(.col_filler, 1L)

    # Prepare the code that will be inserted into the factory-made function to
    # unnest the columns that should be named using `.col_names`:1
    code_col_control <- rlang::expr({
      if (all(vapply(consistency, length, integer(1L)) == 1L)) {
        out <- tidyr::unnest(out, cols = consistency)
      } else {
        out$consistency <- lapply(
          out$consistency,
          function(x) {
            if (length(x) == 1L) list(list(x, `!!`(.col_filler))) else x
          }
        )
        out <- scrutiny::unnest_consistency_cols(
          results = out,
          col_names = c("consistency", `!!!`(.col_names)),
          index = FALSE
        )
      }
    })
  }


  # --- Start of the factory-made function, `fn_out()` ---

  fn_out <- rlang::new_function(
    args = rlang::pairlist2(data = , ... = ),
    body = rlang::expr({

      fun <- `!!`(.fun)
      name_class <- `!!`(.name_class)

      add_class <- function(x, new_class) {
        `class<-`(x, value = c(new_class, class(x)))
      }

      # # Manage key columns in `data`, renaming missing columns using the
      # values of key arguments, if necessary:
      `!!!`(code_key_arg_checks)


      # Checks ---

      scrutiny::check_args_disabled(`!!`(.args_disabled))
      scrutiny::check_factory_dots(fun, `!!`(fun_name), ...)
      scrutiny::check_mapper_input_colnames(data, `!!`(.reported), `!!`(.name_test))

      if (!tibble::is_tibble(data)) {
        cli::cli_abort(c(
          "!" = "`data` must be a tibble.",
          "i" = "Convert it with `tibble::as_tibble()`."
        ))
      }

      
      # Main part ---

      # # Divide the data into tested and non-tested columns, going by the key
      # # column names expected from the `reported` argument:
      # data_tested <- data[, `!!`(.reported)]
      # data_non_tested <- data[!colnames(data) %in% `!!`(.reported)]

      # Support rounding classes:
      `!!!`(code_rounding_class)

      all_classes <- c(`!!`(all_classes), rounding_class)

      # Test for consistency:
      data <- data %>%
        dplyr::mutate(
          consistency = purrr::pmap(data[c(`!!!`(.reported))], fun, ...),
          .after = `!!`(.reported[length(.reported)])
        ) %>%
        dplyr::relocate(`!!!`(.reported), consistency) %>%
        add_class(all_classes)

      # consistency <- purrr::pmap(data[, `!!`(.reported)], fun, ...)

      # # Following scrutiny's requirements for mapper functions, `"consistency"`
      # # goes immediately to the right of the key columns (which here are
      # # identical to the tested columns). Any other columns from the input go to
      # # the right of `"consistency"`:
      # out <-
      #   tibble::tibble(data_tested, consistency, data_non_tested) %>%
      #   add_class(c(`!!`(all_classes), rounding_class))

      # The idea here is that `.col_control` might have been specified as a
      # string that is the name of a logical argument which controls whether or
      # not additional columns beyond `"consistency"` are shown. They would have
      # to be extracted from the `*_scalar()` function and initially stored in a
      # `"consistency"` list-column, together with the actual `consistency`
      # value:
      `!!!`(code_col_control)

      # Unquote-splice the code that finalizes `out`. This includes unnesting if
      # the `"consistency"` column has been a list, and renaming it if
      # `.name_key_result` was specified:
      `!!!`(write_code_col_key_result(
        name_key_result = .name_key_result,
        name_data = rlang::expr(data)
      ))

    })
  )


  # --- End of the factory-made function, `fn_out()` ---

  # Garbage collection:
  rm(
    fun_name, code_key_arg_checks, code_rounding_class, code_col_control,
    all_classes, offenders
  )

  # Insert parameters named after the key columns into `fn_out()`, with `NULL`
  # as the default for each. The key columns need to be present in the input
  # data frame. They are expected to have the names specified in `.reported`. If
  # they don't, however, the user can simply specify the key column arguments as
  # the non-quoted names of the columns meant to fulfill these roles:
  insert_key_args(fun = fn_out, reported = .reported)
}

