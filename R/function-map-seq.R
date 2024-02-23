
#' @include function-factory-helpers.R seq-predicates.R

# Background on helpers and implementation: Unlike `function_map_total_n()`, the
# main function here -- `function_map_seq()` -- is not based on `disperse()` or
# its derivatives. It is not based on `seq_endpoint()` or friends either, which
# is surprising but necessary: `disperse()` is pair-based and does not construct
# a linear sequence, whereas `seq_endpoint()` and friends lack support for
# dispersion and would have been very cumbersome with regard to the
# `.include_reported` argument. It became clear that I needed something new -- a
# function that would perform dispersion while still producing linear output. I
# wrote `seq_disperse()` and `seq_disperse_df()`, and I applied the latter
# within the internal helper function factory below, `function_map_seq_proto()`.
# Later on, I wrote `seq_disperse_df_internal()` as a lightweight internal
# helper to replace `seq_disperse_df()` within `function_map_seq_proto()` in
# order to improve performance.


function_map_seq_proto <- function(.fun = fun, .var = var,
                                   .dispersion = dispersion,
                                   .out_min = out_min, .out_max = out_max,
                                   .include_reported = include_reported, ...) {

  # --- Start of the manufactured helper (!) function ---

  function(data, fun = .fun, var = .var, dispersion = .dispersion,
           out_min = .out_min, out_max = .out_max,
           include_reported = .include_reported, ...) {

    # Extract the vector from the `data` column specified as `var`, then apply
    # the data-frame-level dispersion function to get a list of data frames with
    # dispersed `var` sequences; one per inconsistent value set:
    df_var <- data[var][[1L]] %>%
      lapply(
        seq_disperse_df_internal,
        .dispersion = dispersion,
        .offset_from = 0,
        .out_min = out_min,
        .out_max = out_max,
        .string_output = "auto",
        .include_reported = include_reported,
        .track_diff_var = TRUE
      )

    if (length(df_var) == 0L) {
      return(NULL)
    }

    nrow_list_var <- vapply(df_var, nrow, integer(1L), USE.NAMES = FALSE)
    nrow_data_seq <- seq_along(nrow_list_var)

    # Combine the list elements to one single data frame with `var`,
    # `diff_var`, and `case`:
    df_var <- dplyr::bind_rows(df_var)

    cols_for_testing_names <-
      colnames(data)[1L:match("consistency", colnames(data)) - 1L]

    cols_for_testing_names_without_var <-
      cols_for_testing_names[cols_for_testing_names != var]

    cols_except_last <- seq_along(cols_for_testing_names_without_var)

    # Repeat the vector(s) non-tested key argument names so that they are just
    # as long as the dispersed `var` sequences -- and hence fit together as rows
    # of the same data frame. This returns list-columns, which are immediately
    # unnested. Next, the dispersed `var` sequences are added at the appropriate
    # position, so that the key columns are in the same order as in `data`.
    # These key columns with partially dispersed values are then tested for
    # consistency using `fun()`. Finally, the last columns are added:
    # `diff_var`, which captures the distance between the reported and the
    # original values in `var`; and `case`, which records the row number of the
    # reported `var` value in `data`.
    data[cols_for_testing_names_without_var] %>%
      dplyr::mutate(dplyr::across(
        .cols = {{ cols_except_last }},
        .fns = function(x) purrr::map2(x, nrow_list_var, rep)
      )) %>%
      tidyr::unnest_longer(col = everything()) %>%
      dplyr::mutate(
        {{ var }} := df_var[[1L]],
        .before = all_of(match(var, colnames(data)))
      ) %>%
      fun(...) %>%
      dplyr::mutate(
        diff_var = df_var$diff_var,
        case = unlist(
          purrr::map2(nrow_data_seq, nrow_list_var, rep),
          use.names = FALSE
        )
      )

  }

  # --- End of the manufactured helper (!) function ---

}



#' Create new `*_map_seq()` functions
#'
#' @description `function_map_seq()` is the engine that powers functions such as
#'   [`grim_map_seq()`]. It creates new, "factory-made" functions that apply
#'   consistency tests such as GRIM or GRIMMER to sequences of specified
#'   variables. The sequences are centered around the reported values of those
#'   variables.
#'
#'   By default, only inconsistent values are dispersed from and tested. This
#'   provides an easy and powerful way to assess whether small errors in
#'   computing or reporting may be responsible for inconsistencies in published
#'   statistics.
#'
#'   For background and more examples, see the
#'   \href{https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#sequence-mapper}{sequence
#'   mapper section} of *Consistency tests in depth*.
#'
#' @param .fun Function such as `grim_map()`, or one made by [`function_map()`]:
#'   It will be used to test columns in a data frame for consistency. Test
#'   results are logical and need to be contained in a column called
#'   `"consistency"` that is added to the input data frame. This modified data
#'   frame is then returned by `.fun`.
#' @param .var String. Variables that will be dispersed by the manufactured
#'   function. Defaults to `.reported`.
#' @param .reported String. All variables the manufactured function can disperse
#'   in principle.
#' @param .name_test String (length 1). The name of the consistency test, such
#'   as `"GRIM"`, to be optionally shown in a message when using the
#'   manufactured function.
#' @param .name_class String. If specified, the tibbles returned by the
#'   manufactured function will inherit this string as an S3 class. Default is
#'   `NULL`, i.e., no extra class.
#' @param .args_disabled String. Optionally, names of the basic `*_map()`
#'   function's arguments. These arguments will throw an error if specified when
#'   calling the factory-made function.
#' @param .dispersion Numeric. Sequence with steps up and down from the reported
#'   values. It will be adjusted to these values' decimal level. For example,
#'   with a reported `8.34`, the step size is `0.01`. Default is `1:5`, for five
#'   steps up and down.
#' @param .out_min,.out_max If specified when calling a factory-made function,
#'   output will be restricted so that it's not below `.out_min` or above
#'   `.out_max`. Defaults are `"auto"` for `.out_min`, i.e., a minimum of one
#'   decimal unit above zero; and `NULL` for `.out_max`, i.e., no maximum.
#' @param .include_reported Logical. Should the reported values themselves be
#'   included in the sequences originating from them? Default is `FALSE` because
#'   this might be redundant and bias the results.
#' @param .include_consistent Logical. Should the function also process
#'   consistent cases (from among those reported), not just inconsistent ones?
#'   Default is `FALSE` because the focus should be on clarifying
#'   inconsistencies.
#' @param ... These dots must be empty.
#'
#' @inheritParams function_map
#'
#' @details All arguments of `function_map_seq()` set the defaults for the
#'   arguments in the manufactured function. They can still be specified
#'   differently when calling the latter.
#'
#'   If functions created this way are exported from other packages, they should
#'   be written as if they were created with
#'   \href{https://purrr.tidyverse.org/reference/faq-adverbs-export.html}{purrr
#'   adverbs}; see explanations there, and examples in the
#'   \href{https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#context-and-export}{export
#'   section} of *Consistency tests in depth*.
#'
#'   This function is a so-called function factory: It produces other functions,
#'   such as [`grim_map_seq()`]. More specifically, it is a function operator
#'   because it also takes functions as inputs, such as [`grim_map()`]. See
#'   Wickham (2019, ch. 10-11).

#' @return A function such as those below. ("Testable statistics" are variables
#'   that can be selected via `var`, and are then varied. All variables except
#'   for those in parentheses are selected by default.)
#'
#'   | \strong{Manufactured function}   | \strong{Testable statistics}         | \strong{Test vignette}
#'   | ---                              | ---                                  | ---
#'   | [`grim_map_seq()`]               | `"x"`, `"n"`, (`"items"`)            | `vignette("grim")`
#'   | [`grimmer_map_seq()`]            | `"x"`, `"sd"`, `"n"`, (`"items"`)    | `vignette("grimmer")`
#'   | [`debit_map_seq()`]              | `"x"`, `"sd"`, `"n"`                 | `vignette("debit")`
#'
#'   The factory-made function will also have dots, `...`, to pass arguments
#'   down to `.fun`, i.e., the basic mapper function such as `grim_map()`.

#' @include function-factory-helpers.R

#' @export

#' @section Conventions: The name of a function returned by
#'   `function_map_seq()` should mechanically follow from that of
#'   the input function. For example, [`grim_map_seq()`] derives
#'   from [`grim_map()`]. This pattern fits best if the input function itself
#'   is named after the test it performs on a data frame, followed by `_map`:
#'   [`grim_map()`] applies GRIM, [`grimmer_map()`] applies GRIMMER, etc.
#'
#'   Much the same is true for the classes of data frames returned by the
#'   manufactured function via the `.name_class` argument of
#'   `function_map_seq()`. It should be the function's own name preceded
#'   by the name of the package that contains it, or by an acronym of that
#'   package's name. Therefore, some existing classes are
#'   `scr_grim_map_seq` and `scr_grimmer_map_seq`.

#' @references Wickham, H. (2019). *Advanced R* (Second Edition). CRC
#'   Press/Taylor and Francis Group. https://adv-r.hadley.nz/index.html

#' @examples
#' # Function definition of `grim_map_seq()`:
#' grim_map_seq <- function_map_seq(
#'   .fun = grim_map,
#'   .reported = c("x", "n"),
#'   .name_test = "GRIM",
#' )


# For full example inputs (and connected unit tests), see: grim-map-seq.R

function_map_seq <- function(.fun, .var = Inf, .reported, .name_test,
                             .name_key_result = "consistency",
                             .name_class = NULL, .args_disabled = NULL,
                             .dispersion = 1:5,
                             .out_min = "auto", .out_max = NULL,
                             .include_reported = FALSE,
                             .include_consistent = FALSE, ...) {

  force(.fun)
  force(.var)
  force(.reported)
  force(.name_test)
  force(.name_key_result)
  force(.name_class)
  force(.args_disabled)
  force(.dispersion)
  force(.out_min)
  force(.out_max)
  force(.include_reported)
  force(.include_consistent)

  # The dots are only included to prevent a false-positive CRAN warning, so they
  # must not be used:
  rlang::check_dots_empty()

  check_args_disabled_unnamed(.args_disabled)

  name_fun <- deparse(substitute(.fun))

  # Prepare some code to be inserted into the body of the factory-made function.
  # If one of the key (reported) arguments is `n`, this will be whole numbers,
  # so they should be coerced to integer for better representation in an app.
  # However, if there is no such `n` argument, the code should not assume there
  # is, which would lead to an error.
  code_bind_cols <- if (any(.reported == "n")) {
    rlang::expr({
      out <- out %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(var, n = as.integer(n))
    })
  } else {
    rlang::expr({
      out <- out %>%
        dplyr::bind_rows() %>%
        dplyr::mutate(var)
    })
  }


  # --- Start of the manufactured function, `fn_out()` ---

  fn_out <- rlang::new_function(
    args = rlang::pairlist2(
      data = ,
      var = .var,
      dispersion = .dispersion,
      out_min = .out_min,
      out_max = .out_max,
      include_reported = .include_reported,
      include_consistent = .include_consistent,
      ... =
    ),
    body = rlang::expr({

      name_test <- `!!`(.name_test)
      name_fun <- `!!`(name_fun)
      reported <- `!!`(.reported)
      name_class <- `!!`(.name_class)
      args_disabled <- `!!`(.args_disabled)
      fun <- `!!`(.fun)


      data <- absorb_key_args(data, reported)

      check_factory_dots(fun, name_fun, ...)

      args_excluded <- c(reported, args_disabled)

      arg_list <- call_arg_list()
      arg_list <- arg_list[!names(arg_list) %in% args_excluded]

      check_mapper_input_colnames(data, reported)
      check_consistency_not_in_colnames(data, name_test)
      check_tibble(data)

      # First, basic testing with the `*_map()` function:
      data <- fun(data, ...)

      # Remove consistent cases from `data` if only the inconsistent ones are of
      # interest (the default). The "filtering" code below is equivalent to
      # `dplyr::filter(data, !consistency)`, but much faster.
      if (!include_consistent) {
        data <- data[!data$consistency, ]
      }

      # As `var` is `Inf` by default, it must be referred to the names of
      # designated `reported` variables:
      if (all(is.infinite(var))) {
        var <- reported
      }

      # Create the lower-level testing function via an internal function factory:
      map_seq_proto <- function_map_seq_proto(
        .fun = fun,
        .name_test = name_test,
        .name_class = name_class,
        .dispersion = dispersion,
        .out_min = out_min,
        .out_max = out_max,
        .include_reported = include_reported,
        ...
      )

      # Apply the lower-level function to all user-supplied variables (`var`) and
      # all cases reported in `data`, or at least the inconsistent ones:
      out <- purrr::map(var, ~ map_seq_proto(data = data, var = .x))

      # Remove list-elements that are `NULL`, then check for an early return:
      out[vapply(out, is.null, logical(1L))] <- NULL
      if (length(out) == 0L) {
        msg_setting <- if (interactive()) {
          "`include_consistent = TRUE`"
        } else {
          "unchecking \"Inconsistent cases only\""
        }
        cli::cli_warn(c(
          "!" = "No inconsistent cases to disperse from.",
          "i" = "Try {msg_setting} to disperse from consistent cases, as well."
        ))
        return(tibble::tibble())
      }

      # Repeat the `var` strings so that they form a vector of the length that is
      # the row number of `out`, and that can therefore be added to `out`:
      nrow_out <- vapply(out, nrow, integer(1L), USE.NAMES = FALSE)
      var <- var %>%
        purrr::map2(nrow_out, rep) %>%
        unlist(use.names = FALSE)

      # For better output, `out` should be a single data frame; and for
      # identifying the origin of individual rows, `var` is added. See above.
      `!!!`(code_bind_cols)

      class_dispersion_ascending <- if (is_seq_ascending(dispersion)) {
        NULL
      } else {
        "scr_map_seq_disp_nonlinear"
      }

      # Create classes that will identify `out` as output of the specific
      # manufactured function:
      classes_seq <- c(
        "scr_map_seq",
        # rounding,
        # classes_fun,
        paste0("scr_", tolower(name_test), "_map_seq"),
        class_dispersion_ascending
      )

      out <- add_class(out, classes_seq)

      # Make sure the "rounding class" (i.e., `"scr_rounding_*"`) has the correct
      # value. As this is not naturally guaranteed as in `*_map()` functions, it
      # must be done by hand:
      dots <- rlang::enexprs(...)
      if (length(dots$rounding) > 0L) {
        class(out)[stringr::str_detect(class(out), "^scr_rounding_")] <-
          paste0("scr_rounding_", dots$rounding)
      }

      `!!!`(write_code_col_key_result(.name_key_result))
    }),
    env = rlang::env()
  )


  # --- End of the manufactured function, `fn_out()` ---

  # Insert parameters named after the key columns into `fn_out()`, with `NULL`
  # as the default for each. The key columns need to be present in the input
  # data frame. They are expected to have the names specified in `.reported`. If
  # they don't, however, the user can simply specify the key column arguments as
  # the non-quoted names of the columns meant to fulfill these roles:
  insert_key_args(fn_out, .reported)
}

