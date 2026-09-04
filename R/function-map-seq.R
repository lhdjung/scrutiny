#' @include function-factory-helpers.R seq-predicates.R

# Unlike `function_map_total_n()`, this factory is based on neither `disperse()`
# -- which is pair-based and builds no linear sequence -- nor `seq_endpoint()`
# and friends, which have no dispersion and would have been cumbersome with
# `.include_reported`. It needs both at once, which is what `seq_disperse_df()`
# does. `function_map_seq_proto()` below uses `seq_disperse_df_internal()`, a
# lightweight version of it.

function_map_seq_proto <- function(
  .fun = fun,
  .var = var,
  .dispersion = dispersion,
  .out_min = out_min,
  .out_max = out_max,
  .include_reported = include_reported,
  .name_key_result = "consistency",
  ...
) {
  # --- Start of the manufactured helper (!) function ---

  function(
    data,
    fun = .fun,
    var = .var,
    dispersion = .dispersion,
    out_min = .out_min,
    out_max = .out_max,
    include_reported = .include_reported,
    name_key_result = .name_key_result,
    ...
  ) {
    # The step size has to come from the caller's `digits_*` argument for the
    # current `var`, not from the values themselves. A mean reported as 5.30 is
    # stored as `5.3`, so `seq_disperse()`'s default would step by 0.1 instead
    # of 0.01 -- and it would do so only for the rows that lost a trailing zero,
    # giving different step sizes within a single call. There is no `digits_n`,
    # so dispersing `n` keeps the default (whole numbers).
    .by_var <- list(...)[[paste0("digits_", var)]]
    if (!is.null(.by_var)) {
      .by_var <- 1 / (10^.by_var)
    }

    # Extract the vector from the `data` column specified as `var`, then apply
    # the data-frame-level dispersion function to get a list of data frames with
    # dispersed `var` sequences; one per inconsistent value set:
    df_var <- data[var][[1L]] |>
      lapply(
        seq_disperse_df_internal,
        .by = .by_var,
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

    # Combine the list elements to one single data frame with `var`, `diff_var`,
    # and `case`:
    df_var <- purrr::list_rbind(df_var)

    # Everything to the left of the key result column is input to the test; the
    # column is named `"consistency"` unless the mapper was created with a
    # different `.name_key_result`:
    cols_for_testing_names <-
      colnames(data)[seq_len(match(name_key_result, colnames(data)) - 1L)]

    # Isolate the columns to be tested that are not the current `var` object:
    cols_for_testing_names_without_var <-
      cols_for_testing_names[cols_for_testing_names != var]

    cols_except_last <- seq_along(cols_for_testing_names_without_var)

    # Repeat the non-tested key columns to the length of the dispersed `var`
    # sequences (via list-columns, immediately unnested), insert the dispersed
    # `var` at its original position, test the result with `fun()`, and add
    # `diff_var` -- the distance from the reported value -- and `case`, the row
    # number of the reported `var` value in `data`.
    data[cols_for_testing_names_without_var] |>
      dplyr::mutate(dplyr::across(
        .cols = {{ cols_except_last }},
        .fns = function(x) purrr::map2(x, nrow_list_var, rep)
      )) |>
      tidyr::unnest_longer(col = everything()) |>
      dplyr::mutate(
        {{ var }} := df_var[[1L]],
        .before = all_of(match(var, colnames(data)))
      ) |>
      fun(...) |>
      dplyr::mutate(
        diff_var = df_var$diff_var,
        case = purrr::list_c(
          purrr::map2(nrow_data_seq, nrow_list_var, rep),
          ptype = integer()
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
#'   \href{https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.html#sequence-mapper}{sequence
#'   mapper section} of *Consistency tests in depth*.
#'
#' @param .fun Function such as `grim_map()`, or one made by [`function_map()`]:
#'   It will be used to test columns in a data frame for consistency. Test
#'   results are logical and need to be contained in a column named by
#'   `.name_key_result` -- `"consistency"` by default -- that is added to the
#'   input data frame. This modified data frame is then returned by `.fun`.
#' @param .var String. Variables that will be dispersed by the manufactured
#'   function. Defaults to `.reported`.
#' @param .reported String. All variables the manufactured function can disperse
#'   in principle.
#' @param .name_test String (length 1). The name of the consistency test, such
#'   as `"GRIM"`, to be optionally shown in a message when using the
#'   manufactured function.
#' @param .name_key_result (Experimental) Optionally, a single string that will
#'   be the name of the key result column in the output. Default is
#'   `"consistency"`. It must be the same string that `.fun` was created with:
#'   the manufactured function reads `.fun`'s results off a column of that name.
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
#'   `.out_max`. A number applies to every dispersed variable alike, and `NULL`
#'   removes the limit. Both default to `"auto"`, which takes the limit from
#'   `.var_bounds` -- a different one per variable, since the variables have
#'   different domains.
#' @param .var_bounds Named list, or `NULL` (the default). Each element is named
#'   after a variable in `.reported` and is a numeric vector of length 2, the
#'   least and the greatest value that the variable can take, with `NA` for an
#'   unbounded side. `list(n = c(1, NA), sd = c(0, NA))` says that a sample size
#'   is at least 1 and a standard deviation at least 0, and that neither has an
#'   upper limit. A variable that is not named here is unbounded, except for `n`,
#'   which keeps a minimum of 1 whether it is declared or not.
#'
#'   These bounds only limit the dispersion. They say what values the test could
#'   *conceivably* have been given, so that it is never handed a sample size of
#'   0 or, for DEBIT, a proportion above 1 -- not what it will find consistent.
#' @param .include_reported Logical. Should the reported values themselves be
#'   included in the sequences originating from them? Default is `FALSE` because
#'   this might be redundant and bias the results.
#' @param .include_consistent Logical. Should the function also process
#'   consistent cases (from among those reported), not just inconsistent ones?
#'   Default is `FALSE` because the focus should be on clarifying
#'   inconsistencies.
#' @param ... These dots must be empty.
#'
#' @details All arguments of `function_map_seq()` set the defaults for the
#'   arguments in the manufactured function. They can still be specified
#'   differently when calling the latter.
#'
#'   If functions created this way are exported from other packages, they should
#'   be written as if they were created with
#'   \href{https://purrr.tidyverse.org/reference/faq-adverbs-export.html}{purrr
#'   adverbs}; see explanations there, and examples in the
#'   \href{https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.html#context-and-export}{export
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

#' @section Conventions: The `seq` in `*_map_seq()` is short for the sequences
#'   of candidate values that the manufactured function tests. It builds them
#'   with [`seq_disperse()`]'s internal counterpart, one variable at a time:
#'   each variable in `.var` is dispersed around its own reported value while
#'   the others stay as reported. This is what sets these functions apart from
#'   the `*_map_total_n()` family, which also disperses but varies a pair of
#'   group sizes jointly under a fixed total. It is also why plotting the
#'   output of [`grim_map_seq()`] draws a cross for each reported value set: the
#'   arms are the sequences, and they meet where the reported values are.
#'
#'   The name of a function returned by
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
#'   `scrutiny_grim_map_seq` and `scrutiny_grimmer_map_seq`.

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

function_map_seq <- function(
  .fun,
  .var = Inf,
  .reported,
  .name_test,
  .name_key_result = "consistency",
  .name_class = NULL,
  .args_disabled = NULL,
  .dispersion = 1:5,
  .out_min = "auto",
  .out_max = "auto",
  .var_bounds = NULL,
  .include_reported = FALSE,
  .include_consistent = FALSE,
  ...
) {
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
  force(.var_bounds)
  force(.include_reported)
  force(.include_consistent)

  # The dots are only included to prevent a false-positive CRAN warning, so they
  # must not be used:
  rlang::check_dots_empty()

  check_args_disabled_unnamed(.args_disabled)
  check_var_bounds(.var_bounds)

  name_fun <- deparse(substitute(.fun))

  # Helper-column arguments of `.fun`, such as `items` in `grim_map()`. Their
  # effect is baked into the mapper's output -- `items` is multiplied into the
  # `n` column -- so the re-tests of dispersed values below must not apply them
  # a second time. `function_map()` records them on the mappers it creates; a
  # handwritten mapper has nothing to record, and the attribute is `NULL`:
  args_helper_fun <- attr(.fun, "scrutiny_args_helper", exact = TRUE)

  # An `n` key column holds whole numbers, so coerce it to integer for better
  # display. Only if there is one, though:
  code_bind_cols <- if (any(.reported == "n")) {
    rlang::expr({
      out <- out |>
        purrr::list_rbind() |>
        dplyr::mutate(var, n = as_integer_if_lossless(n))
    })
  } else {
    rlang::expr({
      out <- out |>
        purrr::list_rbind() |>
        dplyr::mutate(var)
    })
  }

  # The `digits_*` arguments to expose: those of non-`n` reported variables that
  # are also formals of `.fun` (`digits_x` for `grim_map()`, `digits_x` and
  # `digits_sd` for `grimmer_map()`):
  digits_args_names <- intersect(
    paste0("digits_", .reported[.reported != "n"]),
    names(formals(.fun))
  )
  digits_pairlist_entries <- setNames(
    replicate(length(digits_args_names), NULL, simplify = FALSE),
    digits_args_names
  )

  # --- Start of the manufactured function, `fn_out()` ---

  fn_out <- rlang::new_function(
    args = rlang::pairlist2(
      data = ,
      !!!digits_pairlist_entries,
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
      name_key_result <- `!!`(.name_key_result)
      args_disabled <- `!!`(.args_disabled)
      fun <- `!!`(.fun)

      # What `data` is comes first, before anything reads columns off it:
      check_tibble(data)

      data <- absorb_key_args(data, reported)

      # Error on an argument that `.args_disabled` ruled out at creation time:
      check_args_disabled(args_disabled)

      check_factory_dots(fun, name_fun, ...)

      # Collect explicitly supplied `digits_*` values while dropping `NULL`
      # defaults so they can be forwarded to `fun()` alongside any extra `...`
      # arguments:
      .digits_vals <- Filter(
        Negate(is.null),
        mget(`!!`(digits_args_names), envir = environment())
      )

      # Unlike the basic and total-n mappers, sequence mappers need a single
      # `digits_*` value per column: it sets the decimal level that each value
      # is dispersed on, it has to survive the filtering of consistent cases,
      # and it becomes a `digits_*` output column. Rather than let a vector
      # produce a confusing error further down, reject it here.
      for (.digits_name in names(.digits_vals)) {
        .digits_length <- length(.digits_vals[[.digits_name]])
        if (.digits_length > 1L) {
          cli::cli_abort(c(
            "`{(.digits_name)}` must be a single number here.",
            "x" = "It has length {(.digits_length)}.",
            "i" = "Sequence mappers disperse every value on the decimal level \\
            given by `{(.digits_name)}`, so it has to be the same for the whole \\
            column.",
            "i" = "Basic mappers such as `{name_fun}()` do accept one value \\
            per row."
          ))
        }
      }

      # `name_test` is not optional: `check_consistency_not_in_colnames()` names
      # the test in its message, and without it cli fails to evaluate that
      # message instead of reporting what is wrong.
      check_mapper_input_colnames(data, reported, name_test, name_key_result)

      # First, basic testing with the `*_map()` function:
      data <- do.call(fun, c(list(data), .digits_vals, list(...)))

      # Everything below reads the key result column off `fun()`'s output by
      # name, so mapper and sequence mapper must have been created with the same
      # `.name_key_result`. Catch a mismatch here, not as a `NULL` further down:
      check_key_result_col(data, name_key_result, name_fun)

      # Equivalent to `dplyr::filter(data, !consistency)`, but much faster.
      # `which()` is what makes it equivalent: indexing by the `NA` of an
      # undecided case would return a row of `NA`s, with no values to disperse.
      # It is dropped instead, not being an inconsistent case.
      if (!include_consistent) {
        data <- data[which(!data[[name_key_result]]), ]
      }

      # As `var` is `Inf` by default, it must be referred to the names of
      # designated `reported` variables:
      if (all(is.infinite(var))) {
        var <- reported
      }

      # Create the lower-level testing function via an internal function
      # factory:
      map_seq_proto <- function_map_seq_proto(
        .fun = fun,
        .name_test = name_test,
        .name_class = name_class,
        .dispersion = dispersion,
        .out_min = out_min,
        .out_max = out_max,
        .include_reported = include_reported,
        .name_key_result = name_key_result,
        ...
      )

      # Forwarded to `map_seq_proto()`, and from there to `fun()`. Helper
      # arguments such as `items` are dropped: the initial `fun()` call above
      # already multiplied `items` into the `n` column that the values are
      # dispersed from, so the re-tests would apply it twice over:
      .fun_args <- c(.digits_vals, list(...))
      .fun_args <- .fun_args[!names(.fun_args) %in% `!!`(args_helper_fun)]

      # Apply the lower-level function to every `var` and every case in `data`.
      # `out_min` and `out_max` are resolved per variable, since the dispersed
      # variables have different domains and a single `"auto"` cannot mean the
      # same for all of them. An explicit value applies to every variable:
      var_bounds <- `!!`(.var_bounds)

      out <- purrr::map(
        var,
        function(.x) {
          .limits <- resolve_var_bounds(
            var = .x,
            out_min = out_min,
            out_max = out_max,
            var_bounds = var_bounds
          )
          do.call(
            map_seq_proto,
            c(
              list(
                data = data,
                var = .x,
                out_min = .limits$out_min,
                out_max = .limits$out_max
              ),
              .fun_args
            )
          )
        }
      )

      # Remove list-elements that are `NULL`, then check for an early return:
      out[vapply(out, is.null, logical(1L))] <- NULL
      if (length(out) == 0L) {
        # The message names the R argument, because this warning is issued to
        # an R session. An app that wants its own wording should catch the
        # condition and rephrase it:
        msg_setting <- "`include_consistent = TRUE`"
        cli::cli_warn(c(
          "!" = "No inconsistent cases to disperse from.",
          "i" = "Try {msg_setting} to disperse from consistent cases, as well."
        ))
        return(tibble::tibble())
      }

      # Repeat the `var` strings so that they form a vector of the length that
      # is the row number of `out`, and that can therefore be added to `out`:
      nrow_out <- vapply(out, nrow, integer(1L), USE.NAMES = FALSE)
      var <- var |>
        purrr::map2(nrow_out, rep) |>
        purrr::list_c(ptype = character())

      # For better output, `out` should be a single data frame; and for
      # identifying the origin of individual rows, `var` is added. See above.
      `!!!`(code_bind_cols)

      # A `digits_*` column per reported variable that `fun()` has a `digits_*`
      # argument for, so that `grim_plot()` and friends can split on
      # decimal-place groups. It takes the argument's value, falling back to
      # `decimal_places()` on the output column -- which is unreliable for lost
      # trailing zeros, the very reason the arguments exist.
      #
      # Only variables `fun()` has such an argument for get a column:
      # `audit_seq()` forwards every `digits_*` column back to `fun()`, which
      # would otherwise reject its own output.
      .digits_col_names <- `!!`(digits_args_names)
      .digits_var_names <- `!!`(sub("^digits_", "", digits_args_names))
      for (.i in seq_along(.digits_col_names)) {
        .digits_arg <- .digits_col_names[.i]
        out[[.digits_arg]] <- if (!is.null(.digits_vals[[.digits_arg]])) {
          .digits_vals[[.digits_arg]]
        } else {
          decimal_places(out[[.digits_var_names[.i]]])
        }
      }
      out <- dplyr::relocate(
        out,
        dplyr::all_of(.digits_col_names),
        .before = dplyr::all_of(name_key_result)
      )

      class_dispersion_ascending <- if (is_seq_ascending(dispersion)) {
        NULL
      } else {
        "scrutiny_map_seq_disp_nonlinear"
      }

      # Create classes that will identify `out` as output of the specific
      # manufactured function:
      classes_seq <- c(
        "scrutiny_map_seq",
        # rounding,
        # classes_fun,
        paste0("scrutiny_", tolower(name_test), "_map_seq"),
        class_dispersion_ascending
      )

      out <- add_class(out, classes_seq)

      # The `"scrutiny_rounding_*"` class is not guaranteed here as it is in
      # `*_map()`, so set it by hand. `list(...)` rather than
      # `rlang::enexprs(...)`, which would yield the argument's *expression* --
      # pasting a variable's name into the class; and `[[` rather than `$`,
      # which matches partially and would read `rounding_something` as
      # `rounding`.
      dots <- list(...)
      if (length(dots[["rounding"]]) > 0L) {
        class(out)[stringr::str_detect(class(out), "^scrutiny_rounding_")] <-
          paste0("scrutiny_rounding_", dots[["rounding"]])
      }

      # Record the arguments that reproduce the test, so that `audit_seq()` can
      # re-run `fun()` on reconstructed data with the very same settings --
      # including those that leave no trace in the output columns, such as
      # `percent`, `threshold`, `symmetric`, or GRIMMER's scale bounds:
      attr(out, "scrutiny_fun_args") <- .fun_args

      # `audit_seq()` reads the test results off a column of `out` and has no
      # other way to learn its name. It falls back to `"consistency"` if
      # subsetting drops this, which is right unless the name was overridden:
      attr(out, "scrutiny_name_key_result") <- name_key_result

      # `rename = FALSE`: `fun()` already named the column, so only the
      # list-column-to-logical half of this code applies here.
      `!!!`(write_code_col_key_result(.name_key_result, rename = FALSE))
    }),
    # The body calls internal helpers, so the manufactured function needs an
    # environment inheriting from scrutiny's namespace. `rlang::env()` creates a
    # child of the present execution environment, which does.
    env = rlang::env()
  )

  # --- End of the manufactured function, `fn_out()` ---

  # Parameters named after the key columns, defaulting to `NULL`, so that users
  # whose columns are not named as `.reported` says can point them at the right
  # ones. They go after the `digits_*` arguments, so that the sequence mapper's
  # signature starts the same way as the basic mapper's:
  insert_key_args(
    fn_out,
    .reported,
    insert_after = 1L + length(digits_args_names)
  )
}
