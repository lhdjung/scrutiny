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

function_map_seq_proto <- function(
  .fun = fun,
  .var = var,
  .dispersion = dispersion,
  .out_min = out_min,
  .out_max = out_max,
  .include_reported = include_reported,
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

    cols_for_testing_names <-
      colnames(data)[seq_len(match("consistency", colnames(data)) - 1L)]

    # Isolate the columns to be tested that are not the current `var` object:
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

  # Prepare some code to be inserted into the body of the factory-made function.
  # If one of the key (reported) arguments is `n`, this will be whole numbers,
  # so they should be coerced to integer for better representation in an app.
  # However, if there is no such `n` argument, the code should not assume there
  # is, which would lead to an error.
  code_bind_cols <- if (any(.reported == "n")) {
    rlang::expr({
      out <- out |>
        purrr::list_rbind() |>
        dplyr::mutate(var, n = as.integer(n))
    })
  } else {
    rlang::expr({
      out <- out |>
        purrr::list_rbind() |>
        dplyr::mutate(var)
    })
  }

  # Determine which digits_* arguments to expose in the manufactured function.
  # Only those corresponding to non-n reported variables AND present as explicit
  # formals of .fun are added (e.g. digits_x for grim_map, digits_x +
  # digits_sd for grimmer_map, none for debit_map):
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
      args_disabled <- `!!`(.args_disabled)
      fun <- `!!`(.fun)

      # What `data` is comes first, before anything reads columns off it:
      check_tibble(data)

      data <- absorb_key_args(data, reported)

      # Throw an error if the user specified an argument that `.args_disabled`
      # ruled out when the present function was created. (The check used to be
      # missing here -- unlike in `function_map()` -- so disabled arguments were
      # silently passed on to `fun()`.)
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

      # `name_test` is not optional: `check_mapper_input_colnames()` passes it
      # on to `check_consistency_not_in_colnames()`, which names the test in its
      # message. Leaving it out here made a `data` that already has a
      # `consistency` column fail with cli's "Could not evaluate cli `{}`
      # expression: `name_test`" instead of the message that says what is wrong.
      check_mapper_input_colnames(data, reported, name_test)

      # First, basic testing with the `*_map()` function:
      data <- do.call(fun, c(list(data), .digits_vals, list(...)))

      # Remove consistent cases from `data` if only the inconsistent ones are of
      # interest (the default). The "filtering" code below is equivalent to
      # `dplyr::filter(data, !consistency)`, but much faster. `which()` is what
      # makes it equivalent: a case the test could not decide is `NA` here, and
      # indexing rows by `NA` would return a row of `NA`s -- a case to disperse
      # values around that has no values to disperse. It is dropped instead,
      # just like a consistent one, since it is not an inconsistent case.
      if (!include_consistent) {
        data <- data[which(!data$consistency), ]
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
        ...
      )

      # Combine `digits_*` values with any extra `...` arguments so both are
      # forwarded to `map_seq_proto()`, and from there to `fun()`. Helper
      # arguments such as `items` are dropped: their effect is already baked
      # into the `data` that the values are dispersed from -- the initial
      # `fun()` call above multiplied `items` into the `n` column -- so passing
      # them on to the re-tests would apply them twice over:
      .fun_args <- c(.digits_vals, list(...))
      .fun_args <- .fun_args[!names(.fun_args) %in% `!!`(args_helper_fun)]

      # Apply the lower-level function to all user-supplied variables (`var`)
      # and all cases reported in `data`, or at least the inconsistent ones.
      # `out_min` and `out_max` are resolved per variable: the variables being
      # dispersed have different domains, so a single `"auto"` cannot mean the
      # same thing for all of them. An explicit value from the caller applies to
      # every variable, as before:
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
        # The message names the R argument, because this warning is issued to an
        # R session. It used to name it only when `interactive()` was `TRUE` and
        # otherwise tell the user to untick a checkbox in the scrutiny Shiny app
        # -- so a script, an Rmd, or a test run, which are exactly the contexts
        # where no checkbox is on screen, got the checkbox message. An app that
        # wants its own wording should catch the condition and rephrase it:
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

      # Add a digits_* column for each reported variable that `fun()` has a
      # digits_* argument for, so that downstream functions (e.g. grim_plot())
      # can split on decimal-place groups without losing track of which rows
      # belong together. Use the explicitly-provided digits_* value if
      # available; otherwise fall back to decimal_places() on the output
      # column. (The fallback is unreliable for numeric columns with trailing
      # zeros, which is why the digits_* arguments exist in the first place.)
      #
      # Only variables that `fun()` accepts a digits_* argument for get a
      # column. `audit_seq()` forwards every digits_* column in the output back
      # to `fun()` as an argument, so a column that `fun()` has no argument for
      # would make it reject its own output.
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
        .before = "consistency"
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

      # Make sure the "rounding class" (i.e., `"scrutiny_rounding_*"`) has the
      # correct value. As this is not naturally guaranteed as in `*_map()`
      # functions, it must be done by hand. `list(...)` rather than
      # `rlang::enexprs(...)`: the class string needs the argument's *value*.
      # (Capturing the expression only ever worked because the dots promises had
      # already been forced further up; `rounding = some_variable` would
      # otherwise have pasted the variable's name into the class.) `[[` rather
      # than `$`: the latter matches partially on a list, so an argument named
      # `rounding_something` would have been read as `rounding`.
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

      `!!!`(write_code_col_key_result(.name_key_result))
    }),
    # The body calls scrutiny-internal helpers such as `absorb_key_args()` and
    # `function_map_seq_proto()`, so the manufactured function must be enclosed
    # in an environment that inherits from scrutiny's namespace. `rlang::env()`
    # creates a child of the present execution environment, which does.
    env = rlang::env()
  )

  # --- End of the manufactured function, `fn_out()` ---

  # Insert parameters named after the key columns into `fn_out()`, with `NULL`
  # as the default for each. The key columns need to be present in the input
  # data frame. They are expected to have the names specified in `.reported`. If
  # they don't, however, the user can simply specify the key column arguments as
  # the non-quoted names of the columns meant to fulfill these roles. They go
  # after the `digits_*` arguments, which are spliced in right after `data`, so
  # that the sequence mapper's signature starts the same way as the basic
  # mapper's:
  insert_key_args(
    fn_out,
    .reported,
    insert_after = 1L + length(digits_args_names)
  )
}
