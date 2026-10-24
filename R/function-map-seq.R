#' @include function-factory-helpers.R seq-predicates.R

# Unlike `function_map_total_n()`, this factory is based on neither `disperse()`
# -- which is pair-based and builds no linear sequence -- nor `base::seq()`,
# which has no dispersion. It needs both at once, which is what `seq_disperse_df()`
# does. `function_map_seq_proto()` below uses `seq_disperse_df_internal()`, a
# lightweight version of it.

function_map_seq_proto <- function(
  fun,
  dispersion,
  include_reported,
  name_key_result = "consistency"
) {
  # --- Start of the manufactured helper (!) function ---

  function(data, .var, .out_min, .out_max, .cases = seq_len(nrow(data)), ...) {
    # The step size has to come from the caller's `digits_*` argument for the
    # current `.var`, not from the values themselves. A mean reported as 5.30 is
    # stored as `5.3`, so `seq_disperse()`'s default would step by 0.1 instead
    # of 0.01 -- and it would do so only for the rows that lost a trailing zero,
    # giving different step sizes within a single call. There is no `digits_n`,
    # so dispersing `n` keeps the default (whole numbers).
    by_var <- list(...)[[paste0("digits_", .var)]]
    if (!is.null(by_var)) {
      by_var <- 1 / (10^by_var)
    }

    # Extract the vector from the `data` column specified as `.var`, then apply
    # the data-frame-level dispersion function to get a list of data frames with
    # dispersed `.var` sequences; one per inconsistent value set:
    df_var <- data[.var][[1L]] |>
      lapply(
        seq_disperse_df_internal,
        by = by_var,
        dispersion = dispersion,
        offset_from = 0,
        out_min = .out_min,
        out_max = .out_max,
        string_output = "auto",
        include_reported = include_reported,
        track_diff_var = TRUE
      )

    if (length(df_var) == 0L) {
      return(NULL)
    }

    nrow_list_var <- vapply(df_var, nrow, integer(1L), USE.NAMES = FALSE)

    # Combine the list elements to one single data frame with `var`, `diff_var`,
    # and `case`:
    df_var <- df_var |>
      purrr::list_rbind()

    # Everything to the left of the key result column is input to the test; the
    # column is named `"consistency"` unless the mapper was created with a
    # different `.name_key_result`:
    cols_for_testing_names <-
      colnames(data)[seq_len(match(name_key_result, colnames(data)) - 1L)]

    # Isolate the columns to be tested that are not the current `.var` object:
    cols_for_testing_names_without_var <-
      cols_for_testing_names[cols_for_testing_names != .var]

    # Repeat the non-tested key columns to the length of the dispersed `.var`
    # sequences, insert the dispersed `.var` at its original position, test the
    # result with `fun()`, and add `diff_var` -- the distance from the reported
    # value -- and `case`, the row number of the reported `.var` value in the
    # mapper's input. Row indexing also works when `.var` is the only key
    # column, where there is nothing to repeat but the row count.
    data_seq <- data[cols_for_testing_names_without_var]
    data_seq <- data_seq[rep(seq_len(nrow(data_seq)), nrow_list_var), ] |>
      dplyr::mutate(
        {{ .var }} := df_var[[1L]],
        .before = all_of(match(.var, colnames(data)))
      )

    fun(data_seq, ...) |>
      dplyr::mutate(
        diff_var = as_integer_if_lossless(df_var$diff_var),
        case = rep(as.integer(.cases), nrow_list_var)
      )
  }

  # --- End of the manufactured helper (!) function ---
}


#' Create new `*_map_seq()` functions
#'
#' @description `function_map_seq()` creates a sequence mapper from a basic
#'   mapper such as [`grim_map()`]: a function that varies the reported values
#'   a few steps up and down and re-tests each resulting value set, as
#'   [`grim_map_seq()`] does. By default, only inconsistent value sets are
#'   dispersed.
#'
#'   See `vignette("consistency-tests-in-depth")`, section *Sequence mapper*.
#'
#' @param .fun A basic mapper function such as [`grim_map()`], typically made
#'   by [`function_map()`]. It must return its input data frame with a logical
#'   column named by `.name_key_result` added.
#' @param .var String. The variables that the factory-made function disperses
#'   by default. Defaults to `.reported`.
#' @param .reported String. All variables the factory-made function can
#'   disperse.
#' @param .name_test String (length 1). Plain-text name of the test, such as
#'   `"GRIM"`, for messages.
#' @param .name_key_result (Experimental) String (length 1). Name of the key
#'   result column. Default is `"consistency"`. Must be the value that `.fun`
#'   was created with.
#' @param .name_class String. Optionally, a class to add to the output. Default
#'   is `NULL`.
#' @param .args_disabled String. Optionally, names of arguments of `.fun` that
#'   the factory-made function should reject.
#' @param .dispersion Numeric. Steps up and down from the reported values, in
#'   units of the last decimal place. Default is `1:5`.
#' @param .out_min,.out_max Limits on the dispersed values. A number applies to
#'   every dispersed variable, and `NULL` removes the limit. Both default to
#'   `"auto"`, which takes the limit from `.var_bounds`.
#' @param .var_bounds Named list, or `NULL` (the default). Each element is named
#'   after a variable in `.reported` and is a numeric vector of its least and
#'   greatest possible value, with `NA` for an unbounded side, such as `list(n =
#'   c(1, NA), sd = c(0, NA))`. A variable not named here is unbounded, except
#'   for `n`, which always has a minimum of 1. The bounds only limit the
#'   dispersion, so that the test never receives an impossible value such as a
#'   sample size of 0. They have no bearing on consistency.
#' @param .include_reported Logical. Should the reported values themselves be
#'   part of the sequences? Default is `FALSE`.
#' @param .include_consistent Logical. Should consistent value sets be
#'   dispersed as well? Default is `FALSE`.
#' @param ... These dots must be empty.
#'
#' @details The arguments of `function_map_seq()` set the defaults of the
#'   corresponding arguments of the factory-made function, which can be
#'   overridden in each call.
#'
#'   Each variable in `var` is dispersed around its own reported value while
#'   the others stay as reported. This is what distinguishes sequence mappers
#'   from the `*_map_total_n()` family, which varies a pair of group sizes
#'   jointly.
#'
#'   To export a factory-made function from another package, follow the
#'   [purrr FAQ on adverbs](https://purrr.tidyverse.org/reference/faq-adverbs-export.html);
#'   see `vignette("consistency-tests-in-depth")`, section *Context and
#'   export*.
#'
#' @return A function such as these:
#'
#'   | \strong{Manufactured function}   | \strong{Dispersed variables}         | \strong{Test vignette}
#'   | ---                              | ---                                  | ---
#'   | [`grim_map_seq()`]               | `"x"`, `"n"`                         | `vignette("grim")`
#'   | [`grimmer_map_seq()`]            | `"x"`, `"sd"`, `"n"`                 | `vignette("grimmer")`
#'   | [`debit_map_seq()`]              | `"x"`, `"sd"`, `"n"`                 | `vignette("debit")`
#'
#'   The settings of `.fun`, i.e., its arguments after `...`, such as
#'   `rounding`, are arguments of this function as well, with the same
#'   defaults. Its `...` pass any other arguments on to `.fun`, which is
#'   only of use for a `.fun` without `...` of its own.
#'
#' @include function-factory-helpers.R
#'
#' @export
#'
#' @section Conventions: Name the factory-made function after `.fun` with
#'   `_seq` appended, as in [`grim_map_seq()`] from [`grim_map()`]. If you add
#'   a class via `.name_class`, name it after the function with the package
#'   name as prefix, as in `scrutiny_grim_map_seq`.
#'
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
  .var = .reported,
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

  # Helper-column arguments of `.fun`, such as `items_averaged` in `grim_map()`.
  # The re-tests of dispersed values below take the helpers as columns of the
  # data they test, so they must not also get them as arguments.
  # `function_map()` records them on the mappers it creates; a handwritten
  # mapper has nothing to record, and the attribute is `NULL`:
  args_helper_fun <- attr(.fun, "scrutiny_args_helper", exact = TRUE)

  # They are arguments of the sequence mapper as well, with the defaults they
  # have in `.fun` -- that is, none for `items_averaged`, which must be given in
  # every call, as an argument or as a column of `data`:
  helper_formals <- formals(.fun)[args_helper_fun]

  # The settings of `.fun`, such as `rounding`, are arguments of the sequence
  # mapper as well, again with `.fun`'s defaults. A disabled one stays in the
  # dots, where `check_args_disabled()` catches it:
  setting_formals <- formals_settings(
    .fun,
    exclude = c(
      args_helper_fun,
      .args_disabled,
      "var",
      "dispersion",
      "out_min",
      "out_max",
      "include_reported",
      "include_consistent"
    )
  )
  args_setting_fun <- names(setting_formals)

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
      # After the key arguments, as in `function_map()`:
      ... = ,
      !!!helper_formals,
      !!!setting_formals,
      var = .var,
      dispersion = .dispersion,
      out_min = .out_min,
      out_max = .out_max,
      include_reported = .include_reported,
      include_consistent = .include_consistent
    ),
    body = rlang::expr({
      # Collect the helper arguments that were given, first thing, before any
      # local variable could shadow one. A missing one is left out, so that it
      # reaches `fun()` as missing, which then says how to supply it:
      helper_vals <- list()
      for (arg_helper in `!!`(args_helper_fun)) {
        if (!eval(call("missing", as.name(arg_helper)))) {
          helper_vals[arg_helper] <- list(get(arg_helper))
        }
      }

      # The same for the settings. One that was not given is left out, so that
      # `fun()` applies its own default, which is the same:
      setting_vals <- list()
      for (arg_setting in `!!`(args_setting_fun)) {
        if (!eval(call("missing", as.name(arg_setting)))) {
          setting_vals[arg_setting] <- list(get(arg_setting))
        }
      }

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

      # Collect the `digits_*` values that were given, to be forwarded to
      # `fun()` alongside any extra `...` arguments. A missing one is left out,
      # as with the helper arguments above, so that `fun()` sees it as missing
      # and raises its own error about it; `mget()` would force the promise and
      # fail with R's generic message when a wrapper passes it along:
      digits_vals <- list()
      for (arg_digits in `!!`(digits_args_names)) {
        if (!eval(call("missing", as.name(arg_digits)))) {
          digits_vals[arg_digits] <- list(get(arg_digits))
        }
      }

      # Unlike the basic and total-n mappers, sequence mappers need a single
      # `digits_*` value per column: it sets the decimal level that each value
      # is dispersed on, it has to survive the filtering of consistent cases,
      # and it becomes a `digits_*` output column. Rather than let a vector
      # produce a confusing error further down, reject it here.
      for (digits_name in names(digits_vals)) {
        digits_length <- length(digits_vals[[digits_name]])
        if (digits_length > 1L || anyNA(digits_vals[[digits_name]])) {
          digits_what <- if (digits_length > 1L) {
            paste0("has length ", digits_length)
          } else {
            "is `NA`"
          }
          cli::cli_abort(c(
            "`{(digits_name)}` must be a single number here.",
            "x" = "It {(digits_what)}.",
            "i" = "Sequence mappers disperse every value on the decimal level \\
            given by `{(digits_name)}`, so it has to be the same for the whole \\
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

      # First, basic testing with the `*_map()` function. What it records about
      # the call, such as `rounding` with defaults resolved, applies to the
      # re-tests below as well, so it is passed on to the output:
      data <- do.call(
        fun,
        c(list(data), digits_vals, helper_vals, setting_vals, list(...))
      )
      meta_fun <- scrutiny_meta(data)

      # Everything below reads the key result column off `fun()`'s output by
      # name, so mapper and sequence mapper must have been created with the same
      # `.name_key_result`. Catch a mismatch here, not as a `NULL` further down:
      check_key_result_col(data, name_key_result, name_fun)

      # Equivalent to `dplyr::filter(data, !consistency)`, but much faster.
      # `which()` is what makes it equivalent: it drops the `NA` of an
      # undecidable case, which has no values to disperse. Such a case is
      # dropped with `include_consistent`, too, where `seq_disperse()` would
      # otherwise fail on its missing value. `cases` are the row numbers of
      # the cases in the input data:
      results <- data[[name_key_result]]
      cases <- which(if (include_consistent) !is.na(results) else !results)
      data <- data[cases, ]

      # Only the key columns are dispersed; `items_averaged`, for one, is not a
      # value that was reported with rounding:
      if (!all(var %in% reported)) {
        cli::cli_abort(c(
          "`var` must only name the reported columns.",
          "x" = "It includes {wrap_in_backticks(setdiff(var, reported))}.",
          "i" = "These are {wrap_in_backticks(reported)}."
        ))
      }

      # A variable named twice would be dispersed twice, and every hit counted
      # twice by `audit_seq()`:
      if (anyDuplicated(var) > 0L) {
        abort_in_export(
          "`var` must not name a column more than once.",
          "x" = "It names {wrap_in_backticks(unique(var[duplicated(var)]))} \
          more than once."
        )
      }

      # Recorded on the output below; `var` itself is repeated per row later:
      var_requested <- var

      # Create the lower-level testing function via an internal function
      # factory:
      map_seq_proto <- function_map_seq_proto(
        fun = fun,
        dispersion = dispersion,
        include_reported = include_reported,
        name_key_result = name_key_result
      )

      # Forwarded to `map_seq_proto()`, and from there to `fun()`. Helper
      # arguments such as `items_averaged` are not among them: the initial
      # `fun()` call above returned them as columns, which the re-tests read
      # them from:
      fun_args <- c(digits_vals, setting_vals, list(...))

      # Apply the lower-level function to every `var` and every case in `data`.
      # `out_min` and `out_max` are resolved per variable, since the dispersed
      # variables have different domains and a single `"auto"` cannot mean the
      # same for all of them. An explicit value applies to every variable:
      var_bounds <- `!!`(.var_bounds)

      # `lapply()` rather than `purrr::map()`, which wraps any error from
      # below in an indexed-error context ("In index: 1.") that means nothing
      # to the user:
      out <- var |>
        lapply(function(variable) {
          limits <- resolve_var_bounds(
            var = variable,
            out_min = out_min,
            out_max = out_max,
            var_bounds = var_bounds
          )
          do.call(
            map_seq_proto,
            c(
              list(
                data = data,
                .var = variable,
                .cases = cases,
                .out_min = limits$out_min,
                .out_max = limits$out_max
              ),
              fun_args
            )
          )
        })

      # Remove list-elements that are `NULL`, then check for an early return:
      out[vapply(out, is.null, logical(1L))] <- NULL

      # Cases to disperse from, but no values to disperse to, e.g. with
      # `dispersion = 0` and `include_reported = FALSE`:
      if (
        length(out) > 0L &&
          all(vapply(out, nrow, integer(1L), USE.NAMES = FALSE) == 0L)
      ) {
        cli::cli_warn(c(
          "!" = "No values to disperse to.",
          "i" = "`dispersion`, `out_min`, and `out_max` leave no value \
          around the reported ones."
        ))
      }

      if (length(out) == 0L) {
        # The message names the R argument, because this warning is issued to
        # an R session. An app that wants its own wording should catch the
        # condition and rephrase it:
        msg_setting <- "`include_consistent = TRUE`"
        n_undecidable <- sum(is.na(results))
        cli::cli_warn(c(
          "!" = if (include_consistent) {
            "No cases to disperse from."
          } else {
            "No inconsistent cases to disperse from."
          },
          "x" = if (n_undecidable > 0L) {
            "{n_undecidable} case{?s} could not be tested, e.g. for a \\
            missing value."
          },
          "i" = if (!include_consistent) {
            "Try {msg_setting} to disperse from consistent cases, as well."
          }
        ))
        # The output still has the columns, classes, and attribute of any other,
        # so that functions downstream such as `audit_seq()` accept it. Columns
        # of `data` named like the result columns are dropped, as the re-tests
        # drop all columns that play no role in the test:
        out <- data[0L, !colnames(data) %in% c("diff_var", "case", "var")] |>
          tibble::add_column(diff_var = integer(0L), case = integer(0L)) |>
          list()
        var <- character(0L)
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
      # decimal-place groups. It takes the argument's value. A `fun()` whose
      # `digits_*` argument has a default may not be given one; its own output
      # then decides whether there is such a column. Guessing the count from
      # the numeric values would be unreliable: trailing zeros are lost.
      #
      # Only variables `fun()` has such an argument for get a column:
      # `audit_seq()` forwards every `digits_*` column back to `fun()`, which
      # would otherwise reject its own output.
      for (digits_arg in names(digits_vals)) {
        out[[digits_arg]] <- digits_vals[[digits_arg]]
      }
      out <- out |>
        dplyr::relocate(
          dplyr::any_of(`!!`(digits_args_names)),
          .before = dplyr::all_of(name_key_result)
        )

      # Create classes that will identify `out` as output of the specific
      # manufactured function:
      out <- out |>
        add_class(c(
          "scrutiny_map_seq",
          class_name_test(name_test, "_map_seq")
        ))

      # `.name_class` used to go into the dots of `function_map_seq_proto()`,
      # which ignores them, so the class was never added:
      if (!is.null(name_class)) {
        out <- out |>
          add_class(name_class)
      }

      # Settings for functions downstream: those that the initial `fun()` call
      # recorded, such as the `rounding` that `grim_plot()` reads -- set here
      # rather than left to survive the binding of `fun()`'s outputs above --
      # and these:
      # - `fun_args`: the arguments that reproduce the test, so that
      #   `audit_seq()` can re-run `fun()` on reconstructed data with the very
      #   same settings -- including those that leave no trace in the output
      #   columns, such as `percent`, `threshold`, or `symmetric`.
      # - `name_key_result`: `audit_seq()` reads the test results off a column
      #   of `out` and has no other way to learn its name.
      # - `dispersion_linear`: `audit_seq()` and `reverse_map_seq()` only work
      #   with a linearly increasing `dispersion`. A single step is trivially
      #   linear, but `is_seq_ascending()` needs two.
      # - `var`: the dispersed variables, which `audit_seq()` can't read off
      #   the `var` column when there are no rows.
      attr(out, "scrutiny") <- c(
        meta_fun,
        list(
          fun_args = fun_args,
          name_key_result = name_key_result,
          var = var_requested,
          dispersion_linear = length(dispersion) < 2L ||
            is_seq_ascending(dispersion)
        )
      )

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
  # ones. They go after the `digits_*` arguments and before the helper
  # arguments, so that the sequence mapper's signature starts the same way as
  # the basic mapper's:
  insert_key_args(
    fn_out,
    .reported,
    insert_after = 1L + length(digits_args_names)
  )
}
