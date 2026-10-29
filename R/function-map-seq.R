#' @include function-factory-helpers.R seq-predicates.R

# This factory needs a linear sequence with dispersion, which neither
# `disperse()` (pair-based, as in `function_map_total_n()`) nor `base::seq()`
# builds. `seq_disperse_df()` does; `function_map_seq_proto()` uses
# `seq_disperse_df_internal()`, a lightweight version of it.

function_map_seq_proto <- function(
  fn,
  dispersion,
  include_reported,
  name_key_result = "consistency"
) {
  # --- Start of the manufactured helper function ---

  function(data, .var, .out_min, .out_max, .cases = seq_len(nrow(data)), ...) {
    # The step size comes from the caller's `digits_*` argument, not from the
    # values: a mean reported as 5.30 is stored as `5.3`, so `seq_disperse()`
    # would step by 0.1 instead of 0.01, and only in the rows that lost a
    # trailing zero. There is no `digits_n`, so `n` steps by whole numbers.
    by_var <- list(...)[[paste0("digits_", .var)]]
    if (!is.null(by_var)) {
      by_var <- 1 / (10^by_var)
    }

    # One data frame with a dispersed `.var` sequence per value set:
    df_var <- data[.var][[1L]] |>
      lapply(
        seq_disperse_df_internal,
        by = by_var,
        dispersion = dispersion,
        offset_from = 0,
        out_min = .out_min,
        out_max = .out_max,
        include_reported = include_reported,
        track_diff_var = TRUE
      )

    if (length(df_var) == 0L) {
      return(NULL)
    }

    nrow_list_var <- vapply(df_var, nrow, integer(1L), USE.NAMES = FALSE)

    df_var <- df_var |>
      purrr::list_rbind()

    # Everything to the left of the key result column is input to the test:
    cols_for_testing_names <-
      colnames(data)[seq_len(match(name_key_result, colnames(data)) - 1L)]

    cols_for_testing_names_without_var <-
      cols_for_testing_names[cols_for_testing_names != .var]

    # Repeat the other columns to the length of the sequences, insert the
    # dispersed `.var` at its original position, test, and add `diff_var`, the
    # distance from the reported value, and `case`, the row number in the
    # mapper's input. Row indexing also works when `.var` is the only key
    # column, where there is nothing to repeat but the row count.
    data_seq <- data[cols_for_testing_names_without_var]
    data_seq <- data_seq[rep(seq_len(nrow(data_seq)), nrow_list_var), ] |>
      dplyr::mutate(
        {{ .var }} := df_var[[1L]],
        .before = all_of(match(.var, colnames(data)))
      )

    fn(data_seq, ...) |>
      dplyr::mutate(
        diff_var = as_integer_if_lossless(df_var$diff_var),
        case = rep(as.integer(.cases), nrow_list_var)
      )
  }

  # --- End of the manufactured helper function ---
}


#' Create new `*_map_seq()` functions
#'
#' @description `function_map_seq()` creates a sequence mapper from a basic
#'   mapper such as [`grim_map()`]: a function that varies the reported values
#'   a few steps up and down and tests each resulting value set, as
#'   [`grim_map_seq()`] does. By default, only inconsistent value sets are
#'   dispersed.
#'
#'   See `vignette("consistency-tests-in-depth")`, section *Sequence mapper*.
#'
#' @param .fn A basic mapper function such as [`grim_map()`], typically made
#'   by [`function_map()`]. It must return its input data frame with a logical
#'   column named by `.name_key_result` added.
#' @param .var String. The variables that the factory-made function disperses
#'   by default. Defaults to `.reported`.
#' @param .reported String. All variables the factory-made function can
#'   disperse.
#' @param .name_test String (length 1). Plain-text name of the test, such as
#'   `"GRIM"`, for messages.
#' @param .name_key_result (Experimental) String (length 1). Name of the key
#'   result column. Default is `"consistency"`. Must be the value that `.fn`
#'   was created with.
#' @param .name_class String. Optionally, a class to add to the output. Default
#'   is `NULL`.
#' @param .args_disabled String. Optionally, names of arguments of `.fn` that
#'   the factory-made function should reject.
#' @param .dispersion Numeric. Steps up and down from the reported values, in
#'   units of the last decimal place. Default is `1:5`.
#' @param .out_min,.out_max Limits on the dispersed values. A number applies to
#'   every dispersed variable, and `NULL` removes the limit. Default is
#'   `"auto"`, which takes the limit from `.var_bounds`.
#' @param .var_bounds Named list, or `NULL` (the default). Each element is named
#'   after a variable in `.reported` and is a numeric vector of its least and
#'   greatest possible value, with `NA` for an unbounded side, such as `list(n =
#'   c(1, NA), sd = c(0, NA))`. A variable not named here is unbounded, except
#'   for `n`, which always has a minimum of 1. For a bound that depends on a
#'   setting, an element may also be a function that takes the settings `.fn`
#'   recorded for its call, as a named list such as `list(sd_meaning =
#'   "sd_sample")`, and returns such a vector. The bounds only keep impossible
#'   values, such as a sample size of 0, from being tested. They have no
#'   bearing on consistency.
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
#'   The settings of `.fn`, i.e., its arguments after `...`, such as
#'   `rounding`, are arguments of this function as well, with the same
#'   defaults. So are its helper arguments, such as `items_averaged`. Its `...`
#'   pass any other arguments on to `.fn`, which is only of use for a `.fn`
#'   without `...` of its own.
#'
#' @include function-factory-helpers.R
#'
#' @export
#'
#' @section Conventions: Name the factory-made function after `.fn` with
#'   `_seq` appended, as in [`grim_map_seq()`] from [`grim_map()`]. If you add
#'   a class via `.name_class`, name it after the function with the package
#'   name as prefix, as in `scrutiny_grim_map_seq`.
#'
#' @examples
#' # Function definition of `grim_map_seq()`:
#' grim_map_seq <- function_map_seq(
#'   .fn = grim_map,
#'   .reported = c("x", "n"),
#'   .name_test = "GRIM"
#' )

function_map_seq <- function(
  .fn,
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
  force(.fn)
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

  # The dots only prevent a false-positive CRAN warning:
  rlang::check_dots_empty()

  check_args_disabled_unnamed(.args_disabled)
  check_var_bounds(.var_bounds)

  name_fn <- deparse(substitute(.fn))

  # Helper arguments of `.fn`, such as `items_averaged`. The re-tests below
  # read them from columns, so they must not also get them as arguments. A
  # mapper not made by `function_map()` has no such attribute:
  args_helper_fn <- attr(.fn, "scrutiny_args_helper", exact = TRUE)

  # They are arguments of the sequence mapper as well, with `.fn`'s defaults,
  # i.e., none for `items_averaged`:
  helper_formals <- formals(.fn)[args_helper_fn]

  # The settings of `.fn`, such as `rounding`, are arguments of the sequence
  # mapper as well, again with `.fn`'s defaults. A disabled one stays in the
  # dots, where `check_args_disabled()` catches it:
  setting_formals <- formals_settings(
    .fn,
    exclude = c(
      args_helper_fn,
      .args_disabled,
      "var",
      "dispersion",
      "out_min",
      "out_max",
      "include_reported",
      "include_consistent"
    )
  )
  args_setting_fn <- names(setting_formals)

  # `n` holds whole numbers, which print better as integer:
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

  # `digits_x` for `grim_map()`, `digits_x` and `digits_sd` for
  # `grimmer_map()`:
  digits_args_names <- intersect(
    paste0("digits_", .reported[.reported != "n"]),
    names(formals(.fn))
  )
  digits_pairlist_entries <- setNames(
    replicate(length(digits_args_names), NULL, simplify = FALSE),
    digits_args_names
  )

  # The sequence mapper's own settings are recorded next to those of `.fn`,
  # and compared to these defaults:
  defaults_seq <- list(
    var = .var,
    dispersion = .dispersion,
    out_min = .out_min,
    out_max = .out_max,
    include_reported = .include_reported,
    include_consistent = .include_consistent
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
      # reaches `fn()` as missing, which then says how to supply it:
      helper_vals <- list()
      for (arg_helper in `!!`(args_helper_fn)) {
        if (!eval(call("missing", as.name(arg_helper)))) {
          helper_vals[arg_helper] <- list(get(arg_helper))
        }
      }

      # The same for the settings. One that was not given is left out, so that
      # `fn()` applies its own default, which is the same:
      setting_vals <- list()
      for (arg_setting in `!!`(args_setting_fn)) {
        if (!eval(call("missing", as.name(arg_setting)))) {
          setting_vals[arg_setting] <- list(get(arg_setting))
        }
      }

      name_test <- `!!`(.name_test)
      name_fn <- `!!`(name_fn)
      reported <- `!!`(.reported)
      name_class <- `!!`(.name_class)
      name_key_result <- `!!`(.name_key_result)
      args_disabled <- `!!`(.args_disabled)
      fn <- `!!`(.fn)

      # First, before anything reads columns off `data`:
      data <- as_tibble_input(data)

      data <- absorb_key_args(data, reported)

      check_args_disabled(args_disabled)

      check_factory_dots(fn, name_fn, ...)

      # A missing `digits_*` argument is left out, as the helpers above are, so
      # that `fn()` raises its own error about it. `mget()` would force the
      # promise and fail with R's generic message when a wrapper passes it on:
      digits_vals <- list()
      for (arg_digits in `!!`(digits_args_names)) {
        if (!eval(call("missing", as.name(arg_digits)))) {
          digits_vals[arg_digits] <- list(get(arg_digits))
        }
      }

      # Unlike the basic and total-n mappers, sequence mappers need a single
      # `digits_*` value per column: it sets the decimal level that each value
      # is dispersed on, it has to survive the filtering of consistent cases,
      # and it becomes a `digits_*` output column:
      for (digits_name in names(digits_vals)) {
        digits_length <- length(digits_vals[[digits_name]])
        if (digits_length > 1L || anyNA(digits_vals[[digits_name]])) {
          digits_what <- if (digits_length > 1L) {
            paste0("has length ", digits_length)
          } else {
            "is `NA`"
          }
          abort_in_export(
            "`{(digits_name)}` must be a single number here.",
            "x" = "It {(digits_what)}.",
            "i" = "Sequence mappers disperse every value on the decimal level \\
            given by `{(digits_name)}`, so it has to be the same for the whole \\
            column.",
            "i" = "Basic mappers such as `{name_fn}()` do accept one value \\
            per row."
          )
        }
      }

      # `name_test` is required: without it, cli fails to evaluate the message
      # of `check_consistency_not_in_colnames()`:
      check_mapper_input_colnames(data, reported, name_test, name_key_result)

      # Test the reported values. What `fn()` records about the call, such as
      # `rounding`, applies to the re-tests below as well, so it is passed on to
      # the output. The input's column names tell the result columns apart
      # from the rest of `data` below:
      cols_input <- colnames(data)
      data <- do.call(
        fn,
        c(list(data), digits_vals, helper_vals, setting_vals, list(...))
      )
      meta_fn <- attr(data, "scrutiny", exact = TRUE)
      args_fn <- scrutiny_args(data)

      # Mapper and sequence mapper must have the same `.name_key_result`. Catch
      # a mismatch here, not as a `NULL` further down:
      check_key_result_col(data, name_key_result, name_fn)

      # Like `dplyr::filter(data, !consistency)`, but much faster. `which()`
      # drops the `NA` of an undecidable case, which has no values to disperse;
      # with `include_consistent`, too, where `seq_disperse()` would fail on
      # it. `cases` are row numbers in the input data:
      results <- data[[name_key_result]]
      cases <- which(if (include_consistent) !is.na(results) else !results)
      data <- data[cases, ]

      # Only key columns are dispersed; `items_averaged`, for one, is not a
      # rounded value:
      if (!all(var %in% reported)) {
        abort_in_export(
          "`var` must only name the reported columns.",
          "x" = "It includes {wrap_in_backticks(setdiff(var, reported))}.",
          "i" = "These are {wrap_in_backticks(reported)}."
        )
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

      # `var` itself is repeated per row later:
      var_requested <- var

      map_seq_proto <- function_map_seq_proto(
        fn = fn,
        dispersion = dispersion,
        include_reported = include_reported,
        name_key_result = name_key_result
      )

      # Forwarded to `fn()` via `map_seq_proto()`. Helper arguments are not
      # among them: the re-tests read them from the columns that the first
      # `fn()` call returned:
      fn_args <- c(digits_vals, setting_vals, list(...))

      # `out_min` and `out_max` are resolved per variable, since the dispersed
      # variables have different domains. An explicit value applies to every
      # variable:
      var_bounds <- `!!`(.var_bounds)

      # Not `purrr::map()`, which would wrap any error in an indexed-error
      # context ("In index: 1.") that means nothing to the user:
      out <- var |>
        lapply(function(variable) {
          limits <- resolve_var_bounds(
            var = variable,
            out_min = out_min,
            out_max = out_max,
            var_bounds = var_bounds,
            args = args_fn
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
              fn_args
            )
          )
        })

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
        # An app that wants its own wording can catch the warning:
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
        # Still with the usual columns, classes, and attribute, so that
        # `audit_seq()` etc. accept it. The same columns as the re-tests
        # return: the input to the test, i.e. everything left of the key
        # result column, and the columns `fn()` added. Other columns of `data`
        # play no role in the test and are dropped, as the re-tests drop them:
        cols_data <- colnames(data)
        is_tested <- seq_along(cols_data) <= match(name_key_result, cols_data)
        is_result <- !cols_data %in% cols_input
        is_kept <- (is_tested | is_result) &
          !cols_data %in% c("diff_var", "case", "var")
        out <- data[0L, is_kept] |>
          tibble::add_column(diff_var = integer(0L), case = integer(0L)) |>
          list()
        var <- character(0L)
      }

      # One `var` string per row of `out`:
      nrow_out <- vapply(out, nrow, integer(1L), USE.NAMES = FALSE)
      var <- rep(var, nrow_out)

      `!!!`(code_bind_cols)

      # A `digits_*` column per `digits_*` argument given, for
      # `grim_plot_grid()` and the like. Counting decimal places in the numeric
      # values would be unreliable: trailing zeros are lost. If such an
      # argument has a default and was not given, `fn()`'s output decides
      # whether there is a column.
      for (digits_arg in names(digits_vals)) {
        out[[digits_arg]] <- as_integer_if_lossless(digits_vals[[digits_arg]])
      }
      out <- out |>
        dplyr::relocate(
          dplyr::any_of(`!!`(digits_args_names)),
          .before = dplyr::all_of(name_key_result)
        )

      out <- out |>
        add_class(c(
          "scrutiny_map_seq",
          class_name_test(name_test, "_map_seq")
        ))

      if (!is.null(name_class)) {
        out <- out |>
          add_class(name_class)
      }

      # Settings for functions downstream: those of the initial `fn()` call,
      # set here rather than left to survive the binding above, then those of
      # this call, such as `var`, which `audit_seq()` needs when there are no
      # rows to read it off. Then:
      # - `results_reported`: the verdict of every input row, which
      #   `audit_seq()` reports for each case without re-testing.
      # - `name_key_result`: `audit_seq()` has no other way to learn it.
      # - `dispersion_linear`: `audit_seq()` and `reverse_map_seq()` require a
      #   linearly increasing `dispersion`. A single step is trivially linear,
      #   but `is_seq_ascending()` needs two.
      meta_fn$settings <- dplyr::bind_rows(
        meta_fn$settings,
        new_settings(
          list(
            var = var_requested,
            dispersion = dispersion,
            out_min = out_min,
            out_max = out_max,
            include_reported = include_reported,
            include_consistent = include_consistent
          ),
          `!!`(defaults_seq),
          `!!`(name_mapper(.name_test, "_map_seq"))
        )
      )
      attr(out, "scrutiny") <- c(
        meta_fn,
        list(
          results_reported = results,
          name_key_result = name_key_result,
          dispersion_linear = length(dispersion) < 2L ||
            is_seq_ascending(dispersion)
        )
      )

      # `rename = FALSE`: `fn()` already named the column, so only the
      # list-column-to-logical half of this code applies here.
      `!!!`(write_code_col_key_result(.name_key_result, rename = FALSE))
    }),

    # The body calls internal helpers, so the manufactured function needs an
    # environment inheriting from scrutiny's namespace. `rlang::env()` creates a
    # child of the present execution environment, which does.
    env = rlang::env()
  )

  # --- End of the manufactured function, `fn_out()` ---

  # After the `digits_*` arguments, so that the signature starts the same way
  # as the basic mapper's:
  insert_key_args(
    fn_out,
    .reported,
    insert_after = 1L + length(digits_args_names)
  )
}
