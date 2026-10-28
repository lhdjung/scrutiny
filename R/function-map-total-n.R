# Is each pair of rows, i.e., each pair of groups, consistent?
mutate_both_consistent <- function(data, name_key_result = "consistency") {
  both_consistent <- data[[name_key_result]] |>
    split_into_groups(group_size = 2) |>
    vapply(all, logical(1L), USE.NAMES = FALSE) |>
    rep(each = 2L)

  data |>
    dplyr::mutate(both_consistent, .after = dplyr::all_of(name_key_result))
}


# Tests one direction of group pairing; called twice by `function_map_total_n()`:
function_map_total_n_proto <- function(
  fn,
  name_key_result = "consistency",
  name_fn = "the mapper"
) {
  function(
    data,
    .reported,
    .reported_orig,
    .dir,
    .dispersion,
    .n_min,
    .n_max,
    .constant,
    ...
  ) {
    reported_n_cols <- ncol(.reported)
    reported_n_vars <- reported_n_cols / 2

    df_list <- data$n |>
      purrr::map(function(n) {
        disperse_total(
          n,
          dispersion = .dispersion,
          n_min = .n_min,
          n_max = .n_max,
          constant = .constant
        )
      })

    # How often the reported values need to be repeated:
    df_list_nrow <- vapply(df_list, nrow, integer(1L), USE.NAMES = FALSE)

    out_df_nested <- .reported |>
      split_into_rows() |>
      purrr::map(function(x) split_into_groups(x, group_size = 2)) |>
      tibble::tibble(.name_repair = function(x) "reported") |>
      dplyr::mutate(
        df_list,
        times = df_list_nrow / 2,
        reported = purrr::map2(
          .data$reported,
          times,
          function(x, y) purrr::map2(x, y, rep)
        )
      ) |>
      tidyr::unnest_wider("reported") |>
      dplyr::rename_with(
        .fn = function(x) .reported_orig,
        .cols = 1L:dplyr::all_of(reported_n_vars)
      ) |>
      dplyr::mutate(
        dplyr::across(
          .cols = 1L:dplyr::all_of(reported_n_vars),
          .fns = function(x) purrr::map(x, tibble::as_tibble)
        ),
        times = NULL
      )

    # Name the nested tibbles after the reported variables, e.g., `c("x", "sd")`:
    out_df_nested[1L:reported_n_vars] <-
      out_df_nested[1L:reported_n_vars] |>
      tidyr::pivot_longer(cols = everything()) |>
      dplyr::mutate(value = purrr::map2(value, name, setNames)) |>
      tidyr::pivot_wider(
        names_from = name,
        values_from = value,
        values_fn = list
      ) |>
      tidyr::unnest(cols = everything())

    # The row of `.reported` that each scenario belongs to:
    case <- rep(seq_along(df_list_nrow), df_list_nrow)

    out_df <- out_df_nested |>
      tidyr::unnest(cols = everything())
    n_change <- out_df$n_change
    colnames(out_df)[seq_along(.reported_orig)] <- .reported_orig

    # With no pair of group sizes left, the reported columns unnest as logical.
    # Give them their input types back (`.reported` has `x1, x2, sd1, ...`):
    if (nrow(out_df) == 0L) {
      out_df[.reported_orig] <-
        .reported[seq(1L, by = 2L, length.out = reported_n_vars)] |>
        lapply(function(col) col[0L])
    }

    # A `digits_*` argument may name one value per group: `digits_x = c(2, 1)`
    # if `x1` has two decimal places and `x2` one. `out_df` lists the groups in
    # alternating rows, so the vector is recycled across it, one value per row.
    # The `"back"` direction pairs the groups the other way round, so the digits
    # are swapped with them.
    dots <- list(...)
    names_dots <- names(dots)
    if (is.null(names_dots)) {
      names_dots <- rep("", length(dots))
    }
    for (i in which(startsWith(names_dots, "digits_"))) {
      if (length(dots[[i]]) > 1L) {
        digits_i <- dots[[i]]
        if (length(digits_i) != 2L) {
          abort_in_export(
            "`{names_dots[i]}` must have length 1 or 2.",
            "x" = "It has length {length(digits_i)}.",
            "i" = "There are two groups, so a length-2 vector states the \\
            number of decimal places for each of them. A single number \\
            applies to both.",
            "i" = "The two values apply to every row of `data`; the groups \\
            can't have different numbers of decimal places per row."
          )
        }
        if (as.character(.dir) == "back") {
          digits_i <- rev(digits_i)
        }
        dots[[i]] <- rep(digits_i, length.out = nrow(out_df))
      }
    }

    # Other columns of `data`, such as `items_averaged`, go along with each
    # case; `fn()` needs helper columns and returns the others as they are:
    for (name in setdiff(colnames(data), c(colnames(.reported), "n"))) {
      out_df[[name]] <- data[[name]][case]
    }

    # A row of `out_df` is a scenario, not a row of the user's `data`, so an
    # error names the case it belongs to (see `rethrow_row_error()`):
    out_df <- withCallingHandlers(
      do.call(fn, c(list(out_df), dots)),
      error = function(cnd) {
        if (is.null(cnd$scrutiny_row)) {
          return()
        }
        cnd$body[length(cnd$body)] <- paste0(
          "In row ",
          case[cnd$scrutiny_row],
          " of `data`."
        )
        cnd$scrutiny_row <- NULL
        rlang::cnd_signal(cnd)
      }
    )

    check_key_result_col(out_df, name_key_result, name_fn)

    if (!any("n_change" == colnames(out_df))) {
      out_df <- out_df |>
        dplyr::mutate(n_change)
    }

    out_df <- out_df |>
      mutate_both_consistent(name_key_result) |>
      dplyr::mutate(case, dir = .dir, n = as_integer_if_lossless(n)) |>
      dplyr::relocate(n_change, .after = n)

    # An even total splits into two equal groups at a `dispersion` step of 0,
    # where pairing the groups the other way round gives the same scenario. It
    # is tested in the `"forth"` direction, so it is dropped here rather than
    # tested and counted twice:
    if (as.character(.dir) == "back") {
      repeated <- out_df$n_change == 0L & 2L * out_df$n == data$n[out_df$case]
      out_df <- out_df[!repeated, ]
    }

    out_df
  }

  # --- End of the manufactured function ---
}


#' Create new `*_map_total_n()` functions
#'
#' @description `function_map_total_n()` creates a total-n mapper from a basic
#'   mapper such as [`grim_map()`]: a function for two groups whose statistics
#'   are reported with the total sample size but without the group sizes, as
#'   [`grim_map_total_n()`] is. It uses [`disperse_total()`] to create pairs of
#'   hypothetical group sizes that add up to the total, and tests the reported
#'   statistics against each pair.
#'
#'   See `vignette("consistency-tests-in-depth")`, section *Total-n mapper*.
#'
#' @param .fn A basic mapper function such as [`grim_map()`], typically made
#'   by [`function_map()`]. It must return its input data frame with a logical
#'   column named by `.name_key_result` added.
#' @param .reported String. Names of the group-specific statistics reported
#'   alongside the total, such as `"x"` for GRIM and `c("x", "sd")` for
#'   DEBIT. In the input of the factory-made function, each appears as two
#'   columns with the suffixes `1` and `2`, such as `x1` and `x2`.
#' @param .name_test String (length 1). Plain-text name of the test, such as
#'   `"GRIM"`, for messages.
#' @param .name_key_result (Experimental) String (length 1). Name of the key
#'   result column. Default is `"consistency"`. Must be the value that `.fn`
#'   was created with.
#' @param .name_class String. Optionally, a class to add to the output. Default
#'   is `NULL`.
#' @param .dispersion,.n_min,.n_max,.constant Passed on to
#'   [`disperse_total()`], with the defaults from there. `.n_min` may also be
#'   a function of the settings of `.fn`, as a named list, that returns the
#'   default `n_min` for a call; the manufactured function's `n_min` then
#'   defaults to `NULL`, which stands for that value.
#' @param ... These dots must be empty.
#'
#' @details To export a factory-made function from another package, follow the
#'   [purrr FAQ on adverbs](https://purrr.tidyverse.org/reference/faq-adverbs-export.html);
#'   see `vignette("consistency-tests-in-depth")`, section *Context and
#'   export*.
#'
#' @references Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It
#'   Light or Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://journals.sagepub.com/doi/10.1177/09567976211058727
#'
#' @return A function such as these:
#'
#'   | \strong{Manufactured function}   | \strong{Reported statistics}  | \strong{Test vignette}
#'   | ---                              | ---                           | ---
#'   | [`grim_map_total_n()`]           | `"x"`                         | `vignette("grim")`
#'   | [`grimmer_map_total_n()`]        | `"x"`, `"sd"`                 | `vignette("grimmer")`
#'   | [`debit_map_total_n()`]          | `"x"`, `"sd"`                 | `vignette("debit")`
#'
#'   The settings of `.fn`, i.e., its arguments after `...`, such as
#'   `rounding`, are arguments of this function as well, with the same
#'   defaults. So are its helper arguments, such as `items_averaged`. Its `...`
#'   pass any other arguments on to `.fn`, which is only of use for a `.fn`
#'   without `...` of its own.
#'
#' @section Conventions: Name the factory-made function after `.fn` with
#'   `_total_n` appended, as in [`grim_map_total_n()`] from [`grim_map()`]. If
#'   you add a class via `.name_class`, name it after the function with the
#'   package name as prefix, as in `scrutiny_grim_map_total_n`.
#'
#' @seealso [`function_map_seq()`]
#'
#' @include utils.R disperse.R function-factory-helpers.R
#'
#' @export
#'
#' @examples
#' # Function definition of `grim_map_total_n()`:
#' grim_map_total_n <- function_map_total_n(
#'   .fn = grim_map,
#'   .reported = "x",
#'   .name_test = "GRIM"
#' )

function_map_total_n <- function(
  .fn,
  .reported,
  .name_test,
  .name_key_result = "consistency",
  .name_class = NULL,
  .dispersion = 0:5,
  .n_min = 1L,
  .n_max = NULL,
  .constant = NULL,
  ...
) {
  force(.fn)
  force(.reported)
  force(.name_test)
  force(.name_key_result)
  force(.name_class)
  force(.dispersion)
  force(.n_min)
  force(.n_max)
  force(.constant)

  # The dots only prevent a false-positive CRAN warning:
  rlang::check_dots_empty()

  if (any("n" == .reported)) {
    abort_in_export(
      "Can't take \"n\" as a reported statistic.",
      "i" = "Functions produced by `function_map_total_n()` \\
      assume that group-wise `n` values were not reported, \\
      and deal with them in hypothetical terms.",
      "i" = "Therefore, \"n\" can't be a `.reported` value."
    )
  }

  name_fn <- deparse(substitute(.fn))

  reported_reduplicated <- rep(.reported, each = 2L)
  reported_reduplicated <- paste0(reported_reduplicated, c("1", "2"))

  # `digits_*` arguments are real formals, not dots, so that they show up in
  # `formals()`, tab completion, and the help page:
  digits_args_names <- intersect(
    paste0("digits_", .reported),
    names(formals(.fn))
  )
  digits_pairlist_entries <- setNames(
    replicate(length(digits_args_names), NULL, simplify = FALSE),
    digits_args_names
  )

  # The same goes for helper arguments, with `.fn`'s defaults, i.e., none for
  # `items_averaged`:
  args_helper_fn <- attr(.fn, "scrutiny_args_helper", exact = TRUE)
  helper_formals <- formals(.fn)[args_helper_fn]

  # And for the settings of `.fn`, such as `rounding`:
  setting_formals <- formals_settings(
    .fn,
    exclude = c(
      args_helper_fn,
      "dispersion",
      "n_min",
      "n_max",
      "constant"
    )
  )
  args_setting_fn <- names(setting_formals)

  # `.n_min` may be a function of the settings of `.fn`, resolved at call
  # time, like the seq tier's `.var_bounds`: GRIMMER and DEBIT can test a
  # group of 1 under `sd_meaning = "sd_pop"` but not under `"sd_sample"`. The
  # formal `n_min` then defaults to `NULL`, which the body replaces:
  n_min_fn <- if (is.function(.n_min)) .n_min else NULL
  n_min_default <- if (is.function(.n_min)) NULL else .n_min

  # The settings of `.fn` with their defaults, for `n_min_fn()`:
  defaults_setting <- eval_defaults(setting_formals, environment(.fn))

  # The total-n mapper's own settings are recorded next to those of `.fn` and
  # compared to these defaults:
  defaults_total_n <- list(
    dispersion = .dispersion,
    n_min = n_min_default,
    n_max = .n_max,
    constant = .constant
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
      dispersion = .dispersion,
      n_min = n_min_default,
      n_max = .n_max,
      constant = .constant
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

      # The default `n_min` may depend on the settings; see `n_min_fn` above.
      # It is also the default that the recorded `n_min` is compared to:
      defaults_total_n <- `!!`(defaults_total_n)
      n_min_fn <- `!!`(n_min_fn)
      if (!is.null(n_min_fn)) {
        args_resolved <- utils::modifyList(`!!`(defaults_setting), setting_vals)
        defaults_total_n$n_min <- n_min_fn(args_resolved)
        if (is.null(n_min)) {
          n_min <- defaults_total_n$n_min
        }
      }

      # One value applies to both groups and every row. A vector would be
      # recycled across the alternating group rows, pairing values with the
      # wrong groups in the `"back"` direction:
      for (arg_helper in names(helper_vals)) {
        if (length(helper_vals[[arg_helper]]) != 1L) {
          abort_in_export(
            "`{arg_helper}` must be a single value here.",
            "x" = "It has length {length(helper_vals[[arg_helper]])}.",
            "i" = "Total-n mappers test hypothetical group sizes, so one \\
            value applies to both groups. For values that differ by row, \\
            give `data` an `{arg_helper}` column."
          )
        }
      }

      name_test <- `!!`(.name_test)
      name_fn <- `!!`(name_fn)
      name_class <- `!!`(.name_class)
      name_key_result <- `!!`(.name_key_result)
      reported <- `!!`(.reported)
      reported_reduplicated <- `!!`(reported_reduplicated)
      fn <- `!!`(.fn)

      # First, before anything reads columns off `data`:
      data <- as_tibble_input(data)

      data <- absorb_key_args(data, c(reported_reduplicated, "n"))

      # Checks ---

      check_factory_dots(fn, name_fn, ...)

      # The other columns of `data` are returned as they are, so one named like
      # a result column, or like a group's reported values, would be
      # overwritten -- or would overwrite them:
      names_taken <- intersect(
        c(
          reported,
          "n_change",
          "both_consistent",
          "case",
          "dir",
          if (!is.null(constant)) "constant"
        ),
        colnames(data)
      )
      if (length(names_taken) > 0L) {
        abort_in_export(
          "`data` has columns that the output needs for its results.",
          "x" = "{wrap_in_backticks(names_taken)} would be overwritten.",
          "i" = "Rename or remove {cli::qty(length(names_taken))}{?it/them}."
        )
      }

      # A missing `digits_*` argument is left out, so that `fn()` raises its
      # own error about it. See `function_map_seq()` for why not `mget()`:
      digits_vals <- list()
      for (arg_digits in `!!`(digits_args_names)) {
        if (!eval(call("missing", as.name(arg_digits)))) {
          digits_vals[arg_digits] <- list(get(arg_digits))
        }
      }
      fn_args <- c(digits_vals, helper_vals, setting_vals, list(...))

      # `check_mapper_input_colnames()` would look for the wrong key columns:
      check_consistency_not_in_colnames(data, name_test, name_key_result)

      if (!any(colnames(data) == "n")) {
        abort_in_export(
          "Column `n` missing.",
          "i" = "`n` should contain the reported total sample sizes \\
          (one per row).",
          "i" = "The function will use `disperse_total()` to go up \\
          and down from the integer at half an even `n`, or the \\
          two integers just around half an odd `n`."
        )
      }

      # 2 is the smallest total that splits into two groups. A missing total
      # can't be split either, and `disperse_total()` would fail on it with a
      # message about something else:
      offenders <-
        data$n[is.na(data$n) | !is_whole_number(data$n) | data$n < 2]

      if (length(offenders) > 0L) {
        if (length(offenders) > 3L) {
          offenders <- offenders[1:3]
          msg_starting_with <- ", starting with"
        } else {
          msg_starting_with <- ":"
        }
        msg_offenders <- wrap_in_backticks(offenders)
        abort_in_export(
          "`n` values must be whole numbers of at least 2.",
          "x" = "The `n` column includes values that are \\
          not{msg_starting_with} {msg_offenders}.",
          "i" = "They are supposed to be (total) sample sizes, each split \\
          into two groups of at least one member."
        )
      }

      # The code below builds its output from the rows of `data`, and fails
      # with an obscure error if there are none:
      if (nrow(data) == 0L) {
        abort_in_export(
          "`data` must have at least one row.",
          "x" = "It has none."
        )
      }

      # Main part ---

      data_forth <- data
      data_back <- data

      reported_orig <- reported

      cols_expected_forth <- reported_reduplicated

      cols_missing <-
        cols_expected_forth[!cols_expected_forth %in% colnames(data)]

      if (length(cols_missing) > 0L) {
        if (length(cols_missing) == 1L) {
          msg_cols <- "Column"
          msg_is_are <- "is"
          msg_it_they <- "It's"
          msg_this_these <- "This column"
        } else {
          msg_cols <- "Columns"
          msg_is_are <- "are"
          msg_it_they <- "They're"
          msg_this_these <- "These columns"
        }
        msg_cols_missing <- wrap_in_backticks(cols_missing)
        abort_in_export(
          "{msg_cols} {msg_cols_missing} {msg_is_are} missing from `data`.",
          "i" = "{msg_it_they} expected because of the `.reported` \\
          specification in the call to `function_map_total_n()` that \\
          created the present function.",
          "i" = "{msg_this_these} should contain reported group statistics. \\
          {msg_it_they} presumably essential to {name_test}."
        )
      }

      # Swap the suffixes `"1"` and `"2"` by pasting them onto `reported`, not
      # by replacing characters: a statistic called `"t1"` has the columns `t11`
      # and `t12`.
      cols_expected_back <- paste0(
        rep(reported, each = 2L),
        c("2", "1")
      )

      # Rename by name, not by position, so the swap works whatever the order
      # of the key columns in `data`:
      names(data_back)[match(cols_expected_forth, names(data_back))] <-
        cols_expected_back

      cols_forth_order <- cols_expected_forth

      # Names in the order of `data_forth`, values swapped:
      data_back <- data_back |>
        dplyr::relocate(all_of(cols_forth_order))

      # `[`, not `dplyr::select()`, which adds the grouping columns of a
      # grouped `data` back in, so that the pairs would no longer line up:
      cols_expected_forth <- data_forth[cols_expected_forth]
      cols_expected_back <- data_back[cols_expected_back][cols_forth_order]

      map_total_n_proto <- function_map_total_n_proto(
        fn = fn,
        name_key_result = name_key_result,
        name_fn = name_fn
      )

      out_forth <- do.call(
        map_total_n_proto,
        c(
          list(
            data = data_forth,
            .reported = cols_expected_forth,
            .reported_orig = reported_orig,
            .dir = factor("forth", levels = "forth"),
            .dispersion = dispersion,
            .n_min = n_min,
            .n_max = n_max,
            .constant = constant
          ),
          fn_args
        )
      )

      # The `constant` values belong to the groups, so they are swapped with
      # them, as the `digits_*` values are in `map_total_n_proto()`:
      constant_back <- if (is.list(constant)) {
        lapply(constant, rev)
      } else {
        rev(constant)
      }
      out_back <- do.call(
        map_total_n_proto,
        c(
          list(
            data = data_back,
            .reported = cols_expected_back,
            .reported_orig = reported_orig,
            .dir = factor("back", levels = "back"),
            .dispersion = dispersion,
            .n_min = n_min,
            .n_max = n_max,
            .constant = constant_back
          ),
          fn_args
        )
      )

      if (!all(colnames(out_forth) == colnames(out_back))) {
        names_out_forth <- wrap_in_backticks(colnames(out_forth))
        names_out_back <- wrap_in_backticks(colnames(out_back))
        cli::cli_abort(
          c(
            "Column names returned by calls to the helper function \\
          `scrutiny:::function_map_total_n_proto()` are not identical.",
            "i" = "Column names in question --",
            "*" = "`colnames(out_forth)`: {names_out_forth}",
            "*" = "`colnames(out_back)`: {names_out_back}",
            "x" = "This is a deep error within at least one of the \\
          function operators `scrutiny::function_map_total_n()` \\
          and `scrutiny:::function_map_total_n_proto()`.",
            "i" = "Please check the source code for these."
          ),
          .internal = TRUE
        )
      }

      out <- dplyr::bind_rows(out_forth, out_back)

      # The settings that `fn()` recorded, such as `rounding`, followed by
      # those of this call:
      meta <- attr(out_forth, "scrutiny", exact = TRUE)
      meta$settings <- dplyr::bind_rows(
        meta$settings,
        new_settings(
          list(
            dispersion = dispersion,
            n_min = n_min,
            n_max = n_max,
            constant = constant
          ),
          defaults_total_n,
          `!!`(name_mapper(.name_test, "_map_total_n"))
        )
      )
      attr(out, "scrutiny") <- meta

      # A case whose total leaves no pair of group sizes within the limits has
      # no rows, so say so:
      cases_dropped <- setdiff(seq_len(nrow(data)), out$case)
      if (length(cases_dropped) > 0L) {
        cli::cli_warn(c(
          "!" = "{length(cases_dropped)} case{?s} of `data` {?has/have} no \\
          pair of group sizes to test.",
          "i" = "{cli::qty(length(cases_dropped))}`dispersion`, `n_min`, \\
          and `n_max` leave none for {?it/them}, so {?it is/they are} not \\
          in the output: row{?s} {cases_dropped}."
        ))
      }

      # `audit_total_n()` dispatches on the generic class; the test-specific one
      # lets users write S3 methods for the output of one particular mapper:
      out <- out |>
        add_class(c(
          "scrutiny_map_total_n",
          class_name_test(name_test, "_map_total_n")
        ))

      if (!is.null(name_class)) {
        out <- out |>
          add_class(name_class)
      }

      # `rename = FALSE`: `fn()` already named the column, so only the
      # list-column-to-logical half of this code applies here.
      `!!!`(write_code_col_key_result(.name_key_result, rename = FALSE))
    }),
    # The body relies on scrutiny's internal helpers, so the enclosure must
    # inherit from scrutiny's namespace. Never use the caller's environment: a
    # factory-made function exported from another package would then fail to
    # find them.
    env = rlang::env()
  )

  # --- End of the manufactured function, `fn_out()` ---

  # Keep the enclosure of `fn_out()` small:
  rm(name_fn)

  # After the `digits_*` arguments, so that all three mapper tiers start their
  # signature the same way. `n` is the column of totals. As a key argument,
  # `n = "total"` matches it exactly instead of partially matching both `n_min`
  # and `n_max`:
  insert_key_args(
    fn_out,
    c(reported_reduplicated, "n"),
    insert_after = 1L + length(digits_args_names)
  )
}
