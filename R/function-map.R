#' Create new `*_map()` functions
#'
#' @description `function_map()` creates new basic mapper functions for
#'   consistency tests, such as [`grimmer_map()`] or [`debit_map()`].
#'
#'   For context, see [*Creating basic mappers with `function_map()`*](
#'   https://lhdjung.github.io/scrutiny/articles/consistency-tests.html#creating-basic-mappers-with-function_map
#'   ).
#'
#' @param .fun Single-case consistency testing function that will be applied to
#'   each row in a data frame. It must return a single logical value, i.e.,
#'   `TRUE`, `FALSE`, or `NA` -- or, if `.col_names` is specified, a list of
#'   values with the key result first.
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
#'   thrown. These arguments are not added to the factory-made function.
#' @param .args_by_row Optionally, a string vector with names of arguments of
#'   the `*_scalar()` function that may vary from row to row, such as
#'   `"digits_x"`. The factory-made function then accepts either a single value
#'   or one value per row of `data` for each of them, and returns them as
#'   columns of the output. All other arguments apply to the whole call.
#' @param .args_defaults Optionally, a named list with defaults for arguments
#'   of the `*_scalar()` function that should differ from the defaults of that
#'   function itself, such as `list(show_reason = TRUE)`.
#' @param .cols_helper Optionally, a string vector with names of arguments of
#'   the `*_scalar()` function that may also be given as columns of `data`, such
#'   as `"items"`. See [`manage_helper_col()`].
#' @param .cols_helper_merge Optionally, a named string vector that pairs
#'   `.cols_helper` names with the names of the `.reported` columns they are
#'   multiplied into for the output, such as `c(items = "n")`. Such a helper
#'   column is not returned by itself. This is presentation only; it does not
#'   affect test results.
#' @param .col_names Optionally, a string vector with the names of the columns
#'   that the `*_scalar()` function returns when asked to show its reconstructed
#'   values. The name of the key result column must come first. Whether the
#'   `*_scalar()` function returns a single value or the full list is up to the
#'   user of the factory-made function, who controls it via an argument such as
#'   `show_rec`; both cases are handled.
#' @param .arg_extra Logical. Should the factory-made function have an `extra`
#'   argument for selecting the columns from `data` that are returned alongside
#'   the test results? Default is `FALSE`, in which case all of them are
#'   returned. This only exists to support the deprecated `extra` argument of
#'   [`grim_map()`] and [`debit_map()`]; don't use it in new mappers. Use
#'   `dplyr::select()` on the output instead.
#' @param ... These dots must be empty.

#' @details The factory-made function has an argument for every argument of
#'   `.fun` that is not named in `.reported` or `.args_disabled`, with the same
#'   default. Values supplied to them are passed on to `.fun`.
#'
#'   The output tibble returned by the factory-made function will inherit one or
#'   two classes independently of the `.name_class` argument:
#' - It will inherit a class named `"scrutiny_{tolower(.name_test)}_map"`; for
#'   example, the class is `"scrutiny_grim_map"` if `.name_test` is `"GRIM"`.
#' - If `.fun` has a `rounding` argument, the output tibble will inherit a class
#'   named `"scrutiny_rounding_{rounding}"`; for example,
#'   `"scrutiny_rounding_up_or_down"`.

#' @return A factory-made function with these arguments:
#' - `data`: Data frame with all the columns named in `.reported`. It must
#'   have columns named after the key arguments in `.fun`. Other columns are
#'   permitted.
#' - Arguments named after the `.reported` values. They can be specified as the
#'   names of `data` columns so that the function will rename that column using
#'   the `.reported` name.
#' - Arguments named after those of `.fun`, with the same defaults; see
#'   *Details*.
#' - `...`: Arguments passed down to `.fun`.

#' @section Value returned by the factory-made function: A tibble with the
#'   `.reported` columns, any `.args_by_row` columns, and `"consistency"`: a
#'   logical column showing whether the values to its left are mutually
#'   consistent (`TRUE`) or not (`FALSE`). Any other columns of `data` follow to
#'   the right.

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

function_map <- function(
  .fun,
  .reported,
  .name_test,
  .name_key_result = "consistency",
  .name_class = NULL,
  .args_disabled = NULL,
  .args_by_row = NULL,
  .args_defaults = NULL,
  .cols_helper = NULL,
  .cols_helper_merge = NULL,
  .col_names = NULL,
  .arg_extra = FALSE,
  ...
) {
  force(.fun)
  force(.reported)
  force(.name_test)
  force(.name_class)
  force(.name_key_result)
  force(.args_disabled)
  force(.args_by_row)
  force(.args_defaults)
  force(.cols_helper)
  force(.cols_helper_merge)
  force(.col_names)
  force(.arg_extra)

  # Checks ---

  # The dots are only included to prevent a false-positive CRAN warning, so they
  # must not be used:
  rlang::check_dots_empty()

  fun_name <- deparse(substitute(.fun))
  formals_fun <- formals(.fun)

  # Check that all values of the arguments that name arguments of `.fun` really
  # do name arguments of `.fun`:
  check_factory_arg_names(.reported, formals_fun, fun_name, ".reported")
  check_factory_arg_names(.args_by_row, formals_fun, fun_name, ".args_by_row")
  check_factory_arg_names(.cols_helper, formals_fun, fun_name, ".cols_helper")
  check_factory_arg_names(
    names(.args_defaults),
    formals_fun,
    fun_name,
    ".args_defaults"
  )

  if (!all(names(.cols_helper_merge) %in% .cols_helper)) {
    cli::cli_abort(c(
      "Every name of `.cols_helper_merge` must be a `.cols_helper` value.",
      "x" = "{wrap_in_backticks(setdiff(names(.cols_helper_merge),
      .cols_helper))} {?is/are} not."
    ))
  }

  if (!all(.cols_helper_merge %in% .reported)) {
    cli::cli_abort(c(
      "Every value of `.cols_helper_merge` must be a `.reported` value.",
      "x" = "{wrap_in_backticks(setdiff(.cols_helper_merge, .reported))} \\
      {?is/are} not."
    ))
  }

  # The arguments of `.fun` that become arguments of the factory-made function,
  # in the order in which `.fun` has them. Key columns are covered by the
  # `.reported` arguments inserted at the very end, and disabled arguments are
  # meant to be unavailable:
  args_promoted <- setdiff(
    names(formals_fun),
    c(.reported, .args_disabled, "...")
  )

  # Three groups among them: arguments that may vary by row and hence ride along
  # as columns of the input to `purrr::pmap()`; arguments that may be given as
  # columns of `data` in the first place; and plain arguments that apply to the
  # whole call: Arguments of `.fun` that have no default of their own must reach
  # it as missing, so that its own error message is shown rather than a generic
  # one. All others are passed on explicitly, which is what makes
  # `.args_defaults` work: `.fun` would otherwise apply its own default to an
  # argument that the factory-made function has a different default for.
  formals_promoted <- formals_fun[args_promoted]
  formals_promoted[names(.args_defaults)] <- .args_defaults
  args_required <- args_promoted[
    vapply(formals_promoted, rlang::is_missing, logical(1L), USE.NAMES = FALSE)
  ]

  args_by_row <- intersect(args_promoted, .args_by_row)
  args_helper <- intersect(args_promoted, .cols_helper)
  args_const <- setdiff(args_promoted, c(args_by_row, args_helper))

  code_key_arg_checks <- paste0("!missing(", .reported, ")", collapse = " || ")
  code_key_arg_checks <- rlang::expr({
    if (`!!`(rlang::parse_expr(code_key_arg_checks))) {
      data <- scrutiny::absorb_key_args(data, `!!`(.reported))
    }
  })

  # `manage_helper_col()` takes the name of the helper column from the
  # expression it was given, so the argument has to be spliced in as a symbol:
  code_cols_helper <- lapply(
    args_helper,
    function(name) {
      rlang::expr(
        data <- scrutiny::manage_helper_col(
          data = data,
          var_arg = `!!`(as.name(name)),
          default = `!!`(formals_promoted[[name]])
        )
      )
    }
  )

  # Same for `check_lengths_congruent()`, which names the offending arguments in
  # its message by deparsing the call it was given. Arguments that may vary by
  # row are exempt -- it is their point to have one value per row:
  code_check_lengths <- if (length(args_const) > 1L) {
    list(rlang::expr(
      check_lengths_congruent(list(`!!!`(lapply(args_const, as.name))))
    ))
  } else {
    NULL
  }

  all_classes <- c(
    paste0("scrutiny_", tolower(.name_test), "_map"),
    .name_class
  )

  code_rounding_class <- if (any(args_promoted == "rounding")) {
    list(rlang::parse_expr(
      "rounding_class <- paste0(\"scrutiny_rounding_\", rounding)"
    ))
  } else {
    list(rlang::expr(rounding_class <- NULL))
  }

  # Parsed from a string, like the key-argument checks above: `extra` is a
  # formal of the factory-made function only, and R CMD check would flag it as a
  # global variable if it appeared as a symbol in the factory's own body.
  code_extra_cols <- if (.arg_extra) {
    list(rlang::parse_expr(
      "other_cols <- manage_extra_cols(data, extra, other_cols)"
    ))
  } else {
    NULL
  }

  # --- Start of the factory-made function, `fn_out()` ---

  fn_out <- rlang::new_function(
    args = rlang::pairlist2(
      data = ,
      !!!formals_promoted,
      !!!(if (.arg_extra) list(extra = Inf) else NULL),
      ... =
    ),
    body = rlang::expr({
      fun <- `!!`(.fun)
      reported <- `!!`(.reported)
      args_by_row <- `!!`(args_by_row)
      args_helper <- `!!`(args_helper)
      args_const <- `!!`(args_const)
      args_required <- `!!`(args_required)
      col_names <- `!!`(.col_names)
      helper_merge <- `!!`(.cols_helper_merge)

      add_class <- function(x, new_class) {
        `class<-`(x, value = c(new_class, class(x)))
      }

      # Manage key columns in `data`, renaming missing columns using the values
      # of key arguments, if necessary:
      `!!!`(code_key_arg_checks)

      # Checks ---

      scrutiny::check_args_disabled(`!!`(.args_disabled))
      scrutiny::check_factory_dots(fun, `!!`(fun_name), ...)
      scrutiny::check_mapper_input_colnames(
        data,
        `!!`(.reported),
        `!!`(.name_test)
      )

      if (!tibble::is_tibble(data)) {
        cli::cli_abort(c(
          "!" = "`data` must be a tibble.",
          "i" = "Convert it with `tibble::as_tibble()`."
        ))
      }

      `!!!`(code_check_lengths)

      # Add a column for every helper argument that `data` doesn't have one for
      # already, and throw an error if `data` and the argument contradict each
      # other:
      `!!!`(code_cols_helper)

      # Main part ---

      # Collect the values of those arguments that the user actually supplied.
      # Arguments left at their default are not passed on to `fun()`: it has the
      # very same defaults, so it applies them itself -- and an argument with no
      # default at all, such as `digits_x`, must reach `fun()` as missing so
      # that the test function's own error message is shown, not a generic one:
      .args_vals <- list()
      for (.name in c(args_by_row, args_const)) {
        if (.name %in% args_required && eval(call("missing", as.name(.name)))) {
          next
        }
        .args_vals[.name] <- list(get(.name))
      }

      # The columns that `fun()` is applied to, row by row: the key columns,
      # then any helper columns, then one column per argument that may vary by
      # row (recycled to the number of rows if it is a single value):
      cols_tested <- as.list(data[reported])

      for (.name in args_helper) {
        cols_tested[[.name]] <- data[[.name]]
      }

      for (.name in args_by_row) {
        if (!is.null(.args_vals[[.name]])) {
          cols_tested[[.name]] <- recycle_digits(
            .args_vals[[.name]],
            nrow(data),
            .name
          )
        }
      }

      .args_const_vals <- .args_vals[intersect(args_const, names(.args_vals))]

      # If an argument that `fun()` requires was not supplied, apply `fun()` to
      # the first row on its own. The error that `fun()` throws about the
      # missing argument -- e.g., the bespoke message about `digits_x` -- is far
      # more helpful on its own than wrapped into the indexed-error context that
      # `purrr::pmap()` would add to it:
      if (!all(args_required %in% names(.args_vals)) && nrow(data) > 0L) {
        do.call(
          fun,
          c(
            lapply(cols_tested, function(x) x[[1L]]),
            .args_const_vals,
            list(...)
          )
        )
      }

      # Test for consistency:
      results <- do.call(
        purrr::pmap,
        c(
          list(cols_tested, fun),
          .args_const_vals,
          list(...)
        )
      )

      # The key columns of the output, with any helper columns either multiplied
      # into the key column they belong to or returned in their own right:
      cols_key <- as.list(data[reported])

      for (.name in args_helper) {
        .merge_into <- helper_merge[[.name]]
        if (is.null(.merge_into)) {
          cols_key[[.name]] <- data[[.name]]
        } else {
          cols_key[[.merge_into]] <- cols_key[[.merge_into]] * data[[.name]]
        }
      }

      # `n` is a sample size, so it is always a whole number. Coercing it to
      # integer makes for better representation, as in `function_map_seq()`:
      if (
        !is.null(cols_key[["n"]]) &&
          all(is_whole_number(cols_key[["n"]]) | is.na(cols_key[["n"]]))
      ) {
        cols_key[["n"]] <- as.integer(cols_key[["n"]])
      }

      cols_by_row <- cols_tested[args_by_row[
        args_by_row %in% names(.args_vals)
      ]]

      # `fun()` returns a single value per row unless it was told to show its
      # reconstructed values, in which case it returns a list of values that are
      # unpacked into the columns named in `col_names`:
      cols_result <- write_result_cols(results, col_names)

      # Any columns of `data` that play no role in the test are returned
      # alongside the results. Columns that the output has already -- e.g., a
      # `digits_x` column in the output of a mapper that is tested again -- are
      # skipped rather than duplicated:
      other_cols <- data[
        !colnames(data) %in%
          c(
            names(cols_key),
            names(cols_by_row),
            names(cols_result),
            reported,
            args_helper
          )
      ]

      `!!!`(code_extra_cols)

      out <- tibble::new_tibble(
        c(cols_key, cols_by_row, cols_result, as.list(other_cols)),
        nrow = nrow(data),
        class = NULL
      )

      # Support rounding classes:
      `!!!`(code_rounding_class)

      all_classes <- c(`!!`(all_classes), rounding_class)

      # Mediate between `seq_endpoint_df()` or `seq_distance_df()`, on the one
      # hand, and `seq_test_ranking()`, on the other:
      if (inherits(data, "scrutiny_seq_df")) {
        all_classes <- c("scrutiny_seq_test", all_classes)
      }

      out <- add_class(out, all_classes)

      # Unquote-splice the code that finalizes `out`. This includes unnesting if
      # the key result column has been a list, and renaming it if
      # `.name_key_result` was specified:
      `!!!`(write_code_col_key_result(
        name_key_result = .name_key_result,
        name_data = rlang::expr(out)
      ))
    }),
    # As in `function_map_seq()` and `function_map_total_n()`: the body relies
    # on scrutiny helpers, so the manufactured function is enclosed in a child
    # of the present execution environment, which inherits from scrutiny's
    # namespace. Never use the caller's environment here -- a factory-made
    # function exported from another package would then fail to find them.
    env = rlang::env()
  )

  # --- End of the factory-made function, `fn_out()` ---

  # Garbage collection:
  rm(
    fun_name,
    formals_fun,
    formals_promoted,
    code_key_arg_checks,
    code_cols_helper,
    code_check_lengths,
    code_rounding_class,
    code_extra_cols,
    all_classes
  )

  # Insert parameters named after the key columns into `fn_out()`, with `NULL`
  # as the default for each. The key columns need to be present in the input
  # data frame. They are expected to have the names specified in `.reported`. If
  # they don't, however, the user can simply specify the key column arguments as
  # the non-quoted names of the columns meant to fulfill these roles:
  insert_key_args(fun = fn_out, reported = .reported)
}
