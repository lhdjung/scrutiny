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
#' @param .reported String. Names of the columns to be tested. May be `NULL` if
#'   `.reported_variadic` is specified.
#' @param .reported_variadic Optionally, a single string naming an argument of
#'   `.fun` that takes all the values of one row at once, as a vector. The
#'   factory-made function then has an argument by that name which selects any
#'   number of columns using tidyselect syntax, so that how many columns are
#'   tested is up to the caller rather than fixed when the factory runs. It has
#'   no default and must be specified in every call. See *Details*.
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
#'   `show_rec`; both cases are handled. If `data` has no rows, only the key
#'   result column is created, because there is no result to read the shape of
#'   the output off.
#' @param .cols_derived Optionally, a named list of functions that compute
#'   further columns which the `*_scalar()` function does not return, such as
#'   `list(probability = grim_probability)`. Each of them is applied to the same
#'   per-row input as the `*_scalar()` function, but only gets those arguments
#'   that it has formals for. The columns follow the key result column in the
#'   output.
#' @param .name_class_flags Optionally, a named string vector that pairs the
#'   name of a logical argument of the `*_scalar()` function with a class to be
#'   added to the output whenever that argument is `TRUE`, such as
#'   `c(percent = "scrutiny_percent_true")`. Use it for arguments that change
#'   what the numbers in the output mean, so that functions downstream of the
#'   mapper can tell.
#' @param ... These dots must be empty.

#' @details The factory-made function has an argument for every argument of
#'   `.fun` that is not named in `.reported`, `.reported_variadic`, or
#'   `.args_disabled`, with the same default. Values supplied to them are passed
#'   on to `.fun`.
#'
#'   Nothing here assumes a particular number of key columns, but that number is
#'   normally fixed when the factory runs: GRIM has two, GRIMMER and DEBIT have
#'   three. `.reported_variadic` is for the other case, where the number is a
#'   property of the caller's data -- as with a test that checks whether the
#'   values in any number of columns add up to the value in one specific other
#'   column. Its `.fun` takes those values as a single vector argument instead
#'   of one argument per column, and the factory-made function selects the
#'   columns with tidyselect:
#'
#'   ```
#'   sum_check_scalar <- function(parts, total, tolerance = 0) {
#'     abs(sum(parts) - total) <= tolerance
#'   }
#'
#'   sum_check_map <- function_map(
#'     .fun = sum_check_scalar,
#'     .reported = "total",
#'     .reported_variadic = "parts",
#'     .name_test = "SUMCHECK"
#'   )
#'
#'   sum_check_map(data, parts = starts_with("item"))
#'   ```
#'
#'   Selection helpers such as `starts_with()` are available inside that
#'   argument whether or not tidyselect is attached. The selection must not be
#'   empty, and it must not include a column that has a role in the test
#'   already -- a key column or a helper column -- because such a column would
#'   be tested twice over and returned twice.
#'
#'   The selected columns are returned as themselves, so the output is as
#'   rectangular as any other mapper's. What varies is only how many columns go
#'   into each test. Such a mapper is basic-tier only: [`function_map_seq()`]
#'   and [`function_map_total_n()`] derive their own arguments from `.reported`,
#'   which says nothing about the variadic columns.
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
#' - If `.reported_variadic` was specified, an argument by that name. It selects
#'   any number of columns of `data` using tidyselect syntax, and has no
#'   default.
#' - Arguments named after those of `.fun`, with the same defaults; see
#'   *Details*.
#' - `...`: Arguments passed down to `.fun`.

#' @section Value returned by the factory-made function: A tibble with any
#'   `.reported_variadic` columns, the `.reported` columns, any `.args_by_row`
#'   columns, and `"consistency"`: a logical column showing whether the values
#'   to its left are mutually consistent (`TRUE`) or not (`FALSE`). Any
#'   `.cols_derived` columns, any
#'   columns from `.col_names`, and any other columns of `data` follow to the
#'   right, in that order.

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
#'
#' # A test over any number of columns, decided by the data rather than by the
#' # factory call: do the parts add up to the total? The `*_scalar()` function
#' # takes the parts as one vector...
#' sum_check_scalar <- function(parts, total, tolerance = 0) {
#'   abs(sum(parts) - total) <= tolerance
#' }
#'
#' # ...which `.reported_variadic` names:
#' sum_check_map <- function_map(
#'   .fun = sum_check_scalar,
#'   .reported = "total",
#'   .reported_variadic = "parts",
#'   .name_test = "SUMCHECK"
#' )
#'
#' df2 <- tibble::tibble(
#'   item_1 = c(10, 20, 30),
#'   item_2 = c(5, 5, 5),
#'   item_3 = c(1, 2, 3),
#'   total  = c(16, 27, 40)
#' )
#'
#' # The `parts` argument selects columns with tidyselect syntax:
#' sum_check_map(df2, parts = starts_with("item"))

function_map <- function(
  .fun,
  .reported = NULL,
  .name_test,
  .name_key_result = "consistency",
  .name_class = NULL,
  .args_disabled = NULL,
  .args_by_row = NULL,
  .args_defaults = NULL,
  .cols_helper = NULL,
  .cols_helper_merge = NULL,
  .col_names = NULL,
  .cols_derived = NULL,
  .name_class_flags = NULL,
  # Last among the named arguments, rather than next to `.reported` where it
  # belongs by meaning, so that adding it does not shift what any existing
  # positional call means. The documentation groups the two together anyway:
  .reported_variadic = NULL,
  ...
) {
  force(.fun)
  force(.reported)
  force(.name_test)
  force(.reported_variadic)
  force(.name_class)
  force(.name_key_result)
  force(.args_disabled)
  force(.args_by_row)
  force(.args_defaults)
  force(.cols_helper)
  force(.cols_helper_merge)
  force(.col_names)
  force(.cols_derived)
  force(.name_class_flags)

  # Checks ---

  # The dots are only included to prevent a false-positive CRAN warning, so they
  # must not be used:
  rlang::check_dots_empty()

  fun_name <- deparse(substitute(.fun))
  formals_fun <- formals(.fun)

  # Check that all values of the arguments that name arguments of `.fun` really
  # do name arguments of `.fun`:
  check_factory_arg_names(.reported, formals_fun, fun_name, ".reported")
  check_factory_arg_names(
    .reported_variadic,
    formals_fun,
    fun_name,
    ".reported_variadic"
  )
  check_factory_arg_names(.args_by_row, formals_fun, fun_name, ".args_by_row")
  check_factory_arg_names(.cols_helper, formals_fun, fun_name, ".cols_helper")
  check_factory_arg_names(
    names(.args_defaults),
    formals_fun,
    fun_name,
    ".args_defaults"
  )
  check_factory_arg_names(
    names(.name_class_flags),
    formals_fun,
    fun_name,
    ".name_class_flags"
  )

  if (!is.null(.reported_variadic)) {
    check_length(.reported_variadic, 1L)
    if (any(.reported_variadic == .reported)) {
      cli::cli_abort(c(
        "`.reported_variadic` must not also be a `.reported` value.",
        "x" = "{wrap_in_backticks(.reported_variadic)} is both.",
        "i" = "A key argument of `{fun_name}()` either takes the values of \\
        one column or the values of any number of them, not both."
      ))
    }
  } else if (length(.reported) == 0L) {
    cli::cli_abort(c(
      "`.reported` must name at least one column.",
      "i" = "Unless `.reported_variadic` is specified, in which case the \\
      columns it selects are the only key columns."
    ))
  }

  if (length(.cols_derived) > 0L && !rlang::is_named(.cols_derived)) {
    cli::cli_abort(
      "`.cols_derived` must be a named list: each name becomes a column name."
    )
  }

  if (!all(vapply(.cols_derived, is.function, logical(1L)))) {
    cli::cli_abort(c(
      "Every element of `.cols_derived` must be a function.",
      "x" = "{wrap_in_backticks(names(.cols_derived)[
      !vapply(.cols_derived, is.function, logical(1L))
      ])} {?is/are} not."
    ))
  }

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
    c(.reported, .reported_variadic, .args_disabled, "...")
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

  # The arguments that may vary by row -- in practice, the `digits_*` arguments
  # -- come first, immediately after `data` and ahead of the key arguments
  # inserted at the very end. They have no defaults and have to be specified in
  # every call, so they belong next to the other argument that does, and their
  # position is the same across all mappers. This must happen after
  # `args_required` is derived above, which pairs `formals_promoted` with
  # `args_promoted` by position:
  formals_promoted <- formals_promoted[
    c(args_by_row, setdiff(names(formals_promoted), args_by_row))
  ]

  # With variadic key columns, `.reported` may name no column at all, and there
  # is then nothing to rename:
  code_key_arg_checks <- if (length(.reported) > 0L) {
    code_missing <- paste0("!missing(", .reported, ")", collapse = " || ")
    list(rlang::expr({
      if (`!!`(rlang::parse_expr(code_missing))) {
        data <- scrutiny::absorb_key_args(data, `!!`(.reported))
      }
    }))
  } else {
    list()
  }

  # The variadic key columns are chosen by the user's tidyselect expression at
  # call time, so they need no renaming mechanism and no check for their
  # presence: `tidyselect::eval_select()` does both jobs. As with the helper
  # columns below, the argument has to be spliced in as a symbol -- here so
  # that `rlang::enquo()` can capture the expression instead of evaluating it.
  code_variadic <- if (is.null(.reported_variadic)) {
    list()
  } else {
    list(rlang::expr({
      quo_variadic <- rlang::enquo(`!!`(as.name(.reported_variadic)))
      check_variadic_arg(quo_variadic, `!!`(.reported_variadic))
      index_variadic <- tidyselect::eval_select(quo_variadic, data)
      check_variadic_cols(
        index = index_variadic,
        data = data,
        spoken_for = c(`!!`(.reported), args_helper),
        name = `!!`(.reported_variadic)
      )
      cols_variadic <- as.list(data)[index_variadic]
      # `eval_select()` may rename the columns it selects, so the names under
      # which the output carries them are not necessarily the names they have
      # in `data`. Both are needed: the new ones for the output, the old ones
      # to keep the columns from being returned a second time:
      names_variadic <- colnames(data)[index_variadic]
      names(cols_variadic) <- names(index_variadic)
    }))
  }

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

  # One class per flag argument that is `TRUE`, such as `percent` in
  # `grim_map()`. As with the helper columns above, the argument has to be
  # spliced in as a symbol:
  code_class_flags <- lapply(
    names(.name_class_flags),
    function(name) {
      rlang::expr(
        if (isTRUE(`!!`(as.name(name)))) {
          all_classes <- c(`!!`(.name_class_flags[[name]]), all_classes)
        }
      )
    }
  )

  # --- Start of the factory-made function, `fn_out()` ---

  fn_out <- rlang::new_function(
    args = rlang::pairlist2(
      data = ,
      !!!formals_promoted,
      ... =
    ),
    body = rlang::expr({
      fun <- `!!`(.fun)
      reported <- `!!`(.reported)
      reported_variadic <- `!!`(.reported_variadic)
      args_by_row <- `!!`(args_by_row)
      args_helper <- `!!`(args_helper)
      args_const <- `!!`(args_const)
      args_required <- `!!`(args_required)
      col_names <- `!!`(.col_names)
      helper_merge <- `!!`(.cols_helper_merge)
      cols_derived_funs <- `!!`(.cols_derived)

      add_class <- function(x, new_class) {
        `class<-`(x, value = c(new_class, class(x)))
      }

      # Checks ---

      # What `data` is comes first, before anything reads columns off it:
      check_tibble(data)

      # Manage key columns in `data`, renaming missing columns using the values
      # of key arguments, if necessary:
      `!!!`(code_key_arg_checks)

      scrutiny::check_args_disabled(`!!`(.args_disabled))
      scrutiny::check_factory_dots(fun, `!!`(fun_name), ...)
      scrutiny::check_mapper_input_colnames(
        data,
        `!!`(.reported),
        `!!`(.name_test)
      )

      # Resolve the variadic key columns, if there are any. This comes after
      # the renaming above, so that a tidyselect expression sees the same
      # column names as the rest of the function:
      cols_variadic <- list()
      names_variadic <- character(0L)
      `!!!`(code_variadic)

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

      # A variadic key argument gets all of a row's values at once, so its
      # "column" is a list with one vector per row. `fun()` has a single
      # argument for it, whatever the number of columns behind it:
      if (!is.null(reported_variadic)) {
        cols_tested <- c(
          rlang::set_names(
            list(purrr::pmap(cols_variadic, function(...) c(...))),
            reported_variadic
          ),
          cols_tested
        )
      }

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
      # into the key column they belong to or returned in their own right. The
      # variadic columns are returned as themselves, so the output is as
      # rectangular as any other mapper's -- only the number of columns that go
      # into each test varies:
      cols_key <- c(cols_variadic, as.list(data[reported]))

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

      # Columns that are not part of `fun()`'s return value but are computed
      # from the same per-row input, such as `probability` in `grim_map()`. Each
      # function only gets those arguments that it has formals for, so it needs
      # to know nothing about the test around it:
      cols_derived <- list()

      for (.name in names(cols_derived_funs)) {
        .fun_derived <- cols_derived_funs[[.name]]
        .args_names_derived <- names(formals(.fun_derived))
        .vals_derived <- do.call(
          purrr::pmap,
          c(
            list(
              cols_tested[intersect(names(cols_tested), .args_names_derived)],
              .fun_derived
            ),
            .args_const_vals[
              intersect(names(.args_const_vals), .args_names_derived)
            ]
          )
        )
        cols_derived[[.name]] <- if (length(.vals_derived) == 0L) {
          numeric(0L)
        } else {
          unlist(.vals_derived, use.names = FALSE)
        }
      }

      # The derived columns follow the key result column, ahead of any columns
      # unpacked from `col_names`:
      cols_result <- c(cols_result[1L], cols_derived, cols_result[-1L])

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
            names_variadic,
            args_helper
          )
      ]

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

      # One class per flag argument that is set, such as `percent`:
      `!!!`(code_class_flags)

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
    code_variadic,
    code_cols_helper,
    code_check_lengths,
    code_rounding_class,
    code_class_flags,
    all_classes
  )

  # Insert parameters named after the key columns into `fn_out()`, with `NULL`
  # as the default for each. The key columns need to be present in the input
  # data frame. They are expected to have the names specified in `.reported`. If
  # they don't, however, the user can simply specify the key column arguments as
  # the non-quoted names of the columns meant to fulfill these roles. They go
  # after `data` and the by-row arguments moved next to it above, led by the
  # variadic key argument if there is one -- which has no default at all,
  # because guessing which columns to test would silently test the wrong ones:
  insert_key_args(
    fun = fn_out,
    reported = .reported,
    insert_after = 1L + length(args_by_row),
    variadic = .reported_variadic
  )
}
