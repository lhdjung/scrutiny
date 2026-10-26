#' Create new `*_map()` functions
#'
#' @description `function_map()` creates a basic mapper: a function that
#'   applies a single-case test such as `grim_scalar()` to every row of a data
#'   frame. [`grim_map()`], [`grimmer_map()`], and [`debit_map()`] are made by
#'   it.
#'
#'   See `vignette("consistency-tests-in-depth")`, section *Creating basic
#'   mappers with `function_map()`*.
#'
#' @param .fun Single-case function to apply to each row. It must return a
#'   single logical value or, if `.col_names` is specified, a list of values
#'   with the key result first.
#' @param .reported String. Names of the arguments of `.fun` that take the
#'   reported values, such as `c("x", "n")`. These become the key columns. May
#'   be `NULL` if `.reported_variadic` is specified.
#' @param .reported_variadic Optionally, a single string naming an argument of
#'   `.fun` that takes the values of one row as a vector. The new function gets
#'   an argument by that name, without a default, that selects any number of
#'   columns with tidyselect syntax. See *Details*.
#' @param .name_test String (length 1). Plain-text name of the test, such as
#'   `"GRIM"`.
#' @param .name_key_result (Experimental) String (length 1). Name of the key
#'   result column. Default is `"consistency"`.
#' @param .name_class String. Optionally, one or more classes to add to the
#'   output. Default is `NULL`; see *Details* for the class that is always
#'   added.
#' @param .args_disabled Optionally, names of arguments of `.fun` that the new
#'   function should not have. Specifying one of them is an error.
#' @param .args_by_row Optionally, names of arguments of `.fun` that may vary
#'   by row, such as `"digits_x"`. The new function takes a single value or one
#'   per row of `data` for each, and returns them as columns.
#' @param .args_fixed Optionally, a named list of values that are always passed
#'   to `.fun`, such as `list(show_reason = TRUE)`. The new function has no
#'   arguments for them; specifying one is an error.
#' @param .cols_helper Optionally, names of arguments of `.fun` that may also be
#'   given as columns of `data`, such as `"items_averaged"`. A helper column is
#'   returned after the key columns if it was given, as a column or as an
#'   argument. See [`manage_helper_col()`].
#' @param .col_names Optionally, names of the columns that a list returned by
#'   `.fun` unpacks into, as with `show_rec = TRUE`. The first name must be
#'   `.name_key_result`. If `data` has no rows, only the key result column is
#'   created.
#' @param .cols_derived Optionally, a named list of functions that compute
#'   further columns from the same per-row input, such as
#'   `list(exp_incon = grim_exp_incon)`. Each gets only the arguments it
#'   has formals for. The columns follow the key result column.
#' @param ... These dots must be empty.
#'
#' @details The new function has an argument for every argument of `.fun` that
#'   is not named in `.reported`, `.reported_variadic`, `.args_disabled`, or
#'   `.args_fixed`, with the same default.
#'
#'   `.reported_variadic` is for a test whose number of key columns depends on
#'   the data, such as whether the values in any number of columns add up to
#'   the value in another one. `.fun` takes those values as a single vector,
#'   and the new function selects the columns with tidyselect; see *Examples*.
#'   The selection must not be empty and must not include a column that already
#'   has a role in the test. The selected columns are returned as themselves.
#'   Such a mapper cannot be wrapped by [`function_map_seq()`] or
#'   [`function_map_total_n()`], which derive their arguments from `.reported`.
#'
#'   The output has the class `"scrutiny_{tolower(.name_test)}_map"`, e.g.,
#'   `"scrutiny_grim_map"`, in addition to any `.name_class`. The arguments that
#'   applied to the whole call, such as `rounding`, are recorded on the output;
#'   see [`scrutiny_meta()`].
#'
#' @return A function with these arguments:
#' - `data`: a data frame with the key columns. Other columns are permitted.
#' - The `.args_by_row` arguments, such as `digits_x`.
#' - If `.reported_variadic` was given, an argument by that name.
#' - One argument per `.reported` value, for naming a column of `data` that
#'   holds that value under another name.
#' - `...`: must be empty. Every argument after it must be named in full.
#' - The other arguments of `.fun`, with the same defaults; see *Details*.
#'
#' @section Value returned by the factory-made function: A tibble with the
#'   `.reported_variadic` columns, the `.reported` columns, the helper columns
#'   that were given, the `.args_by_row` columns, and the key result column
#'   (`consistency` by default), which is `TRUE` for consistent value sets and
#'   `FALSE` for inconsistent ones. Any `.cols_derived` columns, any
#'   `.col_names` columns, and the other columns of `data` follow, in that
#'   order.
#'
#' @include grim.R debit.R function-factory-helpers.R
#'
#' @export
#'
#' @examples
#' # A single-case function for SCHLIM, a mock test:
#' schlim_scalar <- function(y, n) {
#'   (y / 3) > n
#' }
#'
#' # The mapper:
#' schlim_map <- function_map(
#'   .fun = schlim_scalar,
#'   .reported = c("y", "n"),
#'   .name_test = "SCHLIM"
#' )
#'
#' df1 <- tibble::tibble(y = 16:25, n = 3:12)
#' schlim_map(df1)
#'
#' # A test over any number of columns: do the parts add up to the total?
#' # The single-case function takes the parts as one vector...
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
  .args_fixed = NULL,
  .cols_helper = NULL,
  .col_names = NULL,
  .cols_derived = NULL,
  # Last, not next to `.reported`, so that positional calls keep their meaning:
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
  force(.args_fixed)
  force(.cols_helper)
  force(.col_names)
  force(.cols_derived)

  # Checks ---

  # The dots only prevent a false-positive CRAN warning:
  rlang::check_dots_empty()

  check_args_disabled_unnamed(.args_disabled)

  fun_name <- deparse(substitute(.fun))
  formals_fun <- formals(.fun)

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
    names(.args_fixed),
    formals_fun,
    fun_name,
    ".args_fixed"
  )

  if (!is.null(.reported_variadic)) {
    check_length(.reported_variadic, 1L)
    if (any(.reported_variadic == .reported)) {
      abort_in_export(
        "`.reported_variadic` must not also be a `.reported` value.",
        "x" = "{wrap_in_backticks(.reported_variadic)} is both.",
        "i" = "A key argument of `{fun_name}()` either takes the values of \\
        one column or the values of any number of them, not both."
      )
    }
  } else if (length(.reported) == 0L) {
    abort_in_export(
      "`.reported` must name at least one column.",
      "i" = "Unless `.reported_variadic` is specified, in which case the \\
      columns it selects are the only key columns."
    )
  }

  if (length(.cols_derived) > 0L && !rlang::is_named(.cols_derived)) {
    abort_in_export(
      "`.cols_derived` must be a named list: each name becomes a column name."
    )
  }

  if (!all(vapply(.cols_derived, is.function, logical(1L)))) {
    abort_in_export(
      "Every element of `.cols_derived` must be a function.",
      "x" = "{wrap_in_backticks(names(.cols_derived)[
      !vapply(.cols_derived, is.function, logical(1L))
      ])} {?is/are} not."
    )
  }

  if (!is.null(.col_names) && !identical(.col_names[1L], .name_key_result)) {
    col_names_first <- .col_names[1L]
    name_key_result <- .name_key_result
    abort_in_export(
      "`.col_names` must start with `.name_key_result`.",
      "x" = "It starts with \"{col_names_first}\", but `.name_key_result` \\
      is \"{name_key_result}\"."
    )
  }

  # Fixed values are passed to `.fun` by name, so each needs one:
  if (
    !is.null(.args_fixed) &&
      !(is.list(.args_fixed) && rlang::is_named2(.args_fixed))
  ) {
    abort_in_export("`.args_fixed` must be a named list.")
  }

  # A fixed value can't also come from `data` or from the caller:
  args_fixed_offenders <- intersect(
    names(.args_fixed),
    c(.reported, .reported_variadic, .args_by_row, .cols_helper, .args_disabled)
  )
  if (length(args_fixed_offenders) > 0L) {
    abort_in_export(
      "`.args_fixed` can't name arguments that are key, by-row, helper, or \\
      disabled arguments.",
      "x" = "{wrap_in_backticks(args_fixed_offenders)} \\
      {cli::qty(length(args_fixed_offenders))}{?is/are} already one of these."
    )
  }

  # The arguments of `.fun` that become arguments of the factory-made function,
  # in `.fun`'s order. Key arguments are inserted at the very end:
  args_promoted <- setdiff(
    names(formals_fun),
    c(.reported, .reported_variadic, .args_disabled, names(.args_fixed), "...")
  )

  # The body of the factory-made function assigns these names before it reads
  # the values of the arguments, so an argument of `.fun` by one of these names
  # would be shadowed:
  names_locals <- c(
    "data",
    "fun",
    "reported",
    "reported_variadic",
    "args_by_row",
    "args_helper",
    "args_helper_given",
    "args_const",
    "args_fixed",
    "args_required",
    "args_enum",
    "col_names",
    "cols_derived_funs",
    "add_class",
    "cols_variadic",
    "names_variadic",
    "quo_variadic",
    "index_variadic",
    "args_vals",
    "arg"
  )
  args_shadowed <- intersect(args_promoted, names_locals)
  if (length(args_shadowed) > 0L) {
    abort_in_export(
      "`{fun_name}()` has arguments that `function_map()` uses internally.",
      "x" = "Rename {wrap_in_backticks(args_shadowed)}.",
      "i" = "Reserved: {wrap_in_backticks(names_locals)}."
    )
  }

  formals_promoted <- formals_fun[args_promoted]
  args_required <- args_promoted[
    vapply(formals_promoted, rlang::is_missing, logical(1L), USE.NAMES = FALSE)
  ]

  args_by_row <- intersect(args_promoted, .args_by_row)
  args_helper <- intersect(args_promoted, .cols_helper)
  args_const <- setdiff(args_promoted, c(args_by_row, args_helper))

  # Arguments whose default is a set of strings, such as `sd_meaning`. A call
  # that leaves one out gets the first string, as `rlang::arg_match()` would
  # resolve it. Resolving it here records that string, not the whole set, in the
  # "scrutiny" attribute:
  args_enum <- args_const[vapply(
    formals_promoted[args_const],
    is_enum_default,
    logical(1L),
    USE.NAMES = FALSE
  )]

  # For the `default` column of the "scrutiny" attribute:
  defaults_const <- eval_defaults(
    formals_promoted[args_const],
    environment(.fun)
  )

  # The by-row arguments, in practice `digits_*`, have no defaults, so they go
  # right after `data`, in the same position in every mapper. This must come
  # after `args_required`, which pairs `formals_promoted` with `args_promoted`
  # by position:
  formals_promoted <- formals_promoted[
    c(args_by_row, setdiff(names(formals_promoted), args_by_row))
  ]

  # With variadic key columns, `.reported` may be empty, leaving nothing to
  # rename:
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

  # `tidyselect::eval_select()` both finds and renames the variadic columns.
  # The argument is spliced in as a symbol so that `rlang::enquo()` captures
  # the expression instead of evaluating it:
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
      # `eval_select()` may rename the columns it selects. The new names go
      # into the output; the old ones keep the columns from being returned a
      # second time:
      names_variadic <- colnames(data)[index_variadic]
      names(cols_variadic) <- names(index_variadic)
    }))
  }

  # `manage_helper_col()` reads the column name off the expression it was
  # given, so the argument is spliced in as a symbol. A helper without a
  # default, like `items_averaged`, gets none here either, so that a missing
  # one reaches `fun()` as missing:
  code_cols_helper <- lapply(
    args_helper,
    function(name) {
      if (name %in% args_required) {
        return(rlang::expr(
          data <- scrutiny::manage_helper_col(
            data = data,
            var_arg = `!!`(as.name(name))
          )
        ))
      }
      rlang::expr(
        data <- scrutiny::manage_helper_col(
          data = data,
          var_arg = `!!`(as.name(name)),
          default = `!!`(formals_promoted[[name]])
        )
      )
    }
  )

  # Spliced in as symbols too: `check_lengths_congruent()` names offenders by
  # deparsing its call. By-row arguments are exempt, and so are those without
  # a default, which may be missing here and must reach `fun()` as such:
  args_check_lengths <- setdiff(args_const, args_required)
  code_check_lengths <- if (length(args_check_lengths) > 1L) {
    list(rlang::expr(
      check_lengths_congruent(list(`!!!`(lapply(args_check_lengths, as.name))))
    ))
  } else {
    NULL
  }

  all_classes <- c(
    class_name_test(.name_test, "_map"),
    .name_class
  )

  # --- Start of the factory-made function, `fn_out()` ---

  # The dots come right after the key arguments, which `insert_key_args()` puts
  # in front of them below. Every argument after the dots must then be named in
  # full, so that `items = 2` can't partially match `items_averaged`:
  fn_out <- rlang::new_function(
    args = rlang::pairlist2(
      data = ,
      !!!formals_promoted[args_by_row],
      ... = ,
      !!!formals_promoted[setdiff(names(formals_promoted), args_by_row)]
    ),
    body = rlang::expr({
      fun <- `!!`(.fun)
      reported <- `!!`(.reported)
      reported_variadic <- `!!`(.reported_variadic)
      args_by_row <- `!!`(args_by_row)
      args_helper <- `!!`(args_helper)
      args_const <- `!!`(args_const)
      args_fixed <- `!!`(.args_fixed)
      args_required <- `!!`(args_required)
      args_enum <- `!!`(args_enum)
      col_names <- `!!`(.col_names)
      cols_derived_funs <- `!!`(.cols_derived)

      add_class <- function(x, new_class) {
        `class<-`(x, value = c(new_class, class(x)))
      }

      # Checks ---

      # First, before anything reads columns off `data`:
      data <- as_tibble_input(data)

      # Rename columns named by key arguments:
      `!!!`(code_key_arg_checks)

      check_args_disabled(`!!`(c(.args_disabled, names(.args_fixed))))
      check_factory_dots(fun, `!!`(fun_name), ...)
      scrutiny::check_mapper_input_colnames(
        data,
        `!!`(.reported),
        `!!`(.name_test),
        `!!`(.name_key_result)
      )

      # After the renaming, so that tidyselect sees the same column names as
      # the rest of the function:
      cols_variadic <- list()
      names_variadic <- character(0L)
      `!!!`(code_variadic)

      # See `args_enum` above. Before the length check, which would see the
      # whole set:
      for (arg in args_enum) {
        if (eval(call("missing", as.name(arg)))) {
          assign(arg, get(arg)[[1L]])
        }
      }

      `!!!`(code_check_lengths)

      # A helper column is returned if it was given, as a column of `data` or
      # as an argument. One that `fun()` only has a default for is not:
      args_helper_given <- character(0L)
      for (arg in args_helper) {
        if (arg %in% colnames(data) || !eval(call("missing", as.name(arg)))) {
          args_helper_given <- c(args_helper_given, arg)
        }
      }

      # Add missing helper columns; error if `data` and an argument disagree:
      `!!!`(code_cols_helper)

      # Main part ---

      # An argument with no default, such as `digits_x`, must reach `fun()` as
      # missing, so that the test function's own error message is shown rather
      # than a generic one:
      args_vals <- list()
      for (arg in c(args_by_row, args_const)) {
        if (arg %in% args_required && eval(call("missing", as.name(arg)))) {
          next
        }
        args_vals[arg] <- list(get(arg))
      }

      # The columns that `fun()` is applied to, row by row: the key columns,
      # then any helper columns, then one column per argument that may vary by
      # row (recycled to the number of rows if it is a single value):
      cols_tested <- as.list(data[reported])

      # A variadic key argument gets all of a row's values at once, so its
      # "column" is a list with one vector per row. `fun()` has a single
      # argument for it, whatever the number of columns behind it:
      if (!is.null(reported_variadic)) {
        col_variadic <- cols_variadic |>
          purrr::pmap(function(...) c(...)) |>
          list() |>
          rlang::set_names(reported_variadic)
        cols_tested <- c(col_variadic, cols_tested)
      }

      for (arg in args_helper) {
        cols_tested[[arg]] <- data[[arg]]
      }

      for (arg in args_by_row) {
        # An argument with a default that was not given takes its values from
        # a column by its name, if `data` has one:
        arg_given <- !eval(call("missing", as.name(arg)))
        if (!arg_given && !arg %in% args_required && arg %in% colnames(data)) {
          args_vals[[arg]] <- data[[arg]]
        }
        if (!is.null(args_vals[[arg]])) {
          cols_tested[[arg]] <- recycle_digits(
            args_vals[[arg]],
            nrow(data),
            arg
          )
          # A column of `data` by the same name is replaced by the argument's
          # values (see `other_cols` below), which is only right if the two
          # agree, as they do when a mapper's output is tested again:
          if (
            arg %in%
              colnames(data) &&
              !identical(
                as.numeric(data[[arg]]),
                as.numeric(cols_tested[[arg]])
              )
          ) {
            abort_in_export(
              "`{arg}` is both an argument and a column of `data`, and the \
              two disagree.",
              "i" = "Drop or rename the column, or make the two agree."
            )
          }
        }
      }

      args_const_vals <- args_vals[intersect(args_const, names(args_vals))]

      # roundwork checks the rounding arguments once per row, but its errors
      # would name the roundwork function instead of the mapper:
      if ("rounding" %in% names(args_const_vals)) {
        check_rounding_args(
          args_const_vals$rounding,
          args_const_vals$threshold,
          args_const_vals$symmetric
        )
      }

      # Test the first row on its own, so that an error about the arguments,
      # such as a missing `digits_x`, is the test function's own, not wrapped
      # in `purrr::pmap()`'s indexed-error context. The result is discarded.
      #
      # Do *not* muffle warnings or messages here: `lifecycle::deprecate_warn()`
      # records that it has warned before the muffle discards the warning, so a
      # once-per-session warning would be lost for good.
      #
      # The error names row 1 only if row 2 passes; an error about the
      # arguments fails on every row:
      if (nrow(data) > 0L) {
        test_row <- function(row) {
          row_values <- lapply(cols_tested, function(x) x[[row]])
          do.call(fun, c(row_values, args_const_vals, args_fixed, list(...)))
        }
        withCallingHandlers(
          test_row(1L),
          error = function(cnd) {
            if (nrow(data) > 1L && passes_row(test_row, 2L)) {
              signal_in_row(cnd, 1L)
            }
          }
        ) |>
          check_fun_result(`!!`(fun_name))
      }

      results <- withCallingHandlers(
        do.call(
          purrr::pmap,
          c(
            list(cols_tested, fun),
            args_const_vals,
            args_fixed,
            list(...)
          )
        ),
        purrr_error_indexed = rethrow_row_error
      )

      # The key columns of the output, then the helper columns that were given:
      cols_key <- c(cols_variadic, as.list(data[reported]))

      for (arg in args_helper_given) {
        cols_key[[arg]] <- data[[arg]]
      }

      # `n` is a whole number, which prints better as integer:
      if (!is.null(cols_key[["n"]])) {
        cols_key[["n"]] <- as_integer_if_lossless(cols_key[["n"]])
      }

      cols_by_row <- cols_tested[args_by_row[
        args_by_row %in% names(args_vals)
      ]]

      cols_result <- write_result_cols(results, col_names)

      # Derived columns, such as `exp_incon` in `grim_map()`. Each function only gets the
      # arguments it has formals for, so it needs to know nothing about the
      # test around it:
      cols_derived <- list()

      for (col_name in names(cols_derived_funs)) {
        fun_derived <- cols_derived_funs[[col_name]]
        args_names_derived <- names(formals(fun_derived))
        vals_derived <- do.call(
          purrr::pmap,
          c(
            list(
              cols_tested[intersect(names(cols_tested), args_names_derived)],
              fun_derived
            ),
            args_const_vals[
              intersect(names(args_const_vals), args_names_derived)
            ]
          )
        )
        vals_derived <- unlist(vals_derived, use.names = FALSE)
        # A wrong length would make a tibble that only fails when printed:
        if (length(vals_derived) != nrow(data)) {
          abort_in_export(
            "The `.cols_derived` function for `{col_name}` must return one \\
            value per row.",
            "x" = "It returned {length(vals_derived)} value{?s} for \\
            {nrow(data)} row{?s}."
          )
        }
        cols_derived[[col_name]] <- if (is.null(vals_derived)) {
          numeric(0L)
        } else {
          vals_derived
        }
      }

      cols_result <- c(cols_result[1L], cols_derived, cols_result[-1L])

      # A column of `data` by the name of a result column would be silently
      # dropped:
      names_taken <- intersect(names(cols_result), colnames(data))
      if (length(names_taken) > 0L) {
        abort_in_export(
          "`data` has columns that the output needs for its results.",
          "x" = "{wrap_in_backticks(names_taken)} would be overwritten.",
          "i" = "Rename or remove {cli::qty(length(names_taken))}{?it/them}."
        )
      }

      # Columns that the output has already, such as `digits_x` in a mapper's
      # output tested again, are skipped. Variadic columns go by their names in
      # `data`: skipping the new name would drop a column of `data` that has
      # it, rather than catching the clash below:
      other_cols <- data[
        !colnames(data) %in%
          c(
            names(cols_by_row),
            names(cols_result),
            reported,
            names_variadic,
            args_helper
          )
      ]

      cols_out <- c(cols_key, cols_by_row, cols_result, as.list(other_cols))

      # A variadic column renamed to a name the output uses, as in
      # `summands = c(total = a, b)` or to the name of another column of
      # `data`, would appear twice:
      names_dup <- unique(names(cols_out)[duplicated(names(cols_out))])
      if (length(names_dup) > 0L) {
        abort_in_export(
          "The output would have {wrap_in_backticks(names_dup)} twice.",
          "i" = "A selected column can't be renamed to a name the output uses."
        )
      }

      out <- tibble::new_tibble(cols_out, nrow = nrow(data), class = NULL)

      out <- add_class(out, `!!`(all_classes))

      # Settings of this call for functions downstream, such as `rounding`,
      # which `grim_plot_grid()` reads. A missing value, as from a
      # `lifecycle::deprecated()` default of a `.fun` written elsewhere, is no
      # setting:
      attr(out, "scrutiny") <- list(
        settings = new_settings(
          Filter(Negate(rlang::is_missing), args_const_vals),
          `!!`(defaults_const),
          `!!`(name_mapper(.name_test, "_map"))
        )
      )

      # Unnest the key result column if it is a list, and rename it if
      # `.name_key_result` was specified:
      `!!!`(write_code_col_key_result(
        name_key_result = .name_key_result,
        name_data = rlang::expr(out),
        rename = is.null(.col_names)
      ))
    }),
    # The body relies on scrutiny's internal helpers, so the enclosure must
    # inherit from scrutiny's namespace. Never use the caller's environment: a
    # factory-made function exported from another package would then fail to
    # find them.
    env = rlang::env()
  )

  # --- End of the factory-made function, `fn_out()` ---

  # Keep the enclosure of `fn_out()` small:
  rm(
    fun_name,
    formals_fun,
    formals_promoted,
    code_key_arg_checks,
    code_variadic,
    code_cols_helper,
    code_check_lengths,
    args_check_lengths,
    all_classes
  )

  # Key arguments go after `data` and the by-row arguments, led by the
  # variadic one if there is one. That one has no default, because guessing
  # which columns to test would silently test the wrong ones:
  fn_out <- insert_key_args(
    fun = fn_out,
    reported = .reported,
    insert_after = 1L + length(args_by_row),
    variadic = .reported_variadic
  )

  # A helper argument that was given is a column of the output, so
  # `function_map_seq()` and `function_map_total_n()` must not pass it again
  # when they re-test. They read this attribute to know which ones those are:
  attr(fn_out, "scrutiny_args_helper") <- args_helper

  fn_out
}
