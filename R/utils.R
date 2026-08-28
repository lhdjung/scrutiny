#' @include import-reexport.R is-numeric-like.R

utils::globalVariables(c(
  ".",
  "where",
  "desc",
  "all_of",
  "contains",
  "everything",
  "x",
  "items",
  "frac",
  "distance",
  "both_consistent",
  "fun",
  "var",
  "dispersion",
  "out_min",
  "out_max",
  "include_reported",
  "n",
  "times",
  "value",
  "name",
  "setNames",
  "rounding",
  "case",
  "n_sum",
  "consistency",
  "ratio",
  "scrutiny_index_case",
  "starts_with",
  "value_duplicated",
  "variable",
  "sd_lower",
  "sd_incl_lower",
  "sd_upper",
  "sd_incl_upper",
  "x_lower",
  "x_upper",
  "dupe_count",
  "fun_name",
  # Added after rewriting the function factories using `rlang::new_function()`:
  "!!",
  "!!!",
  "constant",
  "constant_index",
  "include_consistent",
  "n_min",
  "n_max",
  # Added for `function_duplicate_cols()`, which uses `rlang::new_function()`:
  "colname_end",
  "ignore",
  "numeric_only",
  # Added for `write_code_col_key_result()`:
  "!!<-"
))


# Do NOT export any of these! ---------------------------------------------

#' Mark a string as wrong
#'
#' @param x Object that should have been a string (it isn't; that's why the
#'   function is called.)
#'
#' @return String.
#'
#' @noRd
wrong_spec_string <- function(x) {
  if (is.character(x)) {
    paste0("\"", x, "\"")
  } else {
    paste0("`", x, "` (not a string)")
  }
}


#' DEBIT helper for SD reconstruction
#'
#' @param formula String. For now, this has to be `"mean_n"`.
#' @param x,n String. Binary mean and sample size.
#' @param group_0,group_1 Numeric. Number of values coded 0 and 1, respectively.
#'
#' @return Numeric.
#'
#' @noRd
reconstruct_sd_scalar <- function(formula, x, n, group_0, group_1) {
  x <- as.numeric(x)

  if (formula == "mean_n") {
    sd_rec <- sd_binary_mean_n(mean = x, n = n)
  } else if (formula == "0_n") {
    sd_rec <- sd_binary_0_n(group_0 = group_0, n = n)
  } else if (formula == "1_n") {
    sd_rec <- sd_binary_1_n(group_1 = group_1, n = n)
  } else if (formula == "groups") {
    sd_rec <- sd_binary_groups(group_0 = group_0, group_1 = group_1)
  } else {
    cli::cli_abort(
      message = c(
        "!" = "`formula` must be \"mean_n\", \"0_n\", \"1_n\", or \\
      \"groups\".",
        "x" = "It is {wrong_spec_string(formula)}."
      ),
      call = rlang::caller_env()
    )
  }

  sd_rec
}


# Vectorized version of `reconstruct_sd_scalar()`:
reconstruct_sd <- Vectorize(reconstruct_sd_scalar, USE.NAMES = FALSE)


#' Write "an" or "a", depending on the next word
#'
#' @param x String. A string value that ends on a vowel letter returns `"an"`;
#'   else, it returns `"a"`.
#'
#' @return String.
#'
#' @noRd
an_a <- function(x) {
  dplyr::if_else(
    stringr::str_detect(x, "^[aeiou]"),
    "an",
    "a"
  )
}


#' Prefix an object's type with "an" or "a"
#'
#' This uses `an_a()` to prepend the type of `x` with "an" or "a". Because the
#' function meant to be used in messages, it replaces "double" by "double
#' (numeric value)" and "character" by "string".
#'
#' @param x Any object.
#'
#' @return String.
#'
#' @noRd
an_a_type <- function(x) {
  type <- typeof(x)

  if (type == "double") {
    type <- "double (numeric value)"
  } else if (type == "character") {
    type <- "string"
  }

  paste(an_a(typeof(x)), type)
}


#' Check whether numbers are whole
#'
#' @description For each element of a numeric vector, `is_whole_number()` checks
#'   whether that element is a whole number.
#'
#'   This is not the same as the integer data type, so doubles and integers are
#'   tested the same way. See the note in `?integer`. To test if R itself
#'   considers a vector integer-like, use `rlang::is_integerish()` instead.
#'
#' @param x Numeric.
#' @param tolerance Numeric. Any difference between `x` and the whole number
#'   nearest to it that is less than `tolerance` (in the absolute value) will be
#'   ignored. The default is close to `1 / (10 ^ 8)`. This avoids errors due to
#'   spurious precision in floating-point arithmetic.
#'
#' @return Logical vector of the same length as `x`.
#'
#' @details This function was adapted (with naming modifications) from the
#'   examples of `?integer`, where a very similar function is called
#'   `is.wholenumber()`.
#'
#' @author R Core Team, Lukas Jung
#'
#' @noRd
# `is_decidable_n_items()` and `check_newly_numeric()` write this test out
# rather than calling it, because they run once per row; keep them in step.
is_whole_number <- function(x, tolerance = WHOLE_NUMBER_TOLERANCE) {
  abs(x - round(x)) < tolerance
}


#' Refuse an enumeration that would not fit in memory
#'
#' @description GRIMMER decides a case by enumerating every integer sum that
#'   the reported mean admits, and, for each of those, every integer sum of
#'   squares that the reported SD admits. Both ranges grow linearly with `n`:
#'   at `n = 1e7` the test takes about a second, at `n = 1e8` about eleven, and
#'   at `n = 3e9` it exhausts memory with nothing to show for it. There is no
#'   warning on the way -- `a:b` simply allocates.
#'
#'   No published summary statistic looks like this, so the limit is far above
#'   anything the test will meet in practice. It exists so that a typo in an
#'   `n` ends in a message rather than a hung session.
#'
#' @param bounds Numeric (length 2). The `c(lower, upper)` of the range about
#'   to be materialized. An empty range (lower above upper) is fine: nothing
#'   gets allocated for it.
#' @param what String (length 1). What is being enumerated, for the message.
#' @param n Numeric (length 1). The sample size, for the message.
#' @param limit Numeric (length 1). The largest range the function will admit.
#'
#' @return No return value. Might throw an error.
#'
#' @noRd
check_enumeration_size <- function(bounds, what, n, limit = 1e6) {
  size <- bounds[2L] - bounds[1L] + 1
  if (isTRUE(size > limit)) {
    cli::cli_abort(c(
      "`n` is too large for GRIMMER to enumerate.",
      "x" = "With `n = {n}`, there are {round(size)} {what}.",
      "i" = "The test works by checking each of them in turn, so it would \\
      allocate a vector of that length -- and one more per candidate sum.",
      "i" = "The limit is {limit}. GRIMMER is a test for reported summary \\
      statistics, and no sample of this size is one."
    ))
  }
}


#' Can a value set be decided at all, going by `n` and `items`?
#'
#' @description All three consistency tests reason about integer data: GRIM
#'   about the integer sum behind a reported mean, GRIMMER about that sum and
#'   the integer sum of squares behind a reported SD, DEBIT about a count of
#'   ones among `n` binary values. A sample size or item count that is not a
#'   positive whole number describes no such data set, so there is nothing for
#'   the test to be consistent or inconsistent *with*.
#'
#'   Before scrutiny 1.0.0 these inputs produced verdicts. `grim(x = 5.19, n =
#'   20.5, digits_x = 2)` was `FALSE`, `grimmer(..., items = 1.5)` was `TRUE`,
#'   and `debit(x = 0.5, sd = 0.5, n = -5, ...)` was `FALSE` -- none of them a
#'   verdict about anything. `NA` says what is actually the case: the test
#'   cannot decide.
#'
#' @param n Numeric (length 1). The reported sample size.
#' @param items Numeric (length 1). The number of items composing `x`. DEBIT
#'   has no such argument and leaves it at the default.
#' @param min_n Numeric (length 1). The smallest `n` the test can work with.
#'   `1` for GRIM: the mean of a single value is that value. `2` for GRIMMER
#'   and DEBIT, which reconstruct a *sample* SD and so divide by `n - 1`. At `n
#'   = 1` that is a division by zero, and both tests used to reach a verdict
#'   through it -- GRIMMER a `NaN` that `na.rm = TRUE` swallowed, DEBIT an
#'   `Inf` that compared as "above the upper bound".
#'
#' @return Logical vector, recycled to the length of the longer of `n` and
#'   `items`: `TRUE` where the test can decide the case, `FALSE` where it
#'   cannot. Never `NA` -- a missing `n` or `items` is itself undecidable,
#'   which is what every test already reports for it. The `*_scalar()`
#'   functions call this on single values; `grim_probability()` on columns.
#'
#' @noRd
is_decidable_n_items <- function(n, items = 1, min_n = 1) {
  # `is.finite()` is `FALSE` for `NA`, `NaN`, and both infinities, and `FALSE &
  # NA` is `FALSE`, so the result is never missing however the comparisons
  # further right turn out. Note that `&` evaluates both of its sides, unlike
  # `&&`: that is what makes this work on columns, and it means the guards
  # absorb the missing values rather than preventing them.
  #
  # The two whole-number tests are `is_whole_number()` written out. This runs
  # once per row in all three tests, and the two calls cost more than every
  # comparison here put together. `WHOLE_NUMBER_TOLERANCE` is the same constant
  # `is_whole_number()` uses, so the two cannot drift on what counts as whole;
  # if the formula itself ever changes, this copy and the one in
  # `check_newly_numeric()` have to change with it.
  #
  # The same condition appears twice below, once with `&&` and once with `&`,
  # which is the one duplication in this function and has to be kept exact. `&&`
  # is a special form: it neither allocates a result vector nor evaluates what
  # it does not need, which makes the scalar version some 1.8 times faster, and
  # that is the version the `*_scalar()` functions reach once per row. `&` is
  # what the column version needs, and `grim_probability()` calls it once for a
  # whole column, where looping over the scalar version would cost far more than
  # it saves. A test in test-utils.R runs both over the same grid and requires
  # them to agree.
  if (length(n) == 1L && length(items) == 1L && length(min_n) == 1L) {
    return(
      is.finite(n) &&
        is.finite(items) &&
        n >= min_n &&
        items > 0 &&
        abs(n - round(n)) < WHOLE_NUMBER_TOLERANCE &&
        abs(items - round(items)) < WHOLE_NUMBER_TOLERANCE
    )
  }

  is.finite(n) &
    is.finite(items) &
    n >= min_n &
    items > 0 &
    abs(n - round(n)) < WHOLE_NUMBER_TOLERANCE &
    abs(items - round(items)) < WHOLE_NUMBER_TOLERANCE
}


#' Coerce a sample-size column to integer, but only if that is lossless
#'
#' @description A sample size is always a whole number, so mappers store the `n`
#'   column as integer: it is the more faithful representation, and it displays
#'   without a decimal point.
#'
#'   The coercion has to be conditional, though. `as.integer()` turns anything
#'   beyond `.Machine$integer.max` into `NA` with nothing but a base-R warning,
#'   and an `NA` in `n` next to a non-`NA` verdict is self-contradictory output:
#'   the verdict was computed from the real value before the coercion destroyed
#'   it. So the coercion is skipped whenever any value is fractional or too
#'   large, and the column stays double.
#'
#' @param n Numeric vector; the `n` column.
#'
#' @return `n`, as integer if every value is a whole number within integer
#'   range, and unchanged otherwise.
#'
#' @noRd
as_integer_if_lossless <- function(n) {
  if (
    all(is_whole_number(n) | is.na(n)) &&
      all(abs(n) <= .Machine$integer.max, na.rm = TRUE)
  ) {
    as.integer(n)
  } else {
    n
  }
}


#' Bring a `digits_*` argument to one value per row
#'
#' A `digits_*` argument may be a single number, in which case every value in
#' the column was reported with that many decimal places; or one number per row
#' of `data`, for columns where the number of decimal places varies. Mappers
#' pass the result on to their `*_scalar()` function row by row.
#'
#' @param digits The `digits_x` or `digits_sd` argument, as given by the user.
#' @param n_rows Number of rows in `data`.
#' @param name_digits_arg String (length 1). Name of the argument, for errors.
#'
#' @returns Numeric vector of length `n_rows`.
#'
#' @noRd
recycle_digits <- function(digits, n_rows, name_digits_arg) {
  if (length(digits) == 1L) {
    rep(digits, n_rows)
  } else if (length(digits) == n_rows) {
    digits
  } else {
    cli::cli_abort(
      message = c(
        "`{name_digits_arg}` must have length 1 or the number of rows \\
      in `data`.",
        "x" = "It has length {length(digits)}, but `data` has \\
      {n_rows} row{?s}.",
        "i" = "Use a single number if all values in the column were reported \\
      with the same number of decimal places."
      ),
      call = rlang::caller_env(2)
    )
  }
}


error_digits_flawed <- function(digits, name_digits_arg, n) {
  check_length(digits, 1)

  # name <- deparse(substitute(digits))

  cli::cli_abort(
    message = c(
      "`{name_digits_arg}` must be a single, whole number.",
      "x" = "It is actually: {digits}"
    ),
    call = rlang::caller_env(n)
  )
}


check_newly_numeric <- function(
  x,
  digits,
  caller_type = c("basic", "mapper", "sequence_mapper")
) {
  # Inlined rather than left to `is_whole_number()`, which this runs ahead of
  # once per key value per row: the call alone cost more than the whole check
  # below it.
  if (
    !is.numeric(digits) ||
      length(digits) != 1L ||
      digits < 0 ||
      abs(digits - round(digits)) >= WHOLE_NUMBER_TOLERANCE
  ) {
    name <- deparse(substitute(digits))
    error_digits_flawed(digits, name, 4)
  }

  # Can `x` be written with `digits` decimal places? `round()` answers that some
  # 200 times faster than counting the decimal places in a string representation
  # of `x`, which is what `decimal_places_scalar()` does with three regular
  # expressions -- once per key value per row, formerly the most expensive part
  # of a mapper call (#92). The test is one-sided, though: `0.1 + 0.2` prints as
  # `"0.3"` without being the double for `0.3`, so whatever it leaves undecided
  # is counted after all. `is.na(x)` has to come first, or the comparisons would
  # be `NA` and the `if ()` would fail outright. An infinity needs no branch of
  # its own: `round()` returns it unchanged, so it passes, which is right -- it
  # has no decimal places to be inconsistent with `digits`, and the tests report
  # it as undecidable.
  if (
    is.na(x) ||
      (is.numeric(x) &&
        (x == round(x, digits) || digits >= decimal_places_scalar(x)))
  ) {
    return(invisible(NULL))
  }

  caller_type <- rlang::arg_match(caller_type)

  # The function the user called, e.g. `grim_map()` rather than the
  # `grim_scalar()` that `purrr::pmap()` led here from. Errors are attributed to
  # its frame so that they say "Error in `grim_map()`":
  caller <- caller_test_fn()

  # Is that function a mapper, such as `grim_map()`? Mappers operate on data
  # frames, so their messages should talk about columns, not arguments.
  caller_is_mapper <- stringr::str_detect(caller$name, "_map")

  # Record the names of the key argument passed down here (likely the mean or
  # SD) and the calling function
  name_x <- deparse(substitute(x))
  name_fn <- paste0("scrutiny::", caller$name)

  if (!is.numeric(x)) {
    # If the user called a mapper function, the error message should talk about
    # columns because mappers operate on data frames. Otherwise, it should
    # mention arguments.
    msg_what <- if (caller_is_mapper) {
      I(paste0("Using string `", name_x, "` columns in `", name_fn, "()`"))
    } else {
      paste0(name_fn, "(", name_x, " = 'must be numeric')")
    }

    msg_digits_name <- paste0(
      "Instead, use `digits_",
      name_x,
      "` to specify the number of decimal places."
    )

    lifecycle::deprecate_stop(
      when = "1.0.0",
      what = msg_what,
      details = c(
        msg_digits_name,
        "This is to ensure a correct number of decimal places.
        Apologies for the inconvenience."
      ),
      env = caller$frame
    )
  }

  # In the remaining case, `digits_*` is not large enough for the number of
  # decimal places that already exist in `x`
  name_digits_arg <- paste0("digits_", name_x)
  digits_in_x <- decimal_places_scalar(x)

  cli::cli_abort(
    message = c(
      "More decimal places than specified digits.",
      "!" = "Number of digits specified via `{name_digits_arg}` \
      must not be less than the number of decimal places in `{name_x}`.",
      "x" = "`{name_digits_arg}` is {digits}.",
      "x" = "`{name_x}` is {x}, so it has {digits_in_x} decimal place{?s}."
    ),
    call = caller$frame
  )
}


# Name of the function that a call invokes, as a single string; `""` if it
# can't be determined. The head of a call is not always a symbol: it can be a
# namespace-qualified call such as `scrutiny::grim_map`, or the function object
# itself, which is how factory-made functions invoke `fun` via `do.call()`.
# Passing either of those to `as.character()` returns a vector of the wrong
# length or throws an error, so each case is handled separately here.
fn_name_from_call <- function(call) {
  if (!is.call(call)) {
    return("")
  }
  fn <- call[[1L]]
  if (is.name(fn)) {
    as.character(fn)
  } else if (rlang::is_call(fn, c("::", ":::"))) {
    # Just the name, not `pkg::name`: callers add the namespace where they want
    # it, and the bare name is what the checks below match against.
    as.character(fn[[3L]])
  } else if (is.call(fn)) {
    # Something else with a callable head, e.g. `obj$method`
    deparse(fn)[1L]
  } else {
    # A function object carries no name of its own
    ""
  }
}


# Find the outermost consistency test function on the call stack: the one the
# user actually called. Error messages about missing or flawed `digits_*`
# arguments should name that function and be attributed to its call.
#
# Counting frames is not a viable alternative. How many frames separate a
# `*_scalar()` function from the call the user typed depends on whether a
# mapper, a `Vectorize()` wrapper (which adds `do.call()` and `mapply()`), or a
# factory-made function sits in between -- and factory-made functions invoke
# `fun` as a function object, so that frame carries no name at all.
#
# Returns a list with the function's `name` and its `frame`. If no such function
# is on the stack, the frame that called `caller_test_fn()` stands in for it.
# That should not happen: this is only called from the two checks below, and
# those are only called from consistency test functions.
caller_test_fn <- function() {
  calls <- sys.calls()
  names_fn <- vapply(calls, fn_name_from_call, character(1L), USE.NAMES = FALSE)
  is_test_fn <- stringr::str_detect(names_fn, PATTERN_NAME_TEST_FN)

  index <- if (any(is_test_fn)) {
    # `sys.calls()` runs from the outermost frame inward, so the first match is
    # the outermost one -- e.g. `grim_map_seq()` rather than the `grim_map()`
    # that it calls internally:
    which(is_test_fn)[1L]
  } else {
    # The last call is `caller_test_fn()` itself, so this is its caller:
    length(calls) - 1L
  }

  if (index < 1L) {
    return(list(name = "", frame = globalenv()))
  }

  list(name = names_fn[index], frame = sys.frames()[[index]])
}


#' Error if a `digits_*` argument is missing
#'
#' Since scrutiny 1.0.0, key arguments of functions related to consistency tests
#' -- `grim*()`, `grimmer*`, and `debit*()` -- that were previously strings no
#' longer are. These arguments are `x` and `sd` (for GRIM functions, only `x`).
#'
#' Instead, the number of decimal places of the respective statistics is
#' conveyed through the new integer arguments `digits_x` and `digits_sd`. As
#' most users will not be familiar with them initially, they will inevitably
#' encounter errors when they call the functions in question without specifying
#' the new arguments.
#'
#' In these cases, `error_digits_missing()` is called to give a more helpful and
#' bespoke error message than the generic `argument "digits_x" is missing, with
#' no default`. In scrutiny code, the usage pattern should always be:
#'
#' ```
#' if (missing(digits_x)) {
#'   error_digits_missing(x)
#' }
#' ```
#'
#' @param x
#'
#' @returns No return value; will throw an error.
#'
#' @noRd
error_digits_missing <- function(x) {
  name_x <- deparse(substitute(x))
  name_digits_arg <- paste0("digits_", name_x)

  # The example below should show the call the user actually made, so it needs
  # the outermost consistency test function on the stack -- e.g. `debit()`
  # rather than the `debit_scalar()` that `Vectorize()` led here from:
  caller <- caller_test_fn()
  name_fn <- caller$name

  # Prepare message with changelog URL to be shown after the error
  on.exit(cli::cli_text(paste0(
    "For more information, visit ",
    cli::style_italic(
      "{.href [scrutiny's changelog]",
      "(https://lhdjung.github.io/scrutiny/news/index.html)}"
    ),
    "."
  )))

  # If the error occurred in a GRIMMER or DEBIT function, include `sd` and
  # `digits_sd` arguments in the example call because they are required there.
  if (grepl("(grimmer|debit)", name_fn)) {
    part_sd <- ", sd = 0.62"
    # ...but not twice if `digits_sd` is the argument that is missing:
    part_digits_sd <- if (name_digits_arg == "digits_sd") {
      NULL
    } else {
      ", digits_sd = 2"
    }
  } else {
    part_sd <- NULL
    part_digits_sd <- NULL
  }

  # If the user called a mapper function (such as `grim_map()`), the example
  # should construct a data frame rather than accepting the value directly
  if (grepl("_map", name_fn)) {
    part_tibble_open <- "tibble::tibble("
    part_tibble_close <- ")"
  } else {
    part_tibble_open <- NULL
    part_tibble_close <- NULL
  }

  # In case the key argument is still a string, tell the user this changed
  msg_key_arg <- if (is.character(x)) {
    paste0(", so please specify `", name_x, "` as a number, not a string")
  } else {
    NULL
  }

  cli::cli_abort(
    message = c(
      "Need to specify `{name_digits_arg}` to state the number of \
      decimal places in `{name_x}`.",
      "i" = "For example, with 1.40 (two decimal places): \
      `{name_fn}({part_tibble_open}x = 1.4{part_sd}, n = 29{part_tibble_close}, \
      {name_digits_arg} = 2{part_digits_sd})`",
      "i" = "This was introduced in scrutiny 1.0.0 to ensure the number \
      of decimal places is stated correctly.",
      "i" = "It replaces the quotes around `{name_x}`{msg_key_arg}."
    ),
    call = caller$frame
  )
}


#' Subset every `n`th element
#'
#' @param x Vector from which the `n`th element should be subsetted.
#' @param n Numeric. Distance between two consecutive elements that will be
#'   subsetted.
#' @param from Numeric. Index of `x` where subsetting will start. Default is
#'   `1L`.
#'
#' @return Vector containing some (or, in theory, all) elements of `x`.
#'
#' @noRd
parcel_nth_elements <- function(x, n, from = 1L) {
  x[seq(from = from, to = length(x), by = n)]
}


#' Switch back and front columns
#'
#' @param data Data frame
#'
#' @return Data frame, like `data` but with the column order reversed.
#'
#' @noRd
reverse_column_order <- function(data) {
  if (ncol(data) == 0L) {
    return(data)
  }

  # Don't mind sequence linting here; the early return above takes care of the
  # empty edge case already!
  col_numbers_reversed <- ncol(data):1L
  data[, order(col_numbers_reversed)]
}


#' Conveniently add classes to an object
#'
#' `add_class()` is pipeable, unlike the replacement function it wraps.
#'
#' @param x Some object. In scrutiny, always a tibble.
#' @param new_class String. One or more classes that will be added to the
#'   `class(x)` attribute. They are prepended before the classes of `x`, so that
#'   subclasses that are added later take precedence over existing -- and more
#'   generic -- base classes.
#'
#' @return `x` but with new classes.
#'
#' @noRd
add_class <- function(x, new_class) {
  `class<-`(x, value = c(new_class, class(x)))
}


#' Check whether lengths are congruent
#'
#' `check_lengths_congruent()` is called within a function `f()` and takes a
#' list of arguments to `f()` supplied by the user (`var_list`). It checks if
#' two or more of those arguments have lengths that are greater than 1.
#'
#' If at least two of these lengths are also different from each other and the
#' `error` argument is `TRUE` (the default), the function will throw a precisely
#' informative error. If they have the same > 1 length and the `warn` argument
#' is `TRUE` (the default), there will be an informative warning.
#'
#' The only dependencies of this function are {rlang} and {cli}. As these are
#' tidyverse backend packages that most users have installed already, the
#' function might conceivably be used more widely.
#'
#' @param var_list List of variables that were passed to the enclosing function
#'   as arguments.
#' @param error Logical (length 1). Should an error be thrown if lengths are not
#'   congruent? Default is `TRUE`.
#' @param warn Logical (length 1). If no error is thrown, should a warning be
#'   issued if appropriate (see description)? Default is `TRUE`.
#'
#' @return No return value; might throw error or warning.
#'
#' @noRd
check_lengths_congruent <- function(var_list, error = TRUE, warn = TRUE) {
  var_lengths <- lengths(var_list)
  var_list_gt1 <- var_list[var_lengths > 1L]

  # Condition of checking for error and warning:
  if (length(var_list_gt1) > 1L) {
    # The argument names are only ever needed inside this branch, and capturing
    # them is by far the most expensive thing the function does -- some thirty
    # times the cost of everything else here put together. It used to be done
    # unconditionally at the top with `rlang::enexprs()`, which every caller
    # paid for on every call, `reround()` included, and that one is called once
    # per candidate sum inside GRIMMER's loop.
    #
    # `substitute()` rather than `enexprs()` because `lengths()` above has
    # already forced the promise, and a forced promise makes `enexprs()` return
    # the *values* rather than the expression -- the names would be lost.
    # `substitute()` reads `PRCODE`, which survives forcing.
    var_names <- as.list(substitute(var_list))[-1L]
    var_names <- as.character(var_names)
    var_names_gt1 <- var_names[var_lengths > 1L]
    vnames_gt1_all <- var_names_gt1 # for the warning

    # Two arguments of the same length are congruent, so only one of each
    # distinct length needs to survive into the error condition below. The
    # duplicates have to be found among the lengths greater than 1, not among
    # all of them: `duplicated(var_lengths)` is as long as `var_list`, and
    # indexing the shorter `var_list_gt1` with it dropped whichever elements
    # happened to line up with a repeated length-1 argument -- usually none of
    # them, so the deduplication did nothing at all. Two arguments that were
    # both length 2 then counted as two distinct lengths and raised an error
    # about having to be the same length, which they already were.
    length_dup <- duplicated(var_lengths[var_lengths > 1L])
    var_list_gt1 <- var_list_gt1[!length_dup]
    var_names_gt1 <- var_names_gt1[!length_dup]

    # Error condition, checking if there is more than one element of `var_list`
    # with a unique length greater than one (the duplicated lengths were
    # filtered out from `var_list_gt1` right above):
    if (error && (length(var_list_gt1) > 1L)) {
      x <- var_list_gt1[[1L]]
      y <- var_list_gt1[[2L]]
      x_name <- var_names_gt1[[1L]]
      y_name <- var_names_gt1[[2L]]

      residues_names <- var_names[!var_names %in% c(x_name, y_name)]

      msg_error <- c(
        "`{x_name}` and `{y_name}` must have the same length \\
        unless either has length 1.",
        "*" = "`{x_name}` has length {length(x)}.",
        "*" = "`{y_name}` has length {length(y)}."
      )

      # Append-to-error-message condition:
      if (length(residues_names) > 0L) {
        residues_names <- paste0("`", residues_names, "`")
        msg_error <- append(
          msg_error,
          c("i" = "This also applies to {residues_names}.")
        )
      }

      # Throw error:
      cli::cli_abort(msg_error, call = rlang::caller_env())
    }

    # Warning condition, triggered if more than one element of `var_list` has
    # length > 1, it's the same length for all (hence no error), and the `warn`
    # argument is `TRUE` (the default):
    if (warn) {
      x_name <- vnames_gt1_all[[1L]]
      y_name <- vnames_gt1_all[[2L]]

      l_vnames <- length(vnames_gt1_all)

      if (l_vnames > 2L) {
        msg_example <- ", for example,"
      } else {
        msg_example <- ""
      }

      if (l_vnames == 2L) {
        one_both_all <- "one or both"
        var_count <- ""
      } else {
        one_both_all <- "all (or all but one)"
        var_count <- l_vnames
      }

      vnames_gt1_all <- paste0("`", vnames_gt1_all, "`")

      # Throw warning:
      cli::cli_warn(c(
        "Values of {vnames_gt1_all} get paired.",
        "!" = "Are you sure that{msg_example} each `{x_name}` value \\
        should correspond to a different `{y_name}` value?",
        ">" = "It might be better if {one_both_all} of these {var_count} \\
        variables have length 1."
      ))
    }
  }
}


#' Check length
#'
#' Make sure a vector `x` has length `l`, otherwise throw an informative error.
#' For example, if a vector called `vals` must have length 1, run:
#' `check_length(vals, 1)`.
#'
#' @param x Vector.
#' @param l Numeric. Length that `x` should have.
#'
#' @return No return value; might throw error.
#'
#' @noRd
check_length <- function(x, l, allow_null = FALSE) {
  if (length(x) != l) {
    name <- deparse(substitute(x))

    if (allow_null && is.null(x)) {
      return(invisible(NULL))
    }

    null_qualifier <- if (allow_null) {
      " unless it's `NULL`"
    } else {
      NULL
    }

    cli::cli_abort(
      message = c(
        "!" = "`{name}` must have length {l}{null_qualifier}.",
        "x" = "It has length {length(x)}."
      ),
      call = rlang::caller_env()
    )
  }
}


#' Check type
#'
#' Much the same as `check_length()`, but for object types rather than lengths.
#' An object `x` must have one of the types in `t`, or else there will be an
#' informative error.
#'
#' @param x Vector.
#' @param t Numeric. Type that `x` should have.
#'
#' @return No return value; might throw error.
#'
#' @noRd
check_type <- function(x, t) {
  if (!any(typeof(x) == t)) {
    msg_name <- deparse(substitute(x))
    if (length(t) == 1L) {
      msg_object <- "be of type"
    } else {
      msg_object <- "be one of these types:"
    }

    cli::cli_abort(
      message = c(
        "!" = "`{msg_name}` must {msg_object} {t}.",
        "x" = "It is {an_a_type(x)}."
      ),
      call = rlang::caller_env()
    )
  }
}


#' Check class
#'
#' Much the same as `check_length()` or `check_type()`, but for classes. An
#' object `x` must have one of the types in `t`, or else there will be an
#' informative error.
#'
#' @param x Vector.
#' @param cl Numeric. Class that `x` should have.
#'
#' @return No return value; might throw error.
#'
#' @noRd
check_class <- function(x, cl) {
  if (!inherits(x, cl)) {
    msg_name <- deparse(substitute(x))

    cli::cli_abort(
      message = c(
        "!" = "`{msg_name}` must inherit class \"{cl}\".",
        "x" = "It doesn't."
      ),
      call = rlang::caller_env()
    )
  }
}


#' Check whether an object is a tibble
#'
#' Note: This assumes the name of `x` within the user-calles function is `data`.
#'
#' @param x A user-supplied data frame.
#'
#' @return Logical (length 1).
#'
#' @noRd
check_tibble <- function(data) {
  if (tibble::is_tibble(data)) {
    return(invisible(NULL))
  }

  msg_what_it_is <- if (is.data.frame(data)) {
    c("i" = "Convert it with `tibble::as_tibble()`.")
  } else if (is.function(data)) {
    c(
      "x" = "It is {.obj_type_friendly {data}}.",
      "i" = "Is there a variable called `data`? If not, R falls back to the \\
      `data()` function, which is what happened here."
    )
  } else {
    c("x" = "It is {.obj_type_friendly {data}}.")
  }

  cli::cli_abort(
    message = c("!" = "`data` must be a tibble.", msg_what_it_is),
    call = rlang::caller_env()
  )
}


#' Check that a number of decimal places is usable
#'
#' @description `check_digits_whole()` is called from `reround()`, the general
#'   interface that every rounding method goes through.
#'
#'   A number of decimal places is a whole number by definition. A fractional
#'   one scales `x` by a non-power of ten -- `10^1.5` is about 31.6 -- so the
#'   result sits on no decimal grid at all: `reround(1.25, digits = 1.5,
#'   rounding = "up")` used to be `1.264911`. Worse, `rounding = "even"` was
#'   unaffected, since [`base::round()`] rounds `digits` to a whole number
#'   before using it, so scrutiny's own methods disagreed with each other on the
#'   same input.
#'
#'   The check is not repeated in the `round_*()` primitives. Those run once per
#'   candidate value inside GRIMMER's loop over sums of squares, and a mapper
#'   multiplies that by the rows; `reround()` is the one place every path
#'   passes through exactly once.
#'
#'   A missing `digits` passes, so that it propagates to an `NA` result the way
#'   a missing `x` does. An infinite one does not: it is not a decimal level,
#'   and `10^Inf` would take every value to `NaN`.
#'
#' @param digits The `digits` argument of the calling function.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_digits_whole <- function(digits) {
  if (is.numeric(digits)) {
    # The normal and cheap case: one pass over `digits`, no allocation beyond
    # it, and no `cli` fn calls. `NaN` is caught by `is.na()` and passes with
    # the other missing values.
    ok <-
      is.na(digits) |
      (is.finite(digits) & abs(digits - round(digits)) < WHOLE_NUMBER_TOLERANCE)

    if (all(ok)) {
      return(NULL)
    }

    # Failure path, where the cost no longer matters:
    offenders <- digits[!ok]
    cli::cli_abort(
      message = c(
        "`digits` must be whole numbers.",
        "x" = "It has {length(offenders)} value{?s} that {?is/are} not: \\
        {offenders}.",
        "i" = "Each value is a number of decimal places, so a fractional one \\
        would scale `x` by a non-power of ten and return a number on no \\
        decimal grid."
      ),
      call = rlang::caller_env()
    )
  }

  cli::cli_abort(
    message = c(
      "`digits` must be whole numbers.",
      "x" = "It is {.obj_type_friendly {digits}}."
    ),
    call = rlang::caller_env()
  )
}


#' Check that a rounding threshold is usable
#'
#' @description `check_threshold_valid()` is called within curly braces inside
#'   of the switch statement in `reconstruct_rounded_numbers_scalar()`, and from
#'   `rounding_offsets()`, if `rounding` includes `"_from"` and therefore
#'   depends on `threshold`.
#'
#'   A threshold is the point within a step at which rounding switches
#'   direction, so it has to lie strictly inside the step: at `0` or `10`, one
#'   of the two directions can never be taken, which silently turns the method
#'   into `"ceiling"`-like or `"floor"`-like behavior.
#'
#'   Before scrutiny 1.0.0, the check here was a different one: it threw an
#'   error if `threshold` was `5`, on the theory that a threshold of `5` must be
#'   the argument's default value showing through, and that the user meant to
#'   specify something else. That conflated "unspecified" with "specified as 5"
#'   -- any caller computing a threshold and passing it on failed spuriously at
#'   exactly the most common value -- and `"up_from"` with a threshold of `5` is
#'   simply `"up"`, which is a correct answer rather than an error.
#'
#' @param threshold The `threshold` argument of the calling function.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_threshold_valid <- function(threshold) {
  if (
    length(threshold) != 1L ||
      !is.numeric(threshold) ||
      !is.finite(threshold) ||
      threshold <= 0 ||
      threshold >= 10
  ) {
    msg_what_it_is <- if (is.numeric(threshold) && length(threshold) == 1L) {
      "It is {threshold}."
    } else {
      "It is {.obj_type_friendly {threshold}}."
    }
    cli::cli_abort(
      message = c(
        "`threshold` must be a single number greater than 0 and less than 10.",
        "x" = msg_what_it_is,
        "i" = "It is the point within a step at which rounding switches \\
        direction, so both directions have to remain possible.",
        "i" = "With `rounding` set to \"up_from\", \"down_from\", or \\
        \"up_from_or_down_from\", `x` is rounded up or down from `threshold` \\
        instead of from 5."
      ),
      call = rlang::caller_env()
    )
  }
}


#' Split into groups
#'
#' Split up a vector `x` into groups that each consist of a number of elements
#' equal to `group_size` -- or, if the division has a remainder, the final group
#' will have fewer elements, and the function will issue an informative warning.
#'
#' @param x Vector.
#' @param group_size Number of elements in each resulting group (except,
#'   perhaps, the last group).
#'
#' @return Named list of groups. The names are equal to the indices.
#'
#' @noRd
split_into_groups <- function(x, group_size) {
  check_length(group_size, 1L)
  remainder <- length(x) %% group_size

  if (remainder != 0L) {
    if (!is_whole_number(group_size)) {
      cli::cli_abort(
        message = c(
          "!" = "`group_size` must be a whole number.",
          "x" = "It is `{group_size}`."
        ),
        call = rlang::caller_env()
      )
    }
    name_x <- deparse(substitute(x))
    msg_el <- if (remainder == 1L) "element" else "elements"
    cli::cli_warn(c(
      "!" = "`x` (`{name_x}`) can't be evenly divided into \\
      groups of {group_size}.",
      "x" = "It has length {length(x)}, so the last group has \\
      {remainder} {msg_el}, not {group_size}."
    ))
  }

  split(x, ceiling(seq_along(x) / group_size))
}


#' Split a data frame into rows
#'
#' Each row becomes a list-element.
#'
#' @param data Data frame or matrix.
#'
#' @return Named list of rows. The names are equal to the row numbers.
#'
#' @noRd
split_into_rows <- function(data) {
  split_into_groups(x = t(data), group_size = ncol(data))
}


#' Lowest step size (stride) of decimal numbers
#'
#' Computes the smallest possible difference between two numbers on the lowest
#' decimal level of `x`. This goes by the one element of `x` with the most
#' decimal numbers.
#'
#' For example, if `x` is `c(7, 3.5, 8.27)`, the greatest number of decimal
#' places is 2, and the smallest possible difference on the level of two decimal
#' places is `0.01`, so this value is returned.
#'
#' @param x Numeric (or string coercible to numeric).
#'
#' @return Numeric.
#'
#' @noRd
step_size <- function(x) {
  digits <- max(decimal_places(x))
  1 / (10^digits)
}


#' Sequence typing helper
#'
#' This helper is called within the sequence-generating functions
#' `seq_distance()`, `seq_endpoint()`, and `seq_disperse()`. It processes their
#' `string_output` argument and coerces the preliminary return sequence, `out`,
#' to the desired type. Depending on how `string_output` was specified by the
#' user, this type might be the type of the argument that was the original
#' starting point of the sequence, `from`.

#' The function coerces `out` to string (and pads it with trailing zeros using
#' `restore_zeros()`) in either of these two cases, and to the type of `from()`
#' otherwise:
#' - `string_output` is `TRUE`.
#' - `string_output` is `"auto"` and `from` is string.

#' @param out Numeric (or string coercible to numeric). Preliminary sequence
#'   output of the calling `seq_*()` function.
#' @param from Numeric (or string coercible to numeric). Argument of the calling
#'   function. The `out` sequence was generated starting from this point.
#' @param string_output Logical (or a string that says `"auto"`).
#' @param digits Numeric. Number of digits to which `out` will be padded if it's
#'   coerced to string.
#'
#' @return Numeric or string (see above).
#'
#' @noRd
manage_string_output_seq <- function(out, from, string_output, digits) {
  if (string_output == "auto") {
    if (is.character(from)) {
      return(restore_zeros(out, width = digits))
    } else {
      return(methods::as(out, typeof(from)))
    }
  } else if (!is.logical(string_output)) {
    if (is.character(string_output)) {
      string_output <- paste0("\"", string_output, "\"")
    } else {
      string_output <- paste0("`", string_output, "`")
    }
    cli::cli_abort(
      message = c(
        "!" = "`string_output` must be logical or \"auto\".",
        "x" = "It is {string_output}."
      ),
      call = rlang::caller_env()
    )
  } else if (string_output) {
    return(restore_zeros(out, width = digits))
  } else if (typeof(from) != "character") {
    return(methods::as(out, typeof(from)))
  }
  out
}


#' Paste and enumerate with commas and `"and"`
#'
#' - If `x` is length 1, it is returned unchanged.
#' - If `x` is length 2, its elements are pasted together but separated by word
#'   "and".
#' - If `x` is length > 2, all of its elements will be pasted into a string, but
#' separated by commas within this string. The word "and" is inserted before the
#' last element.
#'
#' @param x String (or coercible to string).
#'
#' @return String (length 1).
#'
#' @noRd
commas_and <- function(x) {
  if (length(x) == 1L) {
    return(x)
  }

  if (length(x) == 2L) {
    collapse <- " "
    and <- " and "
  } else {
    collapse <- ", "
    and <- ", and "
  }

  x[-length(x)] |>
    stringr::str_flatten(collapse = collapse) |>
    paste0(and, x[length(x)])
}


#' Check that no element of a numeric vector is negative
#'
#' Throws error if any element of `x` is less than 0.
#'
#' @param x Numeric.
#'
#' @return No return value; might throw error.
#'
#' @noRd
check_non_negative <- function(x) {
  offenders <- x[x < 0]

  if (length(offenders) > 0L) {
    if (length(offenders) > 3L) {
      offenders <- offenders[1:3]
      msg_among_others <- ", among others"
    } else {
      msg_among_others <- ""
    }

    offenders <- paste0("`", offenders, "`")
    name <- deparse(substitute(x))

    cli::cli_abort(
      message = c(
        "!" = "`{name}` can't be negative.",
        "x" = "It contains {offenders}{msg_among_others}."
      ),
      call = rlang::caller_env()
    )
  }
}


#' Test for even parity
#'
#' `TRUE` for every even element of `x`, `FALSE` for every odd one.
#'
#' @param x Numeric.
#'
#' @return Logical vector of length `length(x)`.
#'
#' @noRd
is_even <- function(x) {
  x %% 2 == 0
}


#' Check for length-1 sample size in dispersion functions
#'
#' @description Only used within `disperse()` and `disperse_total()`. In these
#'   functions, the `n` argument must be length 1. This is in contrast to
#'   `disperse2()` where it must be length 2, so a length-2 `n` will trigger
#'   an error message that specifically points to `disperse2()`.
#'
#'   All `n` values with a length other than 1 will trigger an error that refers
#'   the user to `?disperse()`.
#'
#' @param n Argument from `disperse()` or `disperse_total()` by the same name.
#' @param msg_single String (length 1). Error message specific to the calling
#'   function, i.e., `disperse()` or `disperse_total()`: Their reasons for
#'   requiring a length-1 `n` differ from each other.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_length_disperse_n <- function(n, msg_single) {
  if (length(n) != 1L) {
    if (length(n) == 2L) {
      msg_single <- paste(
        msg_single,
        "Did you mean to call `disperse2(n = c({n[1L]}, {n[2L]}))`?"
      )
    }

    cli::cli_abort(
      message = c(
        "`n` has length {length(n)}.",
        "x" = msg_single,
        "i" = "See documentation under `?disperse`."
      ),
      call = rlang::caller_env()
    )
  }
}


#' Check if a vector is numeric or coercible to numeric
#'
#' `check_type_numeric_like()` throws an informative error if `is_numeric_like()`
#'   returns `FALSE`. This means it tolerates `NA`, not just `TRUE.`
#'
#' @param x Object to be tested.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_type_numeric_like <- function(x) {
  if (isFALSE(is_numeric_like(x))) {
    name <- deparse(substitute(x))

    if (rlang::is_vector(x)) {
      length_non_na <- length(x[!is.na(x)])

      if (length_non_na == 1L) {
        msg_values <- "a non-`NA` value"
        msg_elements <- "element"
      } else {
        msg_values <- "non-`NA` values"
        msg_elements <- "elements"
      }

      cli::cli_abort(
        message = c(
          "!" = "`{name}` must be numeric or coercible to numeric.",
          "i" = "(This means that converting it to numeric \\
        must return {msg_values} for its {length_non_na} \\
        non-`NA` {msg_elements}.)"
        ),
        call = rlang::caller_env()
      )
    } else {
      cli::cli_abort(
        message = c(
          "!" = "`{name}` must be numeric or coercible to numeric.",
          "x" = "It is {an_a_type(x)}."
        ),
        call = rlang::caller_env()
      )
    }
  }
}


#' Recover the reported value a sequence was dispersed from
#'
#' @description A `*_map_seq()` output records, for every dispersed row, how far
#'   that row sits from the value it was dispersed from: `diff_var` counts the
#'   steps, and each step is one unit of `by`. The reported value is therefore
#'   `value - diff_var * by`, exactly, for every row of the group.
#'
#'   This replaces an earlier version that inferred the reported value from the
#'   *shape* of the sequence -- the midpoint of its gap, or its median if it had
#'   no gap. That inference is only valid when the sequence is complete. When
#'   `out_min` or `out_max` truncated one side of it, the gap disappeared along
#'   with the values below it, the median branch fired, and a plausible but wrong
#'   value was returned: `grim_map_seq()` on a reported mean of `-2.51` came back
#'   from `reverse_map_seq()` as `-2.48`, and `audit_seq()` then re-tested that
#'   and reported the opposite verdict.
#'
#' @param x Numeric (or coercible to numeric). The dispersed values of one
#'   `(case, var)` group.
#' @param diff_var Numeric. The `diff_var` column of the same group, in steps.
#' @param by Numeric (length 1) or `NULL`. The step size. If `NULL`, it is
#'   inferred as the smallest distance between two neighboring values, which is
#'   correct for any linear dispersion but needs at least two distinct values.
#'
#' @return Length 1, of the same type as `x`.
#'
#' @noRd
index_case_from_diff <- function(x, diff_var, by = NULL) {
  x_orig <- x
  x <- as.numeric(x)

  # Without a step size from the caller, the sequence supplies it: the values
  # sit on a grid of `by`, so the smallest gap between two of them is `by`
  # itself. A single-value sequence has no such gap and nothing to go on:
  if (is.null(by)) {
    steps <- diff(sort(unique(x)))
    if (length(steps) == 0L) {
      cli::cli_abort(c(
        "Can't recover the reported value from a single dispersed value.",
        "i" = "The step size is not deducible from one value alone."
      ))
    }
    by <- min(steps)
  }

  # Rounding to the decimal level of the step undoes the representation error
  # in the multiplication, exactly as `seq_disperse()` does when it builds the
  # sequence in the first place:
  digits <- max(decimal_places_scalar(by), 0L)
  candidates <- round(x - (diff_var * by), digits)

  # Every row of the group describes the same reported value from a different
  # offset, so they must all agree. If they don't, the output was not produced
  # by a linear dispersion around one value, and there is no reported value to
  # recover -- better to say so than to return one of the candidates:
  if (!all(dplyr::near(candidates, candidates[[1L]]))) {
    cli::cli_abort(c(
      "Can't recover the reported value: the dispersed values disagree \\
      about it.",
      "x" = "They imply {length(unique(candidates))} different values, \\
      starting with {utils::head(unique(candidates), 3L)}.",
      "i" = "Was this data frame subset or reordered after \\
      `*_map_seq()` returned it?"
    ))
  }

  index_case <- candidates[[1L]]
  index_case <- methods::as(index_case, typeof(x_orig))

  # A string sequence keeps the trailing zeros that its values were written
  # with, so the recovered value has to keep them too:
  if (is.character(index_case)) {
    index_case <- restore_zeros(index_case, width = max(decimal_places(x_orig)))
  }

  index_case
}


#' Compute central index
#'
#' @param x Vector of an odd length (!).
#'
#' @return Index of the central value in `x`.
#'
#' @noRd
index_central <- function(x) {
  ((length(x) - 1) / 2) + 1
}


#' Check for arguments with or via dots
#'
#' @description `check_old_args_split_by_parens()` checks a call to
#'   `split_by_parens()` or `restore_zeros_df()` for certain kinds of errors
#'   that used to be part of the design of these functions, but no longer are:
#'
#'   1. Column names are selected via the dots, `...`.
#'   2. Argument names are prefixed with a dot, like `.transform` or
#'   `.check_decimals`.
#'   3. `col1` or `col2` are specified. (After losing their prefix dots, these
#'   arguments of `split_by_parens()` were renamed to `end1` and `end2`.)
#'
#'   If any of these cases, a precisely informative error is thrown. There is
#'   also a more generic error if any other argument is passed through the dots,
#'   `...`. This used to be checked within `split_by_parens()` and
#'   `restore_zeros_df()` themselves.

#' @param data Input data frame of the main function itself.
#' @param dots Captures in the main function with `rlang::enquos(...)`.
#' @param old_args String vector with the old, dot-prefixed arguments.
#' @param name_fn String. Name of the main function.
#'
#' @details Error 2 also points the user to the shift from `col*` to `end*` if
#'   `.col1` or `.col2` were specified, much like error 3 does.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_new_args_without_dots <- function(data, dots, old_args, name_fn) {
  if (length(dots) == 0L) {
    return(invisible(NULL))
  }

  dots_names <- names(purrr::map(dots, rlang::as_label))

  # Error 1: Column names are selected via the dots, `...`.
  offenders1 <- dots_names[dots_names %in% colnames(data)]
  if (length(offenders1) > 0L) {
    if (length(offenders1) == 1L) {
      msg_cols <- glue::glue("{offenders1}")
    } else {
      msg_cols <- stringr::str_flatten(as.character(offenders1), ", ")
      msg_cols <- paste0("c(", msg_cols, ")")
    }
    cli::cli_abort(
      message = c(
        "!" = "`{name_fn}()` no longer uses the dots, `...`, \\
      for column selection.",
        "i" = "Use the `cols` argument instead, like `cols = {msg_cols}`.",
        "*" = "Apologies for the inconvenience."
      ),
      call = rlang::caller_env()
    )
  }

  arg_names <- names(rlang::caller_call())

  # Error 2: Argument names are prefixed with a dot, like `.transform`.
  offenders2 <- arg_names[arg_names %in% old_args]
  if (length(offenders2) > 0L) {
    if (length(offenders2) == 1L) {
      msg_was_were <- "was"
      msg_dot_dots <- "a dot"
    } else {
      msg_was_were <- "were"
      msg_dot_dots <- "dots"
    }
    msg_new_args <- stringr::str_remove(offenders2, ".")

    if (
      name_fn == "split_by_parens" &&
        any(c("col1", "col2") %in% msg_new_args)
    ) {
      msg_new_args[msg_new_args == "col1"] <- "end1"
      msg_new_args[msg_new_args == "col2"] <- "end2"
      msg_switch_end <- " Note the shift from `col*` to `end*`."
    } else {
      msg_switch_end <- ""
    }
    msg_new_args <- wrap_in_backticks(msg_new_args)
    offenders2 <- wrap_in_backticks(offenders2)
    cli::cli_abort(
      message = c(
        "!" = "In `{name_fn}()`, {offenders2} {msg_was_were} \\
        renamed to {msg_new_args} (without {msg_dot_dots}).{msg_switch_end}",
        "*" = "Apologies for the inconvenience."
      ),
      call = rlang::caller_env()
    )
  }

  if (name_fn == "split_by_parens") {
    # Error 3: `col1` or `col2` are specified (only in `split_by_parens()`).
    offenders3 <- arg_names[arg_names %in% c("col1", "col2")]
    if (length(offenders3) > 0L) {
      if (length(offenders3) == 1L) {
        msg_no_args <- "is not an argument"
        msg_dot_dots <- "with a dot"
      } else {
        msg_no_args <- "are not arguments"
        msg_dot_dots <- "with dots"
      }
      msg_offenders_old <- paste0(".", offenders3)
      msg_offenders_old <- wrap_in_backticks(msg_offenders_old)
      msg_new_args <- stringr::str_replace(offenders3, "col", "end")
      msg_new_args <- wrap_in_backticks(msg_new_args)
      offenders3 <- wrap_in_backticks(offenders3)
      cli::cli_abort(
        message = c(
          "!" = "{offenders3} {msg_no_args} of `{name_fn}()`.",
          "i" = "You're right not to use {msg_offenders_old} anymore \\
          ({msg_dot_dots}), but also note that it says {msg_new_args} now.",
          "*" = "Apologies for the inconvenience."
        ),
        call = rlang::caller_env()
      )
    }
  }

  # Finally, check that no other arguments are passed through the dots:
  rlang::check_dots_empty(env = rlang::caller_env(n = 1L))
}


#' Transformation helper for `split_by_parens()`
#'
#' @description Only called within `split_by_parens()`, and only if the latter
#'   function's `transform` argument is set to `TRUE`.
#'
#'   `transform_split_parens()` pivots the data into a longer format using
#'   `tidyr::pivot_longer()`. It lumps values from all original columns into two
#'   new columns named after the two split-column endings (`"x"` and `"sd"` by
#'   default), but preserves the information about their origin by storing it in
#'   a `.origin` column.
#'
#' @param data Data frame created as an intermediate product within
#'   `split_by_parens()`.

#' @return Data frame with these columns:
#' - `.origin`: Names of the original columns of the data frame that
#'   `split_by_parens()` took as an input.
#' - Two columns named after the values of `split_by_parens()`'s `end1` and
#'   `end2` arguments. Default are `"x"` and `"sd"`.
#'
#' @noRd
transform_split_parens <- function(data, end1, end2) {
  uscore_end1 <- paste0("_", end1)
  uscore_end2 <- paste0("_", end2)

  cols_1 <- data |>
    dplyr::select(contains(uscore_end1)) |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = ".origin",
      values_to = end1
    )

  cols_1 <- cols_1 |>
    dplyr::mutate(key = seq_len(nrow(cols_1)))

  cols_2 <- data |>
    dplyr::select(contains(uscore_end2)) |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = ".origin_2",
      values_to = end2
    )

  cols_2 <- cols_2 |>
    dplyr::mutate(key = seq_len(nrow(cols_2)))

  out <- dplyr::left_join(cols_1, cols_2, by = "key")

  out$key <- NULL
  out$.origin_2 <- NULL

  out |>
    dplyr::mutate(.origin = stringr::str_remove(.data$.origin, uscore_end1)) |>
    dplyr::arrange(.data$.origin)
}


#' Select columns before `"consistency"`
#'
#' Useful helper for selecting all "tested" columns in the sense of
#' `vignette("consistency-tests-in-depth")`; i.e., those columns that factored
#' into a consistency test applied by a mapper function like `grim_map()`.
#'
#' @param data Data frame resulting from a consistency test mapper function,
#'   such as `grim_map()`.
#' @param before String (length 1). Name of the first column that will not be
#'   selected. Default is `"consistency"`, which should hardly be changed.
#'
#' @return Data frame with a number of columns equal to $k - 1$, where $k$ is
#'   the index of `before` in `data`.
#'
#' @noRd
select_tested_cols <- function(data, before = "consistency") {
  index_last_tested_col <- match(before, colnames(data)) - 1L
  cols <- data[1L:index_last_tested_col]
  # Exclude digits_* columns: they are metadata added by *_map_seq() and must
  # not be treated as key test variables (e.g. by reverse_map_seq()):
  cols[!grepl("^digits_", colnames(cols))]
}


#' Wrap into backticks
#'
#' For error messages and similar.
#'
#' @param x String (or coercible to string).
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_backticks <- function(x) {
  paste0("`", x, "`")
}


#' Wrap into quotation marks
#'
#' For error messages and similar.
#'
#' @param x String (or coercible to string).
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_quotes <- function(x) {
  paste0("\"", x, "\"")
}


#' Wrap into quotation marks if string, else in backticks
#'
#' For error messages and similar. Like `wrap_in_quotes_if_string()` except a
#' non-string `x` is wrapped into backticks (rather than being returned
#' unchanged).
#'
#' @param x Any object.
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_quotes_or_backticks <- function(x) {
  if (is.character(x)) {
    paste0("\"", x, "\"")
  } else {
    paste0("`", x, "`")
  }
}


#' Test numbers for near-equality, other objects for identity
#'
#' When testing for equality, strict equality as assessed by `identical()` would
#' be asking too much from numeric values, so `dplyr::near()` is used if both
#' `x` and `y` are numeric. `identical()` is used otherwise.
#'
#' @param x,y Two objects to be compared.
#'
#' @return Logical (length 1).
#'
#' @details Since `near()` is vectorized and `identical()` is not, their results
#'   are not on par with each other, so `near()` must be wrapped in `all()`,
#'   which makes sure that there are no differences beyond the tolerance.
#'
#' @noRd
about_equal <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) {
    all(dplyr::near(x, y))
  } else {
    identical(x, y)
  }
}


#' Get name of function being called
#'
#' Returns the name of the function within which `name_caller_call()` is called
#' (by default of `n`).
#'
#' @param n The number of callers to go back. See `?rlang::caller_call()`.
#' @param wrap Logical. If `TRUE` (the default), the output is wrapped into
#'   backticks and appended with `()`.
#'
#' @return String (length 1).
#'
#' @noRd
name_caller_call <- function(n = 1L, wrap = TRUE) {
  name <- fn_name_from_call(rlang::caller_call(n = n))

  # The caller may have been invoked as a function object rather than by name,
  # as when `audit_seq()` applies a factory-made function via `do.call()`. There
  # is then no name to report, so a description stands in for one -- without
  # backticks, because it is not code. Taking `name[[1L]]` as-is used to throw
  # here ("cannot coerce type 'closure'").
  if (!nzchar(name)) {
    return("the function")
  }

  if (wrap) {
    name <- paste0("`", name, "()`")
  }

  name
}


# `round_up_from()` and `round_down_from()` both shift the scaled value so that
# `floor()` or `ceiling()` cuts it at `threshold` rather than at 5, and both
# nudge it by `ROUNDING_TOLERANCE` beforehand. These are the amounts they add
# and subtract.
#
# The two are mirror images of each other about the step, not about zero:
# `round_up_from()` goes up when the cut-off part is at least `threshold`
# tenths of a step, and `round_down_from()` goes down when it is at most that
# many, so the two differ only in where a value sitting exactly on the
# threshold goes. Both offsets are the distance from the far end of the step to
# the threshold, measured from the end that the function's own primitive
# (`floor()` or `ceiling()`) cuts at.
#
# Up to scrutiny 1.0.0 the two shared one offset, `tie_offset()`, which made
# `round_down_from()` the point reflection of `round_up_from()` -- it switched
# direction at `10 - threshold`. See the `threshold` parameter of
# `round_up_from()` for why that had to go.
#
# Before that, the nudge was written in those functions as `threshold -
# .Machine$double.eps^0.5`, which the `/ 10` below turns into the very same
# additive `ROUNDING_TOLERANCE`. Everything depended on that equality, since
# `unround()` reports bounds that assume one shared tolerance, but it was not
# stated anywhere.

tie_offset_up <- function(threshold) {
  1 - (threshold / 10) + ROUNDING_TOLERANCE
}

tie_offset_down <- function(threshold) {
  (threshold / 10) + ROUNDING_TOLERANCE
}


# The two procedures that a compound rounding method is made of, or the method
# itself if it is not a compound one. `reround()` returns one value per input
# value for a single procedure and two -- interleaved -- for a compound one, so
# a caller that wants to keep working on each of those branches separately needs
# to know which procedure produced it.

rounding_constituents <- function(rounding) {
  # fmt: skip
  switch(
    rounding,
    "up_or_down"           = c("up", "down"),
    "up_from_or_down_from" = c("up_from", "down_from"),
    "ceiling_or_floor"     = c("ceiling", "floor"),
    rounding
  )
}


# `rounding`, `threshold`, and `symmetric` describe one rounding procedure, so
# each of them has to be a single value; only `x` is a vector. `reround()` calls
# this before dispatching, and `rounding_offsets()` before deriving the bounds,
# which between them covers every path a consistency test can take -- including
# the mappers, which used to pass a vector straight down to a `*_scalar()`
# function and let R's own error surface from deep inside it ("'length = 2' in
# coercion to 'logical(1)'").
#
# `unround()` is the one exception, and it makes its own arrangements: it is
# documented as vectorized over `rounding` and recycles all four arguments to a
# common length before calling `rounding_offsets()` once per element.

check_rounding_spec_singular <- function(rounding, threshold, symmetric) {
  if (
    length(rounding) != 1L ||
      length(threshold) != 1L ||
      length(symmetric) != 1L
  ) {
    cli::cli_abort(
      c(
        "`rounding`, `threshold`, and `symmetric` must each have length 1.",
        "x" = "They have lengths {length(rounding)}, {length(threshold)}, \\
        and {length(symmetric)}.",
        "i" = "They describe a single rounding procedure, which is then \\
        applied to all values.",
        "i" = "To compare procedures, test once per procedure. `unround()` \\
        is vectorized over `rounding` if you need the bounds."
      ),
      call = rlang::caller_env()
    )
  }
}


resolve_ties_rounding <- function(rounding, symmetric) {
  # `[[` on a list matches exactly, so a `rounding` of "up" is not caught by
  # "ties_up" here:
  spec <- TIES_METHODS[[rounding]]
  if (is.null(spec)) {
    list(rounding = rounding, symmetric = symmetric)
  } else {
    spec
  }
}


# Give `value` -- typically derived from `abs(x)` -- the sign of `x`, so that
# rounding a negative number mirrors the rounding of its absolute value. Zero
# and positive values keep `value` as it is; `NA` and `NaN` pass through.
#
# `dplyr::if_else()` would say the same thing, but these are the package's
# innermost primitives: `round_trunc()`, `anti_trunc()`, and the `symmetric`
# branches of `round_up_from()` and `round_down_from()` run once per candidate
# value inside GRIMMER's loop over sums of squares, which the seq mappers
# multiply by hundreds of rows.

restore_sign <- function(value, x) {
  value * (1 - 2 * (x < 0))
}


#' Conventional summary statistics for `audit()` methods
#'
#' @description `audit_summary_stats()` takes a tidyselect spec and uses it to
#'   compute statistics like mean, SD, and median by column.
#'
#'   This is used in many `audit()` methods, such as those following up on
#'   `duplicate_*()` functions, as well as on `audit_seq()` and
#'   `audit_total_n()`. (The latter two have their own `audit()` methods to
#'   summarize their results even further.)
#'
#' @param data Data frame.
#' @param selection Tidyselect specification to select the columns from `data`
#'   to operate on. It is spliced into `dplyr::across()`.
#' @param total Logical. Should there be a `.total` row that summarizes across
#'   all values in `data`, regardless of their original columns? If `TRUE`,
#'   `.total` will be the last row of the output tibble. Default is `FALSE`.
#'
#' @return Tibble with summary statistics.
#'
#' @noRd
audit_summary_stats <- function(data, selection, total = FALSE) {
  selection <- rlang::enexprs(selection)

  if (total && any(".total" == colnames(data))) {
    cli::cli_abort(
      message = c(
        "`.total` can't be a column name.",
        "!" = "Please rename the `.total` column, then try again.",
        "i" = "You could use `dplyr::rename()` for this."
      ),
      call = rlang::caller_env()
    )
  }

  # The dots are merely pro forma; their purpose is to swallow up the `na.rm =
  # TRUE` specification in a for loop below.
  na_count <- function(x, ...) {
    length(x[is.na(x)])
  }

  fun_names <- c("mean", "sd", "median", "min", "max", "na_count")
  funs <- list(mean, stats::sd, stats::median, min, max, na_count)

  out <- tibble::tibble()

  # Applying each summarizing function individually, compute the output tibble
  # row by row:
  for (i in seq_along(funs)) {
    temp <- dplyr::summarise(
      data,
      dplyr::across(
        .cols = c(!!!selection),
        .fns = function(x) funs[[i]](x, na.rm = TRUE)
      )
    )
    out <- dplyr::bind_rows(out, temp)
  }

  if (total) {
    total_summary <- vector("list", length(funs))
    values_all <- data |>
      dplyr::select(c(!!!selection)) |>
      tidyr::pivot_longer(dplyr::everything()) |>
      dplyr::pull("value")
    for (i in seq_along(funs)) {
      total_summary[[i]] <- funs[[i]](values_all, na.rm = TRUE)
    }
    total_summary <- c(".total", total_summary)
    names(total_summary) <- c("term", fun_names)
  } else {
    total_summary <- NULL
  }

  out |>
    t() |>
    tibble::as_tibble(.name_repair = function(x) fun_names) |>
    dplyr::mutate("term" = names(out), .before = 1L) |>
    dplyr::bind_rows(total_summary) |>
    dplyr::mutate(na_rate = na_count / nrow(data), .after = "na_rate")
}


#' List of minimal-distance functions for `audit_seq()`
#'
#' @description The functions collected in `list_min_distance_functions` are
#'   mapped in one particular place within `audit_seq()` and shouldn't really be
#'   used elsewhere.
#'
#'   Instead of being individually defined as named functions or being used as
#'   anonymous functions directly inside of `audit_seq()`, they are stored in a
#'   list for greater efficiency -- in terms of both speed and memory.
#'
#'   The `x` parameter in all three functions is an integer vector measuring the
#'   number of dispersion steps between inconsistent reported values and their
#'   consistent neighbors. The notion of "steps" is the same as in, e.g.,
#'   `grim_map_seq()`.
#'
#' @return List of three functions.
#'
#' @noRd
list_min_distance_functions <- list(
  # Absolute distance:
  function(x) {
    vapply(
      x,
      function(x) {
        if (!all(is.numeric(x))) {
          return(NA_real_)
        }
        min(abs(x), na.rm = TRUE)
      },
      numeric(1L),
      USE.NAMES = FALSE
    )
  },

  # Positive distance:
  function(x) {
    vapply(
      x,
      function(x) {
        if (!all(is.numeric(x))) {
          return(NA_real_)
        }
        min(x[x > 0L], na.rm = TRUE)
      },
      numeric(1L),
      USE.NAMES = FALSE
    )
  },

  # Negative distance:
  function(x) {
    vapply(
      x,
      function(x) {
        if (!all(is.numeric(x))) {
          return(NA_real_)
        }
        max(x[x < 0L], na.rm = TRUE)
      },
      numeric(1L),
      USE.NAMES = FALSE
    )
  }
)


#' Resolve `"auto"` dispersion limits for one variable
#'
#' @description A sequence mapper disperses several variables, and they do not
#'   share a domain: a sample size cannot go below 1, a standard deviation
#'   cannot go below 0, a binary proportion is confined to `[0, 1]`, and a mean
#'   is not bounded at all. `out_min` and `out_max` are single arguments applied
#'   to every variable in turn, so `"auto"` has to mean something different for
#'   each of them.
#'
#'   Up to scrutiny 1.0.0, `"auto"` meant one decimal unit above zero, whatever
#'   the variable was. That is right for `n` and wrong for the rest: it silently
#'   removed the entire lower half of the dispersion of a negative mean, and it
#'   put `0` -- a legal mean, SD, and proportion -- out of reach everywhere.
#'
#'   A variable with no declared bounds keeps a floor of 1 if it is `n`, and is
#'   treated as unbounded otherwise.
#'
#' @param var String (length 1). Name of the variable being dispersed.
#' @param out_min,out_max The mapper's arguments. Anything other than the string
#'   `"auto"` is the caller's explicit choice and is returned unchanged.
#' @param var_bounds Named list of length-2 numeric vectors, `c(min, max)`, with
#'   `NA` for an unbounded side. May be `NULL`.
#'
#' @return List of two elements, `out_min` and `out_max`.
#'
#' @noRd
resolve_var_bounds <- function(var, out_min, out_max, var_bounds = NULL) {
  bounds <- var_bounds[[var]]

  # A sample size of 0 leaves nothing to test, and a negative one is not a
  # sample size at all. This is the one bound that holds for every consistency
  # test, so it applies even to a mapper that declares no bounds of its own:
  if (is.null(bounds) && var == "n") {
    bounds <- c(1, NA)
  }

  pick <- function(side) {
    if (is.null(bounds) || is.na(bounds[[side]])) NULL else bounds[[side]]
  }

  list(
    out_min = if (identical(out_min, "auto")) pick(1L) else out_min,
    out_max = if (identical(out_max, "auto")) pick(2L) else out_max
  )
}


#' Check that declared variable bounds are well formed
#'
#' @param var_bounds The `.var_bounds` argument of a function factory.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_var_bounds <- function(var_bounds) {
  if (is.null(var_bounds)) {
    return(invisible(NULL))
  }

  if (!is.list(var_bounds) || is.null(names(var_bounds))) {
    cli::cli_abort(c(
      "`.var_bounds` must be a named list.",
      "x" = "It is {an_a_type(var_bounds)}.",
      "i" = "Name each element after a reported variable, e.g. \\
      `list(n = c(1, NA), sd = c(0, NA))`."
    ))
  }

  for (name in names(var_bounds)) {
    bounds <- var_bounds[[name]]
    if (!is.numeric(bounds) || length(bounds) != 2L) {
      cli::cli_abort(c(
        "`.var_bounds${name}` must be a numeric vector of length 2.",
        "i" = "It states the least and the greatest value that `{name}` can \\
        take, with `NA` for an unbounded side."
      ))
    }
    if (
      !is.na(bounds[[1L]]) &&
        !is.na(bounds[[2L]]) &&
        bounds[[1L]] > bounds[[2L]]
    ) {
      cli::cli_abort(c(
        "`.var_bounds${name}` has its bounds the wrong way round.",
        "x" = "The minimum, {bounds[[1L]]}, is greater than the maximum, \\
        {bounds[[2L]]}."
      ))
    }
  }

  invisible(NULL)
}


#' Check for linearly increasing dispersion in sequence mapper output
#'
#' @description This throws an error if a data frame returned by a sequence
#'   mapper (i.e., a function such as `grim_map_seq()`) was computed with the
#'   `dispersion` argument of that sequence mapper specified as anything other
#'   than a linearly increasing sequence.
#'
#'   For example, the default `1:5` is linearly increasing, but `5:1` and `c(3,
#'   7, 2)` are not.
#'
#' @param data Data frame returned by a function made by `function_map_seq()`.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_dispersion_linear <- function(data) {
  if (inherits(data, "scrutiny_map_seq_disp_nonlinear")) {
    name_mapper <- class(data)[grepl("_map_seq$", class(data))]
    name_mapper <- name_mapper[name_mapper != "scrutiny_map_seq"]
    name_mapper <- sub("scrutiny_*", "", name_mapper)

    cli::cli_abort(
      message = c(
        "Invalid for data with this dispersion.",
        "!" = "`audit_seq()` is only applicable if `dispersion` \\
      in `{name_mapper}()` is a linearly increasing sequence.",
        "i" = "This limitation may be removed in a future version of scrutiny."
      ),
      call = rlang::caller_env()
    )
  }
}


#' Generate code to process the `"consistency"` column
#'
#' @description Call `write_code_col_key_result()` within a function factory
#'   such as `function_map()`. It returns an expression to be unquoted at the
#'   end of the factory-made function's body using ``!!!`()`.
#'
#'   This will insert code into the body that may process the `"consistency"`
#'   column of the output data frame, `out`, in one or both of these two ways:
#'
#'   - If the `name_key_result` argument is not the default `"consistency"`, the
#'   column will be renamed accordingly. This makes sense when applying tests
#'   that are not consistency tests. (As of now, these other procedures also
#'   need to return logical values.)
#'   - If the column is still a list, it is transformed into a logical vector
#'   using `unlist()`.
#'
#' @param name_key_result String (length 1). The `.name_key_result` argument of
#'   the function factory, passed to the present function.
#' @param name_data Expression. It must contain the name of the data frame
#'   operated on. To construct it, use `rlang::expr()`.
#' @param rename Logical (length 1). Should the code rename a `"consistency"`
#'   column to `name_key_result`? `TRUE` in `function_map()`, which is where the
#'   column is created. `FALSE` in the seq and total-n factories: these build on
#'   a basic mapper's output, so the column arrives already carrying its final
#'   name, and only the `unlist()` half of the generated code is wanted.
#'
#' @return Expression.
#'
#' @noRd

# ❯ checking R code for possible problems ... NOTE
# write_code_col_key_result: no visible binding for global variable ‘out’
# write_code_col_key_result: no visible global function definition for
# ‘!!<-’
# Undefined global functions or variables:
#   !!<- out

write_code_col_key_result <- function(
  name_key_result = "consistency",
  name_data = rlang::expr(out),
  out = NULL,
  rename = TRUE
) {
  # Enable renaming the `"consistency"` column for binary procedures that are
  # not consistency tests:
  code_rename <- if (!rename || name_key_result == "consistency") {
    NULL
  } else {
    rlang::expr({
      `!!`(name_data) <- dplyr::rename(
        `!!`(name_data),
        `!!`(name_key_result) := consistency
      )
    })
  }

  # Prepare defused expressions that are more simple to splice into the code
  # further below:
  data_dollar_result <- paste0(name_data, "$", name_key_result)
  condition_not_list <- paste0("!is.list(", data_dollar_result, ")")

  # Convert the strings to expressions:
  condition_not_list <- rlang::parse_expr(condition_not_list)
  data_dollar_result <- rlang::parse_expr(data_dollar_result)

  # Generate code to process the (possibly renamed) key result column:
  rlang::expr({
    `!!!`(code_rename)

    # Use the pre-computed condition expression
    if (`!!`(condition_not_list)) {
      return(`!!`(name_data))
    }

    `$<-`(
      `!!`(name_data),
      `!!`(name_key_result),
      unlist(`!!`(data_dollar_result), use.names = FALSE)
    )
  })
}
