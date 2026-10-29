#' @include import-reexport.R is-numeric-like.R

utils::globalVariables(c(
  ".",
  "where",
  "desc",
  "all_of",
  "contains",
  "everything",
  "x",
  "items_averaged",
  "frac",
  "distance",
  "both_consistent",
  "fn",
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
  "na_count",
  "fn_name",
  # Used in the bodies that the function factories build:
  "!!",
  "!!!",
  "constant",
  "include_consistent",
  "n_min",
  "n_max",
  "!!<-"
))


# Do NOT export any of these! ---------------------------------------------

#' Pipe helper
#'
#' Call a syntactically special or anonymous function, or a function factory,
#' within a pipe workflow.
#'
#' @param x Any object.
#' @param f A function that takes exactly one (required) argument.
#'
#' @examples
#' # From tests/testthat/test-digits-vectorized.R
#' out$n[out$var == "n"] |>
#'   call_on(\(x) x %% 1 == 0) |>
#'   all()
#'
#' @noRd
call_on <- function(x, f) {
  if (is.function(f)) {
    f(x)
  } else {
    abort_in_export("Can only call functions.")
  }
}

#' Mark a string as wrong
#'
#' @param x Object that should have been a string.
#'
#' @return String.
#'
#' @noRd
wrong_spec_string <- function(x) {
  if (is.character(x)) {
    wrap_in_quotes_or_backticks(x)
  } else {
    paste0("`", x, "` (not a string)")
  }
}


#' Write "an" or "a", depending on the next word
#'
#' @param x String. A string that starts with a lowercase vowel returns `"an"`;
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
#' For messages. Writes `"factor"` for a factor, `"double (numeric value)"`
#' for a double, and `"string"` for a character vector.
#'
#' @param x Any object.
#'
#' @return String.
#'
#' @noRd
an_a_type <- function(x) {
  type <- typeof(x)

  if (is.factor(x)) {
    type <- "factor"
  } else if (type == "double") {
    type <- "double (numeric value)"
  } else if (type == "character") {
    type <- "string"
  }

  article <- an_a(type)
  paste(article, type)
}


#' Check whether numbers are whole
#'
#' @description Tests each element of `x` for being a whole number, whatever
#'   its type. Adapted from `is.wholenumber()` in the examples of `?integer`.
#'
#' @param x Numeric.
#' @param tolerance Numeric. Differences from the nearest whole number below
#'   this are ignored. Default is about `1.5e-8`.
#'
#' @return Logical vector of the same length as `x`.
#'
#' @author R Core Team, Lukas Jung
#'
#' @noRd
# `is_decidable_n_items()` and `check_newly_numeric()` write this test out
# rather than calling it, because they run once per row; keep them in step.
is_whole_number <- function(x, tolerance = WHOLE_NUMBER_TOLERANCE) {
  abs(x - round(x)) < tolerance
}


#' Refuse too many candidate sums for GRIMMER
#'
#' @description `grimmer_scalar()` loops over every integer sum the reported
#'   mean admits, about `n * items_averaged / 10^digits_x` of them, so a huge
#'   `n` (e.g., a typo) would run for a very long time. The limit is far above
#'   any published summary statistic.
#'
#' @param bounds Numeric (length 2). The `c(lower, upper)` of the sums.
#' @param n,digits_x Numeric (length 1). For the message.
#' @param limit Numeric (length 1). The largest number of sums admitted.
#'
#' @return No return value. Might throw an error.
#'
#' @noRd
check_enumeration_size <- function(bounds, n, digits_x, limit = 1e6) {
  size <- bounds[2L] - bounds[1L] + 1
  if (isTRUE(size > limit)) {
    abort_in_export(
      "`n` is too large for GRIMMER.",
      "x" = "With `n = {n}` and `digits_x = {digits_x}`, the mean admits \
      {round(size)} integer sums, and GRIMMER checks each of them.",
      "i" = "The limit is {limit}."
    )
  }
}


#' Check that `grim_values()` or `grimmer_values()` has few enough rows
#'
#' @description Both return one row per achievable value, so a huge `n` (e.g.,
#'   a typo) would fill the memory rather than fail. The check runs before the
#'   rows are allocated. The limit, `VALUES_MAX`, applies to each value set.
#'
#' @param size Numeric (length 1). The number of rows so far, or a lower bound.
#' @param what String. What the rows are: `"means"` or `"pairs"`.
#' @param values Named list of the reported values, to say which value set it
#'   is in a vectorized call.
#'
#' @return No return value. Might throw an error.
#'
#' @noRd
check_values_size <- function(size, what, values) {
  if (size <= VALUES_MAX) {
    return(invisible())
  }
  hint <- if (what == "pairs") {
    "Their number grows with {.arg n}, {.arg items_averaged}, and {.arg sd}, \\
    and shrinks with {.arg digits_x} and {.arg digits_sd}."
  } else {
    "Their number grows with {.arg n} and {.arg items_averaged}, and shrinks \\
    with {.arg digits_x}."
  }
  limit <- format(VALUES_MAX, big.mark = ",", scientific = FALSE)
  values <- vapply(values, format, character(1L), scientific = FALSE, digits = 15L)
  value_set <- paste0("`", names(values), " = ", values, "`", collapse = ", ")
  abort_in_export(
    "Too many achievable {what} for {.fn {name_last_export()}}.",
    "x" = "The value set with {value_set} admits more than {limit} {what}, \\
    and the output has one row for each.",
    "i" = hint
  )
}


# A setting describes the test as a whole, so it must be a single value.
# `along` names the arguments that may differ between value sets, and
# `name_mapper` the function to call once per value, if there is one:
check_setting_length <- function(name, value, along, name_mapper) {
  if (length(value) == 1L) {
    return(invisible())
  }
  abort_in_export(
    "{.arg {name}} must be length 1, not {length(value)}.",
    "x" = "It describes the test as a whole, so it cannot vary from one \\
    value set to the next.",
    "i" = "Only {.arg {along}} may differ between value sets.",
    "i" = if (exists(name_mapper, mode = "function")) {
      "To test value sets that differ in {.arg {name}}, call \\
      {.fn {name_mapper}} once for each of its values."
    }
  )
}


#' Can a value set be decided at all, going by `n` and `items_averaged`?
#'
#' @description All three consistency tests reason about integer data: GRIM
#'   about the integer sum behind a reported mean, GRIMMER about that sum and
#'   the integer sum of squares behind a reported SD, DEBIT about a count of
#'   ones among `n` binary values. A sample size or item count that is not a
#'   positive whole number describes no such data set, so the test returns
#'   `NA`.
#'
#' @param n Numeric (length 1). The reported sample size.
#' @param items_averaged Numeric (length 1). The number of items composing `x`.
#'   DEBIT has no such argument and leaves it at the default.
#' @param min_n Numeric (length 1). The smallest `n` the test can work with:
#'   `1` for GRIM, since the mean of a single value is that value; for GRIMMER
#'   and DEBIT, `sd_min_n(sd_meaning)`, since a sample SD divides by `n - 1`.
#'
#' @return Logical vector, recycled to the length of the longer of `n` and
#'   `items_averaged`: `TRUE` where the test can decide the case, `FALSE` where
#'   it cannot. Never `NA`: a missing `n` or `items_averaged` is undecidable.
#'   The `*_scalar()` functions call this on single values,
#'   `grim_exp_incon()` on columns.
#'
#' @noRd
is_decidable_n_items <- function(n, items_averaged = 1, min_n = 1) {
  # `is.finite()` is `FALSE` for `NA`, `NaN`, and the infinities, and `FALSE &
  # NA` is `FALSE`, so the result is never missing: the guards absorb missing
  # values, since `&` evaluates both sides.
  #
  # The whole-number tests are `is_whole_number()` written out, because this
  # runs once per row and the calls would cost more than all the comparisons.
  # Keep them in step with that function and with `check_newly_numeric()`.
  #
  # The condition appears twice, with `&&` and with `&`, and the two must stay
  # identical. `&&` neither allocates nor evaluates what it does not need, so
  # the scalar version, which the `*_scalar()` functions reach once per row, is
  # some 1.8 times faster. `grim_exp_incon()` needs the column version. A
  # test in test-utils.R requires both to agree over a grid.
  if (length(n) == 1L && length(items_averaged) == 1L && length(min_n) == 1L) {
    return(
      is.finite(n) &&
        is.finite(items_averaged) &&
        n >= min_n &&
        items_averaged > 0 &&
        abs(n - round(n)) < WHOLE_NUMBER_TOLERANCE &&
        abs(items_averaged - round(items_averaged)) < WHOLE_NUMBER_TOLERANCE
    )
  }

  is.finite(n) &
    is.finite(items_averaged) &
    n >= min_n &
    items_averaged > 0 &
    abs(n - round(n)) < WHOLE_NUMBER_TOLERANCE &
    abs(items_averaged - round(items_averaged)) < WHOLE_NUMBER_TOLERANCE
}


# The number that the sum of squared deviations is divided by to give the
# square of the reported value, under any `sd_meaning` but `"sd_sample_or_pop"`.
# For an SE, the sample SD divided by `sqrt(n)`, that is `n * (n - 1)`.
#
# `sd_min_n()` is the smallest `n` an `sd_meaning` can work with: a sample SD
# divides by `n - 1`, so it needs two values, a population SD only one.
sd_denominator <- function(n, sd_meaning) {
  switch(sd_meaning, sd_pop = n, se = n * (n - 1), n - 1)
}

sd_min_n <- function(sd_meaning) {
  if (sd_meaning %in% c("sd_pop", "sd_sample_or_pop")) 1 else 2
}


#' Coerce a sample-size column to integer, but only if that is lossless
#'
#' @description Mappers store the `n` column as integer, since a sample size is
#'   a whole number. The coercion is skipped if any value is fractional or
#'   beyond `.Machine$integer.max`, which `as.integer()` would turn into `NA`
#'   next to a verdict computed from the real value.
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
#' A `digits_*` argument is either a single number or one number per row of
#' `data`, for columns where the number of decimal places varies.
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
    abort_in_export(
      "`{name_digits_arg}` must have length 1 or the number of rows \\
      in `data`.",
      "x" = "It has length {length(digits)}, but `data` has \\
      {n_rows} row{?s}.",
      "i" = "Use a single number if all values in the column were reported \\
      with the same number of decimal places."
    )
  }
}


error_digits_flawed <- function(digits, name_digits_arg) {
  check_length(digits, 1)

  abort_in_export(
    "`{name_digits_arg}` must be a single whole number from 0 to 15.",
    "x" = "It is {wrap_in_quotes_or_backticks(digits)}."
  )
}


check_newly_numeric <- function(
  x,
  digits,
  caller_type = c("basic", "mapper", "sequence_mapper")
) {
  # The whole-number test below is inlined rather than left to
  # `is_whole_number()`: this runs once per key value per row, and the call
  # would cost more than the whole check.
  if (length(digits) != 1L || !(is.numeric(digits) || is.na(digits))) {
    name <- deparse(substitute(digits))
    error_digits_flawed(digits, name)
  }

  # A missing number of decimal places makes the case undecidable, like a
  # missing `x`, so a per-row `digits_*` can be `NA` for some rows:
  if (is.na(digits)) {
    return(invisible(NULL))
  }

  # A double holds 15 significant decimal digits. Beyond that, the tests stop
  # being exact and would return wrong verdicts:
  if (
    !is.finite(digits) ||
      digits < 0 ||
      digits > 15 ||
      abs(digits - round(digits)) >= WHOLE_NUMBER_TOLERANCE
  ) {
    name <- deparse(substitute(digits))
    error_digits_flawed(digits, name)
  }

  # The 15 significant digits include the integer digits of `x`, so a value of
  # 1 or more allows fewer decimal places. Compared to a power of 10, which a
  # double holds exactly, rather than counted with `log10()`, which rounds:
  if (is.numeric(x) && is.finite(x) && abs(x) >= 10^(15 - digits)) {
    name <- deparse(substitute(digits))
    name_x <- deparse(substitute(x))
    digits_int <- nchar(sprintf("%.0f", floor(abs(x))))
    abort_in_export(
      "`{name_x}` has more than 15 significant digits.",
      "x" = "With `{name} = {digits}`, `{name_x} = {sprintf('%.*f', digits, x)}` \\
      has {digits + digits_int}.",
      "i" = "A double holds 15 significant digits, so the tests are not exact \\
      beyond that."
    )
  }

  # Can `x` be written with `digits` decimal places? `round()` answers that some
  # 200 times faster than `decimal_places_scalar()` (#92), but only one way:
  # `0.1 + 0.2` prints as `"0.3"` without being that double, so whatever it
  # leaves undecided is counted after all. Keep it one-sided; a tolerance would
  # disagree with the count above about 15 significant digits. An infinity
  # passes, which is right: the tests report it as undecidable.
  if (
    is.na(x) ||
      (is.numeric(x) &&
        (x == round(x, digits) || digits >= decimal_places_scalar(x)))
  ) {
    return(invisible(NULL))
  }

  caller_type <- rlang::arg_match(caller_type)

  # The function the user called, e.g. `grim_map()` rather than
  # `grim_scalar()`, so that errors say "Error in `grim_map()`":
  caller_name <- name_last_export()
  caller_frame <- sys.frame(index_last_export())

  # A mapper's messages talk about columns, not arguments:
  caller_is_mapper <- grepl("_map", caller_name)

  name_x <- deparse(substitute(x))
  name_fn <- paste0("scrutiny::", caller_name)

  if (!is.numeric(x)) {
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
      env = caller_frame
    )
  }

  # `digits_*` is too small for the decimal places in `x`:
  name_digits_arg <- paste0("digits_", name_x)
  digits_in_x <- decimal_places_scalar(x)

  abort_in_export(
    "More decimal places than specified digits.",
    "!" = "Number of digits specified via `{name_digits_arg}` \
      must not be less than the number of decimal places in `{name_x}`.",
    "x" = "`{name_digits_arg}` is {digits}.",
    "x" = "`{name_x}` is {x}, so it has {digits_in_x} decimal place{?s}."
  )
}


#' Error if a `digits_*` argument is missing
#'
#' `x` and `sd` are numeric, so their decimal places are given by `digits_x`
#' and `digits_sd`, which have no defaults. `error_digits_missing()` explains
#' this better than R's generic `argument "digits_x" is missing, with no
#' default`, to users who are used to string input. Always use it like this:
#'
#' ```
#' if (missing(digits_x)) {
#'   error_digits_missing(x)
#' }
#' ```
#'
#' @param x The key argument whose `digits_*` argument is missing.
#'
#' @returns No return value; throws an error.
#'
#' @noRd
error_digits_missing <- function(x) {
  name_x <- deparse(substitute(x))
  name_digits_arg <- paste0("digits_", name_x)

  # The example shows the function the user called, e.g. `debit()` rather than
  # `debit_scalar()`:
  name_fn <- name_last_export()

  # If the error occurred in a GRIMMER or DEBIT function, include `sd` and
  # `digits_sd` arguments in the example call because they are required there.
  # DEBIT's mean and SD must lie between 0 and 1:
  is_debit <- grepl("debit", name_fn)
  is_total_n <- grepl("_total_n", name_fn)
  example_x <- if (is_debit) c("0.35", "0.35") else c("1.40", "1.4")
  # A total-n mapper takes the statistics of two groups:
  part_x <- if (is_total_n) {
    if (is_debit) "x1 = 0.35, x2 = 0.41" else "x1 = 1.4, x2 = 2.3"
  } else {
    paste0("x = ", example_x[2])
  }
  if (grepl("(grimmer|debit)", name_fn)) {
    part_sd <- if (is_total_n) {
      if (is_debit) ", sd1 = 0.48, sd2 = 0.49" else ", sd1 = 0.62, sd2 = 0.71"
    } else if (is_debit) {
      ", sd = 0.48"
    } else {
      ", sd = 0.62"
    }
    # ...but not twice if `digits_sd` is the argument that is missing:
    part_digits_sd <- if (name_digits_arg == "digits_sd") {
      ", digits_x = 2"
    } else {
      ", digits_sd = 2"
    }
  } else {
    part_sd <- NULL
    part_digits_sd <- NULL
  }

  # A mapper's example builds a data frame:
  if (grepl("_map", name_fn)) {
    part_tibble_open <- "tibble::tibble("
    part_tibble_close <- ")"
  } else {
    part_tibble_open <- NULL
    part_tibble_close <- NULL
  }

  # GRIM and GRIMMER functions need `items_averaged` as well:
  part_items <- if (grepl("grim", name_fn)) ", items_averaged = 1"

  # In case the key argument is still a string, tell the user this changed
  msg_key_arg <- if (is.character(x)) {
    paste0(", so please specify `", name_x, "` as a number, not a string")
  } else {
    NULL
  }

  # `is_percentage_of()` has its own key arguments and never took strings:
  is_percentage <- name_x == "percentage"
  args_example <- paste0(part_x, part_sd, ", n = 29")
  if (is_percentage) {
    example_x <- c("33.30", "33.3")
    args_example <- "absolute = 40, percentage = 33.3, n = 120"
  }

  msg_history <- if (is_percentage) {
    NULL
  } else {
    c(
      "i" = "This was introduced in scrutiny 1.0.0 to ensure the number \
      of decimal places is stated correctly.",
      "i" = "It replaces the quotes around `{name_x}`{msg_key_arg}."
    )
  }

  abort_in_export(
    "Need to specify `{name_digits_arg}` to state the number of \
      decimal places in `{name_x}`.",
    "i" = "For example, with {example_x[1]} (two decimal places): \
      `{name_fn}({part_tibble_open}{args_example}{part_tibble_close}, \
      {name_digits_arg} = 2{part_digits_sd}{part_items})`",
    msg_history,
    "i" = "For more information, visit \\
      {.href [scrutiny's changelog](https://lhdjung.github.io/scrutiny/news/index.html)}."
  )
}


#' Error if `items_averaged` is missing
#'
#' Like `digits_x`, `items_averaged` has no default, so that every call states
#' the assumption: a default of 1 would let multi-item scales pass as single
#' items. Usage pattern:
#'
#' ```
#' if (missing(items_averaged)) {
#'   error_items_missing()
#' }
#' ```
#'
#' @returns No return value; throws an error.
#'
#' @noRd
error_items_missing <- function() {
  name_fn <- name_last_export()
  msg_column <- if (grepl("_map", name_fn)) {
    ", or give `data` an `items_averaged` column"
  }

  abort_in_export(
    "Need to specify `items_averaged`{msg_column}.",
    "i" = "It is the number of items averaged into each respondent's score, \\
    such as 4 for a mean over four questions. It is not the number of scale \\
    points.",
    "i" = "For a single item, or a percentage of one yes/no question: \\
    `items_averaged = 1`.",
    "i" = "This argument replaces `items` as of scrutiny 1.0.0, which \\
    defaulted to 1."
  )
}


#' Conveniently add classes to an object
#'
#' `add_class()` is pipeable, unlike the replacement function it wraps.
#'
#' @param x Some object. In scrutiny, always a tibble.
#' @param new_class String. One or more classes, prepended to `class(x)` so
#'   that they take precedence over the existing, more generic ones.
#'
#' @return `x` but with new classes.
#'
#' @noRd
add_class <- function(x, new_class) {
  `class<-`(x, value = c(new_class, class(x)))
}


#' Check length
#'
#' Make sure a vector `x` has length `l`, otherwise throw an informative error.
#' For example, if a vector called `vals` must have length 1, run:
#' `check_length(vals, 1)`.
#'
#' @param x Vector.
#' @param l Numeric. Length that `x` should have.
#' @param allow_null Logical. If `TRUE`, `x` may also be `NULL`.
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

    abort_in_export(
      "!" = "`{name}` must have length {l}{null_qualifier}.",
      "x" = "It has length {length(x)}."
    )
  }
}


#' Check type
#'
#' Like `check_length()`, but for types: `x` must have one of the types in `t`.
#' A factor always fails.
#'
#' @param x Vector.
#' @param t String. Types that `x` may have.
#'
#' @return No return value; might throw error.
#'
#' @noRd
check_type <- function(x, t) {
  # A factor's type is `"integer"`, but its values are level codes, not the
  # numbers it shows:
  if (is.factor(x) || !any(typeof(x) == t)) {
    msg_name <- deparse(substitute(x))
    if (length(t) == 1L) {
      msg_object <- "be of type"
    } else {
      msg_object <- "be one of these types:"
    }

    abort_in_export(
      "!" = "`{msg_name}` must {msg_object} {t}.",
      "x" = "It is {an_a_type(x)}."
    )
  }
}


#' Check class
#'
#' Like `check_type()`, but for classes: `x` must inherit from `cl`.
#'
#' @param x Vector.
#' @param cl String. Class that `x` should have.
#'
#' @return No return value; might throw error.
#'
#' @noRd
check_class <- function(x, cl) {
  if (!inherits(x, cl)) {
    msg_name <- deparse(substitute(x))

    abort_in_export(
      "!" = "`{msg_name}` must inherit class \"{cl}\".",
      "x" = "It doesn't."
    )
  }
}


#' Coerce a user-supplied data frame to a tibble
#'
#' Any data frame is returned as a tibble; anything else is an error. Character
#' row names, such as study labels, are kept as a `rowname` column, since a
#' tibble has none. Integer row names, as left by `data[rows, ]`, are dropped.
#'
#' Assumes that the argument of the user-called function is named `data`.
#'
#' @param data A user-supplied data frame.
#'
#' @return A tibble.
#'
#' @noRd
as_tibble_input <- function(data) {
  if (tibble::is_tibble(data)) {
    return(data)
  }

  if (is.data.frame(data)) {
    if (!is.character(attr(data, "row.names"))) {
      return(tibble::as_tibble(data))
    }
    if (rlang::has_name(data, "rowname")) {
      abort_in_export(
        "!" = "`data` has row names and a `rowname` column.",
        "x" = "The row names would be returned as a `rowname` column, too.",
        "i" = "Rename the column, or drop the row names with \\
        `rownames(data) <- NULL`."
      )
    }
    return(tibble::as_tibble(data, rownames = "rowname"))
  }

  msg_what_it_is <- if (is.function(data)) {
    c(
      "x" = "It is {.obj_type_friendly {data}}.",
      "i" = "Is there a variable called `data`? If not, R falls back to the \\
      `data()` function, which is what happened here."
    )
  } else {
    c("x" = "It is {.obj_type_friendly {data}}.")
  }

  abort_in_export(
    "!" = "`data` must be a data frame.",
    msg_what_it_is
  )
}


#' Check that a number of decimal places is usable
#'
#' @description Called from `reround()` and `unround()`. A fractional `digits`
#'   scales `x` by a non-power of ten, so the result sits on no decimal grid,
#'   while [`base::round()`] rounds `digits` itself: the methods would disagree.
#'
#'   The `round_*()` primitives don't repeat the check because they run once
#'   per candidate value. Even in `reround()`, it is a hot path, so the check
#'   makes one pass over `digits` and touches `cli` only on failure. The
#'   whole-number test is inlined for the same reason as in
#'   `check_newly_numeric()`.
#'
#'   A missing `digits`, including a logical `NA`, passes and propagates to an
#'   `NA` result. An infinite one, or one beyond 308 in either direction, fails:
#'   `10^digits` would be `Inf` or `0`.
#'
#' @param digits The `digits` argument of the calling function.
#' @param name String (length 1). Name of that argument, for errors.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_digits_whole <- function(digits, name = "digits") {
  if (!is.numeric(digits) && !(is.logical(digits) && all(is.na(digits)))) {
    abort_in_export(
      "`{name}` must be whole numbers.",
      "x" = "It is {.obj_type_friendly {digits}}."
    )
  }

  # The cheap, normal case. `NaN` passes with the other missing values. 308 is
  # the greatest power of ten a double can hold:
  ok <-
    is.na(digits) |
    (abs(digits) <= 308 &
      abs(digits - round(digits)) < WHOLE_NUMBER_TOLERANCE)

  if (all(ok)) {
    return(NULL)
  }

  # Failure path. Not being whole is reported first; an infinity counts as not
  # whole:
  offenders <- digits[!ok]
  not_whole <- offenders[
    !is.finite(offenders) |
      abs(offenders - round(offenders)) >= WHOLE_NUMBER_TOLERANCE
  ]

  if (length(not_whole) > 0L) {
    abort_in_export(
      "`{name}` must be whole numbers.",
      "x" = "It has {length(not_whole)} value{?s} that {?is/are} not: \\
        {not_whole}.",
      "i" = "Each value is a number of decimal places, so a fractional one \\
        would scale `x` by a non-power of ten and return a number on no \\
        decimal grid."
    )
  }

  abort_in_export(
    "`{name}` must be between -308 and 308.",
    "x" = "It has {length(offenders)} value{?s} that {?is/are} not: \\
      {offenders}.",
    "i" = "Beyond that, `10^digits` is not a finite, non-zero number, so \\
      `x` could not be scaled by it."
  )
}


#' Split into groups
#'
#' Split a vector `x` into groups of `group_size` elements. If there is a
#' remainder, the last group is smaller, with a warning.
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
      abort_in_export(
        "!" = "`group_size` must be a whole number.",
        "x" = "It is `{group_size}`."
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


#' Check that numbers are positive or non-negative
#'
#' `check_positive()` errors if any element of `x` is 0 or less,
#' `check_non_negative()` if any is less than 0. Missing values pass.
#'
#' @param x Numeric.
#'
#' @return No return value; might throw error.
#'
#' @noRd
check_positive <- function(x) {
  offenders <- x[which(x <= 0)]
  if (length(offenders) > 0L) {
    name <- deparse(substitute(x))
    abort_in_export(
      "!" = "`{name}` must be positive.",
      "x" = "It contains {wrap_in_backticks(utils::head(offenders, 3L))}."
    )
  }
}


check_non_negative <- function(x) {
  offenders <- x[which(x < 0)]

  if (length(offenders) > 0L) {
    if (length(offenders) > 3L) {
      offenders <- offenders[1:3]
      msg_among_others <- ", among others"
    } else {
      msg_among_others <- ""
    }

    offenders <- paste0("`", offenders, "`")
    name <- deparse(substitute(x))

    abort_in_export(
      "!" = "`{name}` can't be negative.",
      "x" = "It contains {offenders}{msg_among_others}."
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
#' @description For `disperse()` and `disperse_total()`, where `n` must have
#'   length 1. A length-2 `n` gets a hint to `disperse2()`.
#'
#' @param n Argument from `disperse()` or `disperse_total()` by the same name.
#' @param msg_single String (length 1). Error message specific to the calling
#'   function.
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

    abort_in_export(
      "`n` has length {length(n)}.",
      "x" = msg_single,
      "i" = "See documentation under `?disperse`."
    )
  }
}


#' Check if a vector is numeric or coercible to numeric
#'
#' `check_type_numeric_like()` throws an error if `is_numeric_like()` returns
#' `FALSE`, so it tolerates `NA`.
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

      abort_in_export(
        "!" = "`{name}` must be numeric or coercible to numeric.",
        "i" = "(This means that converting it to numeric \\
        must return {msg_values} for its {length_non_na} \\
        non-`NA` {msg_elements}.)"
      )
    } else {
      abort_in_export(
        "!" = "`{name}` must be numeric or coercible to numeric.",
        "x" = "It is {an_a_type(x)}."
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
#'   The shape of the sequence, such as the midpoint of its gap, is no guide:
#'   `out_min` or `out_max` may have truncated one side of it.
#'
#' @param x Numeric. The dispersed values of one `(case, var)` group.
#' @param diff_var Numeric. The `diff_var` column of the same group, in steps.
#' @param by Numeric (length 1) or `NULL`. The step size. If `NULL`, it is
#'   inferred as the distance between two values divided by the number of steps
#'   between them, which is correct for any linear dispersion but needs at
#'   least two distinct values.
#'
#' @return Length 1, of the same type as `x`.
#'
#' @noRd
index_case_from_diff <- function(x, diff_var, by = NULL) {
  # Without a step size from the caller, the sequence supplies it: two values
  # are as many steps apart as their `diff_var` values. Not the smallest gap,
  # which is several steps wide if the dispersion skips some, as
  # `dispersion = c(2, 4, 6)` does:
  if (is.null(by)) {
    if (length(unique(diff_var)) < 2L) {
      abort_in_export(
        "Can't recover the reported value from a single dispersed value.",
        "i" = "The step size is not deducible from one value alone."
      )
    }
    index_min <- which.min(diff_var)
    index_max <- which.max(diff_var)
    by <- (x[[index_max]] - x[[index_min]]) /
      (diff_var[[index_max]] - diff_var[[index_min]])
  }

  # Rounding to the decimal level of the step undoes representation error, as
  # in `seq_disperse()`:
  digits <- max(decimal_places_scalar(by), 0L)
  candidates <- round(x - (diff_var * by), digits)

  # Every row of the group describes the same reported value, so they must all
  # agree. If they don't, the output was not produced by a linear dispersion
  # around one value:
  if (!all(dplyr::near(candidates, candidates[[1L]]))) {
    abort_in_export(
      "Can't recover the reported value: the dispersed values disagree \\
      about it.",
      "x" = "They imply {length(unique(candidates))} different values, \\
      starting with {utils::head(unique(candidates), 3L)}.",
      "i" = "Was this data frame subset or reordered after \\
      `*_map_seq()` returned it?"
    )
  }

  methods::as(candidates[[1L]], typeof(x))
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


#' Transformation helper for `split_by_parens()`
#'
#' @description Called by `split_by_parens(transform = TRUE)`. Stacks the
#'   values of all split columns into two columns named after the endings
#'   (`"x"` and `"sd"` by default), recording their origin in `.origin`.
#'
#'   The split columns are looked up by their exact names, `{origin}_{end1}` and
#'   `{origin}_{end2}`. A pattern like `contains("_sd")` would also catch an
#'   original column such as `exp_sd`.
#'
#' @param data Data frame created as an intermediate product within
#'   `split_by_parens()`.
#' @param origins String. Names of the original columns that were split.
#' @param end1,end2 String. Endings of the split column names.
#'
#' @return Tibble with these columns:
#' - `.origin`: Names of the original columns of the data frame that
#'   `split_by_parens()` took as an input.
#' - Two columns named after the values of `split_by_parens()`'s `end1` and
#'   `end2` arguments. Default are `"x"` and `"sd"`.
#'
#' @noRd
transform_split_parens <- function(data, origins, end1, end2) {
  stack_cols <- function(end) {
    unlist(data[paste0(origins, "_", end)], use.names = FALSE)
  }
  tibble::tibble(
    .origin = rep(origins, each = nrow(data)),
    !!end1 := stack_cols(end1),
    !!end2 := stack_cols(end2)
  )
}


# The class that identifies a mapper's output by its test, such as
# `"scrutiny_grim_map"`. A space in `.name_test` becomes an underscore, so
# that the class is a plain name:
class_name_test <- function(name_test, suffix) {
  paste0("scrutiny_", gsub("\\s+", "_", tolower(name_test)), suffix)
}


#' Select columns before `"consistency"`
#'
#' Selects the "tested" columns in the sense of
#' `vignette("consistency-tests-in-depth")`: those that went into the test.
#'
#' @param data Data frame returned by a mapper such as `grim_map()`.
#' @param before String (length 1). Name of the first column not to select.
#'   Default is `"consistency"`.
#'
#' @return Data frame with the columns before `before`, except `digits_*`
#'   columns.
#'
#' @noRd
select_tested_cols <- function(data, before = "consistency") {
  index_last_tested_col <- match(before, colnames(data)) - 1L
  cols <- data[1L:index_last_tested_col]
  # `digits_*` columns are metadata, not key test variables, e.g. for
  # `reverse_map_seq()`:
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
#' For error messages and similar. `NULL` and missing strings get backticks,
#' too.
#'
#' @param x Any object.
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_quotes_or_backticks <- function(x) {
  if (is.null(x)) {
    "`NULL`"
  } else if (is.character(x)) {
    # A missing string is no string:
    ifelse(is.na(x), "`NA`", paste0("\"", x, "\""))
  } else {
    paste0("`", x, "`")
  }
}


#' Test numbers for near-equality, other objects for identity
#'
#' Uses `all(dplyr::near(x, y))` if both are numeric, `identical()` otherwise.
#'
#' @param x,y Two objects to be compared.
#'
#' @return Logical (length 1).
#'
#' @noRd
about_equal <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) {
    all(dplyr::near(x, y))
  } else {
    identical(x, y)
  }
}


# Name of the user-called function as code, e.g., "`grim_map()`"
name_last_export_code <- function() {
  name <- name_last_export()
  # A non-export invoked as a function object, e.g., by `do.call()`, has no name
  if (is.null(name)) "the function" else paste0("`", name, "()`")
}


# Evaluate `expr`, and if it fails, raise its error as one from the exported
# function the user called. roundwork checks the rounding arguments itself, but
# its errors name the roundwork function that scrutiny called, such as
# `sum_range()`, which the user never did.
rethrow_in_export <- function(expr) {
  tryCatch(expr, error = function(cnd) {
    cnd$call <- sys.call(index_last_export())
    stop(cnd)
  })
}


# Check `rounding`, `threshold`, and `symmetric` once per call, ahead of the
# per-row roundwork calls, by asking roundwork for the bounds of a stand-in
# value. A test with no `threshold` or `symmetric` passes `NULL` for them, and
# the defaults stand in.
check_rounding_args <- function(rounding, threshold = 5, symmetric = FALSE) {
  if (is.null(threshold)) {
    threshold <- 5
  }
  if (is.null(symmetric)) {
    symmetric <- FALSE
  }
  rethrow_in_export(
    roundwork::bound_numerators(1, 0L, rounding, threshold, symmetric)
  )
  invisible(NULL)
}


#' Conventional summary statistics for `audit()` methods
#'
#' @description `audit_summary_stats()` computes statistics like mean, SD, and
#'   median for each selected column. Used in many `audit()` methods.
#'
#' @param data Data frame.
#' @param selection Tidyselect specification to select the columns from `data`
#'   to operate on. It is spliced into `dplyr::across()`.
#' @param total Logical. If `TRUE`, a last row, `.total`, summarizes all
#'   selected values regardless of their columns. Default is `FALSE`.
#'
#' @return Tibble with summary statistics.
#'
#' @noRd
audit_summary_stats <- function(data, selection, total = FALSE) {
  selection <- rlang::enexprs(selection)

  if (total && any(".total" == colnames(data))) {
    abort_in_export(
      "`.total` can't be a column name.",
      "!" = "Please rename the `.total` column, then try again.",
      "i" = "You could use `dplyr::rename()` for this."
    )
  }

  # Each statistic is computed on the non-missing values. A column with none has
  # no mean, minimum, or maximum, so it gets `NA` rather than `NaN`, `Inf`, and
  # `-Inf` with warnings. `na_rate` is part of the list so that the `.total` row
  # divides by the number of all values, not by the number of rows:
  on_non_na <- function(f) {
    function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0L) NA_real_ else f(x)
    }
  }
  stats <- list(
    mean = on_non_na(mean),
    sd = on_non_na(stats::sd),
    median = on_non_na(stats::median),
    min = on_non_na(min),
    max = on_non_na(max),
    na_count = function(x) sum(is.na(x)),
    na_rate = function(x) if (length(x) == 0L) NA_real_ else mean(is.na(x))
  )

  # The separator is a carriage return so that `names_sep` can't be confused by
  # an underscore in a column name or in a function name (`na_count`):
  out <- data |>
    dplyr::summarise(dplyr::across(
      .cols = c(!!!selection),
      .fns = stats,
      .names = "{.col}\r{.fn}"
    )) |>
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = c("term", ".value"),
      names_sep = "\r"
    )

  if (total) {
    values_all <- data |>
      dplyr::select(c(!!!selection)) |>
      tidyr::pivot_longer(dplyr::everything()) |>
      dplyr::pull("value")
    stats_total <- stats |>
      purrr::map(function(f) f(values_all))
    out <- out |>
      dplyr::bind_rows(c(list(term = ".total"), stats_total))
  }

  out
}


#' Resolve `"auto"` dispersion limits for one variable
#'
#' @description A sequence mapper disperses several variables, and they do not
#'   share a domain: a sample size cannot go below 1, a standard deviation
#'   cannot go below 0, a binary proportion is confined to `[0, 1]`, and a mean
#'   is not bounded at all. `out_min` and `out_max` are single arguments applied
#'   to every variable in turn, so `"auto"` has to mean something different for
#'   each of them.
#'
#'   A variable with no declared bounds keeps a floor of 1 if it is `n`, and is
#'   treated as unbounded otherwise.
#'
#' @param var String (length 1). Name of the variable being dispersed.
#' @param out_min,out_max The mapper's arguments. Anything other than the string
#'   `"auto"` is the caller's explicit choice and is returned unchanged.
#' @param var_bounds Named list of length-2 numeric vectors, `c(min, max)`, with
#'   `NA` for an unbounded side, or of functions that return one from `args`.
#'   May be `NULL`.
#' @param args The settings of the initial `fn()` call, from `scrutiny_args()`.
#'
#' @return List of two elements, `out_min` and `out_max`.
#'
#' @noRd
resolve_var_bounds <- function(
  var,
  out_min,
  out_max,
  var_bounds = NULL,
  args = NULL
) {
  bounds <- var_bounds[[var]]

  # A bound that depends on a setting, such as the least `n` for an
  # `sd_meaning`, can only be checked now:
  if (is.function(bounds)) {
    bounds <- bounds(args)
    check_var_bounds(rlang::set_names(list(bounds), var))
  }

  # The one bound that holds for every consistency test, so it applies even to a
  # mapper that declares none of its own:
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
    abort_in_export(
      "`.var_bounds` must be a named list.",
      "x" = "It is {an_a_type(var_bounds)}.",
      "i" = "Name each element after a reported variable, e.g. \\
      `list(n = c(1, NA), sd = c(0, NA))`."
    )
  }

  for (name in names(var_bounds)) {
    bounds <- var_bounds[[name]]
    # Its value depends on the call, so it can't be checked yet:
    if (is.function(bounds)) {
      next
    }
    if (!is.numeric(bounds) || length(bounds) != 2L) {
      abort_in_export(
        "`.var_bounds${name}` must be a numeric vector of length 2.",
        "i" = "It states the least and the greatest value that `{name}` can \\
        take, with `NA` for an unbounded side."
      )
    }
    if (
      !is.na(bounds[[1L]]) &&
        !is.na(bounds[[2L]]) &&
        bounds[[1L]] > bounds[[2L]]
    ) {
      abort_in_export(
        "`.var_bounds${name}` has its bounds the wrong way round.",
        "x" = "The minimum, {bounds[[1L]]}, is greater than the maximum, \\
        {bounds[[2L]]}."
      )
    }
  }

  invisible(NULL)
}


#' Check for linearly increasing dispersion in sequence mapper output
#'
#' @description Errors if a sequence mapper's output, e.g. from
#'   `grim_map_seq()`, was computed with a `dispersion` that is not linearly
#'   increasing. The default `1:5` is, but `5:1` and `c(3, 7, 2)` are not.
#'
#' @param data Data frame returned by a function made by `function_map_seq()`.
#' @param name_fn String (length 1). Name of the calling function.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_dispersion_linear <- function(data, name_fn) {
  if (isFALSE(attr(data, "scrutiny", exact = TRUE)$dispersion_linear)) {
    name_mapper <- class(data) |>
      call_on(\(x) x[grepl("_map_seq$", x)]) |>
      call_on(\(x) x[x != "scrutiny_map_seq"]) |>
      sub(pattern = "scrutiny_*", replacement = "")

    abort_in_export(
      "Invalid for data with this dispersion.",
      "!" = "`{name_fn}()` is only applicable if `dispersion` \\
      in `{name_mapper}()` is a linearly increasing sequence.",
      "i" = "This limitation may be removed in a future version of scrutiny."
    )
  }
}


#' Generate code to process the `"consistency"` column
#'
#' @description Called within a function factory such as `function_map()`. It
#'   returns an expression to be unquoted with ``!!!`()` at the end of the
#'   manufactured function's body. That code processes the `"consistency"`
#'   column of the output, `out`:
#'
#'   - If `name_key_result` is not `"consistency"`, the column is renamed, as
#'   for tests that are not consistency tests. They must still return logical
#'   values.
#'   - If the column is a list, it is `unlist()`ed into a logical vector.
#'
#' @param name_key_result String (length 1). The `.name_key_result` argument of
#'   the function factory, passed to the present function.
#' @param name_data Expression with the name of the data frame operated on,
#'   made with `rlang::expr()`.
#' @param out Unused. Keeps R CMD check from reporting `out` as an undefined
#'   global variable.
#' @param rename Logical (length 1). Should the code rename a `"consistency"`
#'   column to `name_key_result`? `TRUE` in `function_map()`, which creates the
#'   column. `FALSE` in the seq and total-n factories, whose input already has
#'   the final name.
#'
#' @return Expression.
#'
#' @noRd

write_code_col_key_result <- function(
  name_key_result = "consistency",
  name_data = rlang::expr(out),
  out = NULL,
  rename = TRUE
) {
  code_rename <- if (!rename || name_key_result == "consistency") {
    NULL
  } else {
    rlang::expr({
      `!!`(name_data) <- `!!`(name_data) |>
        dplyr::rename(`!!`(name_key_result) := consistency)
    })
  }

  data_dollar_result <- paste0(name_data, "$", name_key_result)
  condition_not_list <- paste0("!is.list(", data_dollar_result, ")")

  condition_not_list <- rlang::parse_expr(condition_not_list)
  data_dollar_result <- rlang::parse_expr(data_dollar_result)

  rlang::expr({
    `!!!`(code_rename)

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
