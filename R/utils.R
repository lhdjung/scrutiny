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


# The `*_df()` functions take a data frame or a matrix. `alt` names the
# function for vectors that the user may have meant:
as_data_frame_input <- function(data, alt) {
  if (is.data.frame(data)) {
    return(data)
  }
  if (is.matrix(data)) {
    return(tibble::as_tibble(data, .name_repair = "unique"))
  }
  abort_in_export(
    "!" = "`data` must be a data frame (or a matrix).",
    "x" = "It is {an_a_type(data)}.",
    ">" = "Did you mean {alt}?"
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
# A logical setting such as `show_rec` or `include_consistent` is checked here
# so that a missing value, a string, or a vector fails with a clear message,
# not with `if ()`'s.
check_flag <- function(flag) {
  if (!isTRUE(flag) && !isFALSE(flag)) {
    name <- deparse(substitute(flag))
    abort_in_export(
      "`{name}` must be `TRUE` or `FALSE`.",
      "x" = "It is {wrap_in_quotes_or_backticks(flag)}."
    )
  }
}


# A string argument such as a separator or a column suffix: a vector would be
# recycled across columns and `NA` pasted in as "NA":
check_string <- function(string) {
  if (!is.character(string) || length(string) != 1L || is.na(string)) {
    name <- deparse(substitute(string))
    abort_in_export(
      "`{name}` must be a single string.",
      "x" = "It is {wrap_in_quotes_or_backticks(string)}."
    )
  }
}


# A rate or mean over no values is `NaN`, which reads as a computation gone
# wrong; it is a missing value like any other:
nan_to_na <- function(x) {
  x[is.nan(x)] <- NA
  x
}


is_whole_number <- function(x, tolerance = WHOLE_NUMBER_TOLERANCE) {
  abs(x - round(x)) < tolerance
}


#' Refuse too many candidate sums for GRIMMER
#'
#' @description `grimmer_scalar()` loops over the integer sums the reported
#'   mean admits, about `n * items_averaged / 10^digits_x` of them, but no more
#'   than `n`, so a huge `n` (e.g., a typo) would run for a very long time. With
#'   the mean unknown, it loops over up to `n` remainders. The limit is far
#'   above any published summary statistic.
#'
#' @param bounds Numeric. Pairs of `lower, upper` of the sums, as in
#'   `grimmer_sums_pass()`.
#' @param n,digits_x Numeric (length 1). For the message; `digits_x = NULL`
#'   for an unknown mean.
#' @param limit Numeric (length 1). The largest number of sums admitted.
#'
#' @return No return value. Might throw an error.
#'
#' @noRd
check_enumeration_size <- function(
  bounds,
  n,
  digits_x,
  limit = 1e6,
  name = "GRIMMER"
) {
  lower <- bounds[c(TRUE, FALSE)]
  size <- sum(bounds[c(FALSE, TRUE)] - lower + 1)
  if (isTRUE(size > limit)) {
    msg_given <- if (is.null(digits_x)) {
      "With `n = {format(n, big.mark = ',', scientific = FALSE)}` and the mean \\
      unknown"
    } else {
      "With `n = {format(n, big.mark = ',', scientific = FALSE)}` and \\
      `digits_x = {digits_x}`"
    }
    abort_in_export(
      "`n` is too large for {name}.",
      "x" = paste0(
        msg_given,
        ", {name} checks {format(size, big.mark = ',', scientific = FALSE)} ",
        "candidate sums."
      ),
      "i" = "The limit is {format(limit, big.mark = ',', scientific = FALSE)}."
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
  } else if (what == "samples") {
    "Their number grows with {.arg n} and shrinks with {.arg digits_x}."
  } else {
    "Their number grows with {.arg n} and {.arg items_averaged}, and shrinks \\
    with {.arg digits_x}."
  }
  limit <- format(VALUES_MAX, big.mark = ",", scientific = FALSE)
  values <- vapply(
    values,
    format,
    character(1L),
    scientific = FALSE,
    digits = 15L
  )
  value_set <- paste0("`", names(values), " = ", values, "`", collapse = ", ")
  # `debit_values()` counts the numbers of ones before the SD narrows them down:
  msg_size <- if (what == "samples") {
    "has more than {limit} numbers of ones whose mean would be reported as \\
    {.arg x}, which are listed before {.arg sd} narrows them down."
  } else {
    "admits more than {limit} {what}, and the output has one row for each."
  }
  abort_in_export(
    "Too many achievable {what} for {.fn {name_last_export()}}.",
    "x" = paste("The value set with {value_set}", msg_size),
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

# `n / sd_denominator(n, sd_meaning)`, the factor from the population variance
# to the square of the reported value. Written out because `n * (n - 1)`
# overflows for an SE from `n` around 1e154; a test requires it to agree.
sd_denominator_ratio <- function(n, sd_meaning) {
  switch(sd_meaning, sd_pop = 1, se = 1 / (n - 1), n / (n - 1))
}

# For functions that return values under one premise, such as
# `grimmer_values()`: `"sd_sample_or_pop"` stands for two, which give different
# values for the same data. Returns `sd_meaning` matched.
check_sd_meaning_single <- function(sd_meaning) {
  # Else `arg_match_in_export()` names a valid first value as the culprit:
  if (!identical(sd_meaning, SD_MEANINGS)) {
    check_length(sd_meaning, 1L)
  }
  sd_meaning <- arg_match_in_export(sd_meaning, SD_MEANINGS)
  if (sd_meaning == "sd_sample_or_pop") {
    name_fn <- name_last_export()
    abort_in_export(
      "{.arg sd_meaning} can't be {.val sd_sample_or_pop} here.",
      "x" = "It stands for two premises, and each has its own values.",
      "i" = "Call {.fn {name_fn}} with {.val sd_sample} and with \\
      {.val sd_pop} instead."
    )
  }
  sd_meaning
}


# `roundwork::sum_range()` multiplies the numerators of the rounding bounds by
# `n_items` and compares the products as whole numbers. That is exact only below
# 2^53. A numerator is `x * 10^(digits + 1)` plus an offset of up to 10, all
# times `scale`, so even `x = 0` has bounds that a large `n_items` takes past
# it. `x` alone is checked by `check_newly_numeric()`; this is the product,
# checked by each test before it calls `sum_range()`.
check_sum_range_exact <- function(
  x_num,
  n_items,
  digits,
  arg = "x",
  hint_items = TRUE,
  scale = 1
) {
  product <- (abs(x_num) * 10^(digits + 1L) + 10) * n_items * scale
  if (product >= 2^53) {
    if (arg == "x") {
      msg_what <- "sums"
      msg_formula <- "`(x * 10^(digits_x + 1) + 10) * n`"
    } else {
      msg_what <- "counts"
      msg_formula <- "`(percentage * 10^(digits_percentage + 1) + 10) * n`"
    }
    # Written like 9.007e15, whatever `options(scipen)` says:
    product <- sub(
      "e\\+?0*",
      "e",
      format(product, digits = 4L, scientific = TRUE)
    )
    abort_in_export(
      "`{arg}` times the sample size is too large for an exact test.",
      "x" = "The {msg_what} that `{arg}` admits are compared as whole numbers, \\
      which is exact only below 2^53, about 9.007e15, and {msg_formula} is \\
      about {product}.",
      "i" = if (hint_items) "`items_averaged` multiplies into `n` here.",
      "i" = if (scale > 1) msg_threshold_scale(scale)
    )
  }
}


# A `threshold` with decimal places scales the rounding bounds and their
# denominator by a power of ten (`roundwork::bound_numerators()`), and with them
# the numbers that must stay exact. That power, or 1. The common whole
# threshold is answered without asking roundwork, and so is an invalid one,
# which roundwork ignores unless `rounding` uses it, and then rejects before:
rounding_scale <- function(rounding, threshold, symmetric) {
  if (!is.numeric(threshold) || !isTRUE(threshold != round(threshold))) {
    return(1)
  }
  roundwork::bound_numerators(1, -1L, rounding, threshold, symmetric)$denom
}

msg_threshold_scale <- function(scale) {
  paste0(
    "The decimal places of `threshold` multiply this by ",
    format(scale, scientific = FALSE),
    "."
  )
}


# GRIMMER and DEBIT count up to `n` and subtract from it, which is exact only
# well below 2^53, and with the mean unknown, nothing else limits `n`. No sample
# is this large anyway:
check_n_exact <- function(n) {
  if (n >= 2^52) {
    abort_in_export(
      "`n` is too large for an exact test.",
      "x" = "It is {format(n, digits = 2L)}, but it must be below 2^52, \\
      about 4.5e15.",
      "i" = "Beyond that, a double skips whole numbers."
    )
  }
}


# Round the values within `WHOLE_NUMBER_TOLERANCE` of a whole number to it, so
# that a value is tested as the whole number it is accepted as: `100 * 0.29` is
# 28.999999999999996, which `is_decidable_n_items()` takes for 29, but the
# arithmetic would use as is, and `disperse_total()` would split as odd. Other
# values, such as a fractional `n`, are left for the tests to reject:
round_whole <- function(x) {
  # Named if the caller's variable is the argument, as in `round_whole(n)`:
  name <- substitute(x)
  name <- if (is.symbol(name)) as.character(name)
  check_not_integer64(x, name)
  check_not_factor(x, name)

  if (is.double(x)) {
    whole <- which(is_whole_number(x))
    x[whole] <- round(x[whole])
  }
  x
}


# An `integer64` vector, as a database returns for counts, is a double by type,
# so it passes for a number, but it stores each whole number in the bits of a
# double: 28 would be read as 1.4e-322, and every test would say `NA`. Only
# bit64 can convert it, so that is left to the user. `name` is the argument or
# column. `round_whole()`, which every whole-number argument goes through,
# checks those that no mapper or vectorized test has checked by name:
check_not_integer64 <- function(x, name = NULL) {
  if (inherits(x, "integer64")) {
    what <- if (is.null(name)) {
      "A whole-number argument"
    } else {
      paste0("`", name, "`")
    }

    abort_in_export(
      "{what} must not be a 64-bit integer vector.",
      "x" = "Its class is `integer64`, which can't be read as plain numbers.",
      "i" = "Convert it with `as.numeric()` first."
    )
  }
}


# A factor is no string that was meant as a number, just a column that
# `read.csv(stringsAsFactors = TRUE)` made one, and its codes are not its
# values. `round_whole()` checks every whole-number argument, by name if it
# can:
check_not_factor <- function(x, name = NULL) {
  if (is.factor(x)) {
    what <- if (is.null(name)) {
      "A whole-number argument"
    } else {
      paste0("`", name, "`")
    }
    abort_in_export(
      "{what} must be numeric, not a factor.",
      "i" = "Convert the factor with `as.numeric(as.character())`, or read \\
      the data with `stringsAsFactors = FALSE`."
    )
  }
}


#' Coerce a whole-number column to integer, but only if that is lossless
#'
#' @description Mappers store the `ARGS_WHOLE` columns, such as `n`, and the
#'   `digits_*` columns as integer, since they are whole numbers. The coercion
#'   is skipped if `n` is not numeric, or if any value is fractional or beyond
#'   `.Machine$integer.max`, which `as.integer()` would turn into `NA` next to
#'   a result computed from the real value. A value within tolerance of a
#'   whole number is rounded to it.
#'
#' @param n Numeric vector; a whole-number column such as `n`.
#'
#' @return `n`, as integer if every value is a whole number within integer
#'   range, and unchanged otherwise.
#'
#' @noRd
as_integer_if_lossless <- function(n) {
  if (
    is.numeric(n) &&
      all(is_whole_number(n) | is.na(n)) &&
      all(abs(n) <= .Machine$integer.max, na.rm = TRUE)
  ) {
    as.integer(round_whole(n))
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
  # A factor prints as the number it shows, which would look valid:
  check_not_factor(digits, name_digits_arg)

  abort_in_export(
    "`{name_digits_arg}` must be a single whole number from 0 to 15.",
    "x" = "It is {wrap_in_quotes_or_backticks(digits)}."
  )
}


check_newly_numeric <- function(
  x,
  digits,
  caller_type = c("basic", "mapper", "sequence_mapper"),
  scale = 1
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

  # The rounding bounds come from `round(x * 10^(digits + 1))`
  # (`roundwork::bound_numerators()`). That recovers the decimal `x` stands
  # for only while the product stays below 2^52: a double's error in `x` is
  # at most half a unit in the last place, and scaling it past 2^52 moves it
  # over half a unit, so `71511092749617.9 * 100` lands on `...791`. Roughly,
  # `x` may have 15 significant digits, fewer if they start high; the limit
  # includes the integer digits, so a value of 1 or more allows fewer decimal
  # places.
  if (
    is.numeric(x) && is.finite(x) && abs(x) * 10^(digits + 1L) * scale >= 2^52
  ) {
    name <- deparse(substitute(digits))
    name_x <- deparse(substitute(x))
    # Leading zeros, as in `0.05`, are not significant:
    digits_significant <- sprintf("%.*f", digits, abs(x)) |>
      sub(pattern = ".", replacement = "", fixed = TRUE) |>
      sub(pattern = "^0+", replacement = "") |>
      nchar()
    abort_in_export(
      "`{name_x}` has too many significant digits.",
      "x" = "With `{name} = {digits}`, `{name_x} = {sprintf('%.*f', digits, x)}` \\
      has {digits_significant}.",
      "i" = "The tests are exact while `{name_x} * 10^({name} + 1)` stays below \\
      2^52, about 4.5e15: up to 15 significant digits, or 14 if they start \\
      with 4.5 or more.",
      "i" = if (scale > 1) {
        digits_scale <- round(log10(scale))
        paste0(
          msg_threshold_scale(scale),
          " That costs ",
          digits_scale,
          if (digits_scale == 1) " digit." else " digits."
        )
      }
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

  check_not_factor(x, name_x)

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
  # ...except for a deprecated one, which also has no formals to tell from
  # below whether the example needs `data_type`:
  if (name_fn == "grim_probability") {
    name_fn <- "grim_exp_incon"
  }

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

  # ...and every function that takes `data_type` needs that, so that the
  # example runs as it is:
  fn_called <- get0(name_fn, mode = "function")
  part_data_type <- if (any(names(formals(fn_called)) == "data_type")) {
    ", data_type = \"integer\""
  }

  # In case the key argument is still a string, tell the user this changed
  msg_key_arg <- if (is.character(x)) {
    paste0(", so please specify `", name_x, "` as a number, not a string")
  } else {
    NULL
  }

  # `percentage_of()` has its own key arguments and never took strings:
  is_percentage <- name_x == "percentage"
  args_example <- paste0(part_x, part_sd, ", n = 29")
  if (is_percentage) {
    example_x <- c("12.50", "12.5")
    args_example <- "absolute = 15, percentage = 12.5, n = 120"
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
      {name_digits_arg} = 2{part_digits_sd}{part_items}{part_data_type})`",
    msg_history,
    "i" = "For more information, visit \\
      {.href [scrutiny's changelog](https://lhdjung.github.io/scrutiny/news/index.html)}."
  )
}


#' Error if `data_type` is missing
#'
#' Like `items_averaged`, `data_type` has no default, so that every call states
#' whether the test applies to the data at all.
#'
#' @returns No return value; throws an error.
#'
#' @noRd
error_data_type_missing <- function() {
  name_fn <- name_last_export()
  msg_column <- if (grepl("_map", name_fn)) {
    " or give `data` a `data_type` column"
  }
  # The functions that list or draw the values whole numbers can have, such as
  # `grim_values()`; see `check_data_type_whole()`:
  # Plain strings: cli doesn't interpolate the text it splices in.
  msg_continuous <- if (grepl("_values$|_closest$|_plot_line$", name_fn)) {
    paste0("`", name_fn, "()` only applies to whole numbers.")
  } else {
    paste(
      "The test does not apply, so the result is `TRUE` unless no data at all",
      "can have the value set."
    )
  }

  abort_in_export(
    "Need to specify `data_type`{msg_column}.",
    "*" = "{.val integer}: the values are whole numbers, such as answers on a \\
    rating scale or counts.",
    "*" = "{.val continuous}: the values are not whole numbers, such as \\
    reaction times. {msg_continuous}"
  )
}


# `data_type` in the functions that list or draw the values that whole numbers
# can have, such as `grim_values()`. Continuous data have no such values, so
# `"continuous"` is an error, but the argument is required all the same, so
# that every call states what it assumes. Returns `TRUE` for `"integer"` and
# `FALSE` for `NA`, which leaves the value set undecidable, unless `allow_na`
# is `FALSE`, as for a plot, which then has nothing to draw.
check_data_type_whole <- function(data_type, allow_na = TRUE) {
  check_length(data_type, 1L)
  if (is.factor(data_type)) {
    error_data_type_factor()
  }
  if (is.na(data_type)) {
    if (allow_na) {
      return(FALSE)
    }
    abort_in_export(
      "`data_type` can't be `NA` here.",
      "i" = "If the data may be continuous, there are no values to draw."
    )
  }
  if (arg_match_in_export(data_type, DATA_TYPES) == "continuous") {
    name_fn <- name_last_export()
    abort_in_export(
      "`{name_fn}()` only applies to whole numbers.",
      "x" = "`data_type` is {.val continuous}.",
      "i" = "Continuous data can have any value that would be reported as \\
      the one given, so there are no values to list or draw."
    )
  }
  TRUE
}


# The fewest decimal places that a value with a missing decimal count can
# have: those it shows. More decimal places only narrow the range of numbers
# that are rounded to the value, so a value set that is inconsistent with the
# fewest is inconsistent with any. That answer is forced, so the tests give it
# rather than `NA`. `digits` itself if it is known, or if the value is
# missing or infinite, which has no decimal places to count:
digits_least <- function(x, digits) {
  if (is.na(digits) && is.numeric(x) && is.finite(x)) {
    decimal_places_scalar(x)
  } else {
    digits
  }
}


# The other direction: so many decimal places that only data with exactly the
# value shown pass. A value set that passes with these passes with any number,
# as fewer only widen the ranges, so that answer is forced too.
#
# The range rounded to a value with `d` decimal places reaches at most `10^-d`
# from it. A mean of `n_items` whole numbers that is not exactly `x`, which has
# `digits` decimal places, is at least `1 / (n_items * 10^digits)` away from
# it; past that, only `x` itself is left. The same holds on the percentage
# scale, which adds 2 decimal places on both sides:
digits_exact_mean <- function(digits, n_items) {
  digits + ceiling(log10(n_items)) + 1
}

# Likewise for an SD of `n` means of `items_averaged` whole numbers each. Its
# square is a whole number over `n * items_averaged^2 * denominator`, and the
# square of `sd` is a whole number over `10^(2 * digits)`. Two such squares
# that differ do so by at least the inverse of their product, and the SDs by
# that over their sum, which is at most `2 * sd + 1` for an SD within 1 of
# `sd` -- one farther away is out of reach anyway. `"sd_sample_or_pop"` takes
# the larger denominator of the two, which is that of `"sd_pop"`:
digits_exact_sd <- function(sd, digits, n, items_averaged, sd_meaning) {
  if (sd_meaning == "sd_sample_or_pop") {
    sd_meaning <- "sd_pop"
  }
  denominator <- sd_denominator(n, sd_meaning) * n * items_averaged^2
  2 * digits + ceiling(log10(denominator * (2 * sd + 1))) + 1
}


# A factor `data_type`, as from a column that `read.csv(stringsAsFactors =
# TRUE)` made one, is refused with a way out rather than rlang's type error:
error_data_type_factor <- function() {
  abort_in_export(
    "`data_type` must be a character vector, not a factor.",
    "i" = "Convert it with `as.character()`, or read the data with \\
    `stringsAsFactors = FALSE`."
  )
}


# What the tests return for data that are not plainly whole numbers.
# `test_integer` is a function that runs the test as if they were,
# `continuous` is the result for continuous data, from `verdict_continuous()`,
# and `missing` the one for an undecidable case. A missing `data_type` might be
# either, so it is `NA` unless both agree.
test_data_type <- function(data_type, test_integer, continuous, missing) {
  check_length(data_type, 1L)
  if (is.factor(data_type)) {
    error_data_type_factor()
  }
  if (is.na(data_type)) {
    out <- test_integer()
    agree <- identical(out[[1L]], continuous[[1L]])
    return(if (agree) out else missing)
  }
  data_type <- arg_match_in_export(data_type, DATA_TYPES)
  if (data_type == "continuous") continuous else test_integer()
}


# The verdict on continuous data, named by its reason. Such data can have any
# mean and SD, so a missing value can be anything and passes. Only a value set
# that no data can have does not: it is undecidable, as for whole numbers, if
# `n` or `items_averaged` is given but is no whole number of at least `min_n`
# (see `is_decidable_continuous()`) or a value is infinite. It is inconsistent
# if the SD is negative, or if it is the population SD of a single value,
# which is 0, and 0 would not have been reported as `sd`; and undecidable if a
# missing `n` or SD leaves this open.
verdict_continuous <- function(
  n,
  items_averaged = 1,
  min_n = 1,
  x = NA,
  sd = NA,
  digits_sd = NA,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  if (!is_decidable_continuous(n, items_averaged, min_n)) {
    return(c("No testable value set" = NA))
  }
  if (is.infinite(x) || is.infinite(sd)) {
    return(c("Infinite value" = NA))
  }
  if (!is.na(sd) && sd < 0) {
    return(c("Negative SD" = FALSE))
  }
  # An SD of a single value is only tested where `min_n` is 1, as for a
  # population SD. A missing `n` may be 1 there, and a missing SD may be 0 or
  # not. GRIM tests no SD:
  if (!missing(sd) && min_n == 1 && (is.na(n) || n == 1)) {
    if (is.na(sd)) {
      return(c("SD of a single value missing" = NA))
    }
    # With the decimal places missing, the fewest that `sd` can have: if 0 is
    # not rounded to it with these, it is not with more.
    digits_sd <- digits_least(sd, digits_sd)
    if (!is.na(digits_sd)) {
      bounds <- roundwork::bound_numerators(
        sd,
        digits_sd,
        rounding,
        threshold,
        symmetric
      )
      if (bounds$lower > 0 || (bounds$lower == 0 && !bounds$incl_lower)) {
        return(
          if (is.na(n)) {
            c("Maybe a single value" = NA)
          } else {
            c("SD of a single value" = FALSE)
          }
        )
      }
    }
  }
  c("Continuous data" = TRUE)
}


# Is `target` among the numbers rounded to `value`? With `digits` missing,
# `value` itself must be `target`, since more decimal places narrow the range
# down to it. Data that are all the same have an SD of 0, whatever their
# number, so this asks whether such data fit.
rounds_from <- function(value, target, digits, rounding, threshold, symmetric) {
  if (!is.finite(value)) {
    return(FALSE)
  }
  if (is.na(digits)) {
    return(value == target)
  }
  bounds <- roundwork::bound_numerators(
    value,
    digits,
    rounding,
    threshold,
    symmetric
  )
  target <- target * bounds$denom
  above_lower <- bounds$lower < target ||
    (bounds$lower == target && bounds$incl_lower)
  below_upper <- bounds$upper > target ||
    (bounds$upper == target && bounds$incl_upper)
  above_lower && below_upper
}

# Is every SD rounded to `sd` smaller than any that data of whole numbers, each
# divided by `items_averaged`, can have with a mean rounded to `x`, whatever
# their number? Such data have a mean of `m / items_averaged` for a whole `m`
# when they are all the same, and otherwise a population SD of at least
# `sqrt(f * (1 - f)) / items_averaged`, where `f` is the fractional part of
# `items_averaged` times their mean: the variance is smallest with the values
# on the two neighbors of the mean. A sample SD is larger. `f * (1 - f)` is
# smallest at the ends of the range, so they are compared in exact arithmetic.
# A missing decimal count is replaced by the fewest that the value shows, which
# only widens its range (see `digits_least()`). `FALSE` if the case is open.
sd_below_floor <- function(
  x,
  sd,
  digits_x,
  digits_sd,
  items_averaged,
  rounding,
  threshold,
  symmetric
) {
  digits_x <- digits_least(x, digits_x)
  digits_sd <- digits_least(sd, digits_sd)
  if (anyNA(c(x, sd, digits_x, digits_sd, items_averaged))) {
    return(FALSE)
  }
  bounds_x <- roundwork::bound_numerators(
    x,
    digits_x,
    rounding,
    threshold,
    symmetric
  )
  bounds_sd <- roundwork::bound_numerators(
    sd,
    digits_sd,
    rounding,
    threshold,
    symmetric
  )
  if (is.null(bounds_x) || is.null(bounds_sd) || bounds_sd$upper < 0) {
    return(FALSE)
  }
  denom <- bounds_x$denom
  lower <- bounds_x$lower * items_averaged
  upper <- bounds_x$upper * items_averaged
  if (max(abs(c(lower, upper, denom))) >= 2^52) {
    return(FALSE)
  }
  # The whole number at or below the lower end; a whole number in the range,
  # even at an exclusive end, leaves the case open:
  whole <- floor(lower / denom)
  whole <- whole - (whole * denom > lower) + ((whole + 1) * denom <= lower)
  if (whole * denom == lower || (whole + 1) * denom <= upper) {
    return(FALSE)
  }
  ends <- c(lower, upper) - whole * denom
  # `sd_upper^2 / denom_sd^2 < end * (denom - end) / (denom * items)^2`:
  below <- vapply(
    ends,
    function(end) {
      compare_factors(
        list(
          bounds_sd$upper,
          bounds_sd$upper,
          denom,
          denom,
          items_averaged,
          items_averaged
        ),
        list(end, denom - end, bounds_sd$denom, bounds_sd$denom)
      ) <
        0
    },
    logical(1L)
  )
  all(below)
}


# The least SD that `sd_below_floor()` compares with, as a double, for the
# explanations:
sd_floor_value <- function(
  x,
  digits_x,
  items_averaged,
  rounding,
  threshold,
  symmetric
) {
  bounds <- roundwork::bound_numerators(
    x,
    digits_least(x, digits_x),
    rounding,
    threshold,
    symmetric
  )
  ends <- c(bounds$lower, bounds$upper) * items_averaged / bounds$denom
  fractions <- ends - floor(min(ends))
  sqrt(min(fractions * (1 - fractions))) / items_averaged
}


# `is_decidable_n_items()` for continuous data, where a missing `n` or
# `items_averaged` can be anything, so only one that is given can make the
# value set undecidable. Vectorized, for `grim_total()`.
is_decidable_continuous <- function(n, items_averaged = 1, min_n = 1) {
  n[is.na(n)] <- min_n
  items_averaged[is.na(items_averaged)] <- 1
  is_decidable_n_items(n, items_averaged, min_n)
}


# An argument renamed in scrutiny 1.0.0 still works under its old name, with a
# warning. Returns the value to use under the new name; `new_value` is only
# evaluated if the old name wasn't used, so it may be a missing argument.
# Both names at once are an error. `missing()` is asked in the caller's frame:
# through `new_value`, it is `FALSE` for an argument left at its default.
use_renamed_arg <- function(old_value, new_value, fn_name, old, new) {
  if (!lifecycle::is_present(old_value)) {
    return(new_value)
  }
  if (!eval(call("missing", as.name(new)), rlang::caller_env())) {
    abort_renamed_both(old, new)
  }
  lifecycle::deprecate_warn(
    "1.0.0",
    paste0(fn_name, "(", old, ")"),
    paste0(fn_name, "(", new, ")"),
    env = asNamespace("scrutiny"),
    user_env = rlang::caller_env(2)
  )
  old_value
}


abort_renamed_both <- function(old, new) {
  abort_in_export(
    "`{old}` and `{new}` can't both be given.",
    "i" = "`{old}` is the old name of `{new}`. Use `{new}` only."
  )
}


# GRIM's `percent` argument was replaced by `x_meaning` in scrutiny 1.0.0. The
# functions without dots keep it as a `deprecated()` formal so that they can say
# so; the mappers say it from `check_factory_dots()`.
stop_percent <- function(fn_name) {
  lifecycle::deprecate_stop(
    "1.0.0",
    paste0(fn_name, "(percent)"),
    paste0(fn_name, "(x_meaning)"),
    details = "For `percent = TRUE`, use `x_meaning = \"percentage\"`.",
    env = asNamespace("scrutiny")
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
    if (any(t %in% c("double", "integer"))) {
      check_not_factor(x, msg_name)
    }
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
  # An empty or missing name is no name, not one that is taken twice. Without
  # `names()`, no column has one:
  unnamed <- if (is.data.frame(data) && is.null(colnames(data))) {
    seq_len(length(data))
  } else {
    which(is.na(colnames(data)) | colnames(data) == "")
  }
  if (is.data.frame(data) && length(unnamed) > 0L) {
    msg_which <- if (length(unnamed) == 1L) {
      paste("Column", unnamed, "has none.")
    } else {
      paste("Columns", toString(unnamed), "have none.")
    }
    abort_in_export(
      "!" = "Every column of `data` must have a name.",
      "x" = msg_which
    )
  }

  if (is.data.frame(data) && anyDuplicated(colnames(data)) > 0L) {
    names_dup <- unique(colnames(data)[duplicated(colnames(data))])
    abort_in_export(
      "!" = "`data` has more than one column named \\
      {wrap_in_backticks(names_dup)}.",
      "i" = "Rename or remove the duplicates first."
    )
  }

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
  # With no column to stack, `paste0()` would return `"_x"`, and `unlist()`
  # would return `NULL`, which is no column:
  stack_cols <- function(end) {
    if (length(origins) == 0L) {
      return(character(0L))
    }
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
    dplyr::if_else(is.na(x), "`NA`", paste0("\"", x, "\""))
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
    # Missing values in the same places agree, like any other values:
    all(is.na(x) == is.na(y)) && all(dplyr::near(x, y), na.rm = TRUE)
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
  # A vector of several values is left to the length checks, which say why:
  if (
    length(rounding) == 1L &&
      !(is.character(rounding) && rounding %in% ROUNDING_METHODS)
  ) {
    # Without hyperlinks, `.vignette` would print `vignette(pkg::name)`, which
    # is not valid R, so the call is written out instead:
    msg_vignette <- if (isTRUE(cli::ansi_hyperlink_types()[["vignette"]])) {
      "Click {.vignette roundwork::rounding-options}"
    } else {
      "See `vignette(\"rounding-options\", package = \"roundwork\")`"
    }
    abort_in_export(
      "`rounding` must be one of the rounding methods.",
      "x" = "It is {.val {rounding}}.",
      "i" = "Valid methods are {.or {.val {ROUNDING_METHODS}}}.",
      "i" = paste(msg_vignette, "to learn more.")
    )
  }
  # roundwork reads `threshold` and `symmetric` only under the methods that use
  # them, but every call records them, as in the print header of mapper output:
  if (
    length(threshold) == 1L &&
      !(is.numeric(threshold) && is.finite(threshold))
  ) {
    abort_in_export(
      "`threshold` must be a finite number.",
      "x" = "It is {wrap_in_quotes_or_backticks(threshold)}."
    )
  }
  if (
    length(symmetric) == 1L && !(is.logical(symmetric) && !is.na(symmetric))
  ) {
    abort_in_export(
      "`symmetric` must be `TRUE` or `FALSE`.",
      "x" = "It is {wrap_in_quotes_or_backticks(symmetric)}."
    )
  }

  # At zero, the bounds are the offsets alone:
  bounds <- rethrow_in_export(
    roundwork::bound_numerators(0, 0L, rounding, threshold, symmetric)
  )
  # roundwork scales a decimal `threshold` to whole offsets, up to six decimal
  # places. Any more, and the bounds are no longer whole numbers, nor exact:
  offsets <- c(bounds$lower, bounds$upper)
  if (any(offsets != round(offsets))) {
    abort_in_export(
      "`threshold` has too many decimal places for an exact test.",
      "x" = "It is {threshold}, which roundwork can't scale to whole \\
      numbers, so the rounding bounds would not be exact.",
      "i" = "A `threshold` with up to six decimal places is exact."
    )
  }
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


# For functions that read the "scrutiny" attribute of sequence or total-n
# output, such as `audit_seq()`, when an element is gone:
abort_lost_settings <- function() {
  abort_in_export(
    "`data` has lost the settings of the test that produced it.",
    "x" = "Some dplyr verbs, such as `summarise()`, drop them.",
    "i" = "Call `{name_last_export()}()` on the mapper's output directly, or \\
    after verbs like `filter()`, `slice()`, and `arrange()`."
  )
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

  # Built as calls, not parsed from text, which fails on a name such as
  # `"my result"`:
  data_dollar_result <- rlang::call2(
    "$",
    name_data,
    rlang::sym(name_key_result)
  )
  condition_not_list <- rlang::expr(!is.list(!!data_dollar_result))

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
