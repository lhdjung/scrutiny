#' Explain a GRIM test step by step
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `grim_explain()` tests a single mean like [`grim()`] does, but
#'   also lays out the calculation in plain words: which true means the
#'   reported one could stand for, which sums of the underlying integers
#'   follow from them, and whether any of these sums is possible.
#'
#'   The text is built from the numbers that decide the test itself, so it
#'   always agrees with [`grim()`].
#'
#' @inheritParams grim
#'
#' @return An object of class `scrutiny_explain`: a list with the result as
#'   `consistency` (`TRUE`, `FALSE`, or `NA`), the explanation's `paragraphs`,
#'   and the `test` and its `inputs`. It prints as the explanation.
#'
#' @seealso [`grimmer_explain()`] and [`debit_explain()`] for the other tests,
#'   and [`explain_map()`] for mapper output.
#'
#' @include grim.R
#'
#' @export
#'
#' @examples
#' grim_explain(
#'   x = 5.19,
#'   n = 28,
#'   digits_x = 2,
#'   items_averaged = 1,
#'   data_type = "integer"
#' )
#'
#' grim_explain(
#'   x = 5.19,
#'   n = 32,
#'   digits_x = 2,
#'   items_averaged = 1,
#'   data_type = "integer"
#' )
#'
#' grim_explain(
#'   x = 71,
#'   n = 43,
#'   digits_x = 0,
#'   items_averaged = 1,
#'   data_type = "integer",
#'   x_meaning = "percentage"
#' )
grim_explain <- function(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  percent = deprecated()
) {
  if (lifecycle::is_present(percent)) {
    stop_percent("grim_explain")
  }
  check_length(x, 1L)
  check_length(n, 1L)
  if (missing(items_averaged)) {
    error_items_missing()
  }
  check_length(items_averaged, 1L)
  n <- round_whole(n)
  items_averaged <- round_whole(items_averaged)
  x_meaning <- arg_match_in_export(x_meaning, X_MEANINGS)
  is_percentage <- x_meaning == "percentage"

  # `paste()` would write a number like 100000 as 1e+05:
  rlang::local_options(scipen = 100L)

  # A missing `digits_x` reaches `grim_scalar()`, which says how to fix it:
  if (!missing(digits_x)) {
    check_length(digits_x, 1L)
    digits_x <- round_whole(digits_x)
  }
  check_rounding_args(rounding, threshold, symmetric)

  # Every number in the text comes from `grim_scalar()` itself or from
  # `bound_numerators()`, the helper `sum_range()` derives the sums from, so the
  # text cannot contradict the verdict.
  rec <- grim_scalar(
    x = x,
    n = n,
    digits_x = digits_x,
    items_averaged = items_averaged,
    data_type = data_type,
    x_meaning = x_meaning,
    show_rec = TRUE,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )
  consistency <- rec[[1L]]

  # Inconsistent although `digits_x` is missing: see `digits_least()`.
  if (
    identical(data_type, "integer") && isFALSE(consistency) && is.na(digits_x)
  ) {
    return(explain_digits_least(
      grim_explain(
        x = x,
        n = n,
        digits_x = digits_least(x, digits_x),
        items_averaged = items_averaged,
        data_type = "integer",
        x_meaning = x_meaning,
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric
      ),
      "digits_x"
    ))
  }

  # Consistent although `digits_x` is missing: see `digits_exact_mean()`.
  if (
    identical(data_type, "integer") &&
      isTRUE(consistency) &&
      is.na(digits_x) &&
      !is.na(digits_least(x, digits_x)) &&
      !anyNA(c(n, items_averaged))
  ) {
    return(explain_digits_most(
      grim_explain(
        x = x,
        n = n,
        digits_x = digits_exact_mean(
          digits_least(x, digits_x),
          as.numeric(n) * items_averaged
        ),
        items_averaged = items_averaged,
        data_type = "integer",
        x_meaning = x_meaning,
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric
      ),
      "digits_x"
    ))
  }

  if (!identical(data_type, "integer")) {
    explain_integer <- function(fill = FALSE) {
      grim_explain(
        x = x,
        n = if (fill && is.na(n)) 1 else n,
        digits_x = digits_x,
        items_averaged = if (fill && is.na(items_averaged)) {
          1
        } else {
          items_averaged
        },
        data_type = "integer",
        x_meaning = x_meaning,
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric
      )
    }
    return(explain_data_type(
      data_type,
      consistency,
      explain_integer,
      x_meaning,
      names(verdict_continuous(n, items_averaged, x = x))
    ))
  }

  explain <- function(text) {
    inputs <- list(
      x = format_reported(x, digits_x),
      n = n,
      items_averaged = items_averaged,
      data_type = "\"integer\"",
      x_meaning = if (is_percentage) "\"percentage\""
    )
    new_explanation(
      "GRIM",
      inputs,
      rounding,
      threshold,
      symmetric,
      consistency,
      text
    )
  }

  # Consistent although `n` or `items_averaged` is missing: see `grim_scalar()`.
  if (isTRUE(consistency) && anyNA(c(n, items_averaged))) {
    missing <- c(if (is.na(n)) "n", if (is.na(items_averaged)) "items_averaged")
    explanation <- grim_explain(
      x = x,
      n = if (is.na(n)) 1 else n,
      digits_x = digits_x,
      items_averaged = if (is.na(items_averaged)) 1 else items_averaged,
      data_type = "integer",
      x_meaning = x_meaning,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )
    # The inputs as given, not as tested:
    for (name in missing) {
      explanation$inputs <- sub(
        paste0("(^|, )", name, " = 1(,|$)"),
        paste0("\\1", name, " = NA\\2"),
        explanation$inputs
      )
    }
    explanation$paragraphs <- c(
      paste0(
        paste(missing, collapse = " and "),
        if (length(missing) == 1L) " is" else " are",
        " missing, so the test below takes 1 in its place. A mean that some ",
        "integers have, any number of copies of them have, too, so a mean ",
        "that is consistent here is consistent whatever ",
        paste(missing, collapse = " and "),
        if (length(missing) == 1L) " is." else " are."
      ),
      explanation$paragraphs
    )
    return(explanation)
  }

  # Consistent although `x` is missing: see `grim_all_means_pass()`.
  if (isTRUE(consistency) && is.na(x)) {
    n_items <- as.numeric(n) * items_averaged
    digits_num <- if (is_percentage) digits_x + 2L else digits_x
    text <- paste0(
      "x is missing. But the means of ",
      n_items,
      " integers are 1 / ",
      n_items,
      " apart, ",
      if (n_items >= 10^digits_num) {
        paste0(
          "which is no more than the ",
          format_number(10^-digits_num, digits_num),
          " between ",
          if (is_percentage) "shares" else "means",
          " with ",
          digits_num,
          " decimal places, so some mean would be reported as any ",
          if (is_percentage) "percentage" else "value",
          " with ",
          digits_x,
          if (digits_x == 1) " decimal place." else " decimal places."
        )
      } else {
        paste0(
          "and with the rounding method, some mean would still be reported ",
          "as any ",
          if (is_percentage) "percentage" else "value",
          " with ",
          digits_x,
          if (digits_x == 1) " decimal place" else " decimal places",
          ": every one from -1 to 1 was tested, and the pattern repeats with ",
          "every whole number."
        )
      },
      " So x is consistent whatever it is."
    )
    return(explain(text))
  }

  if (is.na(consistency)) {
    text <- explain_undecidable(
      values = list(
        x = x,
        n = n,
        digits_x = digits_x,
        items_averaged = items_averaged
      ),
      n = n,
      items_averaged = items_averaged
    )
    return(explain(text))
  }

  sum_lower <- rec[[3L]]
  sum_upper <- rec[[4L]]

  # The bounds of `x` on the scale it was reported on, i.e. as a percentage if
  # it is one. Each is an exact numerator over `denom`, and multiplying before
  # dividing keeps a value like 70.5 from showing up as 70.4999999999.
  digits_num <- if (is_percentage) digits_x + 2L else digits_x
  x_num <- if (is_percentage) x / 100 else x
  scale_x <- if (is_percentage) 100 else 1
  bounds <- roundwork::bound_numerators(
    x_num,
    digits_num,
    rounding,
    threshold,
    symmetric
  )
  x_lower <- format_product_exact(
    scale_x,
    bounds$lower,
    bounds$denom,
    digits_num
  )
  x_upper <- format_product_exact(
    scale_x,
    bounds$upper,
    bounds$denom,
    digits_num
  )

  n_items <- as.numeric(n) * items_averaged
  sum_lower_exact <- format_product_exact(
    n_items,
    bounds$lower,
    bounds$denom,
    digits_num
  )
  sum_upper_exact <- format_product_exact(
    n_items,
    bounds$upper,
    bounds$denom,
    digits_num
  )

  # Reported percentages and their bounds have a percent sign, but not the
  # numbers in calculations:
  unit <- if (is_percentage) "%" else ""
  text_bounds <- describe_reported(
    if (is_percentage) "percentage" else "mean",
    x,
    digits_x,
    paste0(x_lower, unit),
    paste0(x_upper, unit),
    bounds$incl_lower,
    bounds$incl_upper,
    unit
  )

  # How the mean or percentage comes from a sum, and which sums it allows:
  if (is_percentage) {
    n_shown <- if (items_averaged == 1) {
      n
    } else {
      paste(n, "*", items_averaged, "=", n_items)
    }
    times <- paste0(" / 100 * ", n_items, " = ")
    text_sums <- paste0(
      "A percentage out of ",
      n_shown,
      " is a count divided by ",
      n_items,
      ", times 100. So ",
      format_reported(x, digits_x),
      "% stands for the proportion ",
      format_reported(x / 100, digits_x + 2L),
      ", with two more decimal places than the percentage shows. ",
      "The count must be "
    )
  } else if (n == 1 && items_averaged == 1) {
    # No sum to calculate, so the bounds are shown as they are:
    times <- NULL
    text_sums <- "The mean of a single integer is that number. So it must be "
  } else if (items_averaged == 1) {
    times <- paste0(" * ", n, " = ")
    text_sums <- paste0(
      "The mean of ",
      n,
      " integers is their sum divided by ",
      n,
      ". ",
      "So the sum must be "
    )
  } else {
    times <- paste0(" * ", n_items, " = ")
    text_sums <- paste0(
      if (n == 1) "The single score" else paste("Each of the", n, "scores"),
      " is the average of ",
      items_averaged,
      " integer items. ",
      if (n == 1) "It" else "Their mean",
      " is therefore the sum of all ",
      n_items,
      " item ",
      "values divided by ",
      n_items,
      ", so this sum must be "
    )
  }

  text_sums <- paste0(
    text_sums,
    describe_range(
      paste0(x_lower, times, if (!is.null(times)) sum_lower_exact),
      paste0(x_upper, times, if (!is.null(times)) sum_upper_exact),
      bounds$incl_lower,
      bounds$incl_upper
    ),
    "."
  )

  # A sum and the mean or percentage it gives, as a calculation. Shown with
  # enough decimal places to tell it from the bounds of `x`: with `n` above
  # `10^digits_x`, a mean like 499 / 999 would otherwise print as 0.4995, the
  # bound it falls short of. `debit_explain()` does the same for its SDs.
  bounds_x <- c(bounds$lower, bounds$upper) * scale_x / bounds$denom
  divide <- function(sum) {
    paste0(
      sum,
      " / ",
      n_items,
      if (is_percentage) " * 100",
      " = ",
      format_number(sum * scale_x / n_items, digits_num, bounds_x)
    )
  }

  # What the mean or percentage from `sum` rounds to. Under `"even"`, one
  # exactly halfway between two rounded values is a tie that `base::round()`
  # may break either way, which is why both are taken to be possible (see
  # `?unround`), so both are named:
  rounds_to <- function(sum) {
    value <- sum * scale_x / n_items
    half_units <- 2 * value * 10^digits_x
    is_tie <- rounding %in%
      c("even", "ties_even") &&
      is_whole_number(half_units) &&
      round(half_units) %% 2 == 1
    if (is_tie) {
      value <- (round(half_units) + c(-1, 1)) / (2 * 10^digits_x)
    }
    value |>
      roundwork::reround(digits_x, rounding, threshold, symmetric) |>
      format_reported(digits_x) |>
      unique() |>
      paste0(unit) |>
      paste(collapse = " or ") |>
      paste0(if (is_tie) " (a tie)")
  }

  whole_intro <- if (is_percentage) {
    "A count is an integer"
  } else {
    "A sum of integers is itself an integer"
  }

  if (consistency) {
    # The sum closest to the reported value is the one shown, which keeps it
    # away from a bound where only some rounding procedures would round it to
    # `x`:
    sum_shown <- min(max(round(x_num * n_items), sum_lower), sum_upper)

    # Under `"even"`, a sum on a bound is a tie that `base::round()` may break
    # either way, which is why both bounds are inclusive (see `?unround`). Show
    # another sum from the range if there is one, and otherwise say so rather
    # than claim a rounding that cannot be predicted:
    is_tie <- function(sum) {
      (rounding %in% c("even", "ties_even")) &
        (dplyr::near(sum * bounds$denom, n_items * bounds$lower) |
          dplyr::near(sum * bounds$denom, n_items * bounds$upper))
    }
    if (is_tie(sum_shown)) {
      sums_other <- c(sum_shown + 1, sum_shown - 1)
      sums_other <- sums_other[
        sums_other >= sum_lower & sums_other <= sum_upper & !is_tie(sums_other)
      ]
      sum_shown <- c(sums_other, sum_shown)[1L]
    }
    text_rounds <- if (is_tie(sum_shown)) {
      ", a tie that rounding to even may break either way"
    } else {
      paste0(", which rounds to ", rounds_to(sum_shown))
    }

    in_range <- if (sum_lower == sum_upper) {
      paste0(", and ", sum_lower, " is in this range: ")
    } else {
      paste0(
        ", and the integers from ",
        sum_lower,
        " to ",
        sum_upper,
        " are in this range. For example, "
      )
    }
    text_whole <- paste0(
      whole_intro,
      in_range,
      divide(sum_shown),
      text_rounds,
      "."
    )
  } else {
    # The range holds no whole number, so `sum_upper` is the nearest one below
    # it and `sum_lower` the nearest one above, and the granules are their
    # means:
    text_whole <- paste0(
      whole_intro,
      ", but there is no integer in this range. The ",
      "nearest ones are ",
      sum_upper,
      " and ",
      sum_lower,
      ". They give ",
      divide(sum_upper),
      " and ",
      divide(sum_lower),
      ", which round to ",
      rounds_to(sum_upper),
      " and ",
      rounds_to(sum_lower),
      ", not to ",
      format_reported(x, digits_x),
      unit,
      "."
    )
  }

  explain(c(text_bounds, text_sums, text_whole))
}


# Shared by all explanations ----------------------------------------------

# The explanation for data that are not plainly whole numbers, as decided by
# `test_data_type()`: the one for whole numbers, `explain_integer()`, with a
# paragraph on the data type. `what` is what continuous data can have any of,
# `reason` the name of `verdict_continuous()`'s result, and `sd` the SD as
# reported, for a value set that is no single value's. `explain_integer(fill =
# TRUE)` takes a missing `n` or `items_averaged` for the least that the test
# admits.
explain_data_type <- function(
  data_type,
  consistency,
  explain_integer,
  what,
  reason,
  sd = NULL
) {
  out <- explain_integer()

  # Why continuous data have this value set or not. Undecidable value sets and
  # negative SDs are no data's, which the explanation for whole numbers says --
  # but a missing `n` or `items_averaged` can be anything for continuous data
  # (`verdict_continuous()`), so it is not the reason there, as it would be for
  # whole numbers. It is filled in to get at the reason that is:
  is_other_reason <- !reason %in% c("Continuous data", "SD of a single value")
  if (identical(data_type, "continuous") && is_other_reason) {
    out$paragraphs <- explain_integer(fill = TRUE)$paragraphs
  }

  text_continuous <- switch(
    reason,
    "Continuous data" = paste0("they can have any ", what),
    "SD of a single value" = paste0(
      "they can have any mean, but a single value has an SD of 0, which ",
      "would not have been reported as ",
      sd
    )
  )

  out$paragraphs <- if (is.null(text_continuous)) {
    out$paragraphs
  } else if (is.na(data_type)) {
    if (identical(consistency, NA)) {
      c(
        "The data type is missing. If the data are integers, the test goes as follows.",
        out$paragraphs,
        paste0(
          "If the data are continuous, ",
          text_continuous,
          ". So whether the value set is consistent depends on the data type."
        )
      )
    } else {
      c(
        out$paragraphs,
        paste0(
          "The data type is missing, but this doesn't matter here. If the ",
          "data are continuous, ",
          text_continuous,
          ", so the value set is ",
          if (consistency) "consistent" else "inconsistent",
          " either way."
        )
      )
    }
  } else if (data_type == "continuous") {
    paste0(
      "The data are continuous, so ",
      text_continuous,
      if (consistency) {
        ". The test only applies to integers, so the value set is consistent."
      } else {
        "."
      }
    )
  } else {
    out$paragraphs
  }

  out$consistency <- consistency
  out$inputs <- sub(
    "data_type = \"integer\"",
    paste0(
      "data_type = ",
      if (is.na(data_type)) "NA" else paste0("\"", data_type, "\"")
    ),
    out$inputs,
    fixed = TRUE
  )
  out
}

new_explanation <- function(
  test,
  inputs,
  rounding,
  threshold,
  symmetric,
  consistency,
  text,
  sd_meaning = "sd_sample"
) {
  # Only the settings that differ from the defaults are worth a mention:
  inputs <- c(
    inputs,
    list(
      rounding = if (rounding != "up_or_down") paste0("\"", rounding, "\""),
      threshold = if (threshold != 5) threshold,
      symmetric = if (symmetric) TRUE,
      sd_meaning = if (sd_meaning != "sd_sample") paste0("\"", sd_meaning, "\"")
    )
  )
  inputs <- Filter(Negate(is.null), inputs)

  # The first range of unrounded values says which rounding it assumes, unless
  # paragraphs taken from another explanation, such as GRIM's, already do:
  first <- grep("Before rounding, ", text, fixed = TRUE)[1L]
  is_said <- any(grepl("Before rounding (assuming", text, fixed = TRUE))
  if (!is.na(first) && !is_said) {
    text[first] <- sub(
      "Before rounding, ",
      paste0(
        "Before rounding (assuming ",
        describe_rounding(rounding, threshold, symmetric),
        "), "
      ),
      text[first],
      fixed = TRUE
    )
  }

  structure(
    list(
      test = test,
      inputs = paste(names(inputs), "=", inputs, collapse = ", "),
      consistency = consistency,
      paragraphs = text
    ),
    class = "scrutiny_explain"
  )
}


# The explanation under `sd_meaning = "sd_sample_or_pop"`: that of the sample
# SD, and if it doesn't pass, that of the population SD as well, minus the
# paragraphs the two share, such as the GRIM test. The verdict is then the
# population SD's, as in `grimmer_sd_sample_or_pop()`. `explain_type()` explains
# the test under one `sd_meaning`, and `consistency` is the verdict under
# `"sd_sample_or_pop"`.
explain_sd_sample_or_pop <- function(explain_type, consistency, n) {
  out <- explain_type("sd_sample")
  verdict <- out$consistency

  if (!isTRUE(verdict)) {
    population <- explain_type("sd_pop")
    # The shared paragraphs are those at the start, such as the GRIM test. A
    # later one may say the same as one of the sample SD's, such as "The
    # remaining 4 sums fail as well.", but about other numbers:
    paragraphs_both <- list(out$paragraphs, population$paragraphs)
    n_both <- min(lengths(paragraphs_both))
    is_shared <- out$paragraphs[seq_len(n_both)] ==
      population$paragraphs[seq_len(n_both)]
    n_shared <- if (all(is_shared)) n_both else which.min(is_shared) - 1L
    is_new <- seq_along(population$paragraphs) > n_shared

    out$paragraphs <- if (is.na(population$consistency)) {
      # Undecidable either way, and the population SD's requirements are the
      # ones that apply:
      population$paragraphs
    } else if (is.na(verdict) && isTRUE(n == 1)) {
      # At `n = 1`, the sample SD is undecidable but the population SD is not:
      c(
        out$paragraphs,
        "A single value has no sample SD, but it has a population SD, which divides by n rather than n - 1.",
        population$paragraphs[is_new]
      )
    } else if (any(is_new)) {
      c(
        out$paragraphs,
        "The SD may also be a population SD, which divides by n rather than n - 1.",
        population$paragraphs[is_new]
      )
    } else {
      out$paragraphs
    }
    # The population SD's verdict, unless a missing value leaves the sample SD
    # open while the population SD fails; see `grimmer_sd_sample_or_pop()`:
    verdict <- combine_sd_sample_or_pop(verdict, population$consistency, n)
  }

  if (!identical(verdict, consistency)) {
    cli::cli_abort(
      "Internal error: the explanations under \
      `sd_meaning = \"sd_sample_or_pop\"` disagree with the test.",
      .internal = TRUE
    )
  }

  # `sd_meaning` is the last setting that `new_explanation()` lists:
  out$inputs <- paste0(out$inputs, ", sd_meaning = \"sd_sample_or_pop\"")
  out$consistency <- consistency
  out
}


#' @export
format.scrutiny_explain <- function(x, ...) {
  verdict <- if (is.na(x$consistency)) {
    "Undecidable"
  } else if (x$consistency) {
    "Consistent"
  } else {
    "Inconsistent"
  }

  width <- min(getOption("width"), 80L)

  # Keep calculations like `166 / 32 = 5.1875` on one line: the spaces around
  # their operators are swapped for a `~`, which the text never contains, while
  # wrapping.
  wrapped <- x$paragraphs |>
    gsub(pattern = " ([-+*/=]) ", replacement = "~\\1~") |>
    lapply(strwrap, width = width) |>
    lapply(paste, collapse = "\n") |>
    gsub(pattern = "~", replacement = " ", fixed = TRUE)

  x$test |>
    paste0(" test: ", x$inputs) |>
    c(wrapped, paste("==>", verdict)) |>
    paste(collapse = "\n\n")
}


#' @export
print.scrutiny_explain <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}


# The explanation of a value set that is inconsistent whatever its missing
# decimal count: `explanation`, the one with the fewest decimal places that the
# value can have, under a paragraph on why that settles it; see
# `digits_least()`. `missing` names the `digits_*` arguments that are missing.
explain_digits_least <- function(explanation, missing) {
  explanation$paragraphs <- c(
    paste0(
      paste(missing, collapse = " and "),
      if (length(missing) == 1L) " is" else " are",
      " missing, so the test below takes the fewest decimal places that ",
      "the reported value can have: those it shows. More decimal places ",
      "would only narrow the range of numbers that are rounded to it, so a ",
      "value set that is inconsistent here is inconsistent whatever the ",
      "number of decimal places."
    ),
    explanation$paragraphs
  )
  explanation
}


# The same for a value set that is consistent whatever its missing decimal
# count: `explanation` is the one with so many decimal places that only the
# value shown passes; see `digits_exact_mean()`.
explain_digits_most <- function(explanation, missing) {
  explanation$paragraphs <- c(
    paste0(
      paste(missing, collapse = " and "),
      if (length(missing) == 1L) " is" else " are",
      " missing, so the test below takes so many decimal places that only ",
      "the value shown itself passes. Fewer decimal places would only widen ",
      "the range of numbers that are rounded to it, so a value set that is ",
      "consistent here is consistent whatever the number of decimal places."
    ),
    explanation$paragraphs
  )
  explanation
}


# Why a case is undecidable. `values` is a named list of the values that must
# not be missing. An `n` or item count that the test can't take leaves the case
# open whatever else is missing, so it comes first, unless it is missing itself:
# at `n = 1` under `"sd_sample_or_pop"`, a missing `x` is not why the sample SD
# is undecidable.
explain_undecidable <- function(values, n, items_averaged = 1, min_n = 1) {
  if (
    !anyNA(c(n, items_averaged)) &&
      !is_decidable_n_items(n, items_averaged, min_n)
  ) {
    return(explain_n_items(n, items_averaged, min_n))
  }

  for (name in names(values)) {
    if (is.na(values[[name]])) {
      return(paste0(
        name,
        " is missing, and the case can't be decided without it."
      ))
    }
  }

  for (name in intersect(names(values), c("x", "sd"))) {
    if (is.infinite(values[[name]])) {
      return(paste0(
        name,
        " is infinite, which no real data can produce, so there is ",
        "nothing to test."
      ))
    }
  }

  explain_n_items(n, items_averaged, min_n)
}


explain_n_items <- function(n, items_averaged, min_n) {
  if (is_decidable_n_items(n, min_n = min_n)) {
    paste0(
      "The test needs the number of items to be a positive integer, but ",
      "it is ",
      items_averaged,
      "."
    )
  } else if (min_n == 2) {
    paste0(
      "The test needs n to be an integer of at least 2, because the ",
      "SD of a sample divides by n - 1. But n is ",
      n,
      "."
    )
  } else {
    paste0(
      "The test needs n to be a positive integer, but it is ",
      n,
      "."
    )
  }
}


# A reported value with all of its decimal places, trailing zeros included:
format_reported <- function(x, digits) {
  if (anyNA(x) || is.na(digits)) {
    as.character(x)
  } else {
    # `+ 0` turns a negative zero, as from `round(-0.04, 1)`, into a positive
    # one, which `formatC()` would print as "-0.0":
    formatC(x + 0, format = "f", digits = digits)
  }
}


# A computed value, with up to `digits + 2` decimal places (at least four), so
# that the bounds of a value reported with `digits` come out in full. A
# fraction must not look whole, though, nor like one of the `bounds` it is
# compared to, or the text would contradict itself next to a number that is
# said to be outside of it. Such a value gets the fewest more places that set it
# apart:
format_number <- function(x, digits = 0L, bounds = numeric()) {
  x |>
    vapply(round_apart, numeric(1L), max(4L, digits + 2L), bounds) |>
    vapply(format_fraction, character(1L))
}


# With 15 significant digits, which hide a double's noise. From about 1e14 on,
# these leave none for the fraction, so a sum of squares of 457247320530420.3
# looked whole, next to a sentence that starts counting integers at the one
# above it. A fraction then gets the 16th or 17th digit:
format_fraction <- function(x) {
  for (digits_significant in 15:17) {
    out <- format(
      x,
      scientific = FALSE,
      digits = digits_significant,
      trim = TRUE
    )
    if (x == round(x) || grepl(getOption("OutDec"), out, fixed = TRUE)) {
      break
    }
  }
  out
}


# `n * num / den` for a bound of the mean, `num / den`, written out in full, as
# is the bound itself with `n = 1`. As a double rounded to 15 digits,
# `0.5 * 899999999999999` showed as `450000000000000`, next to a range that ends
# at 449999999999999, and `format_number()` showed the bound 5.18255 as
# 5.1826. `den` is a power of ten, so the decimal point only needs to be placed:
format_product_exact <- function(n, num, den, digits) {
  places <- round(log10(den))
  if (10^places != den) {
    return(format_number(n * num / den, digits))
  }
  text <- whole_format(whole_mul(n, num))
  sign <- if (startsWith(text, "-")) "-" else ""
  text <- sub("^-", "", text)
  if (places == 0) {
    return(paste0(sign, text))
  }
  text <- paste0(strrep("0", max(places + 1L - nchar(text), 0L)), text)
  cut <- nchar(text) - places
  fraction <- sub("0+$", "", substring(text, cut + 1L))
  integer <- substr(text, 1L, cut)
  paste0(sign, if (fraction == "") integer else paste0(integer, ".", fraction))
}


round_apart <- function(x, places, bounds) {
  # A value that really is whole or at a bound may look like it:
  avoid <- c(round(x), bounds)
  avoid <- avoid[avoid != x]

  shown <- round(x, places)
  while (places < 15L && shown %in% round(avoid, places)) {
    places <- places + 1L
    shown <- round(x, places)
  }
  shown
}


# The sentence on a reported value and the range it could have been rounded
# from. `unit` follows the value, as with a percent sign; `lower` and `upper`
# have it already:
describe_reported <- function(
  name,
  x,
  digits,
  lower,
  upper,
  incl_lower,
  incl_upper,
  unit = ""
) {
  places <- if (digits == 0) {
    "no decimal places"
  } else if (digits == 1) {
    "1 decimal place"
  } else {
    paste(digits, "decimal places")
  }

  paste0(
    "The ",
    name,
    " was reported as ",
    format_reported(x, digits),
    unit,
    ", with ",
    places,
    # A single-point range, as under `"anti_trunc"` at zero, is no "anything":
    if (incl_lower && incl_upper && lower == upper) {
      ". Before rounding, it can only have been "
    } else {
      ". Before rounding, it could have been anything "
    },
    describe_range(lower, upper, incl_lower, incl_upper),
    "."
  )
}


# The rounding method in words, for "Before rounding (assuming ...)":
describe_rounding <- function(rounding, threshold, symmetric) {
  nearest <- "rounding to the nearest value, with ties rounded "
  # `"ties_even"` is `"even"`: `base::round()`, whose ties are decided on the
  # binary double, so both bounds are taken to be possible; see `?unround`:
  even <- paste(
    "rounding half to even, as R's round() does, though a tie may go either",
    "way because round() decides it on the binary number"
  )
  text <- switch(
    rounding,
    up_or_down = "either rounding up from 5 or down from 5",
    up_from_or_down_from = paste0(
      "either rounding up from ",
      threshold,
      " or down from ",
      threshold
    ),
    ceiling_or_floor = "either always rounding up or always rounding down",
    up = "rounding up from 5",
    down = "rounding down from 5",
    even = even,
    up_from = paste("rounding up from", threshold),
    down_from = paste("rounding down from", threshold),
    ceiling = "always rounding up",
    floor = "always rounding down",
    trunc = "truncation, which drops further decimal places",
    anti_trunc = "always rounding away from zero",
    ties_up = paste0(nearest, "up"),
    ties_down = paste0(nearest, "down"),
    ties_away = paste0(nearest, "away from zero"),
    ties_zero = paste0(nearest, "toward zero"),
    ties_even = even
  )
  # The methods that `symmetric` changes; see `?roundwork::reround`:
  is_symmetric <- symmetric &&
    rounding %in%
      c(
        "up_or_down",
        "up_from_or_down_from",
        "up",
        "down",
        "up_from",
        "down_from"
      )
  if (is_symmetric) {
    text <- paste0(
      text,
      ", with negative values rounded like their absolute values"
    )
  }
  text
}


# A range in words, with each bound included or not:
describe_range <- function(lower, upper, incl_lower, incl_upper) {
  if (incl_lower && incl_upper) {
    if (lower == upper) {
      return(paste("exactly", lower))
    }
    paste("from", lower, "to", upper)
  } else if (incl_lower) {
    paste0("from ", lower, " up to, but not including, ", upper)
  } else if (incl_upper) {
    paste0("above ", lower, ", up to and including ", upper)
  } else {
    paste("above", lower, "and below", upper)
  }
}
