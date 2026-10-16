#' Explain a GRIM test step by step
#'
#' @description `grim_explain()` tests a single mean like [`grim()`] does, but
#'   also lays out the calculation in plain words: which true means the
#'   reported one could stand for, which sums of the underlying whole numbers
#'   follow from them, and whether any of these sums is possible.
#'
#'   The text is built from the numbers that decide the test itself, so it
#'   always agrees with [`grim()`].
#'
#' @inheritParams grim
#'
#' @return An object of class `scrutiny_explain`: a list with the verdict as
#'   `consistency` (`TRUE`, `FALSE`, or `NA`), the paragraphs of the
#'   explanation as `text`, and the `test` and its `inputs`. It prints as the
#'   explanation.
#'
#' @seealso [`grimmer_explain()`] and [`debit_explain()`] for the other tests.
#'
#' @include grim.R
#'
#' @export
#'
#' @examples
#' grim_explain(x = 5.19, n = 28, digits_x = 2)
#'
#' grim_explain(x = 5.19, n = 32, digits_x = 2)
#'
#' grim_explain(x = 71, n = 43, digits_x = 0, percent = TRUE)
grim_explain <- function(
  x,
  n,
  digits_x,
  items = 1,
  percent = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  check_length(x, 1L)
  check_length(n, 1L)
  check_length(items, 1L)
  # A missing `digits_x` reaches `grim_scalar()`, which says how to fix it:
  if (!missing(digits_x)) {
    check_length(digits_x, 1L)
  }

  # `grim_scalar()` checks the arguments, so it goes first:
  explanation <- grim_explain_text(
    x = x,
    n = n,
    digits_x = digits_x,
    items = items,
    percent = percent,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  new_explanation(
    test = "GRIM",
    inputs = list(
      x = format_reported(x, digits_x),
      n = n,
      items = if (items != 1) items,
      percent = if (percent) TRUE
    ),
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    consistency = explanation$consistency,
    text = explanation$text
  )
}


# The GRIM paragraphs, which `grimmer_explain()` also starts with. Every number
# in them comes from `grim_scalar()` itself or from `bound_numerators()`, the
# helper `sum_range()` derives the sums from, so the text cannot contradict the
# verdict.
grim_explain_text <- function(
  x,
  n,
  digits_x,
  items,
  percent,
  rounding,
  threshold,
  symmetric
) {
  rec <- grim_scalar(
    x = x,
    n = n,
    digits_x = digits_x,
    items = items,
    percent = percent,
    show_rec = TRUE,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  consistency <- rec[[1L]]

  if (is.na(consistency)) {
    reason <- explain_undecidable(
      values = list(x = x, n = n, digits_x = digits_x, items = items),
      n = n,
      items = items
    )
    return(list(consistency = NA, text = reason))
  }

  sum_lower <- rec[[3L]]
  sum_upper <- rec[[4L]]
  rec_x_upper <- rec[[5L]]
  rec_x_lower <- rec[[6L]]

  # The bounds of `x` on the scale it was reported on, i.e. as a percentage if
  # it is one. Each is an exact numerator over `denom`, and multiplying before
  # dividing keeps a value like 70.5 from showing up as 70.4999999999.
  digits_num <- if (percent) digits_x + 2L else digits_x
  x_num <- if (percent) x / 100 else x
  scale_x <- if (percent) 100 else 1
  bounds <- bound_numerators(x_num, digits_num, rounding, threshold, symmetric)
  x_lower <- format_number(bounds$lower * scale_x / bounds$denom)
  x_upper <- format_number(bounds$upper * scale_x / bounds$denom)

  n_items <- n * items
  sum_lower_exact <- format_number(n_items * bounds$lower / bounds$denom)
  sum_upper_exact <- format_number(n_items * bounds$upper / bounds$denom)

  x_name <- if (percent) "percentage" else "mean"

  text_bounds <- paste0(
    "The ",
    x_name,
    " was reported as ",
    format_reported(x, digits_x),
    ", ",
    "with ",
    describe_digits(digits_x),
    ". Before rounding, it could have ",
    "been anything ",
    describe_range(x_lower, x_upper, bounds$incl_lower, bounds$incl_upper),
    "."
  )

  # How the mean or percentage comes from a sum, and which sums it allows:
  if (percent) {
    n_shown <- if (items == 1) n else paste(n, "*", items, "=", n_items)
    times <- function(value) paste0(value, " / 100 * ", n_items)
    text_sums <- paste0(
      "A percentage out of ",
      n_shown,
      " is a count divided by ",
      n_items,
      ", times 100. So the count must be "
    )
    sum_name <- "count"
  } else if (items == 1) {
    times <- function(value) paste0(value, " * ", n)
    text_sums <- paste0(
      "The mean of ",
      n,
      " whole numbers is their sum divided by ",
      n,
      ". ",
      "So the sum must be "
    )
    sum_name <- "sum"
  } else {
    times <- function(value) paste0(value, " * ", n_items)
    text_sums <- paste0(
      "Each of the ",
      n,
      " scores is the average of ",
      items,
      " whole-number ",
      "items. Their mean is therefore the sum of all ",
      n_items,
      " item ",
      "values divided by ",
      n_items,
      ", so this sum must be "
    )
    sum_name <- "sum"
  }

  text_sums <- paste0(
    text_sums,
    describe_range(
      paste(times(x_lower), "=", sum_lower_exact),
      paste(times(x_upper), "=", sum_upper_exact),
      bounds$incl_lower,
      bounds$incl_upper
    ),
    "."
  )

  # A sum and the mean or percentage it gives, as a calculation:
  divide <- function(sum) {
    value <- sum * scale_x / n_items
    if (percent) {
      paste0(sum, " / ", n_items, " * 100 = ", format_number(value))
    } else {
      paste0(sum, " / ", n_items, " = ", format_number(value))
    }
  }
  rounds_to <- function(value) {
    rounded <- reround(value, digits_x, rounding, threshold, symmetric)
    paste(unique(format_reported(rounded, digits_x)), collapse = " or ")
  }

  whole_intro <- if (percent) {
    "A count is a whole number"
  } else {
    "A sum of whole numbers is itself a whole number"
  }

  if (consistency) {
    # The sum closest to the reported value is the one shown, which keeps it
    # away from a bound where only some rounding procedures would round it to
    # `x`:
    sum_shown <- min(max(round(x_num * n_items), sum_lower), sum_upper)
    value_shown <- sum_shown * scale_x / n_items
    in_range <- if (sum_lower == sum_upper) {
      paste0(", and ", sum_lower, " is in this range: ")
    } else {
      paste0(
        ", and the whole numbers from ",
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
      ", which rounds to ",
      rounds_to(value_shown),
      "."
    )
  } else {
    # The range holds no whole number, so `sum_upper` is the nearest one below
    # it and `sum_lower` the nearest one above, and the granules are their
    # means:
    text_whole <- paste0(
      whole_intro,
      ", but there is no whole number in this range. The ",
      "nearest ones are ",
      sum_upper,
      " and ",
      sum_lower,
      ". They give ",
      divide(sum_upper),
      " and ",
      divide(sum_lower),
      ", which round to ",
      rounds_to(rec_x_lower),
      " and ",
      rounds_to(rec_x_upper),
      ", not to ",
      format_reported(x, digits_x),
      "."
    )
  }

  list(
    consistency = consistency,
    text = c(text_bounds, text_sums, text_whole)
  )
}


# Shared by all explanations ----------------------------------------------

new_explanation <- function(
  test,
  inputs,
  rounding,
  threshold,
  symmetric,
  consistency,
  text
) {
  # Only the settings that differ from the defaults are worth a mention:
  inputs <- c(
    inputs,
    list(
      rounding = if (rounding != "up_or_down") paste0("\"", rounding, "\""),
      threshold = if (threshold != 5) threshold,
      symmetric = if (symmetric) TRUE
    )
  )
  inputs <- Filter(Negate(is.null), inputs)

  structure(
    list(
      test = test,
      inputs = paste(names(inputs), "=", inputs, collapse = ", "),
      consistency = consistency,
      text = text
    ),
    class = "scrutiny_explain"
  )
}


#' @export
print.scrutiny_explain <- function(x, ...) {
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
  paragraphs <- lapply(x$text, function(p) {
    p <- gsub(" ([-+*/=]) ", "~\\1~", p)
    c(gsub("~", " ", strwrap(p, width), fixed = TRUE), "")
  })
  cat(
    paste0(x$test, " test: ", x$inputs),
    "",
    unlist(paragraphs),
    paste("==>", verdict),
    sep = "\n"
  )
  invisible(x)
}


# Why a case is undecidable, in the order in which the tests check it. `values`
# is a named list of the values that must not be missing.
explain_undecidable <- function(values, n, items = 1, min_n = 1) {
  for (name in names(values)) {
    if (is.na(values[[name]])) {
      return(paste0(name, " is missing, so there is nothing to test."))
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
  if (!is_decidable_n_items(n, min_n = min_n)) {
    if (min_n == 2) {
      return(paste0(
        "The test needs n to be a whole number of at least 2, because the ",
        "SD of a sample divides by n - 1. But n is ",
        n,
        "."
      ))
    }
    return(paste0(
      "The test needs n to be a positive whole number, but it is ",
      n,
      "."
    ))
  }
  paste0(
    "The test needs the number of items to be a positive whole number, but ",
    "it is ",
    items,
    "."
  )
}


# A reported value with all of its decimal places, trailing zeros included:
format_reported <- function(x, digits) {
  if (anyNA(x) || is.na(digits)) {
    return(as.character(x))
  }
  formatC(x, format = "f", digits = digits)
}


# A computed value, with up to four decimal places. A fraction must not look
# whole, though, or the text would contradict itself next to a whole number
# that is said to be outside of it:
format_number <- function(x) {
  shown <- round(x, 4L)
  looks_whole <- shown == round(x) & x != round(x)
  shown[looks_whole] <- signif(x[looks_whole], 12L)
  vapply(
    shown,
    format,
    character(1L),
    scientific = FALSE,
    digits = 15L,
    trim = TRUE
  )
}


describe_digits <- function(digits) {
  if (digits == 0) {
    "no decimal places"
  } else if (digits == 1) {
    "1 decimal place"
  } else {
    paste(digits, "decimal places")
  }
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


describe_parity <- function(x) {
  if (x %% 2 == 0) "even" else "odd"
}


# Candidates are tested in turn until one passes. Showing every failure could
# take pages, so after the first three, the rest are only counted.
summarize_candidates <- function(text, passed, noun) {
  failed <- which(!passed)
  if (length(failed) <= 3L) {
    return(text)
  }
  hidden <- length(failed) - 3L
  rest <- if (any(passed)) "next" else "remaining"
  c(
    text[failed[1:3]],
    paste("The", rest, hidden, noun, "fail as well."),
    text[passed]
  )
}
