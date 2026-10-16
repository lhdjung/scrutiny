#' Explain a GRIMMER test step by step
#'
#' @description `grimmer_explain()` tests a single mean and SD like
#'   [`grimmer()`] does, but also lays out the calculation in plain words. It
#'   starts with the GRIM test of the mean, as in [`grim_explain()`], and goes
#'   on to the sum of squares that each possible sum implies with the SD.
#'
#'   The text is built from the numbers that decide the test itself, so it
#'   always agrees with [`grimmer()`].
#'
#' @inheritParams grimmer
#'
#' @inherit grim_explain return
#'
#' @include grim-explain.R grimmer.R
#'
#' @export
#'
#' @examples
#' grimmer_explain(x = 5.23, sd = 2.55, n = 35, digits_x = 2, digits_sd = 2)
#'
#' grimmer_explain(x = 5.23, sd = 2.55, n = 31, digits_x = 2, digits_sd = 2)
#'
#' grimmer_explain(
#'   x = 3.00, sd = 2.08, n = 20, digits_x = 2, digits_sd = 2,
#'   min_val = 1, max_val = 5
#' )
grimmer_explain <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items = 1,
  min_val = NULL,
  max_val = NULL,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  check_length(x, 1L)
  check_length(sd, 1L)
  check_length(n, 1L)
  check_length(items, 1L)
  if (!missing(digits_x)) {
    check_length(digits_x, 1L)
  }
  if (!missing(digits_sd)) {
    check_length(digits_sd, 1L)
  }

  # Checks the arguments, and its reason says how far the test got:
  result <- grimmer_scalar(
    x = x,
    sd = sd,
    n = n,
    digits_x = digits_x,
    digits_sd = digits_sd,
    items = items,
    min_val = min_val,
    max_val = max_val,
    show_reason = TRUE,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )
  consistency <- result[[1L]]
  reason <- result[[2L]]

  has_scale <- !is.null(min_val)
  scale <- if (has_scale) paste("from", min_val, "to", max_val)

  text <- switch(
    reason,
    "Missing value" = ,
    "Infinite value" = ,
    "No testable value set" = explain_undecidable(
      values = list(
        x = x,
        sd = sd,
        n = n,
        digits_x = digits_x,
        digits_sd = digits_sd,
        items = items
      ),
      n = n,
      items = items,
      min_n = 2
    ),
    "Negative SD" = paste0(
      "The SD was reported as ",
      format_reported(sd, digits_sd),
      ", but no ",
      "data set has a negative SD."
    ),
    "Mean out of scale range" = paste0(
      "The mean was reported as ",
      format_reported(x, digits_x),
      ", but the ",
      "values are on a scale ",
      scale,
      ", and no set of such values has a ",
      "mean outside of it."
    ),
    NULL
  )

  if (is.null(text)) {
    grim <- grim_explain_text(
      x = x,
      n = n,
      digits_x = digits_x,
      items = items,
      percent = FALSE,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )
    text <- grim$text
    if (isFALSE(grim$consistency)) {
      text[length(text)] <- paste(
        text[length(text)],
        "Since the mean alone is impossible, the SD doesn't need to be checked."
      )
    } else {
      text <- c(
        text,
        grimmer_explain_text(
          x = x,
          sd = sd,
          n = n,
          digits_x = digits_x,
          digits_sd = digits_sd,
          items = items,
          scale = scale,
          min_val = min_val,
          max_val = max_val,
          rounding = rounding,
          threshold = threshold,
          symmetric = symmetric,
          consistency = consistency
        )
      )
    }
  }

  new_explanation(
    test = "GRIMMER",
    inputs = list(
      x = format_reported(x, digits_x),
      sd = format_reported(sd, digits_sd),
      n = n,
      items = if (items != 1) items,
      min_val = min_val,
      max_val = max_val
    ),
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    consistency = consistency,
    text = text
  )
}


# The paragraphs after the GRIM part, for a mean that passed it. The candidates
# go through `grimmer_candidate()`, the very function that decides them in
# `grimmer_scalar()`.
grimmer_explain_text <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items,
  scale,
  min_val,
  max_val,
  rounding,
  threshold,
  symmetric,
  consistency
) {
  sd_bounds <- grimmer_sd_bounds(
    sd = sd,
    n = n,
    items = items,
    digits_sd = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )
  sd_lower <- format_number(sd_bounds$lower / sd_bounds$denom)
  sd_upper <- format_number(sd_bounds$upper / sd_bounds$denom)

  text_bounds <- paste0(
    "The SD was reported as ",
    format_reported(sd, digits_sd),
    ", with ",
    describe_digits(digits_sd),
    ". Before rounding, it could have been ",
    "anything ",
    describe_range(
      sd_lower,
      sd_upper,
      sd_bounds$incl_lower,
      sd_bounds$incl_upper
    ),
    "."
  )

  # The sum of squares as a calculation, at one bound of the SD:
  sum_squares_at <- function(s, sd_num, sd_shown) {
    sd_term <- if (items == 1) {
      paste0(sd_shown, "^2")
    } else {
      paste0("(", items, " * ", sd_shown, ")^2")
    }
    value <- (n - 1) * items^2 * sd_num^2 / sd_bounds$denom^2 + s^2 / n
    paste0(
      n - 1,
      " * ",
      sd_term,
      " + ",
      s,
      "^2 / ",
      n,
      " = ",
      format_number(value)
    )
  }

  values <- if (items == 1) {
    paste0("the ", n, " values")
  } else {
    paste0("the ", n, " respondents' totals across the ", items, " items")
  }
  formula <- if (items == 1) {
    paste0("(", n, " - 1) * SD^2 + sum^2 / ", n)
  } else {
    paste0("(", n, " - 1) * (", items, " * SD)^2 + sum^2 / ", n)
  }

  text_formula <- paste0(
    "Together, the sum and the SD determine the sum of squares, which is ",
    "what you get by squaring each of ",
    values,
    " and adding them up. It is ",
    formula,
    ". The values are whole numbers, so their squares are whole ",
    "numbers, too, and so is the sum of squares. What's more, a whole number ",
    "and its square are either both odd or both even. So the sum of squares ",
    "must be odd if the sum is odd, and even if the sum is even."
  )

  if (!is.null(scale)) {
    totals <- if (items == 1) {
      ""
    } else {
      paste0(
        ", so each total is between ",
        min_val * items,
        " and ",
        max_val * items
      )
    }
    text_formula <- paste0(
      text_formula,
      " Finally, the values are on a scale ",
      scale,
      totals,
      ". This limits how far they can spread out, so for any given sum, the ",
      "sum of squares can't exceed a certain maximum."
    )
  }

  sums <- sum_range(x, n * items, digits_x, rounding, threshold, symmetric)

  text_candidates <- character()
  passed <- logical()

  for (s in sums[1L]:sums[2L]) {
    candidate <- grimmer_candidate(
      s = s,
      n = n,
      items = items,
      sd_bounds = sd_bounds,
      has_scale = !is.null(scale),
      min_val = min_val,
      max_val = max_val
    )
    text_candidates <- c(
      text_candidates,
      grimmer_explain_candidate(
        s = s,
        n = n,
        candidate = candidate,
        sd_bounds = sd_bounds,
        sum_squares_lower = sum_squares_at(s, sd_bounds$lower, sd_lower),
        sum_squares_upper = sum_squares_at(s, sd_bounds$upper, sd_upper),
        min_total = min_val * items,
        max_total = max_val * items
      )
    )
    passed <- c(passed, candidate$pass_test3)
    if (candidate$pass_test3) {
      break
    }
  }

  if (any(passed) != consistency) {
    cli::cli_abort(
      "Internal error: `grimmer_explain()` disagrees with `grimmer()`."
    )
  }

  c(
    text_bounds,
    text_formula,
    summarize_candidates(text_candidates, passed, "sums")
  )
}


# One paragraph on one candidate sum:
grimmer_explain_candidate <- function(
  s,
  n,
  candidate,
  sd_bounds,
  sum_squares_lower,
  sum_squares_upper,
  min_total,
  max_total
) {
  if (!candidate$in_reach) {
    return(paste0(
      "A sum of ",
      s,
      " is out of reach: ",
      n,
      " values from ",
      min_total,
      " to ",
      max_total,
      " add up to at least ",
      n * min_total,
      " and at most ",
      n * max_total,
      "."
    ))
  }

  text <- paste0(
    "With a sum of ",
    s,
    ", the sum of squares is ",
    describe_range(
      sum_squares_lower,
      sum_squares_upper,
      sd_bounds$incl_lower,
      sd_bounds$incl_upper
    ),
    "."
  )

  lower <- candidate$sum_squares[1L]
  upper <- candidate$sum_squares[2L]

  if (!is.na(candidate$ceiling)) {
    upper <- candidate$ceiling
    text <- paste0(
      text,
      " But within the scale, the sum of squares is greatest when the values ",
      "are as far apart as possible, i.e., ",
      describe_spread_out(s, n, min_total, max_total),
      ". Then it is ",
      candidate$ceiling,
      if (lower > upper) ", which is too small." else ", narrowing the range."
    )
  }

  parity_s <- describe_parity(s)

  whole <- if (lower > upper) {
    if (is.na(candidate$ceiling)) "There is no whole number in this range."
  } else if (lower == upper) {
    parity_lower <- describe_parity(lower)
    paste0(
      "The only whole number in this range is ",
      lower,
      ", which is ",
      parity_lower,
      if (parity_lower == parity_s) ", like " else ", but ",
      s,
      if (parity_lower == parity_s) "." else paste0(" is ", parity_s, ".")
    )
  } else {
    paste0(
      "The whole numbers from ",
      lower,
      " to ",
      upper,
      " are in this range, so one of them is ",
      parity_s,
      ", like ",
      s,
      "."
    )
  }

  paste(c(text, whole), collapse = " ")
}


# The values behind `sum_squares_scale_max()`: as many at the maximum as the sum
# allows, the rest at the minimum, and at most one in between.
describe_spread_out <- function(s, n, val_lower, val_upper) {
  span <- val_upper - val_lower
  if (span == 0) {
    return(paste(n, "values of", val_lower))
  }
  k <- floor_div(s - n * val_lower, span)
  rest <- (s - n * val_lower) - k * span
  middle <- if (rest > 0) 1 else 0
  parts <- c(
    if (k > 0) paste(k, "values of", val_upper),
    if (middle > 0) paste("one of", val_lower + rest),
    if (n - k - middle > 0) paste(n - k - middle, "values of", val_lower)
  )
  if (length(parts) == 3L) {
    paste0(parts[1L], ", ", parts[2L], ", and ", parts[3L])
  } else {
    paste(parts, collapse = " and ")
  }
}
