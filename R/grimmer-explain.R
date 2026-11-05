#' Explain a GRIMMER test step by step
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `grimmer_explain()` tests a single mean and SD like
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
#' grimmer_explain(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 35,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1
#' )
#'
#' grimmer_explain(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 31,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1
#' )
grimmer_explain <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
) {
  check_length(x, 1L)
  check_length(sd, 1L)
  check_length(n, 1L)
  sd_meaning <- arg_match_in_export(sd_meaning, SD_MEANINGS)
  if (missing(items_averaged)) {
    error_items_missing()
  }
  check_length(items_averaged, 1L)
  n <- round_whole(n)
  items_averaged <- round_whole(items_averaged)

  # `paste()` would write a number like 100000 as 1e+05:
  rlang::local_options(scipen = 100L)

  if (!missing(digits_x)) {
    check_length(digits_x, 1L)
    digits_x <- round_whole(digits_x)
  }
  if (!missing(digits_sd)) {
    check_length(digits_sd, 1L)
    digits_sd <- round_whole(digits_sd)
  }
  check_rounding_args(rounding, threshold, symmetric)

  # Checks the arguments, and its reason says how far the test got:
  result <- grimmer_scalar(
    x = x,
    sd = sd,
    n = n,
    digits_x = digits_x,
    digits_sd = digits_sd,
    items_averaged = items_averaged,
    show_reason = TRUE,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    sd_meaning = sd_meaning
  )
  consistency <- result[[1L]]

  # `grimmer_scalar()` has checked `sd_meaning`, and the arguments are known not
  # to be missing:
  if (sd_meaning == "sd_sample_or_pop") {
    explain_type <- function(sd_meaning) {
      grimmer_explain(
        x = x,
        sd = sd,
        n = n,
        digits_x = digits_x,
        digits_sd = digits_sd,
        items_averaged = items_averaged,
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric,
        sd_meaning = sd_meaning
      )
    }
    return(explain_sd_sample_or_pop(explain_type, consistency, n))
  }

  explain <- function(text) {
    inputs <- list(
      x = format_reported(x, digits_x),
      sd = format_reported(sd, digits_sd),
      n = n,
      items_averaged = if (is.na(items_averaged) || items_averaged != 1) {
        items_averaged
      }
    )
    new_explanation(
      "GRIMMER",
      inputs,
      rounding,
      threshold,
      symmetric,
      consistency,
      text,
      sd_meaning
    )
  }

  text <- switch(
    result[[2L]],
    "Missing value" = ,
    "Infinite value" = ,
    "No testable value set" = explain_undecidable(
      values = list(
        x = x,
        sd = sd,
        n = n,
        digits_x = digits_x,
        digits_sd = digits_sd,
        items_averaged = items_averaged
      ),
      n = n,
      items_averaged = items_averaged,
      min_n = sd_min_n(sd_meaning)
    ),
    # `grimmer_scalar()` rules these out before they happen, and
    # `explain_undecidable()` has no words for them:
    "GRIM undecidable" = ,
    "SD rounding bounds undefined" = cli::cli_abort(
      "Internal error: `grimmer_explain()` can't explain the reason \\
      {.val {result[[2L]]}}.",
      .internal = TRUE
    ),
    "Negative SD" = paste0(
      "The SD was reported as ",
      format_reported(sd, digits_sd),
      ", but no ",
      "data set has a negative SD."
    ),
    NULL
  )
  if (!is.null(text)) {
    return(explain(text))
  }

  # With the mean unknown, the only way to a verdict is an SD that no sum of
  # whole numbers admits; `grimmer_scalar()` checked every sum by its remainder
  # when divided by `n`:
  if (is.na(x) || is.na(digits_x)) {
    sd_bounds <- grimmer_sd_bounds(
      sd = sd,
      n = n,
      items_averaged = items_averaged,
      digits_sd = digits_sd,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric,
      sd_meaning = sd_meaning
    )
    text_bounds <- describe_reported(
      if (sd_meaning == "se") "SD, taken to be an SE," else "SD",
      sd,
      digits_sd,
      format_number(sd_bounds$lower / sd_bounds$denom, digits_sd),
      format_number(sd_bounds$upper / sd_bounds$denom, digits_sd),
      sd_bounds$incl_lower,
      sd_bounds$incl_upper
    )
    text_sums <- paste0(
      if (is.na(x)) "x" else "digits_x",
      " is missing, so the sum of the ",
      if (items_averaged == 1) "values" else "respondents' totals",
      " is unknown. But whether a sum admits the SD only depends on its ",
      "remainder when divided by ",
      n,
      ", and none passes: for a sum with any remainder, ",
      "the sums of squares that ",
      n,
      if (n == 1) " whole number" else " whole numbers",
      " with that sum can have miss the range that the SD requires."
    )
    return(explain(c(text_bounds, text_sums)))
  }

  grim <- grim_explain(
    x = x,
    n = n,
    digits_x = digits_x,
    items_averaged = items_averaged,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )
  text_grim <- grim$paragraphs
  if (isFALSE(grim$consistency)) {
    text_grim[length(text_grim)] <- paste(
      text_grim[length(text_grim)],
      "Since the mean alone is impossible, the SD doesn't need to be checked."
    )
    return(explain(text_grim))
  }

  # The candidates go through `grimmer_candidate()`, the very function that
  # decides them in `grimmer_scalar()`.
  sd_bounds <- grimmer_sd_bounds(
    sd = sd,
    n = n,
    items_averaged = items_averaged,
    digits_sd = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    sd_meaning = sd_meaning
  )
  sd_lower <- format_number(sd_bounds$lower / sd_bounds$denom, digits_sd)
  sd_upper <- format_number(sd_bounds$upper / sd_bounds$denom, digits_sd)

  text_bounds <- describe_reported(
    if (sd_meaning == "se") "SD, taken to be an SE," else "SD",
    sd,
    digits_sd,
    sd_lower,
    sd_upper,
    sd_bounds$incl_lower,
    sd_bounds$incl_upper
  )

  # A population SD can be tested with a single value:
  values <- if (n == 1 && items_averaged == 1) {
    "the one value"
  } else if (n == 1) {
    paste0("the one respondent's total across the ", items_averaged, " items")
  } else if (items_averaged == 1) {
    paste0("each of the ", n, " values and adding them up")
  } else {
    paste0(
      "each of the ",
      n,
      " respondents' totals across the ",
      items_averaged,
      " items and adding them up"
    )
  }
  sd_term <- function(sd_shown) {
    if (items_averaged == 1) {
      paste0(sd_shown, "^2")
    } else {
      paste0("(", items_averaged, " * ", sd_shown, ")^2")
    }
  }

  n_dev <- sd_denominator(as.numeric(n), sd_meaning)

  text_formula <- paste0(
    "Together, the sum and the ",
    if (sd_meaning == "se") "SE" else "SD",
    " determine the sum of squares, which is ",
    "what you get by squaring ",
    values,
    ". ",
    switch(
      sd_meaning,
      sd_sample = paste0("It is (", n, " - 1) * "),
      sd_pop = paste0("For a population SD, it is ", n, " * "),
      se = paste0(
        "The SD is the SE times sqrt(",
        n,
        "), so the sum of squares is ",
        n,
        " * (",
        n,
        " - 1) * "
      )
    ),
    sd_term(if (sd_meaning == "se") "SE" else "SD"),
    " + sum^2 / ",
    n,
    ". The values are whole numbers, so their squares are whole ",
    "numbers, too, and so is the sum of squares. What's more, a whole number ",
    "and its square are either both odd or both even. So the sum of squares ",
    "must be odd if the sum is odd, and even if the sum is even."
  )

  # The sum of squares as a calculation, at one bound of the SD. A negative sum
  # is squared as a whole, so it is shown in parentheses:
  sum_squares_at <- function(s, sd_num, sd_shown) {
    value <- n_dev * items_averaged^2 * sd_num^2 / sd_bounds$denom^2 + s^2 / n
    paste0(
      n_dev,
      " * ",
      sd_term(sd_shown),
      " + ",
      if (s < 0) paste0("(", s, ")") else s,
      "^2 / ",
      n,
      " = ",
      format_number(value)
    )
  }

  sums <- roundwork::sum_range(
    x,
    as.numeric(n) * items_averaged,
    digits_x,
    rounding,
    threshold,
    symmetric
  )

  # Candidates are tested in turn until one passes. Showing every failure could
  # take pages, so after the first three, the rest are only counted.
  text_candidates <- character()
  n_failed <- 0L
  passed <- FALSE
  too_large <- FALSE

  # The first `n` sums decide, and only those within reach of the SD can pass,
  # as in `grimmer_scalar()`; if none of them passes, no sum does:
  sums_tested <- sums |>
    grimmer_sums_reachable(n, sd_bounds) |>
    sums_from_bounds()
  none_reachable <- length(sums_tested) == 0L
  # If none can, the first one shows why:
  if (none_reachable) {
    sums_tested <- sums[[1L]]
  }

  # Sums out of reach are skipped, so the reader needs to know why the sums
  # shown are not the first ones:
  n_window <- min(sums[[2L]] - sums[[1L]], n - 1) + 1
  text_reach <- if (length(sums_tested) < n_window) {
    paste0(
      "Not every sum needs to be tried. Of ",
      n,
      " whole numbers with a given sum, those closest to each other have the ",
      "smallest sum of squares: sum^2 / ",
      n,
      " plus r * (",
      n,
      " - r) / ",
      n,
      ", where r is the remainder of the sum divided by ",
      n,
      ". For a sum more than ",
      grimmer_reach(sd_bounds),
      " away from a multiple of ",
      n,
      ", that addition alone is larger than ",
      n_dev,
      " * ",
      sd_term(sd_upper),
      ", so the sum fails. ",
      if (none_reachable) {
        "This holds for every sum here, as the first one shows."
      } else {
        "Only the other sums are tried below."
      }
    )
  }

  for (s in sums_tested) {
    candidate <- grimmer_candidate(
      s = s,
      n = n,
      items_averaged = items_averaged,
      sd_bounds = sd_bounds
    )
    passed <- candidate$pass_test3

    # The paragraphs show the sums of squares before the shift that the test
    # works with, and as numbers, which can't be exact from `2^53` on:
    absolute <- grimmer_candidate_absolute(candidate)
    if (is.null(absolute)) {
      too_large <- TRUE
    } else {
      candidate[names(absolute)] <- absolute
    }

    if (!too_large && (passed || n_failed < 3L)) {
      text_candidates <- c(
        text_candidates,
        grimmer_explain_candidate(
          s = s,
          n = n,
          candidate = candidate,
          sd_bounds = sd_bounds,
          sum_squares_lower = sum_squares_at(s, sd_bounds$lower, sd_lower),
          sum_squares_upper = sum_squares_at(s, sd_bounds$upper, sd_upper)
        )
      )
    }

    if (passed) {
      break
    }
    n_failed <- n_failed + 1L
  }
  # Up to three failures are shown, which may be all of the first `n`. The
  # sums skipped before the one that passes, or in the whole range, fail too:
  n_shown <- min(n_failed, 3L)
  n_failed <- if (passed) s - sums[[1L]] else sums[[2L]] - sums[[1L]] + 1

  if (passed != consistency) {
    cli::cli_abort(
      "Internal error: `grimmer_explain()` disagrees with `grimmer()`.",
      .internal = TRUE
    )
  }

  if (too_large) {
    # No sum is shown, so there is nothing to say about the ones skipped:
    text_reach <- NULL
    text_candidates <- paste0(
      "The sums of squares reach 2^53, about 9e15, which is too large to show ",
      "them here as exact numbers. GRIMMER compares them in exact whole-number ",
      "arithmetic, and ",
      if (passed) {
        "one of the sums passes."
      } else {
        "none of the sums passes."
      }
    )
  } else if (n_failed > n_shown) {
    # Skipped sums may come before the ones shown, so the count is of all
    # those below the one that passes, not of the next ones:
    n_other <- n_failed - n_shown
    text_candidates <- append(
      text_candidates,
      paste0(
        "The ",
        if (passed) "other " else "remaining ",
        if (n_other == 1L) "sum" else paste(n_other, "sums"),
        if (passed) paste0(" below ", s),
        if (n_other == 1L) " fails" else " fail",
        " as well."
      ),
      after = n_shown
    )
  }

  explain(c(text_grim, text_bounds, text_formula, text_reach, text_candidates))
}


# One paragraph on one candidate sum:
grimmer_explain_candidate <- function(
  s,
  n,
  candidate,
  sd_bounds,
  sum_squares_lower,
  sum_squares_upper
) {
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
  minimum <- candidate$sum_squares_min

  parity_s <- if (s %% 2 == 0) "even" else "odd"

  if (lower <= upper && minimum > lower) {
    return(grimmer_explain_minimum(
      text = text,
      s = s,
      n = n,
      lower = lower,
      upper = upper,
      minimum = minimum,
      parity_s = parity_s
    ))
  }

  whole <- if (lower > upper) {
    "There is no whole number in this range."
  } else if (lower == upper) {
    parity_lower <- if (lower %% 2 == 0) "even" else "odd"
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

  # Fewer than five numbers cannot have every sum of squares that passes the
  # minimum and the parity, so say which one they can have, if any:
  first <- lower + (lower - s) %% 2
  attained <- if (n < 5 && lower <= upper && first <= upper) {
    candidate$attained
  }
  attainable <- if (is.null(attained) || identical(attained, first)) {
    NULL
  } else if (n == 1) {
    # A population SD can be tested with a single value, whose only sum of
    # squares is its square:
    paste0(
      "But a single number with a sum of ",
      s,
      " equals ",
      s,
      ", so its sum of squares can only be ",
      s^2,
      "."
    )
  } else if (is.na(attained)) {
    paste0(
      "But fewer than five whole numbers cannot have every such sum of ",
      "squares, and ",
      n,
      " whole numbers with a sum of ",
      s,
      " cannot have ",
      if (lower == upper) lower else "any of those in this range",
      "."
    )
  } else {
    paste0(
      "Fewer than five whole numbers cannot have every such sum of squares, ",
      "and the smallest in this range that ",
      n,
      " whole numbers with a sum of ",
      s,
      " can have is ",
      attained,
      "."
    )
  }

  paste(c(text, whole, attainable), collapse = " ")
}


# The paragraph on a candidate sum whose range of sums of squares starts below
# the smallest one whole numbers with this sum can have:
grimmer_explain_minimum <- function(
  text,
  s,
  n,
  lower,
  upper,
  minimum,
  parity_s
) {
  q <- s %/% n
  r <- s - n * q
  closest <- if (r == 0) {
    paste0("all ", n, " equal to ", q)
  } else {
    paste0(r, " of them ", q + 1, " and ", n - r, " of them ", q)
  }

  in_range <- if (lower == upper) {
    paste0("The only whole number in this range is ", lower, ".")
  } else {
    paste0(
      "The whole numbers from ",
      lower,
      " to ",
      upper,
      " are in this range."
    )
  }

  smallest <- paste0(
    "However, the smallest sum of squares that ",
    n,
    " whole numbers with a sum of ",
    s,
    " can have is ",
    minimum,
    ", with ",
    closest,
    "."
  )

  verdict <- if (minimum > upper) {
    "So none of the sums of squares in this range is possible."
  } else {
    paste0(
      "It is in this range, and it is ",
      parity_s,
      ", like ",
      s,
      ", being a sum of squares of whole numbers."
    )
  }

  paste(c(text, in_range, smallest, verdict), collapse = " ")
}
