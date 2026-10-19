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
grimmer_explain <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items = 1,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  check_length(x, 1L)
  check_length(sd, 1L)
  check_length(n, 1L)
  check_length(items, 1L)

  # `paste()` would write a number like 100000 as 1e+05:
  rlang::local_options(scipen = 100L)

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
    show_reason = TRUE,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )
  consistency <- result[[1L]]

  explain <- function(text) {
    inputs <- list(
      x = format_reported(x, digits_x),
      sd = format_reported(sd, digits_sd),
      n = n,
      items = if (is.na(items) || items != 1) items
    )
    new_explanation(
      "GRIMMER",
      inputs,
      rounding,
      threshold,
      symmetric,
      consistency,
      text
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
        items = items
      ),
      n = n,
      items = items,
      min_n = 2
    ),
    # `grimmer_scalar()` rules these out before they happen, and
    # `explain_undecidable()` has no words for them:
    "GRIM undecidable" = ,
    "SD rounding bounds undefined" = cli::cli_abort(
      "Internal error: `grimmer_explain()` can't explain the reason \\
      {.val {result[[2L]]}}."
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

  grim <- grim_explain(
    x = x,
    n = n,
    digits_x = digits_x,
    items = items,
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
    items = items,
    digits_sd = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )
  sd_lower <- format_number(sd_bounds$lower / sd_bounds$denom, digits_sd)
  sd_upper <- format_number(sd_bounds$upper / sd_bounds$denom, digits_sd)

  text_bounds <- describe_reported(
    "SD",
    sd,
    digits_sd,
    sd_lower,
    sd_upper,
    sd_bounds$incl_lower,
    sd_bounds$incl_upper
  )

  values <- if (items == 1) {
    paste0("the ", n, " values")
  } else {
    paste0("the ", n, " respondents' totals across the ", items, " items")
  }
  sd_term <- function(sd_shown) {
    if (items == 1) {
      paste0(sd_shown, "^2")
    } else {
      paste0("(", items, " * ", sd_shown, ")^2")
    }
  }

  text_formula <- paste0(
    "Together, the sum and the SD determine the sum of squares, which is ",
    "what you get by squaring each of ",
    values,
    " and adding them up. It is (",
    n,
    " - 1) * ",
    sd_term("SD"),
    " + sum^2 / ",
    n,
    ". The values are whole numbers, so their squares are whole ",
    "numbers, too, and so is the sum of squares. What's more, a whole number ",
    "and its square are either both odd or both even. So the sum of squares ",
    "must be odd if the sum is odd, and even if the sum is even."
  )

  # The sum of squares as a calculation, at one bound of the SD:
  sum_squares_at <- function(s, sd_num, sd_shown) {
    value <- (n - 1) * items^2 * sd_num^2 / sd_bounds$denom^2 + s^2 / n
    paste0(
      n - 1,
      " * ",
      sd_term(sd_shown),
      " + ",
      s,
      "^2 / ",
      n,
      " = ",
      format_number(value)
    )
  }

  sums <- sum_range(x, n * items, digits_x, rounding, threshold, symmetric)

  # Candidates are tested in turn until one passes. Showing every failure could
  # take pages, so after the first three, the rest are only counted.
  text_candidates <- character()
  n_failed <- 0L
  passed <- FALSE

  for (s in sums[1L]:sums[2L]) {
    candidate <- grimmer_candidate(
      s = s,
      n = n,
      items = items,
      sd_bounds = sd_bounds
    )
    passed <- candidate$pass_test3

    if (passed || n_failed < 3L) {
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

  if (passed != consistency) {
    cli::cli_abort(
      "Internal error: `grimmer_explain()` disagrees with `grimmer()`."
    )
  }

  if (n_failed > 3L) {
    text_candidates <- append(
      text_candidates,
      paste(
        "The",
        if (passed) "next" else "remaining",
        n_failed - 3L,
        "sums fail as well."
      ),
      after = 3L
    )
  }

  explain(c(text_grim, text_bounds, text_formula, text_candidates))
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

  parity_s <- if (s %% 2 == 0) "even" else "odd"

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

  paste(c(text, whole), collapse = " ")
}
