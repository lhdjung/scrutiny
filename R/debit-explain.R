#' Explain a DEBIT test step by step
#'
#' @description `debit_explain()` tests a single mean and SD of binary data
#'   like [`debit()`] does, but also lays out the calculation in plain words:
#'   which numbers of ones the mean allows, and whether the SD of any of them
#'   would be reported as the given one.
#'
#'   The text is built from the numbers that decide the test itself, so it
#'   always agrees with [`debit()`] under its default `formula = "exact"`.
#'
#' @inheritParams debit
#'
#' @inherit grim_explain return
#'
#' @include grim-explain.R debit.R
#'
#' @export
#'
#' @examples
#' debit_explain(x = 0.36, sd = 0.11, n = 20, digits_x = 2, digits_sd = 2)
#'
#' debit_explain(x = 0.35, sd = 0.49, n = 20, digits_x = 2, digits_sd = 2)
debit_explain <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  check_length(x, 1L)
  check_length(sd, 1L)
  check_length(n, 1L)
  if (!missing(digits_x)) {
    check_length(digits_x, 1L)
  }
  if (!missing(digits_sd)) {
    check_length(digits_sd, 1L)
  }

  # Checks the arguments, and returns the bounds that decided the test:
  rec <- debit_scalar(
    x = x,
    sd = sd,
    n = n,
    digits_x = digits_x,
    digits_sd = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    show_rec = TRUE
  )
  consistency <- rec[[1L]]

  explain <- function(text) {
    inputs <- list(
      x = format_reported(x, digits_x),
      sd = format_reported(sd, digits_sd),
      n = n
    )
    new_explanation(
      "DEBIT",
      inputs,
      rounding,
      threshold,
      symmetric,
      consistency,
      text
    )
  }

  if (is.na(consistency)) {
    text <- explain_undecidable(
      values = list(
        x = x,
        sd = sd,
        n = n,
        digits_x = digits_x,
        digits_sd = digits_sd
      ),
      n = n,
      min_n = 2
    )
    return(explain(text))
  }

  sd_lower <- format_number(rec[[2L]])
  sd_incl_lower <- rec[[3L]]
  sd_upper <- format_number(rec[[4L]])
  sd_incl_upper <- rec[[5L]]
  sum_lower <- rec[[8L]]
  sum_upper <- rec[[9L]]

  # The mean's bounds as `debit_scalar()` has them, i.e. confined to the 0 to 1
  # that a mean of binary data can take. A bound moved there is attainable,
  # hence inclusive:
  bounds_x <- bound_numerators(x, digits_x, rounding, threshold, symmetric)

  x_incl_lower <- bounds_x$incl_lower || bounds_x$lower < 0
  x_incl_upper <- bounds_x$incl_upper || bounds_x$upper > bounds_x$denom

  x_num_lower <- max(bounds_x$lower, 0)
  x_num_upper <- min(bounds_x$upper, bounds_x$denom)

  x_lower <- format_number(x_num_lower / bounds_x$denom)
  x_upper <- format_number(x_num_upper / bounds_x$denom)

  text_intro <- paste0(
    "DEBIT is about binary data, where every value is 0 or 1. The mean is ",
    "then the share of ones, and the SD follows from the number of ones and ",
    "the sample size."
  )

  text_bounds_x <- describe_reported(
    "mean",
    x,
    digits_x,
    x_lower,
    x_upper,
    x_incl_lower,
    x_incl_upper
  )

  ones_range <- describe_range(
    paste(
      x_lower,
      "*",
      n,
      "=",
      format_number(n * x_num_lower / bounds_x$denom)
    ),
    paste(
      x_upper,
      "*",
      n,
      "=",
      format_number(n * x_num_upper / bounds_x$denom)
    ),
    x_incl_lower,
    x_incl_upper
  )
  text_ones <- paste0(
    "With ",
    n,
    " values, the mean is the number of ones divided by ",
    n,
    ", so that number must be ",
    ones_range,
    ". "
  )

  if (sum_lower > sum_upper) {
    text_ones <- paste0(
      text_ones,
      "But it has to be a whole number, and there is none in this ",
      "range. So no number of ones fits the mean, whatever the SD is."
    )
    return(explain(c(text_intro, text_bounds_x, text_ones)))
  }

  text_ones <- paste0(
    text_ones,
    "It has to be a whole number, so it can ",
    if (sum_lower == sum_upper) {
      paste0("only be ", sum_lower, ".")
    } else {
      paste0("be anything from ", sum_lower, " to ", sum_upper, ".")
    }
  )

  text_bounds_sd <- describe_reported(
    "SD",
    sd,
    digits_sd,
    sd_lower,
    sd_upper,
    sd_incl_lower,
    sd_incl_upper
  )

  text_formula <- paste0(
    "With k ones among ",
    n,
    " values, the SD is ",
    "sqrt(k * (",
    n,
    " - k) / (",
    n,
    " * ",
    n - 1,
    "))."
  )

  # Whether each number of ones `k` has an SD in range is decided by the whole
  # number `k * (n - k)`, as in `binary_sd_attainable()`, not by the rounded
  # SD shown. The bounds are those of `debit_scalar()`, an SD's lower bound
  # moved up to zero included:
  bounds_sd <- bound_numerators(sd, digits_sd, rounding, threshold, symmetric)
  if (bounds_sd$lower < 0) {
    bounds_sd$lower <- 0
    bounds_sd$incl_lower <- TRUE
  }

  spread_range <- binary_spread_range(n, bounds_sd)

  ones <- sum_lower:sum_upper
  spread <- ones * (n - ones)
  passed <- spread >= spread_range[1L] & spread <= spread_range[2L]

  if (any(passed) != consistency) {
    cli::cli_abort(
      "Internal error: `debit_explain()` disagrees with `debit()`."
    )
  }

  # The SD of `k` ones as a calculation:
  sd_of <- function(k) {
    paste0(
      "sqrt(", k, " * ", n - k, " / ", n * (n - 1), ") = ",
      format_number(sqrt(k * (n - k) / (n * (n - 1))))
    )
  }

  several <- length(ones) > 1L

  text_ones_sd <- if (any(passed)) {
    k <- ones[passed][1L]

    paste0(
      if (several) "For example, for k = " else "For k = ",
      k, ", that is ", sd_of(k), ", which is in that range."
    )
  } else if (!several) {
    paste0(
      "For k = ", ones, ", that is ", sd_of(ones), ", which is ",
      if (spread < spread_range[1L]) "below" else "above", " that range."
    )
  } else if (all(spread < spread_range[1L])) {
    # The spread, and with it the SD, is greatest for the `k` closest to half
    # of `n`, so that one stands for all:
    k <- ones[which.max(spread)]

    paste0(
      "Even the largest of these SDs, for k = ", k, ", is only ", sd_of(k),
      ", which is below that range."
    )
  } else if (all(spread > spread_range[2L])) {
    k <- ones[which.min(spread)]

    paste0(
      "Even the smallest of these SDs, for k = ", k, ", is ", sd_of(k),
      ", which is above that range."
    )
  } else {
    # Some SDs are below the range and some above, and none in it: those next
    # to it skip over it.
    below <- spread < spread_range[1L]
    k_below <- ones[below][which.max(spread[below])]
    k_above <- ones[!below][which.min(spread[!below])]

    paste0(
      "These SDs skip over that range. For k = ", k_below, ", the SD is ",
      sd_of(k_below), ", which is below it, and for k = ", k_above, ", it is ",
      sd_of(k_above), ", which is above it."
    )
  }

  explain(c(
    text_intro,
    text_bounds_x,
    text_ones,
    text_bounds_sd,
    paste(text_formula, text_ones_sd)
  ))
}
