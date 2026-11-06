#' Explain a DEBIT test step by step
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `debit_explain()` tests a single mean and SD of binary data
#'   like [`debit()`] does, but also lays out the calculation in plain words:
#'   which numbers of ones the mean allows, and whether the SD of any of them
#'   would be reported as the given one.
#'
#'   The text is built from the numbers that decide the test itself, so it
#'   always agrees with [`debit()`] under its default `formula = "exact"`.
#'
#' @inheritParams debit
#' @param formula String. Only `"exact"`, the default, can be explained.
#'
#' @inherit grim_explain return
#'
#' @include grim-explain.R debit.R
#'
#' @export
#'
#' @examples
#' debit_explain(
#'   x = 0.36,
#'   sd = 0.11,
#'   n = 20,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   data_type = "integer"
#' )
#'
#' debit_explain(
#'   x = 0.35,
#'   sd = 0.49,
#'   n = 20,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   data_type = "integer"
#' )
debit_explain <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  data_type,
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
) {
  formula <- arg_match_in_export(formula, DEBIT_FORMULAS)
  sd_meaning <- arg_match_in_export(sd_meaning, SD_MEANINGS)

  if (formula != "exact") {
    abort_in_export(
      "Explanations only cover DEBIT's default `formula = \"exact\"`.",
      "x" = "`formula` is {.val {formula}}."
    )
  }

  check_length(x, 1L)
  check_length(sd, 1L)
  check_length(n, 1L)
  n <- round_whole(n)

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

  # Checks the arguments, and returns the bounds that decided the test:
  rec <- debit_scalar(
    x = x,
    sd = sd,
    n = n,
    digits_x = digits_x,
    digits_sd = digits_sd,
    data_type = data_type,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    sd_meaning = sd_meaning,
    show_rec = TRUE
  )
  consistency <- rec[[1L]]

  # Inconsistent although a decimal count is missing: see `digits_least()`.
  digits_missing <- c(
    if (is.na(digits_x) && !is.na(digits_least(x, digits_x))) "digits_x",
    if (is.na(digits_sd) && !is.na(digits_least(sd, digits_sd))) "digits_sd"
  )
  if (
    identical(data_type, "integer") &&
      isFALSE(consistency) &&
      length(digits_missing) > 0L
  ) {
    return(explain_digits_least(
      debit_explain(
        x = x,
        sd = sd,
        n = n,
        digits_x = digits_least(x, digits_x),
        digits_sd = digits_least(sd, digits_sd),
        data_type = "integer",
        formula = formula,
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric,
        sd_meaning = sd_meaning
      ),
      digits_missing
    ))
  }

  if (!identical(data_type, "integer")) {
    explain_integer <- function(fill = FALSE) {
      debit_explain(
        x = x,
        sd = sd,
        n = if (fill && is.na(n)) sd_min_n(sd_meaning) else n,
        digits_x = digits_x,
        digits_sd = digits_sd,
        data_type = "integer",
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric,
        sd_meaning = sd_meaning
      )
    }
    return(explain_data_type(
      data_type,
      consistency,
      explain_integer,
      "mean and SD",
      names(verdict_continuous(
        n,
        min_n = sd_min_n(sd_meaning),
        sd = sd,
        digits_sd = digits_sd,
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric
      )),
      format_reported(sd, digits_sd)
    ))
  }
  # As a double, as in `debit_scalar()`, which has checked the type:
  n <- as.numeric(n)

  # `debit_scalar()` has checked `sd_meaning`, and the arguments are known not
  # to be missing:
  if (sd_meaning == "sd_sample_or_pop") {
    explain_type <- function(sd_meaning) {
      debit_explain(
        x = x,
        sd = sd,
        n = n,
        digits_x = digits_x,
        digits_sd = digits_sd,
        data_type = "integer",
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
      data_type = "\"integer\""
    )
    new_explanation(
      "DEBIT",
      inputs,
      rounding,
      threshold,
      symmetric,
      consistency,
      text,
      sd_meaning
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
      min_n = sd_min_n(sd_meaning)
    )
    return(explain(text))
  }

  # The bounds that decided the test, as `debit_scalar()` has them. A missing
  # value, or one with a missing decimal count, has none:
  bounds <- debit_bounds(
    x,
    sd,
    digits_x,
    digits_sd,
    rounding,
    threshold,
    symmetric
  )

  text_intro <- paste0(
    "DEBIT is about binary data, where every value is 0 or 1. The mean is ",
    "then the share of ones, and the SD follows from the number of ones and ",
    "the sample size."
  )

  # The case is decided, with one value missing, so the other rules it out on
  # its own: no number of ones has an SD in the SD's range, or no number of
  # ones has a mean in the mean's range.
  if (is.null(bounds$x)) {
    bounds_sd <- bounds$sd
    text_bounds_sd <- describe_reported(
      if (sd_meaning == "se") "SD, taken to be an SE," else "SD",
      sd,
      digits_sd,
      format_number(bounds_sd$lower / bounds_sd$denom, digits_sd),
      format_number(bounds_sd$upper / bounds_sd$denom, digits_sd),
      bounds_sd$incl_lower,
      bounds_sd$incl_upper
    )
    name_missing <- if (is.na(x)) "x" else "digits_x"
    text_none <- if (formula == "exact") {
      paste0(
        name_missing,
        " is missing, so the number of ones is unknown. But it is an integer ",
        "from 0 to ",
        n,
        ", and none of these gives an SD in that range."
      )
    } else {
      paste0(
        name_missing,
        " is missing, so the mean is unknown. But it lies between 0 and 1, ",
        "and no mean in that range gives an SD in the SD's range."
      )
    }
    return(explain(c(text_intro, text_bounds_sd, text_none)))
  }

  sum_lower <- rec[[8L]]
  sum_upper <- rec[[9L]]

  bounds_x <- bounds$x

  x_incl_lower <- bounds_x$incl_lower
  x_incl_upper <- bounds_x$incl_upper

  x_num_lower <- bounds_x$lower
  x_num_upper <- bounds_x$upper

  x_lower <- format_product_exact(1, x_num_lower, bounds_x$denom, digits_x)
  x_upper <- format_product_exact(1, x_num_upper, bounds_x$denom, digits_x)

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
      format_product_exact(n, x_num_lower, bounds_x$denom, digits_x)
    ),
    paste(
      x_upper,
      "*",
      n,
      "=",
      format_product_exact(n, x_num_upper, bounds_x$denom, digits_x)
    ),
    x_incl_lower,
    x_incl_upper
  )
  # A population SD can be tested with a single value:
  values <- if (n == 1) "1 value" else paste(n, "values")

  text_ones <- paste0(
    "With ",
    values,
    ", the mean is the number of ones divided by ",
    n,
    ", so that number must be ",
    ones_range,
    ". "
  )

  if (sum_lower > sum_upper) {
    text_ones <- paste0(
      text_ones,
      "But it has to be an integer, and there is none in this ",
      "range. So no number of ones fits the mean, whatever the SD is."
    )
    return(explain(c(text_intro, text_bounds_x, text_ones)))
  }

  text_ones <- paste0(
    text_ones,
    "It has to be an integer, so it can ",
    if (sum_lower == sum_upper) {
      paste0("only be ", sum_lower, ".")
    } else {
      paste0("be anything from ", sum_lower, " to ", sum_upper, ".")
    }
  )

  # The statistic that the bounds and the calculations below are about:
  stat <- if (sd_meaning == "se") "SE" else "SD"

  # Only formatted here, since a missing `digits_sd` has no bounds:
  text_bounds_sd <- describe_reported(
    if (sd_meaning == "se") "SD, taken to be an SE," else "SD",
    sd,
    digits_sd,
    format_number(rec[[2L]], digits_sd),
    format_number(rec[[4L]], digits_sd),
    rec[[3L]],
    rec[[5L]]
  )

  n_dev <- sd_denominator(n, sd_meaning)
  # Shown in full, so the products are exact even past 2^53, where a double
  # would print a neighbor:
  n_dev_exact <- if (sd_meaning == "se") whole_mul(n, n - 1) else n_dev
  text_n_dev <- whole_format(n_dev_exact)
  text_n_n_dev <- whole_format(whole_mul(n, n_dev_exact))

  text_formula <- paste0(
    "With k ones among ",
    values,
    ", the ",
    switch(
      sd_meaning,
      sd_pop = "population SD is ",
      se = paste0("SE, the SD divided by sqrt(", n, "), is "),
      "SD is "
    ),
    "sqrt(k * (",
    n,
    " - k) / (",
    n,
    " * ",
    text_n_dev,
    "))."
  )

  # Whether each number of ones `k` has an SD in range is decided by the whole
  # number `k * (n - k)`, as in `binary_sd_attainable()`, not by the rounded
  # SD shown.
  spread_range <- binary_spread_range(n, bounds$sd, sd_meaning)

  ones <- binary_ones_shown(sum_lower, sum_upper, n, spread_range)
  below <- binary_spread_cmp(ones, n, spread_range$min) < 0
  above <- binary_spread_cmp(ones, n, spread_range$max) > 0
  passed <- !below & !above
  # Only to pick the `k` to show, so a double is close enough:
  spread <- ones * (n - ones)
  # The spread `k * (n - k)` is the greater the closer `k` is to `n / 2`. This
  # orders it exactly, where `spread` would tie neighbors past 2^53:
  centrality <- -abs(2 * ones - n)
  spread_middle <- mean(vapply(spread_range, whole_double, numeric(1L)))

  if (any(passed) != consistency) {
    cli::cli_abort(
      "Internal error: `debit_explain()` disagrees with `debit()`.",
      .internal = TRUE
    )
  }

  # The SD of `k` ones as a calculation:
  sd_of <- function(k) {
    paste0(
      "sqrt(",
      k,
      " * ",
      n - k,
      " / ",
      text_n_n_dev,
      ") = ",
      format_number(
        sqrt(k * (n - k) / (n * n_dev)),
        digits_sd,
        c(rec[[2L]], rec[[4L]])
      )
    )
  }

  several <- sum_upper > sum_lower

  text_ones_sd <- if (any(passed)) {
    # The `k` whose SD is closest to the middle of the range, so that it doesn't
    # look like a bound once rounded for display:
    k <- ones[passed][which.min(abs(spread[passed] - spread_middle))]

    paste0(
      if (several) "For example, for k = " else "For k = ",
      k,
      ", that is ",
      sd_of(k),
      ", which is in that range."
    )
  } else if (!several) {
    paste0(
      "For k = ",
      ones,
      ", that is ",
      sd_of(ones),
      ", which is ",
      if (below) "below" else "above",
      " that range."
    )
  } else if (all(below)) {
    # The spread, and with it the SD, is greatest for the `k` closest to half
    # of `n`, so that one stands for all:
    k <- ones[which.max(centrality)]

    paste0(
      "Even the largest of these ",
      stat,
      "s, for k = ",
      k,
      ", is only ",
      sd_of(k),
      ", which is below that range."
    )
  } else if (all(above)) {
    k <- ones[which.min(centrality)]

    paste0(
      "Even the smallest of these ",
      stat,
      "s, for k = ",
      k,
      ", is ",
      sd_of(k),
      ", which is above that range."
    )
  } else {
    # Some SDs are below the range and some above, and none in it: those next
    # to it skip over it.
    k_below <- ones[below][which.max(centrality[below])]
    k_above <- ones[!below][which.min(centrality[!below])]

    paste0(
      "These ",
      stat,
      "s skip over that range. For k = ",
      k_below,
      ", the ",
      stat,
      " is ",
      sd_of(k_below),
      ", which is below it, and for k = ",
      k_above,
      ", it is ",
      sd_of(k_above),
      ", which is above it."
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


# The numbers of ones from `sum_lower` to `sum_upper` that the text may need to
# show, i.e., the ones with the least or greatest spread `k * (n - k)` on either
# side of the range or in it, and the one closest to its middle. All of them are
# next to a bound of `k` or a root of `k * (n - k) = spread`, so they can be
# found without listing every `k`, which may be millions. The roots at the ends
# of the range are found exactly, as in `binary_sd_attainable()`; the one in
# the middle only picks an example, so its neighbors are close enough.
binary_ones_shown <- function(sum_lower, sum_upper, n, spread_range) {
  half <- floor(n / 2)
  middle <- mean(vapply(spread_range, whole_double, numeric(1L)))
  roots <- c(
    binary_least_ones(n, spread_range$min, 0, half),
    binary_least_ones(n, spread_range$max, 0, half),
    floor(binary_spread_root(n, middle)),
    half
  )

  ones <- outer(roots, -1:2, `+`)
  ones <- c(sum_lower, sum_upper, ones, n - ones)
  sort(unique(pmin(pmax(ones, sum_lower), sum_upper)))
}
