#' Poisson binomial test for the rate of GRIM consistency
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Given the output of [`grim_map()`] or a function built on it,
#'   `grim_binomial()` tests whether the rate of GRIM-consistent value sets is
#'   above what random decimal digits would produce. This approach has not been
#'   tested extensively; use it with care.
#'
#' @param data Data frame returned by [`grim_map()`] or a function built on
#'   it, such as [`grim_map_seq()`].
#' @param alternative String (length 1). One of `"greater"` (the default),
#'   `"less"`, and `"two.sided"`.
#' @param conf.level Numeric (length 1). Confidence level of the interval
#'   around `estimate`. Default is `0.95`.
#'
#' @details Under the null hypothesis, the decimal digits of each mean are
#'   random, so its probability of consistency is `1 - probability` from
#'   [`grim_probability()`]. Since this differs between value sets, the test
#'   uses the Poisson binomial distribution rather than averaging the
#'   probabilities. Value sets without a verdict are left out.
#'
#'   Correct value sets are always consistent, so a collection of them should
#'   be more consistent than random digits even if some are wrong. There is no
#'   corresponding reason to expect fewer consistent sets than random, hence
#'   the one-sided default.
#'
#'   The confidence interval is the exact Clopper-Pearson interval for the
#'   consistency rate, as in [`stats::binom.test()`], one-sided like the test.
#'   It treats the value sets as sharing one probability, so it is an interval
#'   for their average. Compare it to the mean of `1 - probability`.
#'
#' @returns Tibble with the columns `estimate` (observed consistency rate),
#'   `statistic` (number of consistent cases), `p.value`, `parameter` (number
#'   of cases), `conf.low`, `conf.high`, `conf.level`, `method`, and
#'   `alternative`.
#'
#' @export
#'
#' @examples
#' pigs1 |>
#'   grim_map(digits_x = 2, items_averaged = 1) |>
#'   grim_binomial()

grim_binomial <- function(
  data,
  alternative = c("greater", "less", "two.sided"),
  conf.level = 0.95
) {
  rlang::check_installed("poibin", "for the Poisson binomial test.")

  alternative <- rlang::arg_match(alternative)

  if (!inherits(data, "scrutiny_grim_map")) {
    cli::cli_abort("`data` must be output of a GRIM mapper function.")
  }

  check_cols_needed(
    data,
    c("consistency", "probability"),
    "i" = "`grim_map()` output has them."
  )

  if (
    !is.numeric(conf.level) ||
      length(conf.level) != 1L ||
      is.na(conf.level) ||
      conf.level <= 0 ||
      conf.level >= 1
  ) {
    abort_in_export(
      "`conf.level` must be a single number between 0 and 1.",
      "x" = "It is {conf.level}."
    )
  }

  # A value set the test could not decide -- a missing `n`, say -- has neither a
  # verdict nor a probability. It is left out on both sides: counting it as a
  # case while its `NA` probability went into the Poisson binomial made the
  # p-value `NA` for the whole table:
  data <- data[!is.na(data$consistency) & !is.na(data$probability), ]

  n_consistent <- sum(data$consistency)
  n_cases <- nrow(data)

  # `poibin::ppoibin()` crashes the R session when given no probabilities:
  if (n_cases == 0L) {
    cli::cli_abort(c(
      "No value sets to test.",
      "x" = "`data` has no rows with a GRIM verdict and a probability.",
      "i" = "Rows whose consistency or probability is `NA` are left out."
    ))
  }

  # Per-row probability of consistency under the null (random decimal digits)
  pp <- 1 - data$probability

  # Poisson binomial p-value using per-row null probabilities
  p.value <- switch(
    alternative,
    greater = 1 - poibin::ppoibin(n_consistent - 1L, pp),
    less = poibin::ppoibin(n_consistent, pp),
    two.sided = {
      p_upper <- 1 - poibin::ppoibin(n_consistent - 1L, pp)
      p_lower <- poibin::ppoibin(n_consistent, pp)
      min(1, 2 * min(p_upper, p_lower))
    }
  )

  conf_int <- stats::binom.test(
    n_consistent,
    n_cases,
    alternative = alternative,
    conf.level = conf.level
  )$conf.int

  tibble::tibble(
    estimate = n_consistent / n_cases,
    statistic = n_consistent,
    p.value = p.value,
    parameter = n_cases,
    conf.low = conf_int[1L],
    conf.high = conf_int[2L],
    conf.level = conf.level,
    method = "Poisson binomial test",
    alternative = alternative
  )
}


#' Power of the GRIM binomial test
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `grim_binomial_power()` computes the statistical power of
#'   [`grim_binomial()`] for given parameters, to judge whether the test is
#'   informative for a data set before interpreting its p-value. Like the test
#'   itself, it has not been tested extensively.
#'
#' @param p0 Numeric. Null probability of GRIM consistency per value set, i.e.,
#'   `1 - grim_probability(...)`. Either one value per value set, or a single
#'   value shared by all, in which case `k` is required.
#' @param p1 Numeric (length 1). Probability of consistency under the
#'   alternative, shared by all value sets.
#' @param k Numeric (length 1). Number of value sets. Required if `p0` has
#'   length 1; otherwise ignored in favor of `length(p0)`.
#' @param alpha Numeric (length 1). Significance level. Default is `0.05`.
#' @param alternative String (length 1). One of `"greater"` (the default),
#'   `"less"`, and `"two.sided"`.
#'
#' @returns Numeric (length 1). The probability of rejecting the null
#'   hypothesis when the alternative is true.
#'
#' @export
#'
#' @examples
#' # 10 means with a null probability of 0.5 each, 90% truly consistent:
#' grim_binomial_power(p0 = 0.50, p1 = 0.9, k = 10)
#'
#' # The same with 30 means:
#' grim_binomial_power(p0 = 0.50, p1 = 0.9, k = 30)
#'
#' # One null probability per value set:
#' grim_binomial_power(p0 = c(0.50, 0.70, 0.80, 0.90, 0.60), p1 = 0.9)

grim_binomial_power <- function(
  p0,
  p1,
  k = NULL,
  alpha = 0.05,
  alternative = c("greater", "less", "two.sided")
) {
  rlang::check_installed("poibin", "for Poisson binomial power analysis.")

  alternative <- rlang::arg_match(alternative)

  if (length(p0) != 1L) {
    k <- length(p0)
  } else if (is.null(k)) {
    abort_in_export("`k` is required when `p0` is length 1.")
  }

  # `poibin::ppoibin()` crashes the R session when given no probabilities:
  if (length(p0) == 0L) {
    abort_in_export(
      "There must be at least one value set.",
      "x" = "`p0` has length 0."
    )
  }
  if (
    length(k) != 1L || !is.numeric(k) || !is.finite(k) || k < 1 ||
      !is_whole_number(k)
  ) {
    abort_in_export(
      "`k` must be a single whole number of at least 1.",
      "x" = "It is {wrap_in_quotes_or_backticks(k)}.",
      "i" = "It is the number of value sets."
    )
  }

  # A probability outside of 0 and 1 used to give `NaN`, an error from poibin,
  # or a power of 1:
  check_probability <- function(p, name) {
    if (!is.numeric(p) || anyNA(p) || any(p < 0 | p > 1)) {
      abort_in_export(
        "`{name}` must be between 0 and 1.",
        "x" = "It is {wrap_in_quotes_or_backticks(p)}."
      )
    }
  }
  check_probability(p0, "p0")
  check_probability(p1, "p1")
  check_probability(alpha, "alpha")
  check_length(p1, 1L)
  check_length(alpha, 1L)

  if (length(p0) == 1L) {
    p0 <- rep(p0, k)
  }

  # All possible numbers of consistent cases, from 0 to `k`
  x <- 0:k

  # Probability of each outcome under H1. This uses the standard binomial model
  # because `p1` is shared across all rows.
  pmf_alt <- stats::dbinom(x, size = k, prob = p1)

  # For each outcome `x`, compute the null p-value using the Poisson binomial
  # CDF (which accounts for per-row heterogeneity in `p0`)
  p_values_all <- switch(
    alternative,

    # P(X >= x | H0) for each possible `x`
    greater = 1 - poibin::ppoibin(x - 1, p0),

    # P(X <= x | H0) for each possible `x`
    less = poibin::ppoibin(x, p0),

    two.sided = {
      p_upper <- 1 - poibin::ppoibin(x - 1, p0)
      p_lower <- poibin::ppoibin(x, p0)
      pmin(1, 2 * pmin(p_upper, p_lower))
    },

    cli::cli_abort("Internal error: invalid `alternative` value.")
  )

  # Does this outcome fall into the rejection region?
  is_in_rejection <- p_values_all <= alpha

  # Power = total probability (under H1) of outcomes that reject H0
  sum(pmf_alt[is_in_rejection])
}
