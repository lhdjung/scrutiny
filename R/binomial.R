#' Poisson binomial test for the probability of GRIM inconsistency
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This approach has not been extensively tested, and I am not yet confident
#'   in it. Use with care.
#'
#'   Given the output of [`grim_map()`] or similar, `grim_binomial()` tests
#'   whether the rate of GRIM-consistent value sets is above chance.
#'
#'   Consistency represents "success", and the null hypothesis corresponds to
#'   random decimal digits for the means (or other fractions, such as
#'   percentages.) See details below.
#'
#' @param data Data frame returned by a GRIM mapper function, such as
#'   [`grim_map()`] or [`grim_map_seq()`].
#' @param alternative String (length 1). One of `"greater"`, `"less"`, and
#'   `"two.sided"`. Default is `"greater"`; see details.
#' @param conf.level Numeric (length 1). Confidence level for the confidence
#'   interval in the output. Default is `0.95`.
#'
#' @details This conducts a one-tailed test by default (`alternative =
#'   "greater"`). The idea is to assess whether more means are GRIM-consistent
#'   with their sample sizes than expected given the baseline [probability of
#'   GRIM
#'   inconsistency](https://lhdjung.github.io/scrutiny/articles/grim.html#the-probability-of-grim-inconsistency)
#'   for means with random decimal digits. The *number* of decimal places is
#'   given (as is the sample size); only the specific digits occupying the
#'   decimal places are assumed to be random.
#'
#'   Because each value set can have a different probability of GRIM consistency
#'   under the null (due to varying sample sizes or decimal places), the test
#'   uses the Poisson binomial distribution rather than the standard binomial.
#'   This avoids the need to average per-row probabilities into a single value.
#'
#'   As any correct value set is GRIM-consistent, the key question is whether a
#'   collection of them can be sufficiently distinguished from random. In this
#'   way, even if some value sets are inconsistent, there may still be a trend
#'   towards consistency because correct value sets would bias the distribution
#'   in this way.
#'
#'   However, it is hard to see how *fewer* value sets should be consistent than
#'   expected at random. This suggests a one-tailed test.
#'
#' @returns Tibble (data frame) with columns `estimate` (observed consistency
#'   rate), `statistic` (number of consistent cases), `p.value`, `parameter`
#'   (total number of cases), `conf.level`, `method`, and `alternative`.
#'
#' @export
#'
#' @examples
#' pigs1 |>
#'   grim_map(digits_x = 2) |>
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

  n_consistent <- length(which(data$consistency))
  n_cases <- nrow(data)

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

  tibble::tibble(
    estimate = n_consistent / n_cases,
    statistic = n_consistent,
    p.value = p.value,
    parameter = n_cases,
    conf.level = conf.level,
    method = "Poisson binomial test",
    alternative = alternative
  )
}


#' Power of the GRIM binomial test
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This approach has not been extensively tested, and I am not yet confident
#'   in it. Use with care.
#'
#'   Compute the statistical power of [`grim_binomial()`] for given parameters.
#'   Use this to judge whether the test is informative for a particular dataset
#'   before interpreting its p-value.
#'
#' @param p0 Numeric. Per-row null probability of GRIM consistency, i.e., `1 -
#'   P(GRIM inconsistency)`. Can be a vector with one value per reported mean
#'   (matching the Poisson binomial model used by [`grim_binomial()`]), or a
#'   single value if all rows share the same probability. If length 1, `k` must
#'   be provided. Compute individual values via [`grim_probability()`].
#' @param p1 Numeric (length 1). True probability of GRIM consistency under the
#'   alternative hypothesis. Unlike the null, a single value is used for all
#'   rows.
#' @param k Numeric (length 1). Number of reported means. Required when `p0` is
#'   length 1; ignored when `p0` is a vector (the length of `p0` is used
#'   instead).
#' @param alpha Numeric (length 1). Significance level. Default is `0.05`.
#' @param alternative String (length 1). One of `"greater"`, `"less"`, and
#'   `"two.sided"`. Default is `"greater"`, matching [`grim_binomial()`].
#'
#' @returns Numeric (length 1). The probability of rejecting the null hypothesis
#'   when the alternative is true.
#'
#' @export
#'
#' @examples
#' # 10 means, N ~ 50 with 2 decimal places, 90% truly consistent:
#' grim_binomial_power(p0 = 0.50, p1 = 0.9, k = 10)
#'
#' # Same scenario but with 30 means:
#' grim_binomial_power(p0 = 0.50, p1 = 0.9, k = 30)
#'
#' # Per-row null probabilities (Poisson binomial null):
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

  if (length(p0) == 1L) {
    if (is.null(k)) {
      cli::cli_abort("`k` is required when `p0` is length 1.")
    }
    p0 <- rep(p0, k)
  } else {
    k <- length(p0)
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
