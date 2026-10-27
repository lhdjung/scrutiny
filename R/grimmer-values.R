# `grimmer_values()` enumerates what `grimmer_scalar()` only searches: every
# whole-number sum that `sum_range()` admits for the mean and, for each, every
# sum of squares that `grimmer_candidate()` admits for the SD. The verdict and
# the argument checks come from `grimmer_scalar()` itself.

grimmer_values_scalar <- function(
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
  sd_meaning <- arg_match_in_export(sd_meaning, SD_MEANINGS)

  # The same sum of squares gives a different SD under each premise, so the
  # values of the two can't be told apart in one table:
  if (sd_meaning == "sd_sample_or_pop") {
    abort_in_export(
      "{.arg sd_meaning} can't be {.val sd_sample_or_pop} here.",
      "x" = "It stands for two premises, and each has its own values.",
      "i" = "Call {.fn grimmer_values} with {.val sd_sample} and with \\
      {.val sd_pop} instead."
    )
  }

  consistency <- grimmer_scalar(
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

  n_dev <- sd_denominator(n, sd_meaning)

  # One row of `NA` if undecidable, no rows if inconsistent:
  if (is.na(consistency)) {
    return(grimmer_values_tibble(NA_real_, NA_real_, n, items_averaged, n_dev))
  }
  if (!consistency) {
    return(grimmer_values_tibble(
      numeric(0L),
      numeric(0L),
      n,
      items_averaged,
      n_dev
    ))
  }

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

  sums_consistent <- roundwork::sum_range(
    x_num = x,
    n_items = as.numeric(n) * items_averaged,
    digits = digits_x,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # One element per candidate sum, joined once at the end: appending to a
  # vector in the loop copies it each time, which gets quadratic by `n = 20000`.
  candidate_sums <- sums_consistent[1L]:sums_consistent[2L]
  sums <- vector("list", length(candidate_sums))
  sums_squares <- sums

  for (i in seq_along(candidate_sums)) {
    s <- candidate_sums[i]
    candidate <- grimmer_candidate(
      s = s,
      n = n,
      items_averaged = items_averaged,
      sd_bounds = sd_bounds
    )

    if (!candidate$pass_test3) {
      next
    }

    upper <- candidate$sum_squares[2L]

    squares <- if (n >= 5) {
      # Every whole number from the minimum on with the parity of `s`:
      lower <- max(candidate$sum_squares[1L], candidate$sum_squares_min)
      seq(lower + (lower - s) %% 2, upper, by = 2)
    } else {
      # Fewer numbers skip some of those; `grimmer_candidate()` found the
      # first, and `sum_squares_attainable()` finds each next one:
      q <- candidate$attained
      out <- q
      repeat {
        q <- sum_squares_attainable(s, n, q + 2, upper)
        if (is.na(q)) {
          break
        }
        out <- c(out, q)
      }
      out
    }

    sums[[i]] <- rep(s, length(squares))
    sums_squares[[i]] <- squares
  }

  sums <- unlist(sums)
  sums_squares <- unlist(sums_squares)

  if (length(sums) == 0L) {
    cli::cli_abort(
      "Internal error: `grimmer_values()` disagrees with `grimmer()`.",
      .internal = TRUE
    )
  }

  grimmer_values_tibble(sums, sums_squares, n, items_averaged, n_dev)
}


# The mean and SD that a sum and a sum of squares of `n` whole numbers give;
# see `sd_square_term()` for the formula. One row of `NA` for an undecidable
# value set, so that the element is a tibble either way.
grimmer_values_tibble <- function(
  sums,
  sums_squares,
  n,
  items_averaged,
  n_dev
) {
  tibble::tibble(
    x = sums / (n * items_averaged),
    sd = sqrt((sums_squares - sums^2 / n) / n_dev) / items_averaged,
    sum = sums,
    sum_squares = sums_squares
  )
}


#' Achievable means and SDs behind a reported pair
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `grimmer_values()` returns every pair of a mean and an SD that a sample of
#'   `n` whole numbers can have and that would be reported as `x` and `sd`. It
#'   is the counterpart of [`grim_values()`] for [`grimmer()`], which is `TRUE`
#'   exactly if there is at least one such pair.
#'
#'   Each pair comes with the whole-number sum and sum of squares behind it.
#'   Statistics computed from several groups, such as a t-value, can be
#'   compared exactly through these integers.
#'
#' @details The sums are the whole numbers that [`grim_values()`] divides by
#'   `n * items_averaged`. For each, the sums of squares are the whole numbers
#'   within the rounding bounds of `sd` that `n` whole numbers with that sum can
#'   have. With `items_averaged > 1`, the whole numbers are the respondents'
#'   totals across the items, so the sums are those of the totals.
#'
#'   The number of pairs grows with `n` and `items_averaged`, as the number of
#'   values in [`grim_values()`] does. Under `rounding = "even"`, the pairs
#'   include ties that [`base::round()`] may round away from `x` or `sd`, as
#'   there.
#'
#' @inheritParams grimmer
#' @param sd_meaning String. As in [`grimmer()`], but `"sd_sample_or_pop"` is
#'   not allowed: the same sum of squares gives a different SD under each
#'   premise, so call `grimmer_values()` once for each. Under `"se"`, the `sd`
#'   column holds the standard error.
#'
#' @return A list with one tibble per value set. Its columns are `x` and `sd`,
#'   the achievable values, and `sum` and `sum_squares`, the whole numbers
#'   behind them. The tibble has no rows if the value set is inconsistent, and
#'   one row of `NA` if it cannot be decided, as in [`grimmer()`].
#'
#' @seealso [`grimmer()`], [`grimmer_map()`], and [`grim_values()`].
#'
#' @include grimmer.R
#'
#' @export
#'
#' @examples
#' # A mean of 5.23 and an SD of 2.55 are consistent with a sample size of 31,
#' # and there is exactly one way to get there:
#' grimmer_values(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 31,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1
#' )
#'
#' # With one decimal place, several pairs round to the same values:
#' grimmer_values(
#'   x = 5.3,
#'   sd = 0.7,
#'   n = 30,
#'   digits_x = 1,
#'   digits_sd = 1,
#'   items_averaged = 1
#' )
#'
#' # None at all if the value set is inconsistent:
#' grimmer_values(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 35,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1
#' )

# The signature mirrors `grimmer_values_scalar()`'s; see `map_value_sets()`.
grimmer_values <- function(
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
  map_value_sets(
    fn = grimmer_values_scalar,
    along = c("x", "sd", "n", "digits_x", "digits_sd", "items_averaged"),
    type = "list"
  )
}
