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
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  sd_meaning <- arg_match_in_export(sd_meaning, SD_MEANINGS)
  # `"continuous"` is an error; a missing `data_type` is an error in
  # `grimmer_scalar()`, after the checks for the arguments before it:
  if (!missing(data_type)) {
    check_data_type_whole(data_type)
  }

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
    data_type = data_type,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric,
    sd_meaning = sd_meaning
  )

  # As a double: `n * (n - 1)` overflows an integer from 46341:
  n_dev <- sd_denominator(as.numeric(n), sd_meaning)

  # One row of `NA` if undecidable, no rows if inconsistent. With `data_type`
  # missing, a value set that whole numbers can have is undecidable, too:
  # continuous data have other values.
  if (is.na(consistency) || (consistency && is.na(data_type))) {
    return(grimmer_values_tibble(
      NA_real_,
      NA_real_,
      NA_real_,
      n,
      items_averaged,
      n_dev
    ))
  }
  if (!consistency) {
    return(grimmer_values_tibble(
      numeric(0L),
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

  value_set <- list(x = x, sd = sd, n = n, items_averaged = items_averaged)

  # A sum passes exactly if its remainder modulo `n` does, so the first `n`
  # sums tell which remainders pass, and only the sums with these are listed.
  # Only those within reach of the SD can, as in `grimmer_scalar()`. Each has
  # a pair at least, so they are counted before any is allocated: a mean with
  # many decimal places and a large `items_averaged` admits millions.
  abort_too_large <- function() {
    abort_in_export(
      "The sums of squares are too large to list.",
      "x" = "They reach 2^53, about 9e15, beyond which a double skips whole \\
      numbers.",
      "i" = "`grimmer()` tests this value set exactly all the same."
    )
  }
  # A sum of squares is at least the sum of squared deviations, so all of them
  # are too large if its lower bound is. Checked before the sums are tried:
  if (sd_bounds$term_lower$approx >= 2^53 * (1 + 2^-40)) {
    abort_too_large()
  }

  sum_last <- sums_consistent[[2L]]
  window <- sums_consistent |>
    grimmer_sums_reachable(n, sd_bounds) |>
    sums_from_bounds()
  passing <- window[vapply(
    window,
    function(s) {
      grimmer_candidate(
        s = s,
        n = n,
        items_averaged = items_averaged,
        sd_bounds = sd_bounds
      )$pass_test3
    },
    logical(1L)
  )]
  check_values_size(sum((sum_last - passing) %/% n + 1), "pairs", value_set)
  candidate_sums <- passing |>
    lapply(function(s) seq(s, sum_last, by = n)) |>
    unlist() |>
    sort()

  # One element per candidate sum, joined once at the end: appending to a
  # vector in the loop copies it each time, which gets quadratic by `n = 20000`.
  sums <- vector("list", length(candidate_sums))
  sums_squares <- sums
  deviations <- sums

  # Counted as they come, before anything big is allocated: the pairs grow with
  # `n^2` for SDs with few decimal places. At `x = 50`, `sd = 20`, `n = 5000`,
  # both with no decimals, there are 5e8.
  n_pairs <- 0

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

    # The test works with the sums of squares shifted to the sum `r`; the
    # output shows them as they are, which a double can only do below `2^53`:
    if (is.null(grimmer_candidate_absolute(candidate))) {
      abort_too_large()
    }

    r <- candidate$r
    upper <- candidate$range$upper

    squares <- if (n >= 5) {
      # Every second whole number from the first one with the parity of `s`:
      first <- candidate$lower + (candidate$lower - r) %% 2
      check_values_size(n_pairs + (upper - first) %/% 2 + 1, "pairs", value_set)
      seq(first, upper, by = 2)
    } else {
      # Fewer numbers skip some sums of squares, so they can't be counted
      # without finding them. `grimmer_candidate()` found the first one:
      sum_squares_attainable_all(
        s = r,
        n = n,
        first = candidate$attained,
        upper = upper,
        count_max = VALUES_MAX - n_pairs
      )
    }

    n_pairs <- n_pairs + length(squares)
    check_values_size(n_pairs, "pairs", value_set)

    sums[[i]] <- rep(s, length(squares))
    sums_squares[[i]] <- squares + candidate$shift
    # The sum of squared deviations, computed from the shifted sum of squares,
    # where it doesn't cancel against `s^2 / n` for a large mean:
    deviations[[i]] <- squares - r^2 / n
  }

  sums <- unlist(sums)
  sums_squares <- unlist(sums_squares)
  deviations <- unlist(deviations)

  if (length(sums) == 0L) {
    cli::cli_abort(
      "Internal error: `grimmer_values()` disagrees with `grimmer()`.",
      .internal = TRUE
    )
  }

  grimmer_values_tibble(
    sums,
    sums_squares,
    deviations,
    n,
    items_averaged,
    n_dev
  )
}


# The mean and SD that a sum and a sum of squared deviations of `n` integers give;
# see `sd_square_term()` for the formula. One row of `NA` but for `n` for an
# undecidable value set, so that it still has a row in the output.
grimmer_values_tibble <- function(
  sums,
  sums_squares,
  deviations,
  n,
  items_averaged,
  n_dev
) {
  tibble::tibble(
    # As doubles: the product of two integers overflows from 46,341 each:
    x = sums / (as.numeric(n) * items_averaged),
    sd = sqrt(deviations / n_dev) / items_averaged,
    n = as.numeric(n),
    sum = sums,
    sum_squares = sums_squares
  )
}


#' Achievable means and SDs behind a reported pair
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `grimmer_values()` returns every pair of a mean and an SD that a sample of
#'   `n` integers can have and that would be reported as `x` and `sd`. It
#'   is the counterpart of [`grim_values()`] for [`grimmer()`], which is `TRUE`
#'   exactly if there is at least one such pair.
#'
#'   Each pair comes with the whole-number sum and sum of squares behind it.
#'   Statistics computed from several groups, such as a t-value, can be
#'   compared exactly through these integers.
#'
#' @details The sums are the whole numbers that [`grim_values()`] divides by
#'   `n * items_averaged`. For each, the sums of squares are the whole numbers
#'   within the rounding bounds of `sd` that `n` integers with that sum can
#'   have. With `items_averaged > 1`, the integers are the respondents'
#'   totals across the items, so the sums are those of the totals.
#'
#'   The number of pairs grows with `n`, `items_averaged`, and `sd`, and
#'   shrinks with `digits_x` and `digits_sd`. More than a million pairs for one
#'   value set is an error, since the output has one row for each. So is a sum
#'   of squares of 2^53 or more, about 9e15, which a double can't hold exactly;
#'   [`grimmer()`] tests such value sets all the same.
#'
#'   Under `rounding = "even"`, the pairs include ties that [`base::round()`]
#'   may round away from `x` or `sd`, as in [`grim_values()`].
#'
#' @inheritParams grimmer
#' @param data_type String. `"integer"` if the values are whole numbers,
#'   as in [`grimmer()`]. There is no default, so that every call states
#'   this assumption. `"continuous"` is an error: such data can have any
#'   value that would be reported as the one given, so there are none to
#'   list. Under `NA`, a value set is undecidable unless no data can have it.
#' @param sd_meaning String. As in [`grimmer()`], but `"sd_sample_or_pop"` is
#'   not allowed: the same sum of squares gives a different SD under each
#'   premise, so call `grimmer_values()` once for each. Under `"se"`, the `sd`
#'   column holds the standard error.
#'
#' @return A tibble with one row per achievable pair. `case` is the index of
#'   the value set, as in [`grim_values()`]. `x` and `sd` are the achievable
#'   values, `n` the reported sample size, and `sum` and `sum_squares` the
#'   whole numbers behind them. An inconsistent value set has no rows, and one
#'   that cannot be decided has one row of `NA` but for `n`, as in
#'   [`grimmer()`].
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
#'   items_averaged = 1,
#'   data_type = "integer"
#' )
#'
#' # With one decimal place, several pairs round to the same values:
#' grimmer_values(
#'   x = 5.3,
#'   sd = 0.7,
#'   n = 30,
#'   digits_x = 1,
#'   digits_sd = 1,
#'   items_averaged = 1,
#'   data_type = "integer"
#' )
#'
#' # None at all if the value set is inconsistent:
#' grimmer_values(
#'   x = 5.23,
#'   sd = 2.55,
#'   n = 35,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   items_averaged = 1,
#'   data_type = "integer"
#' )

# The signature mirrors `grimmer_values_scalar()`'s; see `map_value_sets()`.
grimmer_values <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  map_value_sets(
    fn = grimmer_values_scalar,
    along = c(
      "x",
      "sd",
      "n",
      "digits_x",
      "digits_sd",
      "items_averaged",
      "data_type"
    ),
    type = "list"
  ) |>
    bind_cases(PTYPE_GRIMMER_VALUES)
}
