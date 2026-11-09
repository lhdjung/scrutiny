# `debit_values()` lists what `debit_scalar()` only searches: every number of
# ones `k` among `n` binary values whose mean `k / n` and SD would be reported
# as `x` and `sd`. The verdict and the argument checks come from
# `debit_scalar()`, with the bounds and spreads it decides by.

debit_values_scalar <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  sd_meaning <- check_debit_values_args(
    sd_meaning,
    formula,
    if (!missing(data_type)) data_type
  )

  consistency <- debit_scalar(
    x = x,
    sd = sd,
    n = n,
    digits_x = digits_x,
    digits_sd = digits_sd,
    data_type = data_type,
    sd_meaning = sd_meaning,
    formula = formula,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # One row of `NA` if undecidable, no rows if inconsistent. A consistent
  # value set with a missing value is forced to be consistent, but which
  # samples it stands for is still open:
  is_known <- !anyNA(list(x, sd, n, digits_x, digits_sd, data_type))
  if (is.na(consistency) || (consistency && !is_known)) {
    return(debit_values_tibble(NA_real_, n, sd_meaning))
  }
  if (!consistency) {
    return(debit_values_tibble(numeric(0L), n, sd_meaning))
  }

  n <- as.numeric(n)
  bounds <- debit_bounds(
    x,
    sd,
    digits_x,
    digits_sd,
    rounding,
    threshold,
    symmetric
  )

  # The numbers of ones whose mean would be reported as `x`, as in
  # `debit_scalar()`, and of these, those whose spread `k * (n - k)` the SD
  # admits:
  ones <- roundwork::sum_range(x, n, digits_x, rounding, threshold, symmetric)
  ones <- c(max(ones[[1L]], 0), min(ones[[2L]], n))
  check_values_size(
    ones[[2L]] - ones[[1L]] + 1,
    "samples",
    list(x = x, sd = sd, n = n)
  )
  ones <- seq(ones[[1L]], ones[[2L]])
  spread <- binary_spread_range(n, bounds$sd, sd_meaning)
  keep <- binary_spread_cmp(ones, n, spread$min) >= 0 &
    binary_spread_cmp(ones, n, spread$max) <= 0

  if (!any(keep)) {
    cli::cli_abort(
      "Internal error: `debit_values()` disagrees with `debit()`.",
      .internal = TRUE
    )
  }

  debit_values_tibble(ones[keep], n, sd_meaning)
}


# The checks of `debit_values()` and `debit_closest()` beyond `debit()`'s.
# Returns `sd_meaning` matched:
check_debit_values_args <- function(sd_meaning, formula, data_type) {
  # The same data give a different SD under each premise:
  sd_meaning <- check_sd_meaning_single(sd_meaning)
  # Only actual samples have values to list:
  formula <- arg_match_in_export(formula, DEBIT_FORMULAS)
  if (formula == "mean_n") {
    abort_in_export(
      "{.arg formula} can't be {.val mean_n} here.",
      "x" = "It takes the mean as anything that would be reported as `x`, so \\
      there is no finite list of values.",
      "i" = "Use {.val exact}, the default, for the samples of `n` binary \\
      values."
    )
  }
  # `"continuous"` is an error; a missing `data_type` (`NULL` here) is an error
  # in `debit_scalar()`, after the checks for the arguments before it:
  if (!is.null(data_type)) {
    check_data_type_whole(data_type)
  }
  sd_meaning
}


debit_closest_scalar <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  sd_meaning <- check_debit_values_args(
    sd_meaning,
    formula,
    if (!missing(data_type)) data_type
  )
  consistency <- debit_scalar(
    x = x,
    sd = sd,
    n = n,
    digits_x = digits_x,
    digits_sd = digits_sd,
    data_type = data_type,
    sd_meaning = sd_meaning,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # A row of `NA` but for `n` if a value is missing, as in `grimmer_closest()`:
  # the closest sample depends on it, even where the verdict doesn't:
  is_known <- !anyNA(list(x, sd, n, digits_x, digits_sd, data_type))
  if (is.na(consistency) || !is_known) {
    return(debit_values_tibble(NA_real_, n, sd_meaning))
  }

  n <- as.numeric(n)

  # The candidates: if consistent, the samples that `debit_values()` lists;
  # else those whose mean would be reported as `x`; if there are none, the
  # one whose mean is closest to `x`, as `grim_closest()` finds it:
  ones <- if (consistency) {
    debit_values_scalar(
      x = x,
      sd = sd,
      n = n,
      digits_x = digits_x,
      digits_sd = digits_sd,
      data_type = data_type,
      sd_meaning = sd_meaning,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )$sum
  } else {
    range <- roundwork::sum_range(
      x,
      n,
      digits_x,
      rounding,
      threshold,
      symmetric
    )
    range <- c(max(range[[1L]], 0), min(range[[2L]], n))
    if (range[[1L]] <= range[[2L]]) {
      check_values_size(
        range[[2L]] - range[[1L]] + 1,
        "samples",
        list(x = x, sd = sd, n = n)
      )
      seq(range[[1L]], range[[2L]])
    } else {
      grim_closest_scalar(
        x = x,
        n = n,
        digits_x = digits_x,
        items_averaged = 1,
        data_type = "integer",
        rounding = rounding,
        threshold = threshold,
        symmetric = symmetric
      )$sum
    }
  }

  # The SD of each, from the spread `k * (n - k)`, so that `k` and `n - k` tie
  # exactly. Distances equal up to floating-point error tie as well: two SDs
  # can only be equally close if both are rational, and `sqrt()` rarely
  # computes those exactly.
  sd_ones <- sqrt(ones * (n - ones) * sd_denominator_ratio(n, sd_meaning)) / n
  distance <- abs(sd_ones - sd)
  ones <- ones[distance <= min(distance) * (1 + 1e-12)]

  # Then the mean closer to `x`, compared as whole numbers in units of `x`'s
  # last decimal place, then the smaller number of ones:
  if (length(ones) > 1L) {
    distance_mean <- abs(ones * 10^digits_x - round(x * 10^digits_x) * n)
    ones <- ones[distance_mean == min(distance_mean)]
  }
  debit_values_tibble(min(ones), n, sd_meaning)
}


# The mean and SD of `n` binary values with `ones` of them 1. One row of `NA`
# but for `n` for an undecidable value set, so that it still has a row in the
# output.
debit_values_tibble <- function(ones, n, sd_meaning) {
  n <- as.numeric(n)
  share <- ones / n
  tibble::tibble(
    x = share,
    sd = sqrt(share * (1 - share) * sd_denominator_ratio(n, sd_meaning)),
    n = n,
    sum = ones
  )
}


#' Achievable means and SDs of binary data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   `debit_values()` returns every pair of a mean and an SD that `n` binary
#'   values (0 and 1) can have and that would be reported as `x` and `sd`. It
#'   is the counterpart of [`grim_values()`] and [`grimmer_values()`] for
#'   [`debit()`], which is `TRUE` exactly if there is at least one such pair.
#'
#'   `debit_closest()` returns the single achievable pair whose SD is closest
#'   to `sd`, like [`grimmer_closest()`]. If `x` and `sd` are consistent, it
#'   is one of the pairs that `debit_values()` returns. Otherwise, it is
#'   closest among the pairs whose mean would be reported as `x`, or, if there
#'   are none, it is the pair whose mean [`grim_closest()`] returns.
#'
#'   With `k` ones, the mean is `k / n`, and `k` is returned as `sum`, the sum
#'   of the values.
#'
#' @details The values are those of actual samples, so they follow `debit()`'s
#'   default `formula = "exact"`.
#'
#'   More than a million numbers of ones whose mean would be reported as `x`
#'   is an error, since they are listed before the SD narrows them down. That
#'   takes an `n` far beyond any study's.
#'
#'   In `debit_closest()`, pairs whose SDs are equally close go by the mean
#'   closer to `x`, then by the smaller number of ones.
#'
#' @inheritParams debit
#' @param data_type String. `"integer"` if the values are 0 and 1, as in
#'   [`debit()`]. There is no default, so that every call states this
#'   assumption. `"continuous"` is an error: such data can have any value that
#'   would be reported as the one given, so there are none to list. Under
#'   `NA`, a value set is undecidable unless no data can have it.
#' @param formula String. As in [`debit()`], but only `"exact"`, the default,
#'   is allowed: `"mean_n"` takes the mean as anything that would be reported
#'   as `x`, so there are no values to list.
#' @param sd_meaning String. As in [`debit()`], but `"sd_sample_or_pop"` is
#'   not allowed: the same data have a different SD under each premise, so
#'   call `debit_values()` once for each. Under `"se"`, the `sd` column holds
#'   the standard error.
#'
#' @return A tibble with one row per achievable pair. `case` is the index of
#'   the value set, as in [`grim_values()`]. `x` and `sd` are the achievable
#'   values, `n` the reported sample size, and `sum` the number of ones. An
#'   inconsistent value set has no rows, and one that cannot be decided has
#'   one row of `NA` but for `n`, as in [`debit()`].
#'
#'   `debit_closest()` returns the same columns with exactly one row per value
#'   set: of `NA` but for `n` if a value is missing or the value set cannot be
#'   decided.
#'
#' @seealso [`debit()`], [`debit_map()`], [`debit_plot_line()`], and
#'   [`sd_binary_groups()`].
#'
#' @include debit.R
#'
#' @export
#'
#' @examples
#' # A mean of 0.35 and an SD of 0.49 are consistent with 20 binary values,
#' # 7 of them ones:
#' debit_values(
#'   x = 0.35,
#'   sd = 0.49,
#'   n = 20,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   data_type = "integer"
#' )
#'
#' # None if the value set is inconsistent:
#' debit_values(
#'   x = 0.35,
#'   sd = 0.41,
#'   n = 20,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   data_type = "integer"
#' )
#'
#' # With few decimal places and a large sample, there can be several:
#' debit_values(
#'   x = 0.5,
#'   sd = 0.5,
#'   n = 300,
#'   digits_x = 1,
#'   digits_sd = 1,
#'   data_type = "integer"
#' )
#'
#' # The closest pair to an inconsistent one:
#' debit_closest(
#'   x = 0.35,
#'   sd = 0.41,
#'   n = 20,
#'   digits_x = 2,
#'   digits_sd = 2,
#'   data_type = "integer"
#' )

# The signature mirrors `debit_values_scalar()`'s; see `map_value_sets()`.
debit_values <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  map_value_sets(
    fn = debit_values_scalar,
    along = c("x", "sd", "n", "digits_x", "digits_sd", "data_type"),
    type = "list"
  ) |>
    bind_cases(PTYPE_DEBIT_VALUES)
}

#' @rdname debit_values
#' @export
debit_closest <- function(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  map_value_sets(
    fn = debit_closest_scalar,
    along = c("x", "sd", "n", "digits_x", "digits_sd", "data_type"),
    type = "list"
  ) |>
    bind_cases(PTYPE_DEBIT_VALUES)
}
