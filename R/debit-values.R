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
  # `"continuous"` is an error; a missing `data_type` is an error in
  # `debit_scalar()`, after the checks for the arguments before it:
  if (!missing(data_type)) {
    check_data_type_whole(data_type)
  }

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
