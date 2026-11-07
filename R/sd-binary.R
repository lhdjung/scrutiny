#' Standard deviation of binary data
#'
#' @description Compute the SD of binary data (0 and 1 values) from
#'   different inputs:
#'
#' - `sd_binary_groups()` from the counts of 0s and 1s.
#' - `sd_binary_0_n()` from the count of 0s and the total.
#' - `sd_binary_1_n()` from the count of 1s and the total.
#' - `sd_binary_mean_n()` from the mean and the total.
#'
#' These are the SDs that [`debit()`] tests under the same `sd_meaning`.
#'
#' @param group_0 Integer. Count of 0s.
#' @param group_1 Integer. Count of 1s.
#' @param n Integer. Total sample size.
#' @param x Numeric. Mean of the data, i.e., the share of 1s.
#' @param mean `r lifecycle::badge("deprecated")` The old name of `x`.
#' @param sd_meaning String. What to compute: the sample SD, `"sd_sample"`
#'   (the default); the population SD, `"sd_pop"`; or the standard error of
#'   the mean, `"se"`, as in [`debit()`]. `"sd_sample_or_pop"` is an error
#'   here, since it stands for two premises with different values.
#'
#' @return Numeric. The SD or standard error; `NA` where `n` or a count is not
#'   a whole number, and where `n` is below 2 under `"sd_sample"` and
#'   `"se"`, which divide by `n - 1`.
#'
#' @export
#'
#' @name sd-binary
#'
#' @references Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A
#'   Simple Consistency Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @examples
#' # 127 values are 0 and 153 are 1, so n = 280:
#' sd_binary_groups(group_0 = 127, group_1 = 153)
#' sd_binary_0_n(group_0 = 127, n = 280)
#' sd_binary_1_n(group_1 = 153, n = 280)
#'
#' # From the mean, given enough decimal places:
#' sd_binary_mean_n(x = 0.5464286, n = 280)
#'
#' # The population SD and the standard error:
#' sd_binary_groups(group_0 = 127, group_1 = 153, sd_meaning = "sd_pop")
#' sd_binary_groups(group_0 = 127, group_1 = 153, sd_meaning = "se")

# From both groups --------------------------------------------------------

#' @rdname sd-binary

sd_binary_groups <- function(
  group_0,
  group_1,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
) {
  sd_meaning <- check_sd_meaning_single(sd_meaning)
  check_binary_args(list(group_0 = group_0, group_1 = group_1))
  # As doubles, since the product of two integer counts overflows from 46,341.
  # Below, each count is divided by `n` first, as `n^2` overflows from 1e154:
  group_0 <- round_whole(as.numeric(group_0))
  group_1 <- round_whole(as.numeric(group_1))
  check_binary_count(group_0)
  check_binary_count(group_1)
  n <- group_0 + group_1
  sd_binary_spread(
    n,
    (group_0 / n) * (group_1 / n),
    sd_meaning,
    list(group_0, group_1)
  )
}


# From subgroup coded as 0 and sample size --------------------------------

#' @rdname sd-binary
#' @export

sd_binary_0_n <- function(
  group_0,
  n,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
) {
  sd_meaning <- check_sd_meaning_single(sd_meaning)
  check_binary_args(list(group_0 = group_0, n = n))
  group_0 <- round_whole(as.numeric(group_0))
  n <- round_whole(n)
  check_binary_count(group_0, n)
  group_1 <- n - group_0
  sd_binary_spread(n, (group_0 / n) * (group_1 / n), sd_meaning, list(group_0))
}


# From subgroup coded as 1 and sample size --------------------------------

#' @rdname sd-binary
#' @export

sd_binary_1_n <- function(
  group_1,
  n,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
) {
  sd_meaning <- check_sd_meaning_single(sd_meaning)
  check_binary_args(list(group_1 = group_1, n = n))
  group_1 <- round_whole(as.numeric(group_1))
  n <- round_whole(n)
  check_binary_count(group_1, n)
  group_0 <- n - group_1
  sd_binary_spread(n, (group_0 / n) * (group_1 / n), sd_meaning, list(group_1))
}


# From mean and sample size -----------------------------------------------

#' @rdname sd-binary
#' @export

sd_binary_mean_n <- function(
  x,
  n,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  mean = deprecated()
) {
  x <- use_renamed_arg(mean, x, "sd_binary_mean_n", "mean", "x")
  sd_meaning <- check_sd_meaning_single(sd_meaning)
  check_binary_args(list(x = x, n = n))
  n <- round_whole(n)
  if (any(x < 0 | x > 1, na.rm = TRUE)) {
    abort_in_export(
      "`x` must be between 0 and 1.",
      "x" = "It includes {wrap_in_quotes_or_backticks(x[which(x < 0 | x > 1)])}."
    )
  }
  sd_binary_spread(n, x * (1 - x), sd_meaning)
}


# The value under `sd_meaning` from the population variance. With `n` below
# `sd_min_n()`, there is none; the formula would give `Inf` or `NaN`, so the
# warning is suppressed and the result set to `NA`. So is a fractional or
# infinite `n` or count, which describes no data set, as elsewhere in scrutiny,
# and any `NaN`:
sd_binary_spread <- function(n, variance, sd_meaning, counts = list()) {
  # Two roots rather than the root of the product, which underflows to 0 for
  # an SE at an `n` of about 1e300:
  out <- suppressWarnings(
    sqrt(sd_denominator_ratio(n, sd_meaning)) * sqrt(variance)
  )
  # Assigned rather than multiplied in: `NaN * NA` may be either, by platform.
  # `rep_len()` recycles `n` along `variance`, as the formula does:
  length_out <- length(out)
  undefined <- rep_len(n, length_out) < sd_min_n(sd_meaning)
  for (count in c(list(n), counts)) {
    undefined <- undefined | !is_whole_number(rep_len(count, length_out))
  }
  out[is.na(out) | !(undefined %in% FALSE)] <- NA_real_
  out
}


# Numbers, or logical `NA`s, of lengths that recycle as in `debit()`. A
# character `NA` would reach the arithmetic as a string:
check_binary_args <- function(args) {
  for (name in names(args)) {
    x <- args[[name]]
    if (!is.numeric(x) && !(is.logical(x) && all(is.na(x)))) {
      abort_in_export(
        "`{name}` must be one of these types: double and integer.",
        "x" = "It is {an_a_type(x)}."
      )
    }
  }
  recycle_length_common(lengths(args), names(args))
  invisible()
}


# A count of 0s or 1s can't be negative, nor exceed the sample size. The
# formulas would silently give `NaN`:
check_binary_count <- function(group, n = NULL) {
  name <- deparse(substitute(group))
  if (any(n < 0, na.rm = TRUE)) {
    abort_in_export(
      "`n` must not be negative.",
      "x" = "It includes {wrap_in_quotes_or_backticks(n[which(n < 0)])}."
    )
  }
  wrong <- group < 0 | (if (!is.null(n)) group > n else FALSE)
  if (any(wrong, na.rm = TRUE)) {
    abort_in_export(
      if (is.null(n)) {
        "`{name}` must not be negative."
      } else {
        "`{name}` must be from 0 to `n`."
      },
      "x" = "It includes {wrap_in_quotes_or_backticks(group[which(wrong)])}."
    )
  }
}
