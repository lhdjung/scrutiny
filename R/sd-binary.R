#' Standard deviation of binary data
#'
#' @description Compute the sample SD of binary data (0 and 1 values) from
#'   different inputs:
#'
#' - `sd_binary_groups()` from the counts of 0s and 1s.
#' - `sd_binary_0_n()` from the count of 0s and the total.
#' - `sd_binary_1_n()` from the count of 1s and the total.
#' - `sd_binary_mean_n()` from the mean and the total.
#'
#' These are the SDs that [`debit()`] tests under its default
#' `sd_meaning = "sd_sample"`.
#'
#' @param group_0 Integer. Count of 0s.
#' @param group_1 Integer. Count of 1s.
#' @param n Integer. Total sample size.
#' @param mean Numeric. Mean of the data.
#'
#' @return Numeric. Sample standard deviation; `NA` where `n` is below 2,
#'   since the sample SD divides by `n - 1`.
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
#' sd_binary_mean_n(mean = 0.5464286, n = 280)

# From both groups --------------------------------------------------------

#' @rdname sd-binary

sd_binary_groups <- function(group_0, group_1) {
  check_binary_count(group_0)
  check_binary_count(group_1)
  n <- group_0 + group_1
  sd_binary_sample(n, group_0 * group_1 / n^2)
}


# From subgroup coded as 0 and sample size --------------------------------

#' @rdname sd-binary
#' @export

sd_binary_0_n <- function(group_0, n) {
  check_binary_count(group_0, n)
  group_1 <- n - group_0
  sd_binary_sample(n, group_0 * group_1 / n^2)
}


# From subgroup coded as 1 and sample size --------------------------------

#' @rdname sd-binary
#' @export

sd_binary_1_n <- function(group_1, n) {
  check_binary_count(group_1, n)
  group_0 <- n - group_1
  sd_binary_sample(n, group_0 * group_1 / n^2)
}


# From mean and sample size -----------------------------------------------

#' @rdname sd-binary
#' @export

sd_binary_mean_n <- function(mean, n) {
  if (any(mean < 0 | mean > 1, na.rm = TRUE)) {
    abort_in_export(
      "`mean` must be between 0 and 1.",
      "x" = "It includes {wrap_in_quotes_or_backticks(mean[which(mean < 0 | mean > 1)])}."
    )
  }
  sd_binary_sample(n, mean * (1 - mean))
}


# The sample SD from the population variance. With `n` below 2 there is no
# sample SD; the formula would give `Inf` or `NaN`, so the warning is
# suppressed and the result set to `NA`:
sd_binary_sample <- function(n, variance) {
  out <- suppressWarnings(sqrt((n / (n - 1)) * variance))
  # Assigned rather than multiplied in: `NaN * NA` may be either, by platform.
  # `rep_len()` recycles `n` along `variance`, as the formula does:
  out[which(rep_len(n, length(out)) < 2)] <- NA_real_
  out
}


# A count of 0s or 1s can't be negative, nor exceed the sample size. The
# formulas would silently give `NaN`:
check_binary_count <- function(group, n = NULL) {
  name <- deparse(substitute(group))
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
