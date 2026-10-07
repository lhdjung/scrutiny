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
#' [`debit()`] uses these internally.
#'
#' @param group_0 Integer. Count of 0s.
#' @param group_1 Integer. Count of 1s.
#' @param n Integer. Total sample size.
#' @param mean Numeric. Mean of the data.
#'
#' @return Numeric. Sample standard deviation.
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
  n <- group_0 + group_1
  suppressWarnings(sqrt((n / (n - 1)) * ((group_0 * group_1) / (n^2))))
}


# From subgroup coded as 0 and sample size --------------------------------

#' @rdname sd-binary
#' @export

sd_binary_0_n <- function(group_0, n) {
  group_1 <- n - group_0
  suppressWarnings(sqrt((n / (n - 1)) * ((group_0 * group_1) / (n^2))))
}


# From subgroup coded as 1 and sample size --------------------------------

#' @rdname sd-binary
#' @export

sd_binary_1_n <- function(group_1, n) {
  group_0 <- n - group_1
  suppressWarnings(sqrt((n / (n - 1)) * ((group_0 * group_1) / (n^2))))
}


# From mean and sample size -----------------------------------------------

#' @rdname sd-binary
#' @export

sd_binary_mean_n <- function(mean, n) {
  suppressWarnings(sqrt((n / (n - 1)) * (mean * (1 - mean))))
}
