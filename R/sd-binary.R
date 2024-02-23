
#' Standard deviation of binary data
#'
#' @description Compute the sample SD of binary data (i.e., only 0 and 1 values)
#'   in either of four ways, each based on different inputs:

#' - `sd_binary_groups()` takes the cell sizes of both groups, those coded
#' as 0 and those coded as 1.
#' - `sd_binary_0_n()` takes the cell size of the group coded as 0 and the total
#' sample size.
#' - `sd_binary_1_n()` takes the cell size of the group coded as 1 and the total
#' sample size.
#' - `sd_binary_mean_n()` takes the mean and the total sample size.
#'
#' These functions are used as helpers inside [`debit()`], and consequently
#' [`debit_map()`].

#' @param group_0 Integer. Cell size of the group coded as 0.
#' @param group_1 Integer. Cell size of the group coded as 1.
#' @param n Integer. Total sample size.
#' @param mean Numeric. Mean of the binary data.
#'
#' @return Numeric. Sample standard deviation.
#'
#' @seealso `is_subset_of_vals(x, 0, 1)` checks whether `x` (a list or atomic
#'   vector) contains nothing but binary numbers.
#'
#' @export
#'
#' @name sd-binary
#'
#' @references Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A
#'   Simple Consistency Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @examples
#' # If 127 values are coded as 0 and 153 as 1...
#' sd_binary_groups(group_0 = 127, group_1 = 153)
#'
#' # ...so that n = 280:
#' sd_binary_0_n(group_0 = 127, n = 280)
#' sd_binary_1_n(group_1 = 153, n = 280)
#'
#' # If only the mean and total sample size are
#' # given, or these are more convenient to use,
#' # they still lead to the same result as above
#' # if the mean is given with a sufficiently
#' # large number of decimal places:
#' sd_binary_mean_n(mean = 0.5464286, n = 280)



# From both groups --------------------------------------------------------

#' @rdname sd-binary

sd_binary_groups <- function(group_0, group_1) {
  n <- group_0 + group_1
  suppressWarnings(sqrt((n / (n - 1)) * ((group_0 * group_1) / (n ^ 2))))
}



# From subgroup coded as 0 and sample size --------------------------------

#' @rdname sd-binary
#' @export

sd_binary_0_n <- function(group_0, n) {
  group_1 <- n - group_0
  suppressWarnings(sqrt((n / (n - 1)) * ((group_0 * group_1) / (n ^ 2))))
}



# From subgroup coded as 1 and sample size --------------------------------

#' @rdname sd-binary
#' @export

sd_binary_1_n <- function(group_1, n) {
  group_0 <- n - group_1
  suppressWarnings(sqrt((n / (n - 1)) * ((group_0 * group_1) / (n ^ 2))))
}



# From mean and sample size -----------------------------------------------

#' @rdname sd-binary
#' @export

sd_binary_mean_n <- function(mean, n) {
  suppressWarnings(sqrt((n / (n - 1)) * (mean * (1 - mean))))
}

