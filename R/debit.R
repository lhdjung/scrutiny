

#' The DEBIT (descriptive binary) test
#'
#' `debit()` tests summaries of binary data for consistency: If the mean and the
#' standard deviation of binary data are given, are they consistent with the
#' reported sample size?
#'
#' @param x String. Mean of a binary distribution.
#' @param sd String. Sample standard deviation of a binary distribution.
#' @param n Integer. Total sample size.
#' @param formula String. Formula used to compute the SD of the binary
#'   distribution. Currently, only the default, `"mean_n"`, is supported.
#' @param rounding String. Rounding method or methods to be used for
#'   reconstructing the SD values to which `sd` will be compared. Default is
#'   `"up_or_down"` (from 5). For more options, see documentation for `grim()`,
#'   section Details.
#' @param threshold [[Currently defunct!]] Integer. If `rounding` is set to
#'   `"up_from"`, `"down_from"`, or `"up_from_or_down_from"`, set `threshold` to
#'   the number from which the reconstructed values should then be rounded up or
#'   down. Otherwise irrelevant. Default is `NULL`.
#' @param symmetric Boolean. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers with `"up"`, `"down"`, `"up_from"`, or `"down_from"`
#'   should mirror that of positive numbers so that their absolute values are
#'   always equal. Default is `FALSE`.

#' @include debit-table.R
#'
#' @export
#'
#' @return Boolean. `TRUE` if `x`, `sd`, and `n` are mutually consistent,
#'   `FALSE` if not.
#'
#' @seealso `debit_map()` applies `debit()` to any number of cases at once.

# ADD @examples WHEN FUNCTION DONE


debit <- function(x, sd, n,
                  formula = "mean_n", rounding = "up_or_down",
                  threshold = NULL, symmetric = FALSE) {

  check_debit_inputs(x, sd)

  out <- debit_table(
    x = x, sd = sd, n = n,
    formula = formula, rounding = rounding,
    threshold = threshold, symmetric = symmetric
  )

  out$consistency
}



