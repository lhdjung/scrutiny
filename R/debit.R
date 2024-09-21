
# Single-case helper function, not exported but used within both `debit()` and
# `debit_map()`:
debit_scalar <- function(x, sd, n,
                         formula = "mean_n", rounding = "up_or_down",
                         threshold = 5, symmetric = FALSE) {

  check_type(x,  "character")
  check_type(sd, "character")
  check_debit_inputs_all(x, sd)

  out <- debit_table(
    x = x, sd = sd, n = n,
    formula = formula, rounding = rounding,
    threshold = threshold, symmetric = symmetric
  )

  return(out$consistency)
}



#' The DEBIT (descriptive binary) test
#'
#' @description `debit()` tests summaries of binary data for consistency: If the
#'   mean and the sample standard deviation of binary data are given, are they
#'   consistent with the reported sample size?
#'
#'   The function is vectorized, but it is recommended to use [`debit_map()`]
#'   for testing multiple cases.
#'
#' @param x String. Mean of a binary distribution.
#' @param sd String. Sample standard deviation of a binary distribution.
#' @param n Integer. Total sample size.
#' @param formula String. Formula used to compute the SD of the binary
#'   distribution. Currently, only the default, `"mean_n"`, is supported.
#' @param rounding String. Rounding method or methods to be used for
#'   reconstructing the SD values to which `sd` will be compared. Default is
#'   `"up_or_down"` (from 5). See `vignette("rounding-options")`.
#' @param threshold Integer. If `rounding` is set to `"up_from"`, `"down_from"`,
#'   or `"up_from_or_down_from"`, set `threshold` to the number from which the
#'   reconstructed values should then be rounded up or down. Otherwise
#'   irrelevant. Default is `5`.
#' @param symmetric Logical. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers with `"up"`, `"down"`, `"up_from"`, or `"down_from"`
#'   should mirror that of positive numbers so that their absolute values are
#'   always equal. Default is `FALSE`.

#' @include debit-table.R
#'
#' @export
#'
#' @return Logical. `TRUE` if `x`, `sd`, and `n` are mutually consistent,
#'   `FALSE` if not.
#'
#' @seealso [`debit_map()`] applies `debit()` to any number of cases at once.
#'
#' @references Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A
#'   Simple Consistency Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @examples
#' # Check single cases of binary
#' # summary data:
#' debit(x = "0.36", sd = "0.11", n = 20)


debit <- Vectorize(debit_scalar)

