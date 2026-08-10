#' Apply DEBIT to many cases
#'
#' @description Call `debit_map()` to use DEBIT on multiple combinations of
#'   mean, sample standard deviation, and sample size of binary distributions.
#'   Mapping function for [`debit()`].
#'
#'   For summary statistics, call [`audit()`] on the results.
#'
#' @param data Data frame.
#' @param digits_x Integer. The number of decimal places in `x`, including
#'   trailing zeros. There is no default because it cannot be inferred from a
#'   numeric `x`, which has no trailing zeros: both `1.4` and `1.40` are the
#'   number `1.4`, but only the latter has `digits_x = 2`. Use a single number
#'   if the whole column was reported with the same number of decimal places,
#'   or one number per row of `data` if it varies.
#' @param digits_sd Integer. The number of decimal places in `sd`, including
#'   trailing zeros. As with `digits_x`, there is no default, because trailing
#'   zeros don't survive in a numeric value, and it may have one value per row
#'   of `data`.
#' @param x,sd,n Optionally, specify these arguments as column names in `data`.
#' @param rounding,threshold,symmetric Arguments passed on to [`debit()`], with
#'   the same defaults.
#' @param show_rec If set to `FALSE`, the resulting tibble only includes the
#'   columns `x`, `sd`, `n`, and `consistency`. Default is `TRUE`.
#' @param extra Not currently used.
#'
#' @importFrom rlang .data
#'
#' @include debit.R

#' @return A tibble with (at least) these columns --
#' - `x`, `sd`, `n`: the inputs.
#' - `digits_x`, `digits_sd`: the number of decimal places in `x` and `sd`, as
#'   given by `digits_x` and `digits_sd`.
#' - `consistency`: DEBIT consistency of `x`, `sd`, and `n`.
#'
#'   By default, the tibble also includes the rounding method, boundary values,
#'   and information about the boundary values being inclusive or not. The
#'   tibble has the `scrutiny_debit_map` class, which is recognized by the `audit()`
#'   generic.
#'
#' @section Summaries with [`audit()`]: There is an S3 method for the
#'   [`audit()`] generic, so you can call [`audit()`] following `debit_map()`.
#'   It returns a tibble with these columns ---
#'
#' 1. `incons_cases`: the number of DEBIT-inconsistent cases.
#' 2. `all_cases`: the total number of cases.
#' 3. `incons_rate`: the rate of inconsistent cases.
#' 4. `mean_x`: the mean `x` (mean) value.
#' 5. `mean_sd`: the mean `sd` value.
#' 6. `distinct_n`: the number of distinct `n` values.

#' @references Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A
#'   Simple Consistency Test For Binary Data. https://osf.io/5vb3u/.
#'
#' @export
#'
#' @examples
#' # Call `debit_map()` on binary summary
#' # data such as these:
#' pigs3
#'
#' # The `consistency` column shows
#' # whether the values to its left
#' # are DEBIT-consistent:
#' pigs3 |>
#'   debit_map(digits_x = 2, digits_sd = 2)
#'
#' # Get test summaries with `audit()`:
#' pigs3 |>
#'   debit_map(digits_x = 2, digits_sd = 2) |>
#'   audit()

debit_map <- function(
  data,
  digits_x,
  digits_sd,
  x = NULL,
  sd = NULL,
  n = NULL,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  show_rec = TRUE,
  extra = Inf
) {
  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(rounding, threshold, symmetric))

  if (!missing(x)) {
    x <- rlang::enexpr(x)
    data <- manage_key_colnames(data, x, "mean/proportion")
  }

  if (!missing(sd)) {
    sd <- rlang::enexpr(sd)
    data <- manage_key_colnames(data, sd, "standard deviation")
  }

  if (!missing(n)) {
    n <- rlang::enexpr(n)
    data <- manage_key_colnames(data, n, "sample size")
  }

  # Check the column names of `data`:
  check_mapper_input_colnames(data, c("x", "sd", "n"), "DEBIT")
  check_tibble(data)

  # Turn `x` and `sd` into the `data` columns by those names to make them more
  # easy to work with:
  x <- data$x
  sd <- data$sd

  if (missing(digits_x)) {
    error_digits_missing(x)
  }

  if (missing(digits_sd)) {
    error_digits_missing(sd)
  }

  # Check whether x and sd range from 0 to 1:
  check_debit_inputs_all(x, sd)

  # Create `other_cols`, which contains any and all extra columns from `data`
  # (i.e., those which play no role in DEBIT):
  if (ncol(data) > 3L) {
    other_cols <- data |>
      dplyr::select(-x, -sd, -n)
  } else {
    other_cols <- NULL
  }

  # Run checks and isolate the desired extra columns, as specified by the
  # `extra` argument (default is `Inf`, i.e., all extra columns):
  extra_cols <- manage_extra_cols(data, extra, other_cols)

  # Prepare input vectors for the resulting tibble:
  sd <- data$sd
  x <- data$x
  n <- data$n

  # Compute the DEBIT results and construct the resulting tibble:
  # The `digits_*` values ride along as columns so that `purrr::pmap_dfr()`
  # hands each row its own: a single number applies to the whole column, but
  # the decimal places may also vary from row to row.
  data_sd_x_n <- dplyr::select(data, sd, x, n)
  data_sd_x_n$digits_x <- recycle_digits(digits_x, nrow(data), "digits_x")
  data_sd_x_n$digits_sd <- recycle_digits(digits_sd, nrow(data), "digits_sd")

  results <- data_sd_x_n |>
    purrr::pmap_dfr(
      debit_table,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )

  # Finally, return the results, with or without the intermediary values
  # (rounding method, boundary values, and Logical information about the
  # boundary values being inclusive or not):
  digits_x_col <- data_sd_x_n$digits_x
  digits_sd_col <- data_sd_x_n$digits_sd

  if (show_rec) {
    out <- results |>
      dplyr::mutate(
        n = n,
        digits_x = digits_x_col,
        digits_sd = digits_sd_col,
        consistency = consistency
      ) |>
      dplyr::select(
        x,
        sd,
        n,
        digits_x,
        digits_sd,
        consistency,
        rounding,
        sd_lower,
        sd_incl_lower,
        sd_upper,
        sd_incl_upper,
        x_lower,
        x_upper
      )
  } else {
    out <- results |>
      dplyr::mutate(
        n = n,
        digits_x = digits_x_col,
        digits_sd = digits_sd_col,
        consistency = consistency
      ) |>
      dplyr::select(x, sd, n, digits_x, digits_sd, consistency)
  }

  if (length(extra_cols) > 0L) {
    out <- dplyr::mutate(out, extra_cols)
  }

  rounding_class <- glue::glue("scrutiny_rounding_{rounding}")
  out <- add_class(out, c("scrutiny_debit_map", rounding_class))

  return(out)
}
