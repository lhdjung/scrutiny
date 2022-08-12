

#' Apply DEBIT to many cases
#'
#' @description Call `debit_map()` to use DEBIT on multiple combinations of
#'   mean, standard deviation, and sample size of binary distributions. Mapping
#'   function for `debit()`.
#'
#'   For summary statistics, call `audit()` on the results.
#'
#' @param data Data frame.
#' @param x,sd,n Optionally, specify these arguments as column names in `data`.
#' @param rounding,threshold,symmetric Arguments passed on to `debit()`, with
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
#' - `consistency`: DEBIT consistency of `x`, `sd`, and `n`.
#'
#'  By default, the tibble also includes the rounding method, boundary values,
#'  and Boolean information about the boundary values being inclusive or not.
#'  The tibble has the `scr_debit_map` class, which is recognized by the
#'  `audit()` generic.
#'
#' @section Summaries with `audit()`: There is an S3 method for the `audit()`
#'   generic, so you can call `audit()` following `debit_map()`. It returns a
#'   tibble with these columns ---
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
#' pigs3 %>%
#'   debit_map()
#'
#' # Get test summaries with `audit()`:
#' pigs3 %>%
#'   debit_map() %>%
#'   audit()


debit_map <- function(data, x = NULL, sd = NULL, n = NULL,
                      rounding = "up_or_down", threshold = 5,
                      symmetric = FALSE, show_rec = TRUE, extra = Inf) {

  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(rounding, threshold, symmetric))

  # Defuse the argument specifications that can be used to assign the roles of
  # `x`, `sd`, and `n` to specific columns in case these columns don't already
  # have those names:
  x  <- rlang::enexpr(x)
  sd <- rlang::enexpr(sd)
  n  <- rlang::enexpr(n)

  data <- manage_key_colnames(data, x,  "binary mean")
  data <- manage_key_colnames(data, sd, "binary SD")
  data <- manage_key_colnames(data, n,  "sample size")

  # Check the column names of `data`:
  check_mapper_input_colnames(data, c("x", "sd", "n"), "DEBIT")

  x_spec  <- x
  sd_spec <- sd
  n_spec  <- n

  # Provide a way to specify the mean (`x`) column from within a function call
  # even if the column in question is not named `x`:
  if (!is.null(x)) {
    x_orig <- x
    data <- dplyr::mutate(data, x = {{ x }})
  }

  # Same with the `sd` column...
  if (!is.null(sd)) {
    sd_orig <- sd  # rlang::expr_text(sd)
    data <- dplyr::mutate(data, sd = {{ sd }})
  }

  # ... and with the sample size (`n`) column:
  if (!is.null(n)) {
    n_orig <- n  # rlang::expr_text(n)
    data <- dplyr::mutate(data, n = {{ n }})
  }

  # Turn `x` and `sd` into the `data` columns by those names to make them more
  # easy to work with:
  x  <- data$x
  sd <- data$sd

  # With the reported means and standard deviations (`x` and `sd`) now being
  # columns in `data` (if they weren't before), some checks are in order. These
  # use internal helper functions from the utils.R file. First, since trailing
  # zeros matter for DEBIT, make sure both vectors are strings...
  if (!is.null(x))  check_type(x,  "character")
  if (!is.null(sd)) check_type(sd, "character")

  # ...and second, check whether they range from 0 to 1:
  check_debit_inputs_all(x, sd)

  # Create `other_cols`, which contains any and all extra columns from `data`
  # (i.e., those which play no role in DEBIT):
  if (ncol(data) > 3) {
    other_cols <- data %>%
      dplyr::select(-x, -sd, -n)
  } else {
    other_cols <- NULL
  }

  # Run checks and isolate the desired extra columns, as specified by the
  # `extra` argument (default is `Inf`, i.e., all extra columns):
  extra_cols <- manage_extra_cols(data, extra, other_cols)

  # Prepare input vectors for the resulting tibble:
  sd <- sd_chr <- data$sd
  x <- x_chr <- data$x
  n <- data$n

  # Compute the DEBIT results and construct the resulting tibble:
  results <- data %>%
    dplyr::select(sd, x, n) %>%
    purrr::pmap_dfr(
      debit_table,
      rounding = rounding, threshold = threshold,
      symmetric = symmetric
    )

  # Finally, return the results, with or without the intermediary values
  # (rounding method, boundary values, and Boolean information about the
  # boundary values being inclusive or not):
  if (show_rec) {
    out <- results %>%
      dplyr::mutate(
        x = x, n = n, consistency = .data$consistency
      ) %>%
      dplyr::select(
        x, sd, n, .data$consistency, rounding,
        .data$sd_lower, .data$sd_incl_lower, .data$sd_upper,
        .data$sd_incl_upper, .data$x_lower, .data$x_upper
      )
  } else {
    out <- results %>%
      dplyr::mutate(
        sd = sd, x = x, n = n, consistency = .data$consistency
      ) %>%
      dplyr::select(x, sd, n, .data$consistency)
  }

  if (length(extra_cols) > 0) out <- dplyr::mutate(out, extra_cols)

  if (!is.null(x_spec))  out <- dplyr::select(out, -all_of(x_orig))
  if (!is.null(sd_spec)) out <- dplyr::select(out, -all_of(sd_orig))
  if (!is.null(n_spec))  out <- dplyr::select(out, -all_of(n_orig))

  rounding_class <- glue::glue("scr_rounding_{rounding}")
  out <- add_class(out, c("scr_debit_map", rounding_class))

  return(out)
}

