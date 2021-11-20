

#' Apply DEBIT to many cases
#'
#' @description Call `debit_map()` to use DEBIT on multiple combinations of
#'   mean, standard deviation, and sample size of binary distributions. Mapping
#'   function for `debit()`.
#'
#'   For summary statistics, call `audit()` on the results.
#'
#' @param data Data frame.
#' @param x,sd,n Optionally specify which columns from `data` contain the means
#'   (`x`), standard deviations (`sd`), and/or sample sizes (`n`). If not
#'   specified here, `data` itself needs to contain columns by those names.
#'   Default is `NULL`.
#' @param rounding,threshold,symmetric Arguments passed on to `debit()`, with
#'   the same defaults.
#' @param show_rec If set to `FALSE`, the resulting tibble only includes
#'   the columns `x`, `sd`, `n`, and `consistency`. Default is `TRUE`.
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
#'
#' @export
#'
#' @examples
#' # Call `debit_map()` on binary summary
#' # data such as these:
#' # pigs3
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
                      # group_0 = NULL, group_1 = NULL,
                      rounding = "up_or_down", threshold = NULL,
                      symmetric = FALSE, show_rec = TRUE, extra = Inf) {

  # Throw error if `extra` argument it misspecified:
  if (!extra %in% colnames(data) && is.character(extra)) {
    cli::cli_abort(
      "At least one `extra` column name was supplied that is not part of \\
      `data`."
    )
  }

  # Defuse the argument specifications that can be used to assign the roles of
  # `x`, `sd`, and `n` to specific columns in case these columns don't already
  # have those names:
  x  <- rlang::enexpr(x)
  sd <- rlang::enexpr(sd)
  n  <- rlang::enexpr(n)
  # group_0 <- rlang::enexpr(group_0)
  # group_1 <- rlang::enexpr(group_1)

  # Provide a way to specify the mean (`x`) column from within a function call
  # even if the column in question is not named `x`:
  if (!is.null(x)) {
    data <- dplyr::mutate(data, x = {{ x }})
  }

  # Same with the `sd` column...
  if (!is.null(sd)) {
    data <- dplyr::mutate(data, sd = {{ sd }})
  }

  # ... and with the sample size (`n`) column:
  if (!is.null(n)) {
    data <- dplyr::mutate(data, n = {{ n }})
  }

  # Use an internal helper function to check whether the reported means and
  # standard deviations range from 0 to 1:
  check_debit_inputs(data$x, data$sd)

  # In case the input data frame resulted from splitting strings by parentheses
  # using `split_by_parens()`, put it into proper shape:
  if (inherits(data, "scr_split_by_parens")) {
    data <- transform_split_parens_object(data)
  }

  # Create `other_cols`, which contains any and all extra columns from `data`
  # (i.e., those which play no role in DEBIT):
  if (ncol(data) > 3) {
    other_cols <- data %>%
      dplyr::select(-sd, -n, -x)
  }

  # Throw error if the `extra` argument is specified as numeric, but if that
  # number is larger than the actual number of extra columns:
  if (!is.infinite(extra) && is.numeric(extra) && extra > length(other_cols)) {
    cli::cli_abort(c(
      "The number supplied for `extra` columns is too large -- there aren't \\
      as many extra columns in `data`."
    ))
  }

  # Make `other_cols` capture any and all extra columns:
  if (!is.infinite(extra) && length(other_cols) > 0) {
    other_cols <- other_cols %>%
      dplyr::select(tidyselect::all_of(extra))
  }

  # Prepare input vectors for the resulting tibble:
  sd <- sd_chr <- data$sd
  x <- x_chr <- data$x
  n <- data$n

  # Compute the DEBIT results and construct the resulting tibble:
  results <- data %>%
    dplyr::select(sd, x, n) %>%
    purrr::pmap_dfr(debit_table,
                    # group_0 = group_0, group_1 = group_1,
                    rounding = rounding, threshold = threshold,
                    symmetric = symmetric) %>%
    add_class("scr_debit_map")

  # Mediate between `seq_endpoint_df()` or `seq_distance_df()`, on the one hand,
  # and `seq_test_ranking()`, on the other:
  if (inherits(data, "seq_df")) {
    # class(results) <- c("seq_test", class(results))
    results <- results %>%
      add_class("seq_test")
  }

  # Finally, return the results, with or without the intermediary values
  # (rounding method, boundary values, and Boolean information about the
  # boundary values being inclusive or not):
  if (show_rec) {
    results %>%
      dplyr::mutate(x = x, n = n, consistency = .data$consistency) %>%
      dplyr::select(x, sd, n, .data$consistency, rounding,
                    .data$sd_lower, .data$sd_incl_lower, .data$sd_upper,
                    .data$sd_incl_upper, .data$x_lower, .data$x_upper)
  } else {
    results %>%
      dplyr::mutate(sd = sd, x = x, n = n, consistency = .data$consistency) %>%
      dplyr::select(sd, x, n, .data$consistency)
  }

}

