

#' GRIM-test many cases at once
#'
#' @description Call `grim_map()` to GRIM-test any number of combinations of
#'   mean/proportion, sample size, and number of items. Mapping function for
#'   GRIM-testing.
#'
#'   Set `percent` to `TRUE` if the `x` values are percentages. This will
#'   convert `x` values to decimals and adjust the decimal count accordingly.
#'
#'   Display intermediary numbers from GRIM-testing in columns by setting
#'   `show_rec` to `TRUE`.
#'
#'   For summary statistics, call `audit()` on the results.
#'
#' @param data Data frame with columns `x`, `n`, and optionally `items` (see
#'   documentation for `grim()`. By default, any other columns in `data` will be
#'   returned alongside GRIM test results (see `extra` below).
#' @param items Integer. If there is no `items` column in `data`, this specifies
#'   the number of items composing the `x` values. Default is 1, the most common
#'   case.
#' @param percent Boolean. Set `percent` to `TRUE` if the `x` values are
#'   percentages. This will convert them to decimal numbers and adjust the
#'   decimal count (i.e., increase it by 2). It also affects the `ratio` column.
#'   Default is `FALSE`.
#' @param x,n Optionally specify which columns from `data` contain the means
#'   (`x`) and/or sample sizes (`n`). If not specified here, `data` itself needs
#'   to contain columns by those names. Default is `NULL`.
#' @param show_rec Boolean. If set to `TRUE`, the reconstructed numbers from
#'   GRIM-testing are shown as columns. See section *Reconstructed numbers*
#'   below. Default is `FALSE`.
#' @param show_prob Boolean. If set to `TRUE`, adds a `prob` column that
#'   contains the probability of GRIM inconsistency. This is simply the `ratio`
#'   column censored to range between 0 and 1. Default is `FALSE`.
#' @param rounding,threshold,symmetric,tolerance (`threshold` currently defunct!)
#'   Further parameters of GRIM-testing; see documentation for `grim()`.
#' @param testables_only Boolean. If `testables_only` is set to `TRUE`, only
#'   GRIM-testable cases (i.e., those with a positive GRIM ratio) are included.
#'   Default is `FALSE`.
#' @param extra String or integer. The other column(s) from `data` to be
#'   returned in the output tibble alongside test results, referenced by their
#'   name(s) or number(s). Default is `Inf`, which returns all columns. To
#'   return none of them, set `extra` to 0.

#' @return A tibble with these columns --
#' - `x`, `n`, `items`: the inputs.
#' - `consistency`: GRIM consistency of `x`, `n`, and `items`.
#' - `<extra>`: any columns from `data` other than `x`, `n`, and `items`.
#' - `ratio`: the GRIM ratio; see `grim_ratio()`.
#'
#' The tibble has the `scr_grim_map` class, which is recognized by the `audit()`
#' generic.

#' @section Reconstructed numbers: If `show_rec` is set to `TRUE`, the output
#'   includes the following additional columns:
#'
#' - `rec_sum`: the sum total from which the mean or proportion was ostensibly
#'   derived.
#' - `rec_x_upper`: the upper reconstructed `x` value.
#' - `rec_x_lower`: the lower reconstructed `x` value.
#' - `rec_x_upper_rounded`: the rounded `rec_upper` value.
#' - `rec_x_lower_rounded`: the rounded `rec_lower` value.

#' @section Summaries with `audit()`: There is an S3 method for `audit()`, so
#'   you can call `audit()` following `grim_map()` to get a summary of
#'   `grim_map()`'s results. It is a tibble with a single row and these
#'   columns --
#'
#' 1. `incons_cases`: number of GRIM-inconsistent value sets.
#' 2. `all_cases`: total number of value sets.
#' 3. `incons_rate`: proportion of GRIM-inconsistent value sets.
#' 4. `mean_grim_ratio`: average of GRIM ratios.
#' 5. `incons_to_ratio`: ratio of `incons_rate` to `mean_grim_ratio`.
#' 6. `testable_cases`: number of GRIM-testable value sets (i.e., those with a
#' positive ratio).
#' 7. `testable_rate`: proportion of GRIM-testable value sets.
#'
#' @include audit.R grim.R manage-extra-cols.R restore-zeros.R split-transform.R
#'
#' @export
#'
#' @examples
#' # Use `grim_map()` on data like these:
#' pigs1
#'
#' # The `consistency` column shows
#' # whether the values to its left
#' # are GRIM-consistent:
#' pigs1 %>%
#'   grim_map()
#'
#' # Display intermediary numbers from
#' # GRIM-testing with `show_rec = TRUE`:
#' pigs1 %>%
#'   grim_map(show_rec = TRUE)
#'
#' # Specifically, values are consistent
#' # if and only if `x` is near-identical
#' # to either of `rec_x_upper_rounded`
#' # and `rec_x_lower_rounded`:
#' pigs1 %>%
#'   grim_map(show_rec = TRUE) %>%
#'   dplyr::select(x, consistency,
#'                 rec_x_upper_rounded,
#'                 rec_x_lower_rounded)
#'
#' # Get summaries with `audit()`:
#' pigs1 %>%
#'   grim_map() %>%
#'   audit()



# Note: All the arguments passed on to the internal testing function
# `grim_scalar()` are listed here as well as in the internal call to
# `purrr::pmap_lgl(grim)` or `purrr::pmap(grim)` instead of simply being passed
# via `...` so that starting to type them will trigger RStudio's autocomplete.

grim_map <- function(data, items = 1, percent = FALSE, x = NULL, n = NULL,
                     show_rec = FALSE, show_prob = FALSE,
                     rounding = "up_or_down", threshold = 5,
                     symmetric = FALSE, tolerance = .Machine$double.eps^0.5,
                     testables_only = FALSE, extra = Inf) {

  check_lengths_congruent(list(items, rounding, threshold, symmetric))

  # Throw error if `items` is specified in a way that contradicts `data`:
  if ("items" %in% colnames(data) && !items == 1) {
    cli::cli_abort(c(
      "`items` already in `data`",
      "x" = "Specifying the `items` argument in `grim_map()` conflicts with \\
      the `items` column in `data`."
    ))
  }

  # Defuse the argument specifications that can be used to assign the roles of
  # `x` and `n` to specific columns in case these columns don't already have
  # those names:
  x <- rlang::enexpr(x)
  n <- rlang::enexpr(n)

  # Provide a way to specify the mean (`x`) column from within a function call
  # even if the column in question is not named `x`:
  if (!is.null(x)) {
    data <- dplyr::mutate(data, x = {{ x }})
  } else if (!"x" %in% colnames(data)) {
    cli::cli_abort(c(
      "`x` column missing",
      ">" = "The mean/proportion column in `data` needs to be named `x`, \\
      or else specify the `x` argument as the name of that column."
    ))
  }

  # Same with the sample size (`n`) column:
  if (!is.null(n)) {
    data <- dplyr::mutate(data, n = {{ n }})
  } else if (!"n" %in% colnames(data)) {
    cli::cli_abort(c(
      "`n` column missing",
      ">" = "The sample size column in `data` needs to be named `n`, \\
      or else specify the `n` argument as the name of that column."
    ))
  }

  # Convert `n` to integer (mainly because of the `split_by_parens()` issue,
  # which would leave `n` as a string vector):
  data$n <- as.integer(data$n)

  # If an `items` column is not yet present in `data`, supply it from the
  # `items` argument:
  if (!"items" %in% colnames(data)) {
    data$items <- as.integer(items)
  }

  # Create `other_cols`, which contains all extra columns from `data` (i.e.,
  # those which play no role in the GRIM test), and run it through a specified
  # helper function:
  other_cols <- dplyr::select(data, -x, -n, -items) %>%
    check_extra_cols(data, extra, .)

  # Prepare a data frame for the GRIM computations below (steps 4 and 5):
  data_x_n_items <- dplyr::select(data, x, n, items)


  # Create the columns of the resulting tibble --

  # 1.-3.: Define `x`, `n`, and `items` as the respective columns from `data`
  # (these only come into play in the resulting tibble):
  x <- data$x
  n <- data$n
  items <- data$items

  # 4.: GRIM-test all sets of `x`, `n`, and `items` by mapping `grim_scalar()`.
  # Instead of using the dots, `...`, the function manually passes the remaining
  # arguments down to `grim_scalar()` so that starting to type these arguments
  # will trigger RStudio's autocomplete. The particular mapping function,
  # `purrr::pmap_lgl()` or `purrr::pmap()`, depends on whether intermediary
  # numbers were chosen to be shown in the resulting tibble because the former
  # only returns a logical value whereas the latter returns a list:
  if (show_rec == FALSE) {
    consistency <- purrr::pmap_lgl(
      data_x_n_items, grim_scalar, percent = percent,
      show_rec = show_rec, rounding = rounding,
      threshold = threshold, symmetric = symmetric,
      tolerance = tolerance
    )
  } else {
    consistency <- purrr::pmap(
        data_x_n_items, grim_scalar, percent = percent,
        show_rec = show_rec, rounding = rounding,
        threshold = threshold, symmetric = symmetric,
        tolerance = tolerance
      )
  }

  # 5.: Compute the GRIM ratios for all of the same value sets via
  # `grim_ratio()`, which also gets the `percent` argument passed on to:
  ratio <- purrr::pmap_dbl(data_x_n_items, grim_ratio, percent = percent)

  # 6.-?: Any number of other columns from `data` (via the `other_cols` object).


  # Create a tibble with results that also includes extra columns from the input
  # data frame (`other_cols`) unless the `extra` argument has been set to 0 --
  if (is.null(extra)) {
    # (Number:)               1  2    3         4         5
    results <- tibble::tibble(x, n, items, consistency, ratio)
  } else {
    # (Number:)               1  2    3         4         5       6(-?)
    results <- tibble::tibble(x, n, items, consistency, ratio, other_cols)
  }

  # In case the user had set `show_rec` to `TRUE` for displaying the
  # reconstructed values from `grim_scalar()`'s internal computations, these
  # were stored in `consistency` until now. The `consistency` column, then, is a
  # list-column of 6 values per cell. These numbers are now unnested (i.e.,
  # transformed into their own columns) and given their respective proper names:
  if (show_rec) {
    results <- results %>%
      tidyr::unnest_wider(col = consistency) %>%
      suppressMessages() %>%
      dplyr::rename(
        consistency          = .data$`...1`,
        rec_sum              = .data$`...2`,
        rec_x_upper          = .data$`...3`,
        rec_x_lower          = .data$`...4`,
        rec_x_upper_rounded  = .data$`...5`,
        rec_x_lower_rounded  = .data$`...6`
      )
  }

  # If demanded via the `show_prob` argument, add a column that displays the
  # probability of GRIM inconsistency. This is simply the GRIM ratio
  # left-censored at zero, i.e., with negative values set to zero:
  if (show_prob) {
    results <- results %>%
      tibble::add_column(
        prob = dplyr::if_else(ratio < 0, 0, ratio), .after = "ratio"
      )
  }

  # Add the "scr_grim_map" class that `audit()` will take as a shibboleth:
  results <- results %>%
    add_class(c("scr_grim_map", glue::glue("scr_rounding_{rounding}")))

  # Mediate between `seq_endpoint_df()` or `seq_distance_df()`, on the one hand,
  # and `seq_test_ranking()`, on the other:
  if (inherits(data, "scr_seq_df")) {
    results <- results %>%
      add_class("scr_seq_test")
  }

  # If `x` is a percentage, divide it by 100 to adjust its value. The resulting
  # decimal numbers will then be the values actually used in GRIM-testing; i.e.,
  # within `grim_scalar()`. Also, issue an alert to the user about the
  # percentage conversion:
  if (percent) {
    results$x <- restore_zeros(as.numeric(results$x) / 100) %>%
      suppressWarnings()
    results <- results %>%
      add_class("scr_percent_true")

    cli::cli_alert_info(
      "`x` converted from percentage"
    )
  }

  # Finally, return either all of the results or only the GRIM-testable ones:
  if (testables_only) {
    dplyr::filter(results, ratio > 0)
  } else {
    results
  }

}

