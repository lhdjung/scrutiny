
#' GRIMMER-test many cases at once
#'
#' @description Call `grimmer_map()` to GRIMMER-test any number of combinations
#'   of mean, standard deviation, sample size, and number of items. Mapping
#'   function for GRIMMER-testing.
#'
#'   For summary statistics, call `audit()` on the results. Visualize results
#'   using `grim_plot()`, as with GRIM results.
#'
#' @param data Data frame with columns `x`, `sd`, `n`, and optionally `items`
#'   (see documentation for `grim()`). Any other columns in `data` will be
#'   returned alongside GRIMMER test results.
#' @param items *(NOTE: Don't use the `items` argument. It currently contains a
#'   bug that will be fixed in the future.)* Integer. If there is no `items`
#'   column in `data`, this specifies the number of items composing the `x` and
#'   `sd` values. Default is 1, the most common case.
#' @param merge_items Boolean. If `TRUE` (the default), there will be no `items`
#'   column in the output. Instead, values from an `items` column or argument
#'   will be multiplied with values in the `n` column. This does not affect
#'   GRIM- or GRIMMER-testing.
#' @param x,sd,n Optionally, specify these arguments as column names in `data`.
#' @param show_reason Boolean (length 1). Should there be a `reason` column that
#'   shows the reasons for inconsistencies (and `NA` for consistent values)?
#'   Default is `FALSE`.
#' @param rounding,threshold,symmetric,tolerance Further parameters of
#'   GRIMMER-testing; see documentation for `grimmer()`.

#' @return A tibble with these columns --
#' - `x`, `sd`, `n`: the inputs.
#' - `consistency`: GRIMMER consistency of `x`, `n`, and `items`.
#' - `<extra>`: any columns from `data` other than `x`, `n`, and `items`.
#'
#' The tibble has the `scr_grimmer_map` class, which is recognized by the
#' `audit()` generic. It also has the `scr_grim_map` class, so it can be
#' visualized by `grim_plot()`.

#' @section Summaries with `audit()`: There is an S3 method for `audit()`, so
#'   you can call `audit()` following `grimmer_map()` to get a summary of
#'   `grimmer_map()`'s results. It is a tibble with a single row and these
#'   columns --
#'
#' 1. `incons_cases`: number of GRIMMER-inconsistent value sets.
#' 2. `all_cases`: total number of value sets.
#' 3. `incons_rate`: proportion of GRIMMER-inconsistent value sets.
#' 4. `fail_grim`: number of value sets that fail the GRIM test.
#' 5. `fail_test1`: number of value sets that fail the first GRIMMER test (sum
#'     of squares is a whole number)
#' 6. `fail_test2`: number of value sets that fail the second GRIMMER test
#'     (matching SDs)
#' 7. `fail_test3`: number of value sets that fail the third GRIMMER test (equal
#'     parity)

#' @include audit.R grimmer.R function-map.R
#'
#' @references Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
#'   possibility of standard deviations.
#'   https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/
#'
#'   Anaya, J. (2016). The GRIMMER test: A method for testing the validity of
#'   reported measures of variability. *PeerJ Preprints.*
#'   https://peerj.com/preprints/2400v1/
#'
#' @export
#'
#' @examples
#' # Use `grimmer_map()` on data like these:
#' pigs5
#'
#' # The `consistency` column shows whether
#' # the values to its left are GRIMMER-consistent.
#' # If they aren't, the `reason` column says why:
#' pigs5 %>%
#'   grimmer_map()
#'
#' # Get summaries with `audit()`:
#' pigs5 %>%
#'   grimmer_map() %>%
#'   audit()


grimmer_map <- function(data, items = 1, merge_items = TRUE,
                        x = NULL, sd = NULL, n = NULL,
                        show_reason = TRUE, rounding = "up_or_down",
                        threshold = 5, symmetric = FALSE,
                        tolerance = .Machine$double.eps^0.5) {

  check_mapper_input_colnames(data, c("x", "sd", "n"), "GRIMMER")

  # Defuse the argument specifications that can be used to assign the roles of
  # `x`, `sd`, and `n` to specific columns in case these columns don't already
  # have those names:
  x  <- rlang::enexpr(x)
  sd <- rlang::enexpr(sd)
  n  <- rlang::enexpr(n)

  # Check for non-standard column names and, if present, rename them. If the
  # respective argument was not specified as that column name, throw an error:
  data <- manage_key_colnames(data, x,  "mean/proportion")
  data <- manage_key_colnames(data, sd, "standard deviation")
  data <- manage_key_colnames(data, n,  "sample size")

  check_mapper_input_colnames(data, c("x", "n"), "GRIM")
  check_tibble(data)

  data <- manage_helper_col(data = data, var_arg = items, default = 1)

  data_x_sd_n_items <- data[c("x", "sd", "n", "items")]


  x <- data$x

  if (merge_items) {
    n <- data$n * data$items
  } else {
    n <- tibble::tibble(n = data$n, items = data$items)
  }

  if (show_reason) {
    consistency <- purrr::pmap(
      data_x_sd_n_items, grimmer_scalar, show_reason = show_reason,
      rounding = rounding, threshold = threshold, symmetric = symmetric,
      tolerance = tolerance
    )
  } else {
    consistency <- purrr::pmap_lgl(
      data_x_sd_n_items, grimmer_scalar, show_reason = show_reason,
      rounding = rounding, threshold = threshold, symmetric = symmetric,
      tolerance = tolerance
    )
  }

  out <- tibble::tibble(x = data$x, sd = data$sd, n, consistency) %>%
    add_class(c("scr_grimmer_map", paste0("scr_rounding_", rounding)))

  if (show_reason) {
    unnest_consistency_cols(
      out, col_names = c("consistency", "reason"), index = FALSE
    )
  } else {
    out
  }

}



# # Alternative version of `grimmer_map()`, using experimental functionality
# from `function_map()`:
#
# # The `.name_class = "scr_grim_map"` specification has the purpose of allowing
# # GRIMMER results to be visualized by `grim_plot()`:
# grimmer_map_alt <- function_map(
#   .fun = grimmer_scalar,
#   .reported = c("x", "sd", "n"),
#   .name_test = "GRIMMER",
#   .col_names = "reason",
#   .col_filler = "Passed all",
#   .arg_list = list(
#     show_reason = TRUE, rounding = "up_or_down", threshold = 5,
#     symmetric = FALSE, tolerance = .Machine$double.eps^0.5
#   )
# )



