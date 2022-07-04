

#' Vary hypothetical group sizes
#'
#' @description Some published studies only report a total sample size but no
#'   group sizes. However, group sizes are crucial for tests such as
#'   \href{https://lhdjung.github.io/scrutiny/articles/grim.html#handling-unknown-group-sizes-with-grim_map_total_n}{GRIM}.
#'    Call `disperse()` to generate possible group sizes that all add up to the
#'   total sample size, if that total is even.
#'
#'   `disperse2()` is a variant for odd totals. It takes two consecutive numbers
#'   and generates decreasing values from the lower as well as increasing values
#'   from the upper. In this way, all combinations still add up to the total.
#'
#'   `disperse_total()` directly takes the total sample size, checks if it's
#'   even or odd, splits it up accordingly, and applies `disperse()` or
#'   `disperse2()`, respectively.
#'
#'   These functions are primarily intended as helpers. They form the backbone
#'   of `grim_map_total_n()` and all other functions created with
#'   `function_map_total_n()`.

#' @param n Numeric:
#' - In `disperse()`, single number from which to go up and down. This should be
#'   half of an even total sample size.
#' - In `disperse2()`, the two consecutive numbers closest to half of an odd
#'   total sample size (e.g., `c(25, 26)` for a total of 51).
#' - In `disperse_total()`, the total sample size.

#' @param dispersion Numeric. Vector that determines the steps up and down from
#'   `n` (or, in `disperse_total()`, from half `n`). Default is `0:5`.
#' @param n_min Numeric. Minimal group size. Default is `1`.
#' @param n_max Numeric. Maximal group size. Default is `NULL`, i.e., no
#'   maximum.
#' @param reported Optionally, add a length-2 vector or a list of length-2
#'   vectors to accompany the pairs of dispersed values. Default is `NULL`,
#'   i.e., no reported values.
#' @param reported_index Integer (length 1). Index of `reported` in the output
#'   tibble. If `NULL` (the default), `reported` will go to the right of
#'   `n_change`.
#'
#' @details If any group size is less than `n_min` or greater than `n_max`, it
#'   is removed. The complementary size of the other group is also removed.

#' @return A tibble (data frame) with these columns:
#' - `n` includes the dispersed `n` values. Every pair of consecutive rows has
#'   `n` values that each add up to the total.
#' - `n_change` records how the input `n` was transformed to the output `n`. In
#'   `disperse2()`, the `n_change` strings label the lower of the input `n`
#'   values `n1` and the higher one `n2`.

#' @seealso `function_map_total_n()`, `grim_map_total_n()`,
#'   `seq_distance_df()`
#'
#' @include utils.R
#'
#' @export
#'
#' @references Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It
#'   Light or Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://doi.org/10.1177/09567976211058727
#'
#' @examples
#' # For a total sample size of 40,
#' # set `n` to `20`:
#' disperse(n = 20)
#'
#' # Specify `dispersion` to control
#' # the steps up and down from `n`:
#' disperse(n = 20, dispersion = c(3, 6, 10))
#'
#' # In `disperse2()`, specify `n` as two
#' # consecutive numbers -- i.e., group sizes:
#' disperse2(n = c(25, 26))
#'
#' # Use the total sample size directly
#' # with `disperse_total()`. An even total
#' # internally triggers `disperse()`...
#' disperse_total(n = 40)
#'
#' # ...whereas an odd total triggers `disperse2()`:
#' disperse_total(n = 51)
#'
#' # Add "reported" values that repeat along with the
#' # dispersed ones but remain constant themselves.
#' # Such values can be a length-2 vector for a single
#' # column...
#' disperse_total(37, reported = c("5.24", "3.80"))
#'
#' # ... or a list of length-2 vectors for multiple columns:
#' reported_list <- list(c("5.24", "3.80"), 7:8, c(TRUE, FALSE))
#' disperse_total(37, reported = reported_list)


# Basic function for halves of even totals --------------------------------

disperse <- function(n, dispersion = 0:5, n_min = 1, n_max = NULL,
                     reported = NULL, reported_index = NULL) {

  # Checks ---

  msg_single <- "It needs to have length 1."
  check_length_disperse_n(n, msg_single)
  check_non_negative(dispersion)


  # Main part ---

  # (Note: The checks below count towards to the main part because they may lead
  # to input transformations.)

  if (!is.null(n_min)) {
    if (length(n_min) > 1) {
      cli::cli_abort(c(
        "`n_min` has length {length(n_min)}.",
        "x" = "It needs to have length 1 or to be `NULL`."
      ))
    }
    dispersion <- dispersion[(n - dispersion) >= n_min]
  }

  if (!is.null(n_max)) {
    if (length(n_max) > 1) {
      cli::cli_abort(c(
        "`n_max` has length {length(n_max)}.",
        "x" = "It needs to have length 1 or to be `NULL`."
      ))
    }
    dispersion <- dispersion[(n + dispersion) <= n_max]
  }

  dispersion_index <- rep(dispersion, each = 2)

  n_minus <- n - dispersion
  n_plus  <- n + dispersion

  n_orig <- n

  minus_plus_df <- tibble::tibble(n_minus, n_plus)

  out <- minus_plus_df %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "n_change",
      values_to = "n"
    )

  n_change_num <- out$n - n_orig
  out$n_change <- n_change_num

  if (!is_whole_number(n)) {
    digits <- decimal_places_scalar(n)
    out$n_change <- round(out$n_change, digits)
  }

  out <- out %>%
    reverse_column_order() %>%
    add_class("scr_disperse")

  if (!is.null(reported)) {
    manage_reported <- function(reported, out, list_input = FALSE) {
      if (list_input) {
        reported_list_element <- reported
        check_length_or_null(reported_list_element, 2)
      } else {
        check_length_or_null(reported, 2)
      }
      rep(reported, times = nrow(out) / 2)
    }

    if (is.list(reported)) {
      reported <- purrr::map(reported, manage_reported, out, list_input = TRUE)
      reported <- tibble::as_tibble(
        reported, .name_repair = ~ paste0("reported", 1:length(reported))
      )
    } else {
      reported <- manage_reported(reported, out)
    }

    if (is.null(reported_index)) {
      reported_index <- match("n_change", colnames(out)) + 1
    }
    out <- dplyr::mutate(out, reported, .before = reported_index)
  }

  return(out)
}



# Variant for halves of odd totals ----------------------------------------

#' @rdname disperse
#' @export

disperse2 <- function(n, dispersion = 0:5, n_min = 1, n_max = NULL,
                      reported = NULL, reported_index = NULL) {

  # Checks ---

  check_length(n, 2)

  if (!dplyr::near(n[2] - n[1], 1)) {
    cli::cli_warn(c(
      "`n` was given as `{n[1]}` and `{n[2]}`.",
      "!" = "It should be two consecutive numbers.",
      ">" = "(The second value in `n` should be the first plus 1.)"
    ))
  }


  # Main part ---

  # Take the mean of the two `n` values and disperse from there:
  out <- disperse(
    n = mean(n), reported = reported, reported_index = reported_index,
    dispersion = dispersion,
    n_min = n_min, n_max = n_max
  )

  # Determine which row numbers in the output tibble have an `n` that needs to
  # be increased or decreased (using an internal helper function from utils.R):
  seq_rows <- 1:nrow(out)
  locations1 <- seq_rows %>% parcel_nth_elements(n = 2, from = 1)
  locations2 <- seq_rows %>% parcel_nth_elements(n = 2, from = 2)

  # Increase or decrease the dispersed values so that the lower values decrease
  # from the first of the two `n` values, the higher values increase from the
  # second one, and both interleaved sequences proceed by increments of 1:
  out$n <- out$n %>% purrr::modify_at(locations1, `-`, 0.5)
  out$n <- out$n %>% purrr::modify_at(locations2, `+`, 0.5)

  # # Adjust the `n_change` labels in the output by labeling the lower `n` value
  # # `n1` and the higher `n` value `n2`:
  # out$n_change <- out$n_change %>%
  #   stringr::str_replace("n_", paste0("n", c("1", "2"), "_"))

  # Return the resulting tibble:
  return(out)
}



# Variant for totals; with even / odd splitting ---------------------------

#' @rdname disperse
#' @export

disperse_total <- function(n, dispersion = 0:5, n_min = 1, n_max = NULL,
                           reported = NULL, reported_index = NULL) {

  # Checks ---

  msg_single <- "It needs to have length 1; `n` is supposed \\
    to be a *single*, total sample size."
  check_length_disperse_n(n, msg_single)


  # Main part ---

  n_half <- n / 2

  # Test if `n` is even, then call the appropriate function. If `n` is even,
  # call `disperse()`; if `n` is odd, call `disperse2()`:
  if (is_even(n)) {

    return(disperse(
      n = n_half, dispersion = dispersion, n_min = n_min, n_max = n_max,
      reported = reported, reported_index = reported_index
    ))

  } else {

    # Determine the two whole numbers closest to half of the odd `n`:
    n1 <- n_half - 0.5
    n2 <- n1 + 1
    n_both <- c(n1, n2)

    return(disperse2(
      n = n_both, dispersion = dispersion, n_min = n_min, n_max = n_max,
      reported = reported, reported_index = reported_index
    ))
  }
}


