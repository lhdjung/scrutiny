

#' Vary hypothetical group sizes
#'
#' @description Some published studies only report a total sample size but no
#'   group sizes. However, group sizes are crucial for consistency tests such as
#'   GRIM. Call `disperse()` to generate possible group sizes that all add up to
#'   the total sample size, if that total is even.
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
#' @param n_min Numeric. Minimal group size. Default is `1L`.
#' @param n_max Numeric. Maximal group size. Default is `NULL`, i.e., no
#'   maximum.
#' @param constant Optionally, add a length-2 vector or a list of length-2
#'   vectors (such as a data frame with exactly two rows) to accompany the pairs
#'   of dispersed values. Default is `NULL`, i.e., no constant values.
#' @param constant_index Integer (length 1). Index of `constant` or the first
#'   `constant` column in the output tibble. If `NULL` (the default), `constant`
#'   will go to the right of `n_change`.
#'
#' @details If any group size is less than `n_min` or greater than `n_max`, it
#'   is removed. The complementary size of the other group is also removed.
#'
#'   `constant` values are pairwise repeated. That is why `constant` must be
#'   a length-2 atomic vector or a list of such vectors. If `constant` is a data
#'   frame or some other named list, the resulting columns will have the same
#'   names as the list-element names. If the list is not named, the new column
#'   names will be `"constant1"`, `"constant2"`, etc; or just `"constant"`, for
#'   a single pair.

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

#' @references Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It
#'   Light or Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://journals.sagepub.com/doi/10.1177/09567976211058727

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
#' # You may add values that repeat along with the
#' # dispersed ones but remain constant themselves.
#' # Such values can be stored in a length-2 vector
#' # for a single column...
#' disperse_total(37, constant = c("5.24", "3.80"))
#'
#' # ... or a list of length-2 vectors for multiple
#' # columns. This includes data frames with 2 rows:
#' df_constant <- tibble::tibble(
#'   name = c("Paul", "Mathilda"), age = 27:28,
#'   registered = c(TRUE, FALSE)
#' )
#' disperse_total(37, constant = df_constant)


# Basic function for halves of even totals --------------------------------

disperse <- function(n, dispersion = 0:5, n_min = 1L, n_max = NULL,
                     constant = NULL, constant_index = NULL) {

  # Checks ---

  msg_single <- "It must have length 1."
  check_length_disperse_n(n, msg_single)
  check_non_negative(dispersion)


  # Main part ---

  # (Note: The checks below count towards to the main part because they may lead
  # to input transformations.)

  if (!is.null(n_min)) {
    if (length(n_min) > 1L) {
      cli::cli_abort(c(
        "!" = "`n_min` must have length 1 or to be `NULL`.",
        "x" = "It has length {length(n_min)}."
      ))
    }
    dispersion <- dispersion[(n - dispersion) >= n_min]
  }

  if (!is.null(n_max)) {
    if (length(n_max) > 1L) {
      cli::cli_abort(c(
        "!" = "`n_max` must have length 1 or to be `NULL`.",
        "x" = "It has length {length(n_max)}."
      ))
    }
    dispersion <- dispersion[(n + dispersion) <= n_max]
  }

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

  if (!is.null(constant)) {
    repeat_constant <- function(constant, out, list_input = FALSE) {
      if (list_input) {
        constant_list_element <- constant
        check_length_or_null(constant_list_element, 2L)
      } else {
        check_length_or_null(constant, 2L)
      }
      rep(constant, times = nrow(out) / 2)
    }

    if (is.list(constant)) {
      constant <- purrr::map(constant, repeat_constant, out, list_input = TRUE)
      constant_is_named_list <- !is.null(names(constant)) &&
        length(names(constant)) == length(constant)
      if (constant_is_named_list) {
        constant <- tibble::as_tibble(
          constant, .name_repair = ~ names(constant)
        )
      } else {
        constant <- tibble::as_tibble(
          constant, .name_repair = ~ paste0("constant", seq_along(constant))
        )
      }
    } else {
      constant <- repeat_constant(constant, out)
    }

    if (is.null(constant_index)) {
      constant_index <- match("n_change", colnames(out)) + 1L
    }

    out <- dplyr::mutate(out, constant, .before = constant_index)
  }

  return(out)
}



# Variant for halves of odd totals ----------------------------------------

#' @rdname disperse
#' @export

disperse2 <- function(n, dispersion = 0:5, n_min = 1L, n_max = NULL,
                      constant = NULL, constant_index = NULL) {

  # Checks ---

  check_length(n, 2L)

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
    n = mean(n), constant = constant, constant_index = constant_index,
    dispersion = dispersion,
    n_min = n_min, n_max = n_max
  )

  # Determine which row numbers in the output tibble have an `n` that must
  # be increased or decreased (using an internal helper function from utils.R):
  seq_rows <- seq_len(nrow(out))
  locations1 <- seq_rows %>% parcel_nth_elements(n = 2, from = 1L)
  locations2 <- seq_rows %>% parcel_nth_elements(n = 2, from = 2L)

  # Increase or decrease the dispersed values so that the lower values decrease
  # from the first of the two `n` values, the higher values increase from the
  # second one, and both interleaved sequences proceed by increments of 1:
  out$n <- out$n %>% purrr::modify_at(locations1, `-`, 0.5)
  out$n <- out$n %>% purrr::modify_at(locations2, `+`, 0.5)

  # Return the resulting tibble:
  return(out)
}



# Variant for totals; with even / odd splitting ---------------------------

#' @rdname disperse
#' @export

disperse_total <- function(n, dispersion = 0:5, n_min = 1L, n_max = NULL,
                           constant = NULL, constant_index = NULL) {

  # Checks ---

  msg_single <- "It must have length 1; `n` is supposed \\
    to be a *single*, total sample size."
  check_length_disperse_n(n, msg_single)


  # Main part ---

  n_half <- n / 2

  # Test if `n` is even, then call the appropriate function. If `n` is even,
  # call `disperse()`; if `n` is odd, call `disperse2()`:
  if (is_even(n)) {

    out <- disperse(
      n = n_half, dispersion = dispersion, n_min = n_min, n_max = n_max,
      constant = constant, constant_index = constant_index
    )

    return(out)

  } else {

    # Determine the two whole numbers closest to half of the odd `n`:
    n1 <- n_half - 0.5
    n2 <- n1 + 1
    n_both <- c(n1, n2)

    out <- disperse2(
      n = n_both, dispersion = dispersion, n_min = n_min, n_max = n_max,
      constant = constant, constant_index = constant_index
    )

    return(out)
  }
}


