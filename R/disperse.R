

#' Vary hypothetical group sizes
#'
#' @description Some published studies only report a total sample size but no
#'   group sizes. However, group sizes are crucial for tests such as
#'   \href{https://lhdjung.github.io/scrutiny/articles/grim.html#handling-unknown-group-sizes-with-grim_map_disperse}{GRIM}.
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
#'   of `grim_map_disperse()`.
#'

#' @param n Numeric.
#' - In `disperse()`, single number from which to go up and down. This should be
#'   half of an even total sample size.
#' - In `disperse2()`, the two consecutive numbers closest to half of an odd
#'   total sample size (e.g., `c(25, 26)` for a total of 51).
#' - In `disperse_total()`, the total sample size.

#' @param dispersion Numeric. Vector that determines the steps up and down from
#'   `n` (in `disperse_total()`, from half `n`). Default is `0:5`.
#' @param n_min Numeric. Minimal group size. Default is `1`.
#' @param n_max Numeric. Maximal group size. Default is `NULL`, i.e., no
#'   maximum.
#' @param x1,x2 Optionally, combine the low and high hypothetical group sizes
#'   with other values, such as means. Default is `NA`.
#'
#' @details If any group size is less than `n_min` or greater than `n_max`, it
#'   is removed. The corresponding size of the other group is also removed.
#'
#'   In case you specify `x1` and `x2`, it makes sense to swap their
#'   specifications and run `disperse()` again, because it is not known which
#'   value corresponds to which group. `grim_map_disperse()` does that
#'   automatically.

#' @return A tibble (data frame) with these columns:
#' - `n` includes the dispersed `n` values. Every pair of consecutive rows has
#'   `n` values that each add up to the input `n`.
#' - `n_change` records how the input `n` was transformed to the output `n`. In
#'   `disperse2()`, the `n_change` strings label the lower of the input `n`
#'   values `n1` and the higher one `n2`.

#' @seealso `grim_map_disperse()`, `seq_distance_df()`
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
#' # you may set `n` to `20`:
#' disperse(n = 20)
#'
#' # Specify `dispersion` to control
#' # the steps up and down from `n`:
#' disperse(n = 20, dispersion = c(3, 6, 10))
#'
#' # Specify `x1` and `x2` to add
#' # corresponding values, such as means:
#' disperse(n = 20, x1 = "4.71", x2 = "5.3")



# Basic function for even totals ------------------------------------------

disperse <- function(n, dispersion = 0:5, n_min = 1, n_max = NULL,
                     x1 = NA, x2 = NA) {

  # Checks ---

  check_length(n, "n", 1)

  if (any(dispersion < 0)) {
    offenders <- dispersion[dispersion < 0]
    if (length(offenders) > 3) {
      offenders <- offenders[1:3]
      msg_among_others <- ", among others"
    } else {
      msg_among_others <- ""
    }
    cli::cli_abort(c(
      "`dispersion` given as {offenders}{msg_among_others}",
      "x" = "It can't be negative."
    ))
  }


  # Main part ---

  if (!is.null(n_min)) {
    if (length(n_min) > 1) {
      cli::cli_abort(c(
        "`n_min` has length {length(n_min)}",
        "x" = "It needs to have length 1 or to be `NULL`."
      ))
    }
    dispersion <- dispersion[(n - dispersion) >= n_min]
  }

  if (!is.null(n_max)) {
    if (length(n_max) > 1) {
      cli::cli_abort(c(
        "`n_max` has length {length(n_max)}",
        "x" = "It needs to have length 1 or to be `NULL`."
      ))
    }
    dispersion <- dispersion[(n + dispersion) <= n_max]
  }

  n_minus <- n - dispersion
  n_plus  <- n + dispersion

  out <- tibble::tibble(n_minus, n_plus) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "n_change",
      values_to = "n"
    ) %>%
    dplyr::mutate(
      n_change = paste0(n_change, "_", rep(dispersion, each = 2))
    )


  if (is.na(x1) & is.na(x2)) {

    return(
      out %>%
        dplyr::relocate(n, n_change) %>%
        add_class("scr_disperse")
    )

  } else {

    x <- c(x1, x2)
    x_rep_count <- nrow(out) / 2

    return(
      out %>%
        dplyr::mutate(x = rep(x, x_rep_count)) %>%
        dplyr::relocate(x, n, n_change) %>%
        add_class("scr_disperse")
    )

  }

}



# Variant for odd totals --------------------------------------------------

#' @rdname disperse
#' @export

disperse2 <- function(n, dispersion = 0:5, n_min = 1, n_max = NULL,
                      x1 = NA, x2 = NA) {

  # Checks ---

  check_length(n, "n", 2)

  if (!dplyr::near(n[2] - n[1], 1)) {
    cli::cli_warn(c(
      "`n` was given as `{n[1]}` and `{n[2]}`",
      "!" = "It should be two consecutive numbers.",
      ">" = "(The second value in `n` should be the first plus 1.)"
    ))
  }


  # Main part ---

  # Take the mean of the two `n` values and disperse from there:
  out <- disperse(
    n = mean(n), dispersion = dispersion, n_min = n_min, n_max = n_max,
    x1 = x1, x2 = x2
  )

  # Determine which row numbers in the output tibble have an `n` that needs to
  # be increased or decreased (using an internal helper function from utils.R):
  seq_rows <- 1:nrow(out)
  locations_minus <- seq_rows %>% parcel_nth_elements(n = 2, from = 1)
  locations_plus  <- seq_rows %>% parcel_nth_elements(n = 2, from = 2)

  # Increase or decrease the dispersed values so that the lower values start
  # from the lower of the two `n` values, the higher values from the higher one,
  # and both of the two interleaved sequences proceed by increments of 1:
  out$n <- out$n %>% purrr::modify_at(locations_minus, `-`, 0.5)
  out$n <- out$n %>% purrr::modify_at(locations_plus,  `+`, 0.5)

  # Adjust the `n_change` labels in the output by labeling the lower `n` value
  # `n1` and the higher `n` value `n2`:
  out$n_change <- out$n_change %>%
    stringr::str_replace("n_", paste0("n", c(1, 2), "_"))

  # Return the resulting tibble:
  return(out)
}



# Variant for even / odd splitting ----------------------------------------

#' @rdname disperse
#' @export

disperse_total <- function(n, dispersion = 0:5, n_min = 1, n_max = NULL,
                           x1 = NA, x2 = NA) {

  # Checks ---

  if (!(length(n) %in% c(1, 2))) {
    cli::cli_abort(c(
      "`n` has length {length(n)}",
      "x" = "It needs to have length 1 or 2.",
      ">" = "See documentation under `?disperse`."
    ))
  }


  # Main part ---

  # Test if `n` is even, then call the appropriate function. If `n` is even,
  # call `disperse()`; if `n` is odd, call `disperse2()`:
  if (n %% 2 == 0) {

    return(disperse(
      n = (n / 2), dispersion = dispersion, n_min = n_min, n_max = n_max,
      x1 = x1, x2 = x2
    ))

  } else {

    # Determine the two whole numbers closest to half of the odd `n`:
    n1 <- (n / 2) - 0.5
    n2 <- n1 + 1

    return(disperse2(
      n = c(n1, n2), dispersion = dispersion, n_min = n_min, n_max = n_max,
      x1 = x1, x2 = x2
    ))
  }

}


