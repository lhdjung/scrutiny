

# Following Bauer and Francis (2021)


#' Lay out hypothetical group sizes
#'
#' @description Some published studies only report a total sample size but no
#'   group sizes. However, group sizes are crucial for procedures such as GRIM.
#'   Call `disperse()` to get a bird's-eye view of possible group sizes that all
#'   add up to the total sample size.
#'
#'   For convenience, mean (or other) values corresponding to the hypothetical
#'   sample sizes can be added.
#'
#' @param n Numeric. Number from which to go up and down. This might be half the
#'   total sample size if that number is even. You may want to choose a number
#'   around half if it is odd.
#' @param dispersion Numeric. Vector that determines the steps up and down from
#'   `n`. Default is `0:5`.
#' @param x_low,x_high Optionally, combine the low and high hypothetical group
#'   sizes with other values, such as means. Default is `NA`.
#' @param zero Boolean. If set to `TRUE`, allows empty groups. Default is
#'   `FALSE`.
#'
#' @details All negative group sizes and their complements are internally
#'   removed. By default (`zero = FALSE`), group sizes of 0 and their
#'   complements are also removed.
#'
#' @return A tibble (data frame).
#'
#' @export
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
#' # Specify `x_low` and `x_high` to add
#' # corresponding values, such as means:
#' disperse(n = 20, x_low = "4.71", x_high = "5.3")


disperse <- function(n, dispersion = 0:5, x_low = NA, x_high = NA,
                     zero = FALSE) {

  # Checks ---

  if (length(n) > 1) {
    cli::cli_abort(c(
      "`n` has length {length(n)}",
      "x" = "It needs to have length 1."
    ))
  }

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

  if (zero) {
    `%down_to%` <- `>=`
  } else {
    `%down_to%` <- `>`
  }

  dispersion <- dispersion[(n - dispersion) %down_to% 0]

  n_minus <- n - dispersion
  n_plus  <- n + dispersion

  out <- tibble(n_minus, n_plus) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "n_change",
      values_to = "n"
    ) %>%
    dplyr::mutate(
      n_change = paste0(n_change, "_", rep(dispersion, each = 2)),
      n = as.integer(n)
    )


  if (is.na(x_low) & is.na(x_high)) {

    out %>%
      dplyr::relocate(n, n_change)

  } else {

    x <- c(x_low, x_high)
    x_rep_count <- nrow(out) / 2

    out %>%
      dplyr::mutate(x = rep(x, x_rep_count)) %>%
      dplyr::relocate(x, n, n_change)
  }

}

