

# Following Bauer and Francis (2021)


#' Vary hypothetical group sizes
#'
#' @description Some published studies only report a total sample size but no
#'   group sizes. However, group sizes are crucial for procedures such as
#'   \href{https://lhdjung.github.io/scrutiny/articles/grim.html}{GRIM}. Call
#'   `disperse()` to get a bird's-eye view of possible group sizes that all add
#'   up to the total sample size.
#'
#'   For convenience, mean (or other) values corresponding to the hypothetical
#'   sample sizes can be added. The output will then be testable with functions
#'   like `grim_map()`.
#'
#' @param n Numeric. Number from which to go up and down. This might be half the
#'   total sample size if that number is even. You may want to choose a number
#'   around half if it is odd.
#' @param dispersion Numeric. Vector that determines the steps up and down from
#'   `n`. Default is `0:5`.
#' @param n_min Numeric. Minimal group size. Default is `1`.
#' @param n_max Numeric. Maximal group size. Default is `NULL`, i.e., no
#'   maximum.
#' @param x_low,x_high Optionally, combine the low and high hypothetical group
#'   sizes with other values, such as means. Default is `NA`.
#'
#' @details If any group size is less than `n_min` or greater than `n_max`, it
#'   is removed. The corresponding size of the other group is also removed.
#'
#'   If you follow up on `disperse()` with `grim_map()`, that will return an
#'   extra column, `both_consistent`. This column marks scenarios (row pairs) in
#'   which both group sizes are GRIM-consistent with their respective `x` and
#'   `items` values.
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
#' out <- disperse(n = 20, x_low = "4.71", x_high = "5.3")
#' out
#'
#' # With `grim_map()`, the extra column
#' # `both_consistent` shows where both
#' # group sizes fit the other values:
#' grim_map(out)

disperse <- function(n, dispersion = 0:5, n_min = 1, n_max = NULL,
                     x_low = NA, x_high = NA) {

  # Checks ---

  if (length(n) > 1) {
    cli::cli_abort(c(
      "`n` has length {length(n)}",
      "x" = "It needs to have length 1."
    ))
  }

  if (!is_whole_number(n)) {
    cli::cli_warn(c(
      "`n` is `{n}` -- not a whole number",
      "!" = "It is meant to be a (hypothetical) group size."
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


  if (is.na(x_low) & is.na(x_high)) {

    out %>%
      dplyr::relocate(n, n_change) %>%
      add_class("scr_disperse")

  } else {

    x <- c(x_low, x_high)
    x_rep_count <- nrow(out) / 2

    out %>%
      dplyr::mutate(x = rep(x, x_rep_count)) %>%
      dplyr::relocate(x, n, n_change) %>%
      add_class("scr_disperse")
  }

}

