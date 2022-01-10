
# Helper function used in the main function `reround()`:
reconstruct_rounded_numbers_scalar <- function(x, digits, rounding,
                                               threshold, symmetric) {

  if (rounding == "even") {
    round(x, digits)
  } else if (rounding == "up_or_down") {
    c(
      round_up(x, digits, symmetric),
      round_down(x, digits, symmetric)
    )
  } else if (rounding == "up_from_or_down_from") {
    c(
      round_up_from(x, digits, threshold, symmetric),
      round_down_from(x, digits, threshold, symmetric)
    )
  } else if (rounding == "ceiling_or_floor") {
    c(
      round_ceiling(x, digits),
      round_floor(x, digits)
    )
  } else if (rounding == "up") {
    round_up(x, digits, symmetric)
  } else if (rounding == "down") {
    round_down(x, digits, symmetric)
  } else if (rounding == "up_from") {
    round_up_from(x, digits, threshold, symmetric)
  } else if (rounding == "down_from") {
    round_down_from(x, digits, threshold, symmetric)
  } else if (rounding == "ceiling") {
    round_ceiling(x, digits)
  } else if (rounding == "floor") {
    round_floor(x, digits)
  } else if (rounding == "trunc") {
    round_trunc(x, digits)
  } else if (rounding == "anti_trunc") {
    round_anti_trunc(x, digits)
  } else {
    # Finally, if the `rounding` argument was not specified as any of the
    # designated rounding options, throw an error:
    cli::cli_abort(c(
      "`rounding` misspecified",
      "x" = "`rounding` was given as {wrong_spec_string(rounding)}.",
      ">" = "Please use one of the designated string values instead. See \\
      documentation for `grim()`, section `Rounding`."
    ))
  }
}


reconstruct_rounded_numbers <- Vectorize(reconstruct_rounded_numbers_scalar,
                                         USE.NAMES = FALSE)



#' General interface to reconstructing rounded numbers
#'
#' @description `reround()` takes one or more intermediate reconstructed values
#'   and rounds them in some specific way -- namely, the way they are supposed
#'   to have been rounded originally, in the process that generated the reported
#'   values.
#'
#'   This function provides an interface to all of scrutiny's rounding functions
#'   as well as `base::round()`. It is used as a helper within `grim()` and
#'   `debit()`, and it might find use in other places for consistency testing or
#'   reconstruction of statistical analyses.
#'

#   `reround()` takes one or more intermediate reconstructed values and rounds
#   them to the same number of decimal places as in the respective reported
#   value. Rounding (e.g., rounding up or down from 5) proceeds in the way
#   that's presumed to have been used in the generation of the reported value.
#   The latter doesn't come into play here, but consistency testing functions
#   go on to compare it to the output of the present function.

#'
#' @details `reround()` internally calls the appropriate rounding function(s)
#'   determined by the `rounding` argument. See documentation for `grim()`,
#'   section `Rounding`, for a complete list of values that `rounding` can take.
#'
#'   For the nine rounding functions themselves, see documentation at
#'   `round_up()`, `round_ceiling()`, and `base::round()`.
#'
#' @param x Numeric. Vector of possibly original values.
#' @param digits Integer. Number of decimal places in the reported key values
#'   (i.e., mean or percentage within `grim()`, or standard deviation within
#'   `debit()`).
#' @param rounding String. The rounding method that is supposed to have been
#'   used originally. See documentation for `grim()`, section `Rounding`.
#'   Default is `"up_or_down"`, which returns two values: `x` rounded up *and*
#'   down.
#' @param threshold (Currently defunct!) Integer. If `rounding` is set to
#'   `"up_from"`, `"down_from"`, or `"up_from_or_down_from"`, `threshold` needs
#'   to be set to the number from which the reconstructed values should then be
#'   rounded up or down. Otherwise irrelevant. Default is `NULL`.
#' @param symmetric Boolean. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers with `"up_or_down"`, `"up"`, `"down"`,
#'   `"up_from_or_down_from"`, `"up_from"`, or `"down_from"` should mirror that
#'   of positive numbers so that their absolute values are always equal.
#'   Otherwise irrelevant. Default is `FALSE`.
#'
#' @include utils.R round.R round-ceil-floor.R
#'
#' @export
#'
#' @return Numeric vector of length 1 or 2. (It has length 1 unless `rounding`
#'   is `"up_or_down"`, `"up_from_or_down_from"`, or`"ceiling_or_floor"`, in
#'   which case it has length 2.)


reround <- function(x, digits = 0, rounding = "up_or_down",
                    threshold = 5, symmetric = FALSE) {

  # Checks --

  # Throw an error if the lengths of the first two arguments are inconsistent:
  if (length(x) > 1 &&
      length(rounding) > 1 &&
      length(x) != length(rounding)) {
    cli::cli_abort(c(
      "Lengths of `x` and `rounding` not congruent",
      "x" = "Both must have the same length unless either has length 1."
    ))
  }

  # Prepare a test for the remaining checks:
  rounding_needs_threshold <- suppressWarnings(any(
    rounding == c("up_from", "down_from", "up_from_or_down_from")
  ))

  # Throw error if `rounding` was set to either "up_from", "down_from", or
  # `"up_from_or_down_from"` -- which require `threshold` to be set to some
  # number -- but `threshold` was not, in fact, set to any number:
  if (rounding_needs_threshold && !is.numeric(threshold)) {
    cli::cli_abort(c(
      "`threshold` not specified",
      ">" = "If `rounding` is set to `\"up_from\"`, `\"down_from\"`, or \\
      `\"up_from_or_down_from\"`, please set `threshold` to the number \\
      from which the reconstructed values should then be rounded up or down, \\
      respectively. (If that number is 5, you can simply set `rounding` to \\
      `\"up\"`, `\"down\"`, or `\"up_or_down\"` instead.)"
    ))
  }

  if (length(rounding) > 1) {
    length_2ers <- c("up_or_down", "up_from_or_down_from", "ceiling_or_floor")
    if (any(length_2ers %in% rounding)) {
      offenders <- length_2ers[length_2ers %in% rounding]
      msg_no_other <- glue::glue("If `rounding` is \"{offenders[1]}\", \\
      there can be no other `rounding` values.")
      if (length(offenders) > 1) {
        offenders[-1] <- paste0("\"", offenders[-1], "\"")
        msg_no_other <- paste(
          msg_no_other, "This also applies to {offenders[-1]}."
        )
      }
      cli::cli_abort(c(
        "\"{offenders[1]}\" in `rounding`, which has length {length(rounding)}",
        "x" = msg_no_other
      ))
    }
  }

  # # Throw error if the reverse of the above case is true -- `threshold` was
  # # specified, but `rounding` was not specified in any one of the ways which
  # # require that argument:
  # if (!is.null(threshold)) {
  #   if (!rounding_needs_threshold) {
  #     cli::cli_warn(c(
  #       "`threshold` unnecessarily specified",
  #       "!" = "The `threshold` argument only needs to be specified if \\
  #     `rounding` is either \"up_from\", \"down_from\", or \\
  #     \"up_from_or_down_from\". Did you mean to use one of these options?"
  #     ))
  #   }
  # } else {
  #   # If `threshold` is `NULL` and thus not needed, it gets assigned a different
  #   # value because its being `NULL` would prevent vectorization of the helper
  #   # function:
  #   threshold <- 0
  # }

  # # Provisorically, at least while the above is outcommented:
  # threshold <- 0

  # Main part ---

  # Go through the rounding options and, once the correct option (as per
  # `rounding`) has been found, proceed as described in the `Details` section of
  # the documentation. To vectorize the arguments, this is done via the helper
  # function at the top of the present file:
  as.vector(reconstruct_rounded_numbers(
    x, digits, rounding, threshold, symmetric
  ))

}





# Possible alternative to the helper `reconstruct_rounded_numbers()` -- but it
# doesn't currently work:


# dplyr::case_when(
#
#   rounding == "up_or_down"  ~ c(
#     round_up(x, digits, symmetric),
#     round_down(x, digits, symmetric)
#   ),
#
#   rounding == "up_from_or_down_from"  ~ c(
#     round_up_from(x, digits, threshold, symmetric),
#     round_down_from(x, digits, threshold, symmetric)
#   ),
#
#   rounding == "ceiling_or_floor"  ~ c(
#     round_ceiling(x, digits),
#     round_floor(x, digits)
#   ),
#
#   rounding == "even"  ~ round(x, digits),
#   rounding == "up"  ~ round_up(x, digits, symmetric),
#   rounding == "down"  ~ round_down(x, digits, symmetric),
#   rounding == "up_from"  ~ round_up_from(x, digits, threshold, symmetric),
#   rounding == "down_from"  ~ round_down_from(x, digits, threshold, symmetric),
#   rounding == "ceiling"  ~ round_ceiling(x, digits),
#   rounding == "floor"  ~ round_floor(x, digits),
#   rounding == "trunc"  ~ round_trunc(x, digits),
#   rounding == "anti_trunc"  ~ round_anti_trunc(x, digits),
#
#   TRUE  ~ cli::cli_abort(c(
#     "`rounding` misspecified",
#     "x" = "`rounding` was given as {wrong_spec_string(rounding)}.",
#     ">" = "Please use one of the designated string values instead. See \\
#       documentation for `grim()`, section `Rounding`."
#   ))
#
# )


