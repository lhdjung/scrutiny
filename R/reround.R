
# # Full example inputs:
# x <- 65.3488492
# digits <- 2
# rounding <- "up_or_down"
# threshold <- 5
# symmetric <- FALSE

reconstruct_rounded_numbers_scalar <- function(x, digits, rounding,
                                               threshold, symmetric) {
  switch(
    rounding,
    "up_or_down" = c(
      round_up(x, digits, symmetric),
      round_down(x, digits, symmetric)
    ),
    # Throw error if `rounding` was set to `"up_from_or_down_from"` -- which
    # requires `threshold` to be set to some number -- but `threshold` was not,
    # in fact, specified as anything other than its default, `5`:
    "up_from_or_down_from" = {
      check_threshold_specified(threshold)
      c(
        round_up_from(x, digits, threshold, symmetric),
        round_down_from(x, digits, threshold, symmetric)
      )
    },
    "ceiling_or_floor" = c(
      round_ceiling(x, digits),
      round_floor(x, digits)
    ),
    "even" = round(x, digits),
    "up" = round_up(x, digits, symmetric),
    "down" = round_down(x, digits, symmetric),
    # The next two are checked like `"up_from_or_down_from"` above:
    "up_from" = {
      check_threshold_specified(threshold)
      round_up_from(x, digits, threshold, symmetric)
    },
    "down_from" = {
      check_threshold_specified(threshold)
      round_down_from(x, digits, threshold, symmetric)
    },
    "ceiling" = round_ceiling(x, digits),
    "floor" = round_floor(x, digits),
    "trunc" = round_trunc(x, digits),
    "anti_trunc" = round_anti_trunc(x, digits),
    cli::cli_abort(c(
      "`rounding` must be one of the designated string values.",
      "x" = "It was given as {wrong_spec_string(rounding)}.",
      "i" = "See `vignette(\"rounding-options\")`."
    ))
  )
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
#'   as well as [`base::round()`]. It is used as a helper within [`grim()`],
#'   [`grimmer()`], and [`debit()`]; and it might find use in other places for
#'   consistency testing or reconstruction of statistical analyses.
#'

#' @details `reround()` internally calls the appropriate rounding function(s)
#'   determined by the `rounding` argument. See `vignette("rounding-options")`
#'   for a complete list of values that `rounding` can take.
#'
#'   For the specific rounding functions themselves, see documentation at
#'   [`round_up()`], [`round_ceiling()`], and [`base::round()`].
#'
#' @param x Numeric. Vector of possibly original values.
#' @param digits Integer. Number of decimal places in the reported key values
#'   (i.e., mean or percentage within [`grim()`], or standard deviation within
#'   [`grimmer()`]).
#' @param rounding String. The rounding method that is supposed to have been
#'   used originally. See `vignette("rounding-options")`. Default is
#'   `"up_or_down"`, which returns two values: `x` rounded up *and* down.
#' @param threshold Integer. If `rounding` is set to `"up_from"`, `"down_from"`,
#'   or `"up_from_or_down_from"`, `threshold` must be set to the number from
#'   which the reconstructed values should then be rounded up or down. Otherwise
#'   irrelevant. Default is `5`.
#' @param symmetric Logical. Set `symmetric` to `TRUE` if the rounding of
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


reround <- function(x, digits = 0L, rounding = "up_or_down",
                    threshold = 5, symmetric = FALSE) {

  # For calls with multiple rounding procedures, each individual procedure needs
  # to be singular; i.e., `rounding` can either be (1) a string vector of length
  # 1 indicating two procedures, such as `"up_or_down"`; or (2) a string vector
  # of any length with values such as `"up"` or `"even"`, but not
  # `"up_or_down"`:
  if (length(rounding) > 1L) {
    check_rounding_singular(rounding, "up_or_down", "up", "down")
    check_rounding_singular(rounding, "up_from_or_down_from", "up_from", "down_from")
    check_rounding_singular(rounding, "ceiling_or_floor", "ceiling", "floor")
    # Throw an error if the lengths of the first two arguments are inconsistent:
    if (length(x) > 1L && length(x) != length(rounding)) {
      cli::cli_abort(c(
        "!" = "`x` and `rounding` must have the same length \\
      unless either has length 1.",
      "i" = "`x` has length {length(x)}.",
      "i" = "`rounding` has length {length(rounding)}."
      ))
    }
  }

  # Go through the rounding options and, once the correct option (as per
  # `rounding`) has been found, proceed as described in the `Details` section of
  # the documentation. To vectorize the arguments, this is done via the helper
  # function at the top of the present file. Finally, attributes are removed.
  # This is because the helper returns a matrix structure.
  `attributes<-`(
    reconstruct_rounded_numbers(x, digits, rounding, threshold, symmetric),
    NULL
  )

}

