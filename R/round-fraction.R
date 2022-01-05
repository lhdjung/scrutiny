#
#
# # Inspired by `janitor::round_to_fraction()`;
# # https://github.com/sfirke/janitor/blob/main/R/round_to_fraction.R:
# x <- 0.22
# fractions <- 4
# digits <- 2
#
# # frac_factor <- fractions * (10^digits)
#
# x <- x * (10 ^ (digits - 1))
#
# # Here is the core functionality of `janitor::round_to_fraction()`. The question
# # now is: Simply refer to this function, or find a way to generalize it across
# # levels of decimal depth? (`janitor::round_to_fraction()` only applies that
# # fractional logic to integers.)
#
# round(x = (x * fractions), digits = 0) / fractions / (10 ^ (digits - 1))
#
#
#
# # round_fraction <- function(x = NULL, decimals = NULL, rounding = "up",
# #                            threshold = NULL, symmetric = FALSE)
#

reround_fraction <- function(x = NULL, denominator = 1, digits = 0,
                             rounding = "up_or_down", threshold = NULL,
                             symmetric = FALSE) {

  if (length(x) > 1 && length(denominator) > 1) {
    if (length(denominator) != length(x)) {
      cli::cli_abort(c(
        "Lengths of `x` and `denominator` are not congruent",
        "x" = "`x` has length {length(x)} and `denominator` has length \\
        {length(denominator)}.",
        "!" = "Both need to have the same length unless either has length 1."
      ))
    } else {
      cli::cli_warn(c(
        "`x` and `denominator` values get paired",
        "!" = "Are you sure each `x` value should have a different \\
        denominator?",
        ">" = "It might be better if at least one of `x` and `denominator` \\
        has length 1."
      ))
    }
  }

  if (!denominator >= 1) {
    cli::cli_abort(c(
      "`denominator` given as {denominator}",
      "x" = "It needs to be 1 or greater."
    ))
  }

  out <- reround(x = x * denominator, digits = 0, rounding = rounding,
                 threshold = threshold, symmetric = symmetric) / denominator

  reround(out, digits = digits, rounding = rounding, threshold = threshold,
          symmetric = symmetric)
}



