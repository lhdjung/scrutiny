

# Helper to check input ranges (not exported) -----------------------------

check_debit_inputs <- function(input, type, symbol) {

  # For all input values, check if they are between 0 and 1:
  input_in_range <- input %>%
    as.numeric() %>%
    dplyr::between(0, 1)

  # If at least one of the values is outside of that range, this will lead to an
  # error. First, the error message is prepared...
  offenders <- input[!input_in_range]

  if (length(offenders) > 0) {

    if (length(offenders) == 1) {
      msg_is_are <- "is"
    } else {
      msg_is_are <- "are"
    }

    if (length(offenders) > 3) {
      offenders_all <- offenders
      offenders <- offenders[1:3]
      msg_offenders <- ", starting with"
    } else {
      msg_offenders <- ":"
    }

    # ...and second, the actual error is thrown:
    cli::cli_abort(c(
      "DEBIT only works with binary summary data.",
      "!" = "Binary {type} (`{symbol}`) values must range from 0 to 1.",
      "x" = "{length(offenders_all)} out of {length(input)} \\
      `{symbol}` values {msg_is_are} not in that \\
      range{msg_offenders} {offenders}."
    ))
  }
}



# Building up on the above, the following function is used within `debit()` and
# `debit_map()` to check the numeric range of their `x` and `sd` inputs:
check_debit_inputs_all <- function(x, sd) {
  check_debit_inputs(input = x, type = "mean", symbol = "x")
  check_debit_inputs(input = sd, type = "standard deviation", symbol = "sd")
}




# Basic source function (not exported) ------------------------------------

#' @include utils.R decimal-places.R sd-binary.R round.R unround.R reround.R

debit_table <- function(x, sd, n, unround_x = TRUE, unround_sd = TRUE,
                        group_0 = NA, group_1 = NA,
                        formula = "mean_n", rounding = "up_or_down",
                        threshold = 5, symmetric = FALSE, show_rec = TRUE) {

  # Checks ---

  # As trailing zeros matter for DEBIT, mean and standard deviation need to be
  # given as strings:
  # check_type(x,  "character")
  # check_type(sd, "character")

  # if (!is.character(x)) {
  #   cli::cli_abort(c(
  #     "`x` is {an_a_type(x)}.",
  #     "i" = "It needs to be a string."
  #   ))
  # }
  #
  # if (!is.character(sd)) {
  #   cli::cli_abort(c(
  #     "`sd` is {an_a_type(sd)}.",
  #     "i" = "It needs to be a string."
  #   ))
  # }


  # Main part ---

  # Count decimal places of the standard deviation (SD) and the distribution
  # mean, both as reported:
  digits_x  <- decimal_places(x)
  digits_sd <- decimal_places(sd)

  #  if (n < 10 ^ (digits_sd + 1)) {
  #    rlang::abort("The reported sample size is too small for an SD with so many decimal places.")
  #  }
  #
  #  if (n < 10 ^ (digits_x + 1)) {
  #    rlang::abort("The reported sample size is too small for a mean with so many decimal places.")
  #  }

  sd_chr <- sd
  x_chr <- x

  # Coerce the values reported for SD and average to numeric (because they were
  # given as strings):
  sd <- as.numeric(sd)
  x <- as.numeric(x)

  # Recover lower and upper bounds for the original SD and mean values, going by
  # the reported value each...
  sd_unrounded <- suppressMessages(unround(
    sd_chr[unround_sd], rounding = rounding, digits = digits_sd
  ))

  x_unrounded  <- suppressMessages(unround(
    x_chr[unround_x], rounding = rounding, digits = digits_x
  ))

  # ...and define values out of the tibble resulting from that call:
  # sd_upper <- sd_unrounded$upper
  # sd_lower <- sd_unrounded$lower
  # sd_incl_lower <- sd_unrounded$incl_lower
  # sd_incl_upper <- sd_unrounded$incl_upper
  #
  # x_upper <- x_unrounded$upper
  # x_lower <- x_unrounded$lower
  # x_incl_upper <- x_unrounded$incl_upper
  # x_incl_lower <- x_unrounded$incl_lower

  # x_upper       <- dplyr::if_else(unround_x,   x_unrounded$upper, x)
  # x_lower       <- dplyr::if_else(unround_x,   x_unrounded$lower, x)
  # x_incl_lower  <- dplyr::if_else(unround_x,   x_unrounded$incl_lower, TRUE)
  # x_incl_upper  <- dplyr::if_else(unround_x,   x_unrounded$incl_upper, TRUE)
  #
  # sd_upper      <- dplyr::if_else(unround_sd, sd_unrounded$upper, sd)
  # sd_lower      <- dplyr::if_else(unround_sd, sd_unrounded$lower, sd)
  # sd_incl_lower <- dplyr::if_else(unround_sd, sd_unrounded$incl_lower, TRUE)
  # sd_incl_upper <- dplyr::if_else(unround_sd, sd_unrounded$incl_upper, TRUE)


  if (unround_x) {
    x_upper       <- x_unrounded$upper
    x_lower       <- x_unrounded$lower
    x_incl_lower  <- x_unrounded$incl_lower
    x_incl_upper  <- x_unrounded$incl_upper
  } else {
    x_upper       <- x
    x_lower       <- x
    x_incl_lower  <- TRUE
    x_incl_upper  <- TRUE
  }

  if (unround_sd) {
    sd_upper      <- sd_unrounded$upper
    sd_lower      <- sd_unrounded$lower
    sd_incl_lower <- sd_unrounded$incl_lower
    sd_incl_upper <- sd_unrounded$incl_upper
  } else {
    sd_upper      <- sd
    sd_lower      <- sd
    sd_incl_lower <- TRUE
    sd_incl_upper <- TRUE
  }


  # Reconstruct the original SD:
  sd_rec_lower <- reconstruct_sd(formula, x_lower, n)  # ADD `group_0, group_1` TO SUPPORT OTHER FORMULAS
  sd_rec_upper <- reconstruct_sd(formula, x_upper, n)  # ADD `group_0, group_1` TO SUPPORT OTHER FORMULAS

  # Round the reconstructed SD in the specified way via an internal helper
  # function that mediates between reconstructed values and the various rounding
  # functions. This also allows for the reconstructed SD to be rounded to the
  # same number of decimal places present in the reported SD (default for
  # `rounding` is to allow both up and down from 5, in which case `threshold` is
  # irrelevant; default for `symmetric` is FALSE):
  sd_rec_lower <- reround(
    x = sd_rec_lower,
    digits = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  sd_rec_upper <- reround(
    x = sd_rec_upper,
    digits = digits_sd,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )


  # Test if the reconstructed SD is within the range of the reported SD, as
  # determined by `unround()`:

  sd_rec_both <- c(sd_rec_lower, sd_rec_upper)

  # consistency <- dplyr::case_when(
  #   sd_incl_lower  &&  sd_incl_upper   ~  any(sd_lower <= sd_rec_both) && any(sd_rec_both <= sd_upper),
  #   sd_incl_lower  && !sd_incl_upper   ~  any(sd_lower <= sd_rec_both) && any(sd_rec_both <  sd_upper),
  #   !sd_incl_lower &&  sd_incl_upper   ~  any(sd_lower <  sd_rec_both) && any(sd_rec_both <= sd_upper),
  #   TRUE                               ~  any(sd_lower <  sd_rec_both) && any(sd_rec_both <  sd_upper)
  # )

  if (sd_incl_lower && sd_incl_upper) {
    consistency <- any(sd_lower <= sd_rec_both) && any(sd_rec_both <= sd_upper)
  } else if (sd_incl_lower && !sd_incl_upper) {
    consistency <- any(sd_lower <= sd_rec_both) && any(sd_rec_both < sd_upper)
  } else if (!sd_incl_lower && sd_incl_upper) {
    consistency <- any(sd_lower < sd_rec_both)  && any(sd_rec_both  <= sd_upper)
  } else {
    consistency <- any(sd_lower < sd_rec_both)  && any(sd_rec_both  < sd_upper)
  }

  # `n` is always a whole number, so it's good to convert it to integer:
  n <- as.integer(n)


  # Finally, return the results, with or without the reconstructed numbers
  # (rounding method, boundary values, and Boolean information about the
  # boundary values being inclusive or not):
  if (show_rec) {
    out <- tibble::tibble(
      sd = sd_chr, x = x_chr, n, consistency, rounding,
      sd_lower, sd_incl_lower, sd_incl_upper, sd_upper,
      x_lower, x_incl_lower, x_upper, x_incl_upper
    )
    return(out)
  } else {
    out <- tibble::tibble(sd = sd_chr, x = x_chr, n, consistency)
    return(out)
  }

}

