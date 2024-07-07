
# NOTE: This file was taken from the rsprite2 package
# (https://github.com/LukasWallrich/rsprite2/blob/master/R/core-functions.R).
# However, it only contains the `.sd_limits()` and `grimmer_scalar()` functions
# from the original file.

# The idea is to take them as a basis for revamping scrutiny's implementation of
# GRIMMER, notably fixing the `n_items` argument. TODO: run the
# grimmer-replace-names.R script on the copy.

# # All the variable names:
# GRIMMER_test
# mean
# SD
# n_obs
# n_items
# decimals_mean
# decimals_SD
# realmean
# realsum
# effective_n
# Lsigma
# Usigma
# Lowerbound
# Upperbound
# possible_integers
# Predicted_Variance
# Predicted_SD
# Matches_Oddness
# FirstTest
# Matches_SD
# Third_Test


# Helper function ---------------------------------------------------------

# Determine minimum and maximum SDs for given scale ranges, N, and mean.
.sd_limits <- function(n_obs, mean, min_val, max_val, sd_prec = NULL, n_items = 1) {

  if (is.null(sd_prec)) {
    sd_prec <- max(nchar(sub("^[0-9]*", "", mean)) - 1, 0)
  }

  result <- c(-Inf, Inf)

  aMax <- min_val                                # "aMax" means "value of a to produce the max SD"
  aMin <- floor(mean*n_items)/n_items
  bMax <- max(max_val, min_val + 1, aMin + 1)   # sanity check (just max_val would normally be ok)
  bMin <- aMin + 1/n_items
  total <- round(mean * n_obs * n_items)/n_items

  poss_values <- max_val
  for (i in seq_len(n_items)) {
    poss_values <- c(poss_values, min_val:(max_val-1) + (1 / n_items) * (i - 1))
  }
  poss_values <- sort(poss_values)

  for (abm in list(c(aMin, bMin, 1), c(aMax, bMax, 2))) {

    a <- abm[1]
    b <- abm[2]
    m <- abm[3]


    k <- round((total - (n_obs * b)) / (a - b))
    k <- min(max(k, 1), n_obs - 1)               # ensure there is at least one of each of two numbers
    vec <- c(rep(a, k), rep(b, n_obs - k))
    diff <- sum(vec) - total

    if ((diff < 0)) {
      vec <- c(rep(a, k - 1), a + abs(diff), rep(b, n_obs - k))
    }
    else if ((diff > 0)) {
      vec <- c(rep(a, k), b - diff, rep(b, n_obs - k - 1))
    }

    if (round(mean(vec), sd_prec) != round(mean, sd_prec) | !all(floor(vec*10e9) %in% floor(poss_values*10e9))) {
      stop("Error in calculating range of possible standard deviations")
    }

    result[m] <- round(sd(vec), sd_prec)
  }

  return(result)
}



# Main function -----------------------------------------------------------

GRIMMER_test <- function(mean, sd, n_obs, m_prec = NULL, sd_prec = NULL, n_items = 1, min_val = NULL, max_val = NULL) {
  if (is.null(m_prec)) {
    m_prec <- max(nchar(sub("^[0-9]*", "", mean)) - 1, 0)
  }

  if (is.null(sd_prec)) {
    sd_prec <- max(nchar(sub("^[0-9]*", "", sd)) - 1, 0)
  }

  assert_count(m_prec)
  assert_count(sd_prec)
  assert_count(n_obs)
  assert_count(n_items)
  assert_number(mean)
  assert_number(sd)

  effective_n = n_obs * n_items

  # Applies the GRIM test, and computes the possible mean.
  sum <- mean * effective_n
  realsum <- round(sum)
  realmean <- realsum / effective_n

  #Checks whether mean and SD are within possible range
  if (!is.null(min_val) & !is.null(max_val)) {
    if (mean < min_val | mean > max_val) {
      warning("The mean must be between the scale minimum and maximum")
      return(FALSE)
    }
    sd_limits <- .sd_limits(n_obs, mean, min_val, max_val, sd_prec, n_items)
    if (sd < sd_limits[1] | sd > sd_limits[2]) {
      warning("Given the scale minimum and maximum, the standard deviation has to be between ", sd_limits[1], " and ", sd_limits[2], ".")
      return(FALSE)
    }
  }
  # Creates functions to round a number consistently up or down, when the last digit is 5
  round_down <- function(number, decimals = 2) {
    to_round <- number * 10^(decimals + 1) - floor(number * 10^(decimals)) * 10
    number_rounded <- ifelse(to_round == 5,
                             floor(number * 10^decimals) / 10^decimals,
                             round(number, digits = decimals))
    return(number_rounded)
  }

  round_up <- function(number, decimals = 2) {
    to_round <- number * 10^(decimals + 1) - floor(number * 10^(decimals)) * 10
    number_rounded <- ifelse(to_round == 5,
                             ceiling(number * 10^decimals) / 10^decimals,
                             round(number, digits = decimals))
    return(number_rounded)
  }

  # Applies the GRIM test, to see whether the reconstituted mean is the same as the reported mean (with both down and up rounding)

  consistent_down <- round_down(number = realmean, decimals = m_prec) == mean
  consistent_up <- round_up(number = realmean, decimals = m_prec) == mean

  if (!consistent_down & !consistent_up) {
    warning("GRIM inconsistent - so GRIMMER test cannot be run. See ?GRIM_test")
    return(FALSE)
  }

  # Computes the lower and upper bounds for the sd.

  Lsigma <- ifelse(sd < 5 / (10^(sd_prec+1)), 0, sd - 5 / (10^(sd_prec+1)))
  Usigma <- sd + 5 / (10^(sd_prec+1))

  # Computes the lower and upper bounds for the sum of squares of items.

  lower_bound <- ((n_obs - 1) * Lsigma^2 + n_obs * realmean^2)*n_items^2
  upper_bound <- ((n_obs - 1) * Usigma^2 + n_obs * realmean^2)*n_items^2

  # Checks that there is at least an integer between the lower and upperbound

  if (ceiling(lower_bound) > floor(upper_bound)) {
    return(FALSE)
  }

  # Takes a vector of all the integers between the lowerbound and upperbound

  possible_integers <- ceiling(lower_bound):floor(upper_bound)

  # Creates the predicted variance and sd

  Predicted_Variance <- (possible_integers/n_items^2 - n_obs * realmean^2) / (n_obs - 1)
  Predicted_SD <- sqrt(Predicted_Variance)

  # Computes whether one Predicted_SD matches the SD (trying to round both down and up)

  Rounded_SD_down <- round_down(Predicted_SD, sd_prec)
  Rounded_SD_up <- round_up(Predicted_SD, sd_prec)

  Matches_SD <- Rounded_SD_down == sd | Rounded_SD_up == sd

  if (!any(Matches_SD)) {
    return(FALSE)
  }

  # Computes whether there is an integer of the correct oddness between the lower and upper bounds.
  oddness <- realsum %% 2
  Matches_Oddness <- possible_integers %% 2 == oddness
  return(any(Matches_SD & Matches_Oddness))

  return(TRUE)
}

