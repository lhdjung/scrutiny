#' Define parameters for SPRITE algorithm
#'
#' The SPRITE algorithm aims to construct possible distributions that conform to
#' observed/reported parameters. This function performs some checks and returns a list of these
#' parameters that can then be passed to the functions that actually generate
#' the distributions (e.g. \code{\link{find_possible_distribution}})
#'
#' Restrictions can be used to define how often a specific value should appear in the sample.
#' They need to be passed as a list in the form `value = frequency`. Thus, to specify that
#' there should be no 3s and five 4s in the distribution, you would pass
#' `restrictions_exact = list("3" = 0, "4" = 5)`. To specify that there should be at least
#' one 1 and one 7, you would pass `restrictions_minimum = list("1" = 1, "7" = 1)`. If you just want to
#' specify that the minimum and maximum values appear at least once (for instance when they are the
#' reported rather than possible range), you can use the shortcut `restrictions_minimum = "range"`. Finally,
#' if you work with multi-item scales that result in decimal responses, round those names to two decimal points, e.g.,
#' when `n_items = 3` you could specify `list("1.67" = 0)`.
#'
#' @param mean The mean of the distribution
#' @param sd The standard deviation of the distribution
#' @param n_obs The number of observations (sample size)
#' @param min_val The minimum value
#' @param max_val The maximum value
#' @param m_prec The precision of the mean, as number of digits after the decimal point.
#' If not provided, taken based on the significant digits of `mean` - so only needed if reported mean ends in 0
#' @param sd_prec The precision of the standard deviation, again only needed if
#' reported standard deviation ends in 0.
#' @param n_items Number of items in scale, if distribution represents scale averages.
#' Defaults to 1, which represents any single-item measure.
#' @param restrictions_exact Restrictions on the exact frequency of specific responses, see Details
#' @param restrictions_minimum Restrictions on the minimum frequency of specific responses, see Details
#' @param dont_test By default, this function tests whether the mean is possible, given the sample size (GRIM-test) and whether
#' the standard deviation is possible, given mean and sample size (GRIMMER test), and fails otherwise. If you want to override this,
#' and run SPRITE anyway, you can set this to TRUE.
#'
#' @return A named list of parameters, pre-processed for further rsprite2 functions.
#'
#' @examples
#'
#' set.seed(1234) #To get reproducible results
#'
#' # Simple case
#' sprite_parameters <- set_parameters(mean = 2.2, sd = 1.3, n_obs = 20, min_val = 1, max_val = 5)
#' find_possible_distribution(sprite_parameters)
#'
#' # With restrictions
#' sprite_parameters <- set_parameters(mean = 1.95, sd = 1.55, n_obs = 20,
#'                                     min_val = 1, max_val = 5, n_items = 3,
#'                                     restrictions_exact = list("3"=0, "3.67" = 2),
#'                                     restrictions_minimum = "range")
#' find_possible_distribution(sprite_parameters)
#'
#' @export

set_parameters <- function(mean, sd, n_obs, min_val, max_val,
                           m_prec = NULL, sd_prec = NULL,
                           n_items = 1, restrictions_exact = NULL,
                           restrictions_minimum = NULL,
                           dont_test = FALSE) {
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
  assert_int(min_val)
  assert_int(max_val)
  assert_number(mean)
  assert_number(sd)

  if (min_val >= max_val) {
    stop("max_val needs to be larger than min_val")
  }

  if (!dont_test) {

    if (n_obs * n_items <= 10 ^ m_prec) {
      if (!GRIM_test(mean, n_obs, m_prec, n_items)) {
        stop("The mean is not consistent with this number of observations (fails GRIM test).
             You can use GRIM_test() to identify the closest possible mean and try again.")
      }
      }

  if (!GRIMMER_test(mean, sd, n_obs, m_prec, sd_prec, n_items)) {
    stop("The standard deviation is not consistent with this mean and number of observations (fails GRIMMER test).
         For details, see ?GRIMMER_test.")
  }
  }

  sd_limits <- .sd_limits(n_obs, mean, min_val, max_val, sd_prec, n_items)

  if (!(sd >= sd_limits[1] & sd <= sd_limits[2])) {
    stop("The standard deviation is outside the possible range, given the other parameters.
         It should be between ", sd_limits[1], " and ", sd_limits[2], ".")
  }

    if (!(mean >= min_val & mean <= max_val)) {
    stop("The mean is outside the possible range, which is impossible - please check inputs.")
    }

  if (isTRUE(checkmate::check_choice(restrictions_minimum, "range"))) {
    restrictions_minimum <- list(1, 1)
    names(restrictions_minimum) <- c(min_val, max_val)
  }

  poss_values <- max_val
  for (i in seq_len(n_items)) {
    poss_values <- c(poss_values, min_val:(max_val-1) + (1 / n_items) * (i - 1))
  }
  poss_values <- sort(poss_values)

  poss_values_chr <- round(poss_values, 2)

  fixed_responses <- numeric()
  fixed_values <- NA


  if(!is.null(restrictions_minimum)&!is.null(restrictions_exact)) {

  if(any(duplicated(c(round(as.numeric(names(restrictions_exact)), 2), round(as.numeric(names(restrictions_minimum)), 2))))) {
    duplicated <- c(round(as.numeric(names(restrictions_exact)), 2), round(as.numeric(names(restrictions_minimum)), 2))[duplicated(c(round(as.numeric(names(restrictions_exact)), 2), round(as.numeric(names(restrictions_minimum)), 2)))]
    stop("Several restrictions for same value found. Ensure there is only one restriction (exact or minimum) for: ", duplicated)
  }
}
  if(!is.null(restrictions_minimum)) {

  if(any(!(round(as.numeric(names(restrictions_minimum)), 2) %in% poss_values_chr))) {
    no_match <- names(restrictions_minimum)[!(round(as.numeric(names(restrictions_minimum)), 2) %in% poss_values_chr)]
    stop("Invalid names in restrictions_minimum. The following could not be matched to possible response values: ", no_match)
  }

  #Ensure restrictions are ordered
  restrictions_minimum <- restrictions_minimum[as.character(poss_values_chr[poss_values_chr %in% names(restrictions_minimum)])]

  fixed_responses <- c(fixed_responses, rep(poss_values[poss_values_chr %in% names(restrictions_minimum)], unlist(restrictions_minimum)))

  }

  if(!is.null(restrictions_exact)) {


    if(any(!(round(as.numeric(names(restrictions_exact)), 2) %in% poss_values_chr))) {
      no_match <- names(restrictions_exact)[!(round(as.numeric(names(restrictions_exact)), 2) %in% poss_values_chr)]
      stop("Invalid names in restrictions_exact. The following could not be matched to possible response values: ", no_match)
    }


    #Ensure restrictions are ordered
    restrictions_exact <- restrictions_exact[as.character(poss_values_chr[poss_values_chr %in% names(restrictions_exact)])]

    fixed_responses <- c(fixed_responses, rep(poss_values[poss_values_chr %in% names(restrictions_exact)], unlist(restrictions_exact)))

    fixed_values <- poss_values[poss_values_chr %in% names(restrictions_exact)]
  }

  possible_values <- setdiff(poss_values, fixed_values)
  n_fixed <- length(fixed_responses)

  out <- .named_list(mean, sd, n_obs, min_val, max_val, m_prec, sd_prec, n_items, restrictions_minimum, restrictions_exact, possible_values, fixed_values, fixed_responses, n_fixed)

  class(out) <- c("sprite_parameters", class(out))

  out
}

.named_list <- function(...) {
  out <- list(...)
  names(out) <- as.list(match.call())[-1]
  out
}

#' Find several possible distributions.
#'
#' This function aims to find several possible distribution that would give rise to
#' the observed sample parameters. For that, you need to pass a list of parameters,
#' created with \code{\link{set_parameters}}
#'
#' @param parameters List of parameters, see \code{\link{set_parameters}}
#' @param n_distributions The target number of distributions to return.
#' @param seed An integer to use as the seed for random number generation. Set this in scripts to ensure reproducibility.
#' @param return_tibble Should a tibble, rather than a list, be returned? Requires the `tibble`-package, ignored if that package is not available.
#' @param return_failures Should distributions that failed to produce the desired SD be returned? Defaults to false
#'
#' @return A tibble or list (depending on the `return_tibble` argument) with:
#' \item{outcome}{success or failure - character}
#' \item{distribution}{The distribution that was found (if success) / that had the closest variance (if failure) - numeric}
#' \item{mean}{The exact mean of the distribution - numeric}
#' \item{sd}{The SD of the distribution that was found (success) / that came closest (failure) - numeric}
#' \item{iterations}{The number of iterations required to achieve the specified SD - numeric - the first time this distribution was found}
#'
#' @examples
#'
#' sprite_parameters <- set_parameters(mean = 2.2, sd = 1.3, n_obs = 20,
#'                                     min_val = 1, max_val = 5)
#'
#' find_possible_distributions(sprite_parameters, 5, seed = 1234)
#'
#' @export


find_possible_distributions <- function(parameters, n_distributions = 10, seed = NULL, return_tibble = TRUE, return_failures = FALSE) {

  if (!is.null(seed)) {
    assert_int(seed)
    set.seed(seed)
  }

  assert_count(n_distributions)
  assert_logical(return_tibble)

  outcome <- character()
  distributions <- list()
  found_sd <- numeric()
  found_mean <- numeric()
  iterations <- numeric()

    duplications <- 0

    for (i in 1:(n_distributions * rSprite.maxDupLoops)) {

      n_found <- sum(outcome == "success")

      #This break should possibly be earlier?
      if(length(outcome) - max(c(which(outcome == "success"),0)) >= 10) {
        warning("No successful distribution found in last 10 attempts. Exiting.", if (n_found == 0) " There might not be any possible distribution, but you can try running the search again.")
        break
      }
      if (n_found >= n_distributions) break

      # Calculate the maximum number of consecutive duplicates we will accept before deciding to give up.
      # The value of 0.00001 below is our nominal acceptable chance of missing a valid solution;
      #  however, it's extremely likely that all possible solutions are not all equally likely to be found.
      # So we also set a floor of 100 attempts.
      max_duplications <- max(round(log(0.00001) / log(n_found / (n_found + 1))), 100)

      res <- find_possible_distribution(parameters, seed = NULL)

      res$values <- sort(res$values) # sorting lets us find duplicates more easily

      distributions <- c(list(res$values), distributions)
      if(head(duplicated(distributions, fromLast = TRUE), 1)) {
        distributions <- distributions[-1]
        duplications <- duplications + 1
        if (duplications > max_duplications) {
          break
        }
      } else {
        outcome <- c(res$outcome, outcome)
        found_sd <- c(res$sd, found_sd)
        found_mean <- c(res$mean, found_mean)
        iterations <- c(res$iterations, iterations)
      }

    }

    if (n_found < n_distributions) message("Only ", n_found, " matching distributions could be found. You can try again - given that SPRITE is based on random number generation, more distributions might be found then.")

    if (return_tibble & suppressWarnings(requireNamespace("tibble", quietly = TRUE))) {
      out <- tibble::tibble(id = seq_along(outcome), outcome = outcome, distribution = distributions, mean = found_mean, sd = found_sd, iterations = iterations)
      class(out) <- c("sprite_distributions", class(out))
      attr(out, "parameters") <- parameters
      if(!return_failures) return(out[out$outcome == "success",])
      out
    } else {
      if(!return_failures) {
        successes <- outcome == "success"
      return(list(outcome = outcome[successes], distribution = distributions[successes], mean = found_mean[successes], sd = found_sd[successes], iterations = iterations[successes]))
      }
      list(outcome = outcome, distribution = distributions, mean = found_mean, sd = found_sd, iterations = iterations)
    }

}

#' Find a possible distribution.
#'
#' This function aims to find a possible distribution that would give rise to
#' the observed sample parameters. For that, you need to pass a list of parameters,
#' best created with \code{\link{set_parameters}}
#'
#' @param parameters List of parameters, see \code{\link{set_parameters}}
#' @param seed An integer to use as the seed for random number generation. Set this in scripts to ensure reproducibility.
#' @param values_only Should only values or a more informative list be returned. See Value section.
#'
#' @return Unless `values_only = TRUE`, a list with:
#' \item{outcome}{success or failure - character}
#' \item{distribution}{The distribution that was found (if success) / that had the closest variance (if failure) - numeric}
#' \item{mean}{The exact mean of the distribution - numeric}
#' \item{sd}{The SD of the distribution that was found (success) / that came closest (failure) - numeric}
#' \item{iterations}{The number of iterations required to achieve the specified SD - numeric}
#' If `values_only = TRUE`, then the distribution is returned if one was found, and NULL if it failed.
#'
#' @examples
#' sprite_parameters <- set_parameters(mean = 2.2, sd = 1.3, n_obs = 20,
#'                                     min_val = 1, max_val = 5)
#' find_possible_distribution(sprite_parameters)
#'
#' @export
#'


find_possible_distribution <- function(parameters, seed = NULL, values_only = FALSE) {

  assert_class(parameters, "sprite_parameters")

  if (!is.null(seed)) {
    assert_int(seed)
    set.seed(seed)
  }


  # Generate some random starting data.
  rN <- parameters$n_obs -  parameters$n_fixed
  vec <- sample(parameters$possible_values, rN, replace = TRUE)

  # Adjust mean of starting data.
  max_loops <- parameters$n_obs * length(parameters$possible_values)
  vec <- .adjust_mean(max_loops, vec, parameters$fixed_responses, parameters$mean, parameters$m_prec, parameters$possible_values)


  # Find distribution that also matches SD
  maxLoops <- min(max(round(parameters$n_obs * (length(parameters$possible_values)^2)), rSprite.maxDeltaLoopsLower), rSprite.maxDeltaLoopsUpper)
  granule_sd <- ((0.1^parameters$sd_prec) / 2) + rSprite.dust # allow for rounding errors

  result <- NULL

  for (i in seq_len(maxLoops)) {

    #Should one break out of loop when vec no longer changes? Prob not worth all the comparisons?
    current_sd <- sd(c(vec, parameters$fixed_responses))
    if (abs(current_sd - parameters$sd) <= granule_sd) {
      result <- c(vec, parameters$fixed_responses)
      iter <- i
      break
    }
    vec <- .shift_values(vec, parameters$mean, parameters$sd, parameters$min_val, parameters$max_val, parameters$m_prec, parameters$sd_prec, parameters$fixed_responses, parameters$possible_values, parameters$fixed_values)
  }

  if (!is.null(result)) {
    if(values_only) return(result)
    return(list(outcome = "success", values = result, mean = mean(result), sd = current_sd, iterations = iter))
  } else {
    if(values_only) return(NULL)
    return(list(outcome = "failure", values = c(vec, parameters$fixed_responses), mean = mean(c(vec, parameters$fixed_responses)), sd = current_sd, iterations = maxLoops))
  }
}

.adjust_mean <- function(max_iter, vec, fixed_vals, target_mean, m_prec, poss_values) { #poss_values to exclude those restricted

  meanOK <- FALSE

  for (i in 1:max_iter) {
    fullVec <- c(vec, fixed_vals)
    current_mean <- mean(fullVec)
    if ((round(current_mean, m_prec) == target_mean)) {
      meanOK <- TRUE
      break
    }

    increaseMean <- (current_mean < target_mean)
    if (increaseMean) {
      filter <- (vec < (poss_values[length(poss_values)]))
    } else {
      filter <- (vec > (poss_values[1]))
    }

    possible_bump <- which(filter)
    bumpMean <- possible_bump[as.integer(runif(1) * length(possible_bump)) + 1] # select a  number
    vec[bumpMean] <- poss_values[which(poss_values == vec[bumpMean]) + ifelse(increaseMean, 1, -1)]
  }
  if (!meanOK) {
    if (length(fixed_vals)>0) {
      stop("Couldn't initialize data with correct mean. This *might* be because the restrictions cannot be satisfied.")
    } else {
      stop("Couldn't initialize data with correct mean") # this probably indicates a coding error, if the mean is in range
    }
  }
  return(vec)
}

.shift_values <- function(vec, target_mean, target_sd, min_val, max_val, m_prec = 2, sd_prec, fixed_responses, poss_non_restricted, fixed_vals) { #poss_vals are only those not

  # Backup
  vec_original <- vec

  # Decide if we want to increment or decrement first.
  incFirst <- sample(c(TRUE, FALSE), 1)

  # Decide if we need to increase or decrease the SD.
  fullVec <- c(vec, fixed_responses)
  increaseSD <- (sd(fullVec) < target_sd)

  poss_values <- sort(c(poss_non_restricted, fixed_vals))

  maxToInc <- poss_values[length(poss_values) - 1] # maximum value that we can increment
  minToDec <- poss_values[2] # minimum value that we can decrement

  # Select an element to increment or decrement.
  # For better performance, we select from unique elements only; this means that any number that appears in the vector is
  #  equally likely to be chosen regardless of how often it appears. I'm not sure if this is good or bad.
  # TK - check performance impact.
  uniqueCanBump1 <- !duplicated(vec)

  # The element that we change should be less than the maximum (increment) or greater than the minimum (decrement).
  notEdge1 <- if (incFirst) (vec <= maxToInc) else (vec >= minToDec)
  indexCanBump1 <- uniqueCanBump1 & notEdge1

  # If we can't find an element to change, just return the original vector and let our caller sort it out.
  if (sum(indexCanBump1) == 0) {
    return(vec_original)
  }

  # Unless we have no other choice:
  # - If we want to make the SD larger, there is no point in incrementing the smallest element, or decrementing the largest
  # - If we want to make the SD smaller, there is no point in decrementing the smallest element, or incrementing the largest
  if (increaseSD) {
    noPoint1 <- if (incFirst) (vec == min(vec)) else (vec == max(vec))
  } else {
    noPoint1 <- if (incFirst) (vec == maxToInc) else (vec == minToDec)
  }
  indexCanBump1Try <- indexCanBump1 & (!noPoint1)
  if (any(indexCanBump1Try)) {
    indexCanBump1 <- indexCanBump1Try
  }

  whichCanBump1 <- which(indexCanBump1)
  whichWillBump1 <- whichCanBump1[as.integer(runif(1) * length(whichCanBump1)) + 1]
  willBump1 <- vec[whichWillBump1]
  new1 <- poss_non_restricted[which(poss_non_restricted == willBump1) + ifelse(incFirst, 1, -1)]
  gap1 <- new1 - vec[whichWillBump1] # Note when restricted values have been skipped
  vec[whichWillBump1] <- new1

  # At this point we can decide to only change one of the elements (decrement one without incrementing another, or vice versa).
  # This enables us to explore different means that still round to the same target value.
  # So here we perform the first increment or decrement first, and see if the mean is still GRIM-consistent with the target mean.
  # If it is, then in a proportion of cases we don't adjust the other cell.
  newFullVec <- c(vec, fixed_responses)
  newMean <- mean(newFullVec)
  meanChanged <- (round(newMean, m_prec) != target_mean) # new mean is no longer GRIM-consistent

  if (meanChanged || (runif(1) < 0.4)) {
    vecBump2 <- vec # make a scratch copy of the input vector so we can change it
    vecBump2[whichWillBump1] <- NA # remove the element chosen in part 1...
    uniqueCanBump2 <- !duplicated(vecBump2) # ... but if there was more than one copy of that, it's still a candidate
    notEdge2 <- if (!incFirst) (vecBump2 <= maxToInc) else (vecBump2 >= minToDec)
    indexCanBump2 <- uniqueCanBump2 & notEdge2 & (!is.na(vecBump2))

    # If we can't find an element to change in the opposite direction to the first, then if the mean with the first change is still OK,
    #  we return either the vector with that change. Otherwise we return the original vector and let our caller sort it out.
    if (sum(indexCanBump2) == 0) {
      return(if (meanChanged) vec_original else vec)
    }

    # Unless we have no other choice:
    # - If we want to make the SD larger:
    #   - If in step 1 we chose an element to increment, there is no point in now changing (decrementing) a larger one
    #   - If in step 1 we chose an element to decrement, there is no point in now changing (incrementing) a smaller one
    # - If we want to make the SD smaller:
    #   - If in step 1 we chose an element to increment, there is no point in now changing (decrementing) a smaller one
    #   - If in step 1 we chose an element to decrement, there is no point in now changing (incrementing) a larger one
    # There is also no point in incrementing an element that is equal to the new value of the one that we have already chosen.
    noPoint2 <- ((if (increaseSD == incFirst) (vec > willBump1) else (vec < willBump1)) |
      (vec == (new1))
    )
    indexCanBump2Try <- indexCanBump2 & (!noPoint2)
    if (any(indexCanBump2Try)) {
      indexCanBump2 <- indexCanBump2Try
    }

    whichCanBump2 <- which(indexCanBump2)
    whichWillBump2 <- whichCanBump2[as.integer(runif(1) * length(whichCanBump2)) + 1]
    willBump2 <- vec[whichWillBump2]
    new2 <- poss_non_restricted[which(poss_non_restricted == willBump2) + ifelse(incFirst, -1, 1)]
    gap2 <- new2 - vec[whichWillBump2] # Note when restricted values have been skipped

    gap_resolved <- NA
    # Go into restricted values handling only when necessary - should be good for performance, but
    # leads to more complex backtracking here.
    if (!.equalish(abs(gap1), abs(gap2))) {
      gap_resolved <- FALSE
      poss <- which(.equalish(diff(poss_non_restricted), abs(gap1)))
      if (length(poss) > 1) {
        low <- poss_non_restricted[poss]
        up <- poss_non_restricted[poss + 1]
        if (incFirst) {
          # Should not now move down from target - otherwise we are simply reversing course
          target <- which(up == new1)
        } else {
          target <- which(low == new1)
        }
        up <- up[-target]
        low <- low[-target]

        if (incFirst) {
          from <- up[up %in% vec]
          to <- low[up %in% vec]
        } else {
          to <- up[low %in% vec]
          from <- low[low %in% vec]
        }
        if (length(from) > 0) {
          replace <- sample(seq_along(from), 1)
          vec[cumsum(cumsum(vec == from[replace])) == 1] <- to[replace]
          gap_resolved <- TRUE
        }
      }

      if (!gap_resolved) {
        # Cannot do a single second replacement without undoing change so far
        restricted_runs <- rle(poss_values %in% poss_non_restricted)
        longest_run <- max(restricted_runs$lengths[restricted_runs$values == FALSE])
        vec_backup <- vec
        for (i in seq_len(longest_run)) {
          vec <- vec_backup
          replaced <- 0
          i <- i + 1 # Gap of 1 suggest 2 steps might be needed
          poss <- which(.equalish(diff(poss_non_restricted), gap1 / i))
          if (length(poss) > 0) {
            low <- poss_non_restricted[poss]
            up <- poss_non_restricted[poss + 1]

            for (j in seq_len(i)) {
              if (incFirst) {
                from <- up[up %in% vec]
                to <- low[up %in% vec]
              } else {
                to <- up[low %in% vec]
                from <- low[low %in% vec]
              }
              if (length(from) > 0) {
                replaced <- replaced + 1
                replace <- sample(seq_along(from), 1)
                vec[cumsum(cumsum(vec == from[replace])) == 1] <- to[replace]
              } else {
                break
              }
            }
            if (replaced == i) {
              gap_resolved <- TRUE
              break
            }
          }
        }


        if (!gap_resolved) {
          # No way to get this done with multiple replacements - so, exit
          return(if (meanChanged) vec_original else vec)
        }
      }
    } else {
      vec[whichWillBump2] <- new2
    }
  }

   newFullVec <- c(vec, fixed_responses)
  newMean <- mean(newFullVec)
  meanChanged <- (round(newMean, m_prec) != target_mean) # new mean is no longer GRIM-consistent

  # Floating point issues lead to mean drift with multi-item scales - curtail this straight-away
  if (meanChanged) return(vec_original)

  #if(!.equalish(sum(added), sum(removed)) & meanChanged) browser()

    #message(gap_resolved, "Mean diff detected when adding ", added, " while removing ", removed)

  return(vec)

}

.equalish <- function(x, y, tol = rSprite.dust) {
  x <= (y + rSprite.dust) &
    x >= (y - rSprite.dust)
}

#' GRIM test for mean
#'
#' This function tests whether a given mean (with a specific precision) can
#' result from a sample of a given size based on integer responses to one or more
#' items. The test is based on Brown & Heathers (2017).
#' If `return_values = TRUE` and if there is more than one precise mean compatible
#' with the given parameters, all possible means are returned. In that case, if the
#' given mean is not consistent, the closest consistent mean is returned with a
#' warning.
#'
#' @param return_values Should all means consistent with the given parameters be returned?
#' @inheritParams set_parameters
#'
#' @return Either TRUE/FALSE, or all possible means (if test passes)/closest consistent mean (if test fails)
#' @export
#'
#' @examples
#' # A sample of 28 integers cannot result in a mean of 5.19. This is shown by
#' GRIM_test(5.19, 28)
#'
#' # To find the closest possible mean, set return_values to TRUE
#' GRIM_test(5.19, 28, return_values = TRUE)
#'
#' @references
#' \insertRef{brown2017grim}{rsprite2}


GRIM_test <- function(mean, n_obs, m_prec = NULL, n_items = 1, return_values = FALSE) {
  if (is.null(m_prec)) {
    m_prec <- max(nchar(sub("^[0-9]*", "", mean)) - 1, 0)
  }

  assert_count(m_prec)
  assert_count(n_obs)
  assert_count(n_items)
  assert_logical(return_values)
  assert_number(mean)

  if (n_obs * n_items > 10 ^ m_prec) {
    warning("The sample size (x number of items) is too big compared to the precision of the reported mean. The GRIM test is only meaningful when N < 10 ^ precision (e.g. N < 100 for means reported to two decimal places).")
  }

  int <- round(mean * n_obs * n_items) # nearest integer
  frac <- int / (n_obs * n_items) # mean resulting from nearest integer
  dif <- abs(mean - frac)
  granule <- ((0.1^m_prec) / 2) + rSprite.dust # allow for rounding errors
  if (dif > granule) {
    if (!return_values) {
      return(FALSE)
    }
    valid_mean <- round(frac, m_prec)
    prec_format <- paste("%.", m_prec, "f", sep = "")
    warning("Mean ", sprintf(prec_format, mean), " fails GRIM test - closest consistent value: ", sprintf(prec_format, valid_mean))
    return(valid_mean)
  } else {
    if (!return_values) {
      return(TRUE)
    }
    possible_means <- frac
    i <- 1
    original_int <- int
    while (TRUE) {
      int <- int + i
      frac <- int / (n_obs * n_items) # mean resulting from nearest integer
      dif <- abs(mean - frac)
      if (dif > granule) break()
      possible_means <- c(possible_means, frac)
      i <- i + 1
    }
    i <- 1
    int <- original_int
    while (TRUE) {
      int <- int - i
      frac <- int / (n_obs * n_items) # mean resulting from nearest integer
      dif <- abs(mean - frac)
      if (dif > granule) break()
      possible_means <- c(possible_means, frac)
      i <- i + 1
    }
    return(possible_means)
  }

  stop("Branching error - should not get here")
}

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


#' GRIMMER test for standard deviation
#'
#' This function tests whether a given standard deviation (with a specific precision)
#' can result from a sample of a given size based on integer responses to one or more
#' items. The test was first proposed by [Anaya (2016)](https://peerj.com/preprints/2400/); here, the algorithm
#' developed by [Allard (2018)](https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/) is used,
#' extended by Aurélien Allard to support multi-item scales.
#'
#' @inheritParams set_parameters
#' @param min_val (Optional) Scale minimum. If provided alongside max_val, the function checks whether the SD is consistent with that range.
#' @param max_val (Optional) Scale maximum.
#'
#' @return Logical TRUE/FALSE indicating whether given standard deviation is possible, given the other parameters
#' @export
#'
#' @examples
#' # A sample of 18 integers with mean 3.44 cannot have an SD of 2.47. This is shown by
#' GRIMMER_test(mean = 3.44, sd = 2.47, n_obs = 18)
#'
#'
#' @references
#' \insertRef{anaya2016grimmer}{rsprite2}

# ToDos:
# - add return_values argument to return possible SDs


# # Example inputs:
# mean    <- 1.03
# sd      <- 0.41
# n_obs   <- 40
# n_items <- 1
# m_prec  <- 2
# sd_prec <- 2
# min_val <- NULL
# max_val <- NULL


GRIMMER_test <- function(mean, sd, n_obs, m_prec = NULL, sd_prec = NULL, n_items = 1, min_val = NULL, max_val = NULL) {
  if (is.null(m_prec)) {
    m_prec <- max(nchar(sub("^[0-9]*", "", mean)) - 1, 0)
  }

  if (is.null(sd_prec)) {
    sd_prec <- max(nchar(sub("^[0-9]*", "", sd)) - 1, 0)
  }

  # IN SCRUTINY: specified checkmate namespace
  checkmate::assert_count(m_prec)
  checkmate::assert_count(sd_prec)
  checkmate::assert_count(n_obs)
  checkmate::assert_count(n_items)
  checkmate::assert_number(mean)
  checkmate::assert_number(sd)

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

