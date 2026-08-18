# Original function by Allard ---------------------------------------------

aGrimmer <- function(n, mean, SD, decimals_mean = 2, decimals_SD = 2) {
  # if(n>10^decimals_mean){
  #   print("The sample size is too big compared to the precision of the reported mean, it is not possible to apply GRIM.")
  # }

  #Applies the GRIM test, and computes the possible mean.

  sum <- mean * n
  realsum <- round(sum)
  realmean <- realsum / n

  # Creates functions to round a number consistently up or down, when the last digit is 5

  round_down <- function(number, decimals = 2) {
    is_five <- number * 10^(decimals + 1) - floor(number * 10^(decimals)) * 10
    number_rounded <- ifelse(
      is_five == 5,
      floor(number * 10^decimals) / 10^decimals,
      round(number, digits = decimals)
    )
    return(number_rounded)
  }

  round_up <- function(number, decimals = 2) {
    is_five <- number * 10^(decimals + 1) - floor(number * 10^(decimals)) * 10
    number_rounded <- ifelse(
      is_five == 5,
      ceiling(number * 10^decimals) / 10^decimals,
      round(number, digits = decimals)
    )
    return(number_rounded)
  }

  # Applies the GRIM test, to see whether the reconstituted mean is the same as the reported mean (with both down and up rounding)

  consistency_down <- round_down(number = realmean, decimals = decimals_mean) ==
    mean
  consistency_up <- round_up(number = realmean, decimals = decimals_mean) ==
    mean

  if (consistency_down + consistency_up == 0) {
    return("GRIM inconsistent")
  }

  #Computes the lower and upper bounds for the sd.

  Lsigma <- ifelse(SD < 5 / (10^decimals_SD), 0, SD - 5 / (10^decimals_SD))
  Usigma <- SD + 5 / (10^decimals_SD)

  #Computes the lower and upper bounds for the sum of squares of items.

  Lowerbound <- (n - 1) * Lsigma^2 + n * realmean^2
  Upperbound <- (n - 1) * Usigma^2 + n * realmean^2

  #Checks that there is at least an integer between the lower and upperbound

  FirstTest <- ifelse(ceiling(Lowerbound) > floor(Upperbound), FALSE, TRUE)

  if (FirstTest == FALSE) {
    return("GRIMMER inconsistent (test 1)")
  }

  #Takes a vector of all the integers between the lowerbound and upperbound

  Possible_Integers <- ceiling(Lowerbound):floor(Upperbound)

  #Creates the predicted variance and sd

  Predicted_Variance <- (Possible_Integers - n * realmean^2) / (n - 1)
  Predicted_SD <- sqrt(Predicted_Variance)

  #Computes whether one Predicted_SD matches the SD (trying to round both down and up)

  Rounded_SD_down <- round_down(Predicted_SD, decimals_SD)
  Rounded_SD_up <- round_up(Predicted_SD, decimals_SD)

  Matches_SD <- Rounded_SD_down == SD | Rounded_SD_up == SD

  if (sum(Matches_SD) == 0) {
    return("GRIMMER inconsistent (test 2)")
  }

  #Computes first whether there is any integer between lower and upper bound, and then whether there is
  #an integer of the correct oddness between the lower and upper bounds.
  oddness <- realsum %% 2
  Matches_Oddness <- Possible_Integers %% 2 == oddness
  Third_Test <- Matches_SD & Matches_Oddness
  return(ifelse(
    sum(Third_Test) == 0,
    "GRIMMER inconsistent (test 3)",
    "The mean and SD are consistent."
  ))
}


# Modified function in rsprite2 -------------------------------------------

# (Note the few changes I made here, explained at the appropriate places within
# the function in comments starting on "IN SCRUTINY".)

GRIMMER_test <- function(
  mean,
  sd,
  n_obs,
  m_prec = NULL,
  sd_prec = NULL,
  n_items = 1,
  min_val = NULL,
  max_val = NULL
) {
  if (is.null(m_prec)) {
    m_prec <- max(nchar(sub("^[0-9]*", "", mean)) - 1, 0)
  }

  if (is.null(sd_prec)) {
    sd_prec <- max(nchar(sub("^[0-9]*", "", sd)) - 1, 0)
  }

  # IN SCRUTINY: removed calls to functions from the checkmate package --
  # scrutiny shouldn't depend on it, and they only checked input formats that
  # were a given here anyway.

  effective_n <- n_obs * n_items

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
      warning(
        "Given the scale minimum and maximum, the standard deviation has to be between ",
        sd_limits[1],
        " and ",
        sd_limits[2],
        "."
      )
      return(FALSE)
    }
  }
  # Creates functions to round a number consistently up or down, when the last digit is 5
  round_down <- function(number, decimals = 2) {
    to_round <- number * 10^(decimals + 1) - floor(number * 10^(decimals)) * 10
    number_rounded <- ifelse(
      to_round == 5,
      floor(number * 10^decimals) / 10^decimals,
      round(number, digits = decimals)
    )
    return(number_rounded)
  }

  round_up <- function(number, decimals = 2) {
    to_round <- number * 10^(decimals + 1) - floor(number * 10^(decimals)) * 10
    number_rounded <- ifelse(
      to_round == 5,
      ceiling(number * 10^decimals) / 10^decimals,
      round(number, digits = decimals)
    )
    return(number_rounded)
  }

  # Applies the GRIM test, to see whether the reconstituted mean is the same as the reported mean (with both down and up rounding)

  consistent_down <- round_down(number = realmean, decimals = m_prec) == mean
  consistent_up <- round_up(number = realmean, decimals = m_prec) == mean

  if (!consistent_down & !consistent_up) {
    # IN SCRUTINY: outcommented the below warning (and turned it into a message)
    # that was thrown when the inputs were GRIM-inconsistent. I think one
    # inconsistency should (essentially) be treated like any other, and such a
    # warning is not desirable when testing. It can be incommented to check when
    # this function thinks the inputs are GRIM-inconsistent!

    # message("GRIM inconsistent - so GRIMMER test cannot be run. See ?GRIM_test")
    return(FALSE)
  }

  # Computes the lower and upper bounds for the sd.

  Lsigma <- ifelse(sd < 5 / (10^(sd_prec + 1)), 0, sd - 5 / (10^(sd_prec + 1)))
  Usigma <- sd + 5 / (10^(sd_prec + 1))

  # Computes the lower and upper bounds for the sum of squares of items.

  lower_bound <- ((n_obs - 1) * Lsigma^2 + n_obs * realmean^2) * n_items^2
  upper_bound <- ((n_obs - 1) * Usigma^2 + n_obs * realmean^2) * n_items^2

  # Checks that there is at least an integer between the lower and upperbound

  if (ceiling(lower_bound) > floor(upper_bound)) {
    # # IN SCRUTINY: added message
    # message("Failed test 1")
    return(FALSE)
  }

  # Takes a vector of all the integers between the lowerbound and upperbound

  possible_integers <- ceiling(lower_bound):floor(upper_bound)

  # Creates the predicted variance and sd

  Predicted_Variance <- (possible_integers / n_items^2 - n_obs * realmean^2) /
    (n_obs - 1)
  Predicted_SD <- sqrt(Predicted_Variance)

  # Computes whether one Predicted_SD matches the SD (trying to round both down and up)

  Rounded_SD_down <- round_down(Predicted_SD, sd_prec)
  Rounded_SD_up <- round_up(Predicted_SD, sd_prec)

  Matches_SD <- Rounded_SD_down == sd | Rounded_SD_up == sd

  if (!any(Matches_SD)) {
    # # IN SCRUTINY: added message
    # message("Failed test 2")
    return(FALSE)
  }

  # Computes whether there is an integer of the correct oddness between the lower and upper bounds.
  oddness <- realsum %% 2
  Matches_Oddness <- possible_integers %% 2 == oddness

  if (!any(Matches_SD & Matches_Oddness)) {
    # # IN SCRUTINY: added message
    # message("Failed test 3")
    return(FALSE)
  }

  return(TRUE)
}


# Preparations ------------------------------------------------------------

tested_cases_orig <- 7500

# Randomly generating a great number of values in the first step leaves nearly
# as many values with exactly 2 decimal places in the second. There are the
# first values by that number greater than 1 that have exactly two decimal
# places and where the second decimal place is not zero (so it counts as a
# decimal place even without a string transformation):
df1_mean <- seq(1, length.out = tested_cases_orig, by = 0.01)
df1_mean <- df1_mean[decimal_places(df1_mean) == 2]

length(df1_mean)

# Random `n` values with the same number as the mean values, truncated because
# they can only be whole numbers:
df1_n <- runif(length(df1_mean), 10, 150)
df1_n <- trunc(df1_n)

# Create an example data frame:
df1 <- tibble::tibble(
  n = df1_n,
  mean = as.character(df1_mean),
  sd = as.character(round(as.numeric(mean) * (2 / 5), 2))
)

# # The same data frame but with a different name for the `sd` column; this is
# # just due to a naming difference between the two functions:
df1 <- df1 |>
  dplyr::rename(SD = sd) |>
  dplyr::mutate(mean = as.numeric(mean), SD = as.numeric(SD))

df2 <- df1

# Helper that turns the original function's string output into `TRUE` if
# consistent and `FALSE` if inconsistent:
as_logical_consistency <- function(x) {
  !stringr::str_detect(x, "inconsistent")
}


names(df1) <- c("n", "x", "sd")
df1 <- df1 |>
  dplyr::mutate(
    x = as.numeric(restore_zeros(x, width = 2)),
    sd = as.numeric(restore_zeros(sd, width = 2)),
    digits_x = 2L,
    digits_sd = 2L
  )


# Testing -----------------------------------------------------------------

# Apply both functions, modified and original, to data frames containing the
# same data but (possibly) different column names:

start1 <- Sys.time()
out1 <- purrr::pmap_lgl(df1, grimmer_scalar)
end1 <- Sys.time()
diff1 <- difftime(end1, start1, units = "secs")
# message("\nApplying `grimmer_scalar()` took:\n", round(diff1, 2), " seconds\n")

start2 <- Sys.time()
out2 <- purrr::pmap_chr(df2, aGrimmer)
end2 <- Sys.time()
diff2 <- difftime(end2, start2, units = "secs")
# message("Applying `aGrimmer()` took:\n", round(diff2, 2), " seconds\n")

# Convert the original function's string output to logical so that it will be
# comparable to scrutiny-style Boolean output:
out2 <- as_logical_consistency(out2)


df_out <- tibble::tibble(df1, out1, out2)
df_out <- dplyr::mutate(df_out, digits_sd = decimal_places(sd))

# The problem seems to be restricted to cases where `out1` is consistent and
# `out2` is not, and where `n` is either `40` or `80`:
df_disagree <- df_out |>
  dplyr::filter(out1 != out2)


df_disagree


disagree_rate <- nrow(df_disagree) / nrow(df_out)

disagree_rate

# message("The rate of disagreement between implementations is ", round(disagree_rate, 2))

df_disagree_out1_true <- df_disagree |>
  dplyr::filter(out1)

# Proportion of cases within the disagreements where `grimmer_scalar()` thinks
# the inputs are consistent but `aGrimmer()` thinks they are not:
disagree_new_impl_true_rate <- nrow(df_disagree_out1_true) / nrow(df_disagree)

disagree_new_impl_true_rate


# The reason behind this test is that `grimmer_scalar()`, and thereby all of
# scrutiny's GRIMMER implementation, is somewhat different from the original
# `aGrimmer()` function -- circa 70 percent of the disagreements are due to
# `grimmer_scalar()` being more lenient. The overall rate of disagreement
# revolves around 0.3 percent, but due to the element of randomness and the
# relatively low number of tested cases (equal to `nrow(df_out)`), the test only
# requires the rate to be below 5 percent. This will be met absent some
# significant changes in `grimmer_scalar()`. In such a situation, it would be
# better for the test to fail.
test_that("the two functions disagree on less than 3 percent of cases", {
  disagree_rate |> expect_lt(0.05)
})


# Resolve disagreements ---------------------------------------------------

# TODO discuss internally -- after testing 75k cases (ten times more than
# usual), the cases below were the only cases of disagreement (`df_disagree`).
# (Note that `out1` is the result of `grimmer_scalar()`, and `out2` of
# `aGrimmer()`. Most important is that `n` is always 40 or 80!)
tibble::tibble(
  n = rep(c(40, 80), c(6L, 1L)),
  x = c("4.02", "32.43", "512.57", "515.57", "517.57", "519.57", "521.67"),
  sd = c("1.61", "12.97", "205.03", "206.23", "207.03", "207.83", "208.67"),
  out1 = TRUE,
  out2 = FALSE,
  digits_sd = 2L,
)

c(
  n = "40",
  x = "16.03",
  sd = "6.41",
  out1 = "TRUE",
  out2 = "FALSE",
  digits_sd = "2"
)
c(
  n = "40",
  x = "64.73",
  sd = "25.89",
  out1 = "TRUE",
  out2 = "FALSE",
  digits_sd = "2"
)
c(
  n = "80",
  x = "64.73",
  sd = "25.89",
  out1 = "TRUE",
  out2 = "FALSE",
  digits_sd = "2"
)
c(
  n = "80",
  x = "32.68",
  sd = "13.07",
  out1 = "TRUE",
  out2 = "FALSE",
  digits_sd = "2"
)
c(
  n = "40",
  x = "64.27",
  sd = "25.71",
  out1 = "TRUE",
  out2 = "FALSE",
  digits_sd = "2"
)
c(
  n = "80",
  x = "16.22",
  sd = "6.49",
  out1 = "TRUE",
  out2 = "FALSE",
  digits_sd = "2"
)
c(
  n = "40",
  x = "256.03",
  sd = "102.41",
  out1 = "TRUE",
  out2 = "FALSE",
  digits_sd = "2"
)
c(
  n = "40",
  x = "519.93",
  sd = "207.97",
  out1 = "TRUE",
  out2 = "FALSE",
  digits_sd = "2"
)

# # Use this to get a vector such as above:
# df_disagree |> dplyr::slice(1) |> unlist() |> constructive::construct(one_liner = TRUE)

# Here they are in tibble form. Run `GRIMMER_test()` on them and see whether
# this is all due to GRIM's 40/80 leniency!
df_disagree_all <- tibble::tibble(
  n = c(40, 40, 80, 80, 40, 80, 40, 40, 40, 40, 80, 40, 40, 40),
  x = c(
    "16.03",
    "64.73",
    "64.73",
    "32.68",
    "64.27",
    "16.22",
    "256.03",
    "519.93",
    "32.32",
    "256.03",
    "512.33",
    "512.93",
    "513.07",
    "518.93"
  ),
  sd = c(
    "6.41",
    "25.89",
    "25.89",
    "13.07",
    "25.71",
    "6.49",
    "102.41",
    "207.97",
    "12.93",
    "102.41",
    "204.93",
    "205.17",
    "205.23",
    "207.57"
  ),
) |>
  dplyr::relocate(x, sd, n) |>
  dplyr::mutate(n = as.numeric(n))

# See if there are warnings about GRIM (!) when mapping `GRIMMER_test()`:
df_disagree_all |>
  dplyr::rename(mean = x, n_obs = n) |>
  dplyr::mutate(mean = as.numeric(mean), sd = as.numeric(sd)) |>
  purrr::pmap(GRIMMER_test)


# New implementation from rsprite2 ----------------------------------------

test_that("GRIMMER works correctly by default", {
  grimmer_scalar(5.21, 1.6, 28, digits_x = 2, digits_sd = 1)  |> expect_true()
  grimmer_scalar(3.44, 2.47, 18, digits_x = 2, digits_sd = 2) |> expect_false()
})

test_that("GRIMMER works correctly when compared to the rsprite2 implementation", {
  grimmer_scalar(1.2, 0.3,  57, digits_x = 1, digits_sd = 1) |> expect_equal(GRIMMER_test(1.2, 0.3,  57))
  grimmer_scalar(8.3, 7.5, 103, digits_x = 1, digits_sd = 1) |> expect_equal(GRIMMER_test(8.3, 7.5, 103))

  # Dealing with test-3 inconsistencies:
  grimmer_scalar(5.23, 2.55, 35, digits_x = 2, digits_sd = 2)  |> expect_equal(GRIMMER_test(5.23, 2.55, 35))
  grimmer_scalar(5.23, 2.55, 127, digits_x = 2, digits_sd = 2) |> expect_equal(GRIMMER_test(5.23, 2.55, 127))
  grimmer_scalar(5.2 , 2.5 , 35, digits_x = 1, digits_sd = 1)  |> expect_equal(GRIMMER_test(5.2 , 2.5 , 35))

  # This value set is from `pigs5`. It used to be flagged as a test-3
  # inconsistency by `grimmer_scalar()`, but it is consistent according to both
  # the new version and rsprite2:
  grimmer_scalar(2.57, 2.57, 30, digits_x = 2, digits_sd = 2) |> expect_equal(GRIMMER_test(2.57, 2.57, 30))

  # Some finer variations:
  grimmer_scalar(3.756, 4.485, 89, digits_x = 3, digits_sd = 3) |> expect_equal(GRIMMER_test(3.756, 4.485, 89))
  grimmer_scalar(3.756, 4.485, 12, digits_x = 3, digits_sd = 3) |> expect_equal(GRIMMER_test(3.756, 4.485, 12))
  grimmer_scalar(3.75,  4.48,  12, digits_x = 2, digits_sd = 2) |> expect_equal(GRIMMER_test(3.75, 4.48, 12))
  grimmer_scalar(3.75,  4.48,  89, digits_x = 2, digits_sd = 2) |> expect_equal(GRIMMER_test(3.75, 4.48, 89))
})

test_that("GRIMMER works correctly with `items = 2`", {
  grimmer_scalar(5.21, 1.60, 28, digits_x = 2, digits_sd = 2, items = 2) |> expect_equal(GRIMMER_test(5.21, 1.6 , 28, n_items = 2))
  grimmer_scalar(3.44, 2.47, 18, digits_x = 2, digits_sd = 2, items = 2) |> expect_equal(GRIMMER_test(3.44, 2.47, 18, n_items = 2))
})

test_that("GRIMMER works correctly with `items = 3`", {
  grimmer_scalar(5.21, 1.60, 28, digits_x = 2, digits_sd = 2, items = 3) |> expect_equal(GRIMMER_test(5.21, 1.6 , 28, n_items = 3))
  grimmer_scalar(3.44, 2.47, 18, digits_x = 2, digits_sd = 2, items = 3) |> expect_equal(GRIMMER_test(3.44, 2.47, 18, n_items = 3))
})


# Issue #85 -----------------------------------------------------------------

test_that("GRIMMER checks SD-match and parity against the same candidate sum of squares (#85)", {
  # With 3+ candidate sums of squares, one candidate could satisfy the SD-match
  # test and a *different* candidate could satisfy the parity test, which used
  # to make `grimmer()` return `TRUE` even though no single candidate passed
  # both. This only affects `rounding = "up"`/`"down"`; see
  # https://github.com/lhdjung/scrutiny/issues/85.
  grimmer(x = 0.11, sd = 0.87, n = 64, digits_x = 2, digits_sd = 2, rounding = "up") |>
    expect_false()

  cases <- tibble::tribble(
    ~x,   ~sd,  ~n,
    0.00, 0.62, 129,
    0.11, 0.87, 64,
    0.14, 1.12, 64,
    0.17, 1.37, 64,
    0.48, 0.37, 256,
    0.52, 0.37, 256,
    0.83, 1.37, 64,
    0.86, 1.12, 64,
    0.89, 0.87, 64,
    1.00, 0.62, 129
  )

  result_false <- cases |>
    purrr::pmap_lgl(function(x, sd, n) {
    grimmer(x = x, sd = sd, n = n, digits_x = 2, digits_sd = 2, rounding = "up")
  })

  expect_false(any(result_false))

  # The same value is genuinely GRIMMER-consistent under "up_or_down", where a
  # third candidate integer for the sum of squares is not at stake:
  grimmer(x = 0.11, sd = 0.87, n = 64, digits_x = 2, digits_sd = 2, rounding = "up_or_down") |>
    expect_true()
})


# Issue #86 -----------------------------------------------------------------

# The candidate-sum range used to be derived from floating-point products such
# as `floor(upper_bound * n)`. Where the product was mathematically an exact
# integer, its `double` representation could land on either side of it, so a
# legitimate sum was dropped or a phantom sum admitted. See
# https://github.com/lhdjung/scrutiny/issues/86.

test_that("GRIMMER does not flag values that real datasets produce (#86)", {
  # 7 ones and 193 zeros: mean 0.035 (reported as 0.03 or 0.04), SD 0.1842409
  # (reported as 0.18). The only viable sum of squares is 7, and it used to be
  # dropped because `unround(0.03)$upper * 200` is 6.9999999999999991 rather
  # than the exact 7 it is mathematically.
  witness <- c(rep(1, 7), rep(0, 193))
  expect_equal(mean(witness), 0.035)
  expect_equal(round(stats::sd(witness), 2), 0.18)

  grimmer(x = 0.03, sd = 0.18, n = 200, digits_x = 2, digits_sd = 2) |>
    expect_true()
  grimmer(x = 0.04, sd = 0.18, n = 200, digits_x = 2, digits_sd = 2) |>
    expect_true()
  grimmer(x = 0.04, sd = 0.18, n = 200, digits_x = 2, digits_sd = 2, rounding = "up") |>
    expect_true()
})


test_that("GRIMMER admits no phantom candidate sums (#86)", {
  # The admissible sum range here is just {11}: a sum of 12 would mean a mean of
  # 0.075, which rounds up to 0.08 rather than 0.07. But `unround(0.07,
  # "up")$upper * 160` is 12.000000000000002, so 12 used to slip past the
  # exclusive upper bound and supply a parity-matching sum of squares. Note that
  # `grim()` itself passes here, so the GRIM gate cannot catch this.
  grim(x = 0.07, n = 160, digits_x = 2, rounding = "up") |>
    expect_true()
  grimmer(x = 0.07, sd = 0.08, n = 160, digits_x = 2, digits_sd = 2, rounding = "up") |>
    expect_false()
})


test_that("GRIMMER never flags an actual two-value dataset (#86)", {
  # Every dataset below is real, so GRIMMER must not flag its rounded mean and
  # SD. The `n` values are the boundary-dense ones, i.e. those where a candidate
  # sum can fall exactly on a rounding bound.
  false_flags <- 0L

  for (rounding in c("up_or_down", "up")) {
    for (n in c(20, 40, 80, 200, 400)) {
      for (a in 0:n) {
        values <- c(rep(1, a), rep(0, n - a))
        sd_value <- stats::sd(values)
        x <- reround(mean(values), digits = 2, rounding = rounding)[1L]
        sd_rounded <- reround(sd_value, digits = 2, rounding = rounding)[1L]
        consistent <- grimmer(
          x = x,
          sd = sd_rounded,
          n = n,
          digits_x = 2,
          digits_sd = 2,
          rounding = rounding
        )
        if (!isTRUE(consistent)) {
          false_flags <- false_flags + 1L
        }
      }
    }
  }

  expect_equal(false_flags, 0L)
})

# Exact sum-of-squares arithmetic (#86) -------------------------------------

test_that("GRIMMER derives the sum-of-squares bounds exactly", {
  # Two subjects, three items each, every item sum 23: subject scores are
  # 23 / 3 = 7.667, so the mean is 7.67 and the SD is 0.00. The exact sum of
  # squares is 2 * 23^2 = 1058, but it used to compute as 1058.0000000000002
  # and get ceilinged to 1059, above the upper bound of 1058 -- so the only
  # viable sum of squares was dropped and this real data set was flagged.
  # `round(sum_squares_lower, 12)` could not repair that: past about 1000, two
  # neighboring doubles are already more than 1e-12 apart.
  witness <- c(23, 23)
  expect_equal(round(mean(witness / 3), 2), 7.67)
  expect_equal(stats::sd(witness / 3), 0)

  grimmer(
    x = 7.67, sd = 0, n = 2, items = 3, digits_x = 2, digits_sd = 2
  ) |>
    expect_true()

  # Same mechanism at a range of magnitudes, all of them real data sets with
  # zero variance -- the case where the lower bound lands exactly on an
  # integer:
  false_flags <- 0L

  for (items in 2:6) {
    for (n in c(2, 3, 5, 10, 37)) {
      for (item_sum in c(23, 25, 28, 106, 400, 631, 1000, 2317)) {
        x <- reround(item_sum / items, digits = 2)[1L]
        consistent <- grimmer(
          x = x,
          sd = 0,
          n = n,
          items = items,
          digits_x = 2,
          digits_sd = 2
        )
        if (!isTRUE(consistent)) {
          false_flags <- false_flags + 1L
        }
      }
    }
  }

  expect_equal(false_flags, 0L)
})


test_that("GRIMMER never flags a real multi-item data set", {
  set.seed(1234)
  false_flags <- 0L

  for (trial in 1:400) {
    items <- sample(2:6, 1)
    n <- sample(2:40, 1)
    values <- sample(0:sample(c(3, 9, 60, 200), 1), n * items, replace = TRUE)
    scores <- colSums(matrix(values, nrow = items)) / items
    sd_value <- stats::sd(scores)
    consistent <- grimmer(
      x = reround(mean(scores), digits = 2)[1L],
      sd = reround(sd_value, digits = 2)[1L],
      n = n,
      items = items,
      digits_x = 2,
      digits_sd = 2
    )
    if (!isTRUE(consistent)) {
      false_flags <- false_flags + 1L
    }
  }

  expect_equal(false_flags, 0L)
})


# Rounding methods ----------------------------------------------------------

test_that("GRIMMER supports the compound rounding methods", {
  # These used to abort, because `unround()` -- which supplied the SD bounds --
  # does not know them, even though `grim()` and `reround()` both do.
  for (rounding in c("ceiling_or_floor", "up_from_or_down_from")) {
    grimmer(
      x = 5.23, sd = 2.55, n = 31, digits_x = 2, digits_sd = 2,
      rounding = rounding, threshold = 3
    ) |>
      expect_type("logical")
  }
})


test_that("`symmetric` is passed on to the GRIM stage", {
  # `grimmer()` runs `grim()` first, so a mean that `symmetric` rules out must
  # make GRIMMER inconsistent as well:
  expect_false(
    grim(-0.07, n = 40, digits_x = 2, rounding = "up", symmetric = TRUE)
  )
  expect_false(
    grimmer(
      x = -0.07, sd = 0.1, n = 40, digits_x = 2, digits_sd = 1,
      rounding = "up", symmetric = TRUE
    )
  )
})


test_that("GRIMMER returns `NA` where GRIM itself is undecidable", {
  # GRIMMER runs GRIM first and branches on its verdict, which is `NA` where the
  # mean's rounding bounds are undefined -- as with a missing mean -- and where
  # `n` leaves nothing to test. The branch used to fail on the `NA` with
  # "missing value where TRUE/FALSE needed" instead of passing it on. `debit()`
  # has the same test. (Until `anti_trunc()` stopped sending zero away from
  # zero, `rounding = "anti_trunc"` at a mean of zero was a second such case.)
  NA |> grim(n = 40, digits_x = 2) |> expect_na()
  expect_na(
    grimmer(x = NA, sd = 0.41, n = 40, digits_x = 2, digits_sd = 2)
  )
  expect_na(grimmer(x = 1.03, sd = 0.41, n = 0, digits_x = 2, digits_sd = 2))

  # The reason names the stage the case got stuck at, the way the reason for an
  # undecidable SD does. It must not read as a GRIM *inconsistency*, which
  # `audit()` counts by matching that string:
  # (A missing `x` would be reported as `"Missing value"` by the mapper, which
  # screens for missingness ahead of the test, so the vehicle here is an `n`
  # that leaves nothing to test.)
  out <- grimmer_map(
    tibble::tibble(x = c(1.03, 1.03), sd = c(0.41, 0.41), n = c(0L, 40L)),
    digits_x = 2,
    digits_sd = 2,
    show_reason = TRUE
  )
  out$consistency |> expect_equal(c(NA, FALSE))
  out$reason[1L] |> expect_equal("No testable value set")
  audit(out)$fail_grim |> expect_equal(0L)
  audit(out)$incons_cases |> expect_equal(1L)
})

# Scale bounds ------------------------------------------------------------

test_that("`min_val` and `max_val` must be specified together and be valid", {
  args <- list(x = 3, sd = 1, n = 20, digits_x = 2, digits_sd = 2)
  do.call(grimmer, c(args, list(min_val = 1))) |>
    expect_error("specified together")
  do.call(grimmer, c(args, list(max_val = 5))) |>
    expect_error("specified together")
  do.call(grimmer, c(args, list(min_val = 1.5, max_val = 5))) |>
    expect_error("whole numbers")
  do.call(grimmer, c(args, list(min_val = 5, max_val = 1))) |>
    expect_error("greater than")
  do.call(grimmer, c(args, list(min_val = 1, max_val = 5))) |>
    expect_type("logical")
})


test_that("a mean outside the scale is inconsistent by itself", {
  grimmer(
    x = 7.22, sd = 1.10, n = 30, digits_x = 2, digits_sd = 2,
    min_val = 1, max_val = 5
  ) |>
    expect_false()
  grimmer_scalar(
    x = 7.22, sd = 1.10, n = 30, digits_x = 2, digits_sd = 2,
    min_val = 1, max_val = 5, show_reason = TRUE
  )[[2L]] |>
    expect_equal("Mean out of scale range")
})


test_that("scale bounds rule out SDs that an unbounded scale allows", {
  # Ten 1s and ten 5s are as spread out as a five-point scale gets at a mean of
  # 3, and even they only have an SD of 2.05:
  grimmer(x = 3.00, sd = 2.08, n = 20, digits_x = 2, digits_sd = 2) |>
    expect_true()
  grimmer(
    x = 3.00, sd = 2.08, n = 20, digits_x = 2, digits_sd = 2,
    min_val = 1, max_val = 5
  ) |>
    expect_false()
  grimmer_scalar(
    x = 3.00, sd = 2.08, n = 20, digits_x = 2, digits_sd = 2,
    min_val = 1, max_val = 5, show_reason = TRUE
  )[[2L]] |>
    expect_equal("GRIMMER inconsistent (scale range)")

  # A mean at the very bottom of the scale forces every value to be there, too:
  grimmer(
    x = 1.00, sd = 0.32, n = 20, digits_x = 2, digits_sd = 2,
    min_val = 1, max_val = 5
  ) |>
    expect_false()
  grimmer(
    x = 1.00, sd = 0.00, n = 20, digits_x = 2, digits_sd = 2,
    min_val = 1, max_val = 5
  ) |>
    expect_true()
})


test_that("scale bounds change nothing that doesn't depend on the scale", {
  # There is a second bound on the sum of squares: the least one that `n` whole
  # numbers adding up to a given sum can have, i.e. the values as equal as
  # possible. GRIMMER does not apply it, and it is deliberately not applied here
  # either, because it does not depend on the scale at all -- the near-equal
  # values always lie inside the range, since their mean does. Applying it under
  # `min_val`/`max_val` would make verdicts turn on an argument that has no
  # bearing on them.
  #
  # Ten whole numbers adding up to 13 have an SD of at least 0.48, so an SD of
  # 0.11 is impossible. GRIMMER passes it anyway, and passes it just the same
  # when told about a scale that is equally irrelevant to it:
  grimmer(x = 1.30, sd = 0.11, n = 10, digits_x = 2, digits_sd = 2) |>
    expect_true()
  grimmer(
    x = 1.30, sd = 0.11, n = 10, digits_x = 2, digits_sd = 2,
    min_val = 1, max_val = 5
  ) |>
    expect_true()
})


test_that("scale bounds never make GRIMMER more permissive", {
  violations <- 0L
  for (n in c(15, 25, 40)) {
    for (x in seq(1, 5, by = 0.1)) {
      for (sd in seq(0.05, 2.5, by = 0.05)) {
        args <- list(x = x, sd = sd, n = n, digits_x = 2, digits_sd = 2)
        bounded <- do.call(grimmer, c(args, list(min_val = 1, max_val = 5)))
        if (isTRUE(bounded) && !isTRUE(do.call(grimmer, args))) {
          violations <- violations + 1L
        }
      }
    }
  }

  expect_equal(violations, 0L)
})


test_that("no sample within the scale is reported as inconsistent", {
  # Exhaustive over every multiset of `n` responses that the scale allows. The
  # bounds are a necessary condition, so a value set that really occurs must
  # never be ruled out -- whatever `items` is, since the bounds are per
  # response, not per scale score.
  check_all_samples <- function(n, items, min_val, max_val) {
    grid <- expand.grid(rep(list((min_val * items):(max_val * items)), n))
    grid <- grid[apply(grid, 1L, function(v) !is.unsorted(v)), , drop = FALSE]
    verdicts <- vapply(
      seq_len(nrow(grid)),
      function(i) {
        scores <- as.numeric(grid[i, ]) / items
        grimmer(
          x = round(mean(scores), 2L),
          sd = round(stats::sd(scores), 2L),
          n = n,
          digits_x = 2, digits_sd = 2, items = items,
          min_val = min_val, max_val = max_val
        )
      },
      logical(1L)
    )
    expect_true(all(verdicts))
  }

  check_all_samples(n = 5L, items = 1, min_val = 1, max_val = 5)
  check_all_samples(n = 6L, items = 1, min_val = 1, max_val = 4)
  check_all_samples(n = 4L, items = 2, min_val = 1, max_val = 5)
  check_all_samples(n = 5L, items = 3, min_val = 1, max_val = 3)
  check_all_samples(n = 4L, items = 1, min_val = 0, max_val = 6)
})


test_that("`grimmer_map()` passes the scale bounds down and `audit()` counts", {
  df <- tibble::tibble(
    x = c(3.00, 7.22, 3.00),
    sd = c(2.08, 1.10, 1.45),
    n = c(20L, 30L, 20L)
  )
  out <- grimmer_map(df, digits_x = 2, digits_sd = 2, min_val = 1, max_val = 5)

  out$consistency |> expect_equal(c(FALSE, FALSE, TRUE))
  out$reason |>
    expect_equal(c(
      "GRIMMER inconsistent (scale range)",
      "Mean out of scale range",
      "Passed all"
    ))
  audit(out)$fail_scale |> expect_equal(2L)

  # Zero for a call that says nothing about the scale:
  grimmer_map(df, digits_x = 2, digits_sd = 2) |> audit() |> _$fail_scale |>
    expect_equal(0L)
})


# A brute-force oracle, complementing the random trials above. Those sample 400
# data sets at larger `n`; this one enumerates *every* multiset of `n` whole
# numbers for small `n`, so nothing in that space can hide. GRIMMER's conditions
# are necessary but not sufficient, so only this direction can be checked: every
# (mean, SD) pair that a real sample produces must pass.

grimmer_multisets <- function(n, vals) {
  # Every multiset of size `n` from `vals`, as columns:
  idx <- utils::combn(length(vals) + n - 1L, n)
  apply(idx, 2L, function(k) vals[k - seq_len(n) + 1L])
}


grimmer_reported_pairs <- function(m, digits, rounding, items) {
  xs <- reround(colMeans(m) / items, digits, rounding)
  ss <- reround(apply(m, 2L, stats::sd) / items, digits, rounding)
  # A compound `rounding` returns both variants per sample, interleaved, and
  # either is a way the value could have been reported:
  reps <- length(xs) / ncol(m)
  out <- list()
  for (i in seq_len(ncol(m))) {
    pair <- ((i - 1L) * reps + 1L):(i * reps)
    for (a in unique(xs[pair])) {
      for (b in unique(ss[pair])) {
        out[[length(out) + 1L]] <- c(a, b)
      }
    }
  }
  unique(do.call(rbind, out))
}


test_that("GRIMMER never rejects an enumerable sample", {
  n_checked <- 0L

  for (n in 3:6) {
    for (digits in 1:2) {
      pairs <- grimmer_reported_pairs(
        grimmer_multisets(n, 0:5), digits, "up_or_down", items = 1
      )
      verdict <- mapply(
        function(x, sd) {
          grimmer(
            x = x, sd = sd, n = n,
            digits_x = digits, digits_sd = digits
          )
        },
        pairs[, 1L],
        pairs[, 2L]
      )
      n_checked <- n_checked + length(verdict)
      false_negatives <- which(!verdict %in% TRUE)
      expect_equal(
        length(false_negatives),
        0L,
        info = paste0(
          "n = ", n, ", digits = ", digits, " -- pairs: ",
          toString(utils::head(paste0(
            "(", pairs[false_negatives, 1L], ", ", pairs[false_negatives, 2L], ")"
          ), 5L))
        )
      )
    }
  }

  # Guard against the loops silently collapsing to nothing:
  expect_gt(n_checked, 500L)
})


test_that("GRIMMER never rejects an enumerable multi-item sample", {
  # `items = 2` halves the granularity of both the mean and the SD, which is
  # where the item multiplication in the sum-of-squares stage has to keep up:
  pairs <- grimmer_reported_pairs(
    grimmer_multisets(4L, 0:6), digits = 2, rounding = "up_or_down", items = 2
  )
  verdict <- mapply(
    function(x, sd) {
      grimmer(
        x = x, sd = sd, n = 4L, items = 2,
        digits_x = 2, digits_sd = 2
      )
    },
    pairs[, 1L],
    pairs[, 2L]
  )

  expect_equal(sum(!verdict %in% TRUE), 0L)
  expect_gt(length(verdict), 100L)
})


test_that("GRIMMER never rejects an enumerable sample within scale bounds", {
  # Scale bounds may only ever rule cases *out*, so a sample that really lies
  # within `min_val` and `max_val` must survive them:
  pairs <- grimmer_reported_pairs(
    grimmer_multisets(5L, 0:5), digits = 2, rounding = "up_or_down", items = 1
  )
  verdict <- mapply(
    function(x, sd) {
      grimmer(
        x = x, sd = sd, n = 5L,
        digits_x = 2, digits_sd = 2, min_val = 0, max_val = 5
      )
    },
    pairs[, 1L],
    pairs[, 2L]
  )

  expect_equal(sum(!verdict %in% TRUE), 0L)
  expect_gt(length(verdict), 100L)
})


test_that("an `n` too large to enumerate is an error, not a hang", {
  # GRIMMER enumerates every integer sum the reported mean admits, and for each
  # of those every integer sum of squares the reported SD admits. Both ranges
  # grow linearly with `n`: `a:b` simply allocated, so `n = 3e9` exhausted
  # memory with no warning on the way.
  grimmer(x = 5.19, sd = 2.5, n = 3e9, digits_x = 2, digits_sd = 2) |>
    expect_error("too large for GRIMMER to enumerate")
  grimmer(x = 5.19, sd = 2.5, n = 1e8, digits_x = 2, digits_sd = 2) |>
    expect_error("too large for GRIMMER to enumerate")

  # The limit is far above anything a published summary statistic looks like,
  # so a large but plausible `n` still goes through:
  grimmer(x = 5.19, sd = 2.5, n = 1e6, digits_x = 2, digits_sd = 2) |>
    expect_type("logical")
})
