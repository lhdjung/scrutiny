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

  FirstTest <- ceiling(Lowerbound) <= floor(Upperbound)

  if (!FirstTest) {
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
df1_n <- runif(length(df1_mean), 10, 150) |> trunc()

# Create an example data frame:
df1 <- tibble::tibble(
  n = df1_n,
  mean = as.character(df1_mean),
  sd = (as.numeric(mean) * (2 / 5)) |> round(2) |> as.character()
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
    digits_sd = 2L,
    items_averaged = 1
  )


# Testing -----------------------------------------------------------------

# Apply both functions, modified and original, to data frames containing the
# same data but (possibly) different column names:

start1 <- Sys.time()
out1 <- df1 |> purrr::pmap_lgl(grimmer_scalar)
end1 <- Sys.time()
diff1 <- difftime(end1, start1, units = "secs")
# message("\nApplying `grimmer_scalar()` took:\n", round(diff1, 2), " seconds\n")

start2 <- Sys.time()
out2 <- df2 |> purrr::pmap_chr(aGrimmer)
end2 <- Sys.time()
diff2 <- difftime(end2, start2, units = "secs")
# message("Applying `aGrimmer()` took:\n", round(diff2, 2), " seconds\n")

# Convert the original function's string output to logical so that it will be
# comparable to scrutiny-style Boolean output:
out2 <- as_logical_consistency(out2)


df_out <- tibble::tibble(df1, out1, out2) |>
  dplyr::mutate(digits_sd = decimal_places(sd))

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
  5.21 |> grimmer_scalar(1.6, 28, digits_x = 2, digits_sd = 1, items_averaged = 1)  |> expect_true()
  3.44 |> grimmer_scalar(2.47, 18, digits_x = 2, digits_sd = 2, items_averaged = 1) |> expect_false()
})

test_that("GRIMMER works correctly when compared to the rsprite2 implementation", {
  # rsprite2 accepts this, but whole numbers with a mean of 1.2 at `n = 57` have
  # an SD of at least 0.368, as nine 2s and 48 1s. This is the minimum sum of
  # squares that `sum_squares_min()` adds to test 1:
  1.2 |> grimmer_scalar(0.3,  57, digits_x = 1, digits_sd = 1, items_averaged = 1) |> expect_false()
  8.3 |> grimmer_scalar(7.5, 103, digits_x = 1, digits_sd = 1, items_averaged = 1) |> expect_equal(GRIMMER_test(8.3, 7.5, 103))

  # Dealing with test-3 inconsistencies:
  5.23 |> grimmer_scalar(2.55, 35, digits_x = 2, digits_sd = 2, items_averaged = 1)  |> expect_equal(GRIMMER_test(5.23, 2.55, 35))
  5.23 |> grimmer_scalar(2.55, 127, digits_x = 2, digits_sd = 2, items_averaged = 1) |> expect_equal(GRIMMER_test(5.23, 2.55, 127))
  5.2  |> grimmer_scalar(2.5 , 35, digits_x = 1, digits_sd = 1, items_averaged = 1)  |> expect_equal(GRIMMER_test(5.2 , 2.5 , 35))

  # This value set is from `pigs5`. It used to be flagged as a test-3
  # inconsistency by `grimmer_scalar()`, but it is consistent according to both
  # the new version and rsprite2:
  2.57 |> grimmer_scalar(2.57, 30, digits_x = 2, digits_sd = 2, items_averaged = 1) |> expect_equal(GRIMMER_test(2.57, 2.57, 30))

  # Some finer variations:
  3.756 |> grimmer_scalar(4.485, 89, digits_x = 3, digits_sd = 3, items_averaged = 1) |> expect_equal(GRIMMER_test(3.756, 4.485, 89))
  3.756 |> grimmer_scalar(4.485, 12, digits_x = 3, digits_sd = 3, items_averaged = 1) |> expect_equal(GRIMMER_test(3.756, 4.485, 12))
  3.75  |> grimmer_scalar(4.48,  12, digits_x = 2, digits_sd = 2, items_averaged = 1) |> expect_equal(GRIMMER_test(3.75, 4.48, 12))
  3.75  |> grimmer_scalar(4.48,  89, digits_x = 2, digits_sd = 2, items_averaged = 1) |> expect_equal(GRIMMER_test(3.75, 4.48, 89))
})

test_that("GRIMMER works correctly with `items_averaged = 2`", {
  5.21 |> grimmer_scalar(1.60, 28, digits_x = 2, digits_sd = 2, items_averaged = 2) |> expect_equal(GRIMMER_test(5.21, 1.6 , 28, n_items = 2))
  3.44 |> grimmer_scalar(2.47, 18, digits_x = 2, digits_sd = 2, items_averaged = 2) |> expect_equal(GRIMMER_test(3.44, 2.47, 18, n_items = 2))
})

test_that("GRIMMER works correctly with `items_averaged = 3`", {
  5.21 |> grimmer_scalar(1.60, 28, digits_x = 2, digits_sd = 2, items_averaged = 3) |> expect_equal(GRIMMER_test(5.21, 1.6 , 28, n_items = 3))
  3.44 |> grimmer_scalar(2.47, 18, digits_x = 2, digits_sd = 2, items_averaged = 3) |> expect_equal(GRIMMER_test(3.44, 2.47, 18, n_items = 3))
})


# Issue #85 -----------------------------------------------------------------

test_that("GRIMMER checks SD-match and parity against the same candidate sum of squares (#85)", {
  # With 3+ candidate sums of squares, one candidate could satisfy the SD-match
  # test and a *different* candidate could satisfy the parity test, which used
  # to make `grimmer()` return `TRUE` even though no single candidate passed
  # both. This only affects `rounding = "up"`/`"down"`; see
  # https://github.com/lhdjung/scrutiny/issues/85.
  0.11 |>
    grimmer(sd = 0.87, n = 64, digits_x = 2, digits_sd = 2, items_averaged = 1, rounding = "up") |>
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
      grimmer(x = x, sd = sd, n = n, digits_x = 2, digits_sd = 2, items_averaged = 1, rounding = "up")
    })

  result_false |> any() |> expect_false()

  # The same value is genuinely GRIMMER-consistent under "up_or_down", where a
  # third candidate integer for the sum of squares is not at stake:
  0.11 |>
    grimmer(sd = 0.87, n = 64, digits_x = 2, digits_sd = 2, items_averaged = 1, rounding = "up_or_down") |>
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
  witness |> mean()                |> expect_equal(0.035)
  witness |> stats::sd() |> round(2) |> expect_equal(0.18)

  0.03 |> grimmer(sd = 0.18, n = 200, digits_x = 2, digits_sd = 2, items_averaged = 1)                  |> expect_true()
  0.04 |> grimmer(sd = 0.18, n = 200, digits_x = 2, digits_sd = 2, items_averaged = 1)                  |> expect_true()
  0.04 |> grimmer(sd = 0.18, n = 200, digits_x = 2, digits_sd = 2, items_averaged = 1, rounding = "up") |> expect_true()
})


test_that("GRIMMER admits no phantom candidate sums (#86)", {
  # The admissible sum range here is just {11}: a sum of 12 would mean a mean of
  # 0.075, which rounds up to 0.08 rather than 0.07. But `unround(0.07,
  # "up")$upper * 160` is 12.000000000000002, so 12 used to slip past the
  # exclusive upper bound and supply a parity-matching sum of squares. Note that
  # `grim()` itself passes here, so the GRIM gate cannot catch this.
  0.07 |> grim(n = 160, digits_x = 2, items_averaged = 1, rounding = "up") |> expect_true()
  0.07 |>
    grimmer(sd = 0.08, n = 160, digits_x = 2, digits_sd = 2, items_averaged = 1, rounding = "up") |>
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
        x          <- values   |> mean() |> reround(digits = 2, rounding = rounding) |> purrr::pluck(1L)
        sd_rounded <- sd_value |> reround(digits = 2, rounding = rounding) |> purrr::pluck(1L)
        consistent <- x |> grimmer(sd = sd_rounded, n = n, digits_x = 2, digits_sd = 2, items_averaged = 1, rounding = rounding)
        if (!isTRUE(consistent)) {
          false_flags <- false_flags + 1L
        }
      }
    }
  }

  false_flags |> expect_equal(0L)
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
  (witness / 3) |> mean()      |> round(2) |> expect_equal(7.67)
  (witness / 3) |> stats::sd()             |> expect_equal(0)

  7.67 |>
    grimmer(sd = 0, n = 2, items_averaged = 3, digits_x = 2, digits_sd = 2) |>
    expect_true()

  # Same mechanism at a range of magnitudes, all of them real data sets with
  # zero variance -- the case where the lower bound lands exactly on an
  # integer:
  false_flags <- 0L

  for (items in 2:6) {
    for (n in c(2, 3, 5, 10, 37)) {
      for (item_sum in c(23, 25, 28, 106, 400, 631, 1000, 2317)) {
        x <- (item_sum / items) |> reround(digits = 2) |> purrr::pluck(1L)
        consistent <- x |> grimmer(sd = 0, n = n, items_averaged = items, digits_x = 2, digits_sd = 2)
        if (!isTRUE(consistent)) {
          false_flags <- false_flags + 1L
        }
      }
    }
  }

  false_flags |> expect_equal(0L)
})


test_that("GRIMMER never flags a real multi-item data set", {
  set.seed(1234)
  false_flags <- 0L

  for (trial in 1:400) {
    items <- sample(2:6, 1)
    n <- sample(2:40, 1)
    value_max <- sample(c(3, 9, 60, 200), 1)
    values <- sample(0:value_max, n * items, replace = TRUE)
    scores <- colSums(matrix(values, nrow = items)) / items
    sd_value <- stats::sd(scores)
    x          <- scores   |> mean() |> reround(digits = 2) |> purrr::pluck(1L)
    sd_rounded <- sd_value |> reround(digits = 2) |> purrr::pluck(1L)
    consistent <- x |> grimmer(sd = sd_rounded, n = n, items_averaged = items, digits_x = 2, digits_sd = 2)
    if (!isTRUE(consistent)) {
      false_flags <- false_flags + 1L
    }
  }

  false_flags |> expect_equal(0L)
})


# Rounding methods ----------------------------------------------------------

test_that("GRIMMER supports the compound rounding methods", {
  # These used to abort, because `unround()` -- which supplied the SD bounds --
  # does not know them, even though `grim()` and `reround()` both do.
  for (rounding in c("ceiling_or_floor", "up_from_or_down_from")) {
    5.23 |>
      grimmer(
        sd = 2.55, n = 31, digits_x = 2, digits_sd = 2, items_averaged = 1,
        rounding = rounding, threshold = 3
      ) |>
      expect_type("logical")
  }
})


test_that("`symmetric` is passed on to the GRIM stage", {
  # `grimmer()` runs `grim()` first, so a mean that `symmetric` rules out must
  # make GRIMMER inconsistent as well:
  -0.07 |>
    grim(n = 40, digits_x = 2, items_averaged = 1, rounding = "up", symmetric = TRUE) |>
    expect_false()
  -0.07 |>
    grimmer(
      sd = 0.1, n = 40, digits_x = 2, digits_sd = 1, items_averaged = 1,
      rounding = "up", symmetric = TRUE
    ) |>
    expect_false()
})


test_that("GRIMMER returns `NA` where GRIM itself is undecidable", {
  # GRIMMER runs GRIM first and branches on its verdict, which is `NA` where the
  # mean's rounding bounds are undefined -- as with a missing mean -- and where
  # `n` leaves nothing to test. The branch used to fail on the `NA` with
  # "missing value where TRUE/FALSE needed" instead of passing it on. `debit()`
  # has the same test. (Until `anti_trunc()` stopped sending zero away from
  # zero, `rounding = "anti_trunc"` at a mean of zero was a second such case.)
  NA   |> grim(n = 40, digits_x = 2, items_averaged = 1)                              |> expect_na()
  NA   |> grimmer(sd = 0.41, n = 40, digits_x = 2, digits_sd = 2, items_averaged = 1) |> expect_na()
  1.03 |> grimmer(sd = 0.41, n = 0, digits_x = 2, digits_sd = 2, items_averaged = 1)  |> expect_na()

  # The reason names the stage the case got stuck at, the way the reason for an
  # undecidable SD does. It must not read as a GRIM *inconsistency*, which
  # `audit()` counts by matching that string:
  # (A missing `x` would be reported as `"Missing value"` by the mapper, which
  # screens for missingness ahead of the test, so the vehicle here is an `n`
  # that leaves nothing to test.)
  out <- tibble::tibble(x = c(1.03, 1.03), sd = c(0.41, 0.41), n = c(0L, 40L)) |>
    grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1)
  out$consistency |> expect_equal(c(NA, FALSE))
  out$reason[1L] |> expect_equal("No testable value set")
  out |> audit() |> purrr::pluck("fail_grim")    |> expect_equal(0L)
  out |> audit() |> purrr::pluck("incons_cases") |> expect_equal(1L)
})

# A brute-force oracle, complementing the random trials above. Those sample 400
# data sets at larger `n`; this one enumerates *every* multiset of `n` whole
# numbers for small `n`, so nothing in that space can hide. It checks one
# direction: every (mean, SD) pair that a real sample produces must pass. The
# brute force further below checks both.

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
      pairs <- n |> grimmer_multisets(0:5) |> grimmer_reported_pairs(digits, "up_or_down", items = 1)
      verdict <- purrr::map2_lgl(
        pairs[, 1L],
        pairs[, 2L],
        function(x, sd) {
          grimmer(
            x = x, sd = sd, n = n,
            digits_x = digits, digits_sd = digits, items_averaged = 1
          )
        }
      )
      n_checked <- n_checked + length(verdict)
      false_negatives <- which(!verdict %in% TRUE)
      pairs_rejected <- paste0("(", pairs[false_negatives, 1L], ", ", pairs[false_negatives, 2L], ")")
      pairs_rejected <- pairs_rejected |> utils::head(5L) |> toString()
      info <- paste0("n = ", n, ", digits = ", digits, " -- pairs: ", pairs_rejected)
      false_negatives |> length() |> expect_equal(0L, info = info)
    }
  }

  # Guard against the loops silently collapsing to nothing:
  n_checked |> expect_gt(500L)
})


test_that("GRIMMER never rejects an enumerable multi-item sample", {
  # `items = 2` halves the granularity of both the mean and the SD, which is
  # where the item multiplication in the sum-of-squares stage has to keep up:
  pairs <- 4L |> grimmer_multisets(0:6) |> grimmer_reported_pairs(digits = 2, rounding = "up_or_down", items = 2)
  verdict <- purrr::map2_lgl(
    pairs[, 1L],
    pairs[, 2L],
    function(x, sd) {
      grimmer(
        x = x, sd = sd, n = 4L, items_averaged = 2,
        digits_x = 2, digits_sd = 2
      )
    }
  )

  verdict |> call_on(\(x) !x %in% TRUE) |> sum() |> expect_equal(0L)
  verdict |> length()     |> expect_gt(100L)
})


test_that("an `n` too large to enumerate is an error, not a hang", {
  5.19 |> grimmer(sd = 2.5, n = 3e9, digits_x = 2, digits_sd = 2, items_averaged = 1) |> expect_error("too large for GRIMMER")
  5.19 |> grimmer(sd = 2.5, n = 1e8, digits_x = 2, digits_sd = 2, items_averaged = 1) |> expect_error("too large for GRIMMER")

  # The limit is far above anything a published summary statistic looks like,
  # so a large but plausible `n` still goes through:
  5.19 |> grimmer(sd = 2.5, n = 1e6, digits_x = 2, digits_sd = 2, items_averaged = 1) |> expect_type("logical")
})


test_that("a wide range of sums of squares is no error at a small `n`", {
  # A large SD reported with few decimals spans millions of sums of squares.
  # These used to trip the limit meant for a huge `n`:
  35000.5 |> grimmer(sd = 50000, n = 20, digits_x = 1, digits_sd = 0, items_averaged = 1)  |> expect_true()
  512.3   |> grimmer(sd = 260, n = 2500, digits_x = 1, digits_sd = 0, items_averaged = 1)  |> expect_true()
  45000.5 |> grimmer(sd = 12000, n = 100, digits_x = 1, digits_sd = 0, items_averaged = 1) |> expect_true()
})


test_that("a negative SD is inconsistent, and says why", {
  5 |> grimmer(-0.5, 10, 2, 2, items_averaged = 1) |> expect_false()
  5 |>
    grimmer_scalar(-0.5, 10, 2, 2, show_reason = TRUE, items_averaged = 1) |>
    expect_equal(list(FALSE, "Negative SD"))
})


test_that("`sum_squares_min()` is the smallest sum of squares of whole numbers", {
  for (n in 2:4) {
    m <- n |> grimmer_multisets(-3:6)
    sums <- colSums(m)
    squares <- colSums(m^2)
    # Sums whose closest whole numbers, `q` and `q + 1`, are within `-3:6`:
    for (s in (-3L * n):(5L * n)) {
      info <- paste0("n = ", n, ", s = ", s)
      s |> sum_squares_min(n) |> expect_equal(min(squares[sums == s]), info = info)
    }
  }
})


test_that("GRIMMER rejects a sum of squares below what whole numbers can reach", {
  # The SD admits only 146, but 11 whole numbers summing to 40 reach 148 at
  # least: seven 4s and four 3s.
  3.6 |> grimmer(0.2, 11, digits_x = 1, digits_sd = 1, items_averaged = 1) |> expect_false()
  3.6 |> grimmer(0.5, 11, digits_x = 1, digits_sd = 1, items_averaged = 1) |> expect_true()
  # Here, the minimum of 91 is part of the range from 90 to 91:
  1.9 |> grimmer(0.3, 25, digits_x = 1, digits_sd = 1, items_averaged = 1) |> expect_true()
  # With two items, the whole numbers are the respondents' totals. The SD admits
  # only 109, but 10 totals summing to 33 reach 111 at least:
  1.65 |> grimmer(0.05, 10, digits_x = 2, digits_sd = 2, items_averaged = 2) |> expect_false()

  3.6 |>
    grimmer_scalar(0.2, 11, 1, 1, show_reason = TRUE, items_averaged = 1) |>
    expect_equal(list(FALSE, "GRIMMER inconsistent (test 1)"))
})


test_that("`sum_squares_attainable()` agrees with every sample of fewer than five", {
  for (n in 2:4) {
    m <- n |> grimmer_multisets(-6:6)
    sums <- colSums(m)
    squares <- colSums(m^2)
    # A sum of squares of at most 36 keeps every number within `-6:6`:
    for (s in (-2L * n):(2L * n)) {
      attained <- unique(squares[sums == s & squares <= 36])
      first <- sum_squares_min(s, n)
      for (q in first:36) {
        info <- paste0("n = ", n, ", s = ", s, ", q = ", q)
        found <- s |> sum_squares_attainable(n, q, q)
        found |> is.na() |> expect_equal(!q %in% attained, info = info)
        # Over a range, it finds the smallest attainable one from `q` on:
        upper <- min(q + 7, 36)
        expected <- attained[attained >= q & attained <= upper]
        expected <- if (length(expected) == 0L) NA_real_ else min(expected)
        s |> sum_squares_attainable(n, q, upper) |> expect_equal(expected, info = info)
      }
    }
  }
})


test_that("`sum_squares_attainable()` finds the next square for two numbers without a scan", {
  # The excess of two numbers is `2 * e^2` for an even sum, so the next one
  # above `1e9` is `2 * 22361^2`; a scan used to allocate the whole range:
  minimum <- sum_squares_min(1e6, 2)
  1e6 |> sum_squares_attainable(2, minimum + 1e9, minimum + 2e9) |> expect_equal(minimum + 2 * 22361^2)
  1e6 |> sum_squares_attainable(2, minimum + 1e9, minimum + 1e9) |> expect_na()
  # For an odd sum, `2 * excess + 1` is an odd square: `44723^2` is the next
  # above `2e9 + 1`:
  minimum <- sum_squares_min(1e6 + 1, 2)
  (1e6 + 1) |> sum_squares_attainable(2, minimum + 1e9, minimum + 2e9) |> expect_equal(minimum + (44723^2 - 1) / 2)
  # Many items averaged make the range of sums of squares that wide:
  2.5 |> grimmer(1.2, 2, digits_x = 2, digits_sd = 2, items_averaged = 50000) |> expect_true()
  2.5 |> grimmer(1.2, 3, digits_x = 2, digits_sd = 2, items_averaged = 50000) |> expect_true()
})


test_that("GRIMMER rejects sums of squares that fewer than five numbers cannot have", {
  # Two whole numbers with a sum of 2 and a sum of squares of 6 would be
  # `1 +/- sqrt(2)`:
  1   |> grimmer(2,   2, digits_x = 1, digits_sd = 1, items_averaged = 1) |> expect_false()
  1   |> grimmer(1.4, 3, digits_x = 1, digits_sd = 1, items_averaged = 1) |> expect_false()
  1.5 |> grimmer(1.5, 4, digits_x = 1, digits_sd = 1, items_averaged = 1) |> expect_false()
  # Four numbers with a sum of 4 cannot have 96, but 98, as -7, 2, 3, 6:
  1   |> grimmer(6,   4, digits_x = 1, digits_sd = 0, items_averaged = 1) |> expect_true()

  1 |>
    grimmer_scalar(2, 2, 1, 1, show_reason = TRUE, items_averaged = 1) |>
    expect_equal(list(FALSE, "GRIMMER inconsistent (test 1)"))
})


# Every pair of a sum and a sum of squares that `k` numbers from `vals` can
# have, for `k` up to `n_max`, built up one number at a time from the empty
# sample, with sums in rows and sums of squares in columns. It shares nothing
# with `grimmer()`: no rounding bounds, no candidate sums, only integer
# arithmetic. Means from 1 to 5 and SDs up to 2.5 keep every value of up to 8
# numbers within `-6:12`, so the pairs are complete there.
grimmer_reachable_pairs <- function(vals = -6:12, n_max = 8L) {
  s_min <- min(vals) * n_max
  rows <- max(vals) * n_max - s_min + 1L
  cols <- max(vals^2) * n_max + 1L

  reach <- matrix(FALSE, rows, cols)
  reach[1L - s_min, 1L] <- TRUE
  pairs <- vector("list", n_max)
  for (k in seq_len(n_max)) {
    reach_next <- matrix(FALSE, rows, cols)
    for (v in vals) {
      from_row <- max(1L, 1L - v):min(rows, rows - v)
      from_col <- seq_len(cols - v^2)
      to_row <- from_row + v
      to_col <- from_col + v^2
      reach_next[to_row, to_col] <- reach_next[to_row, to_col] | reach[from_row, from_col]
    }
    reach <- reach_next
    found <- which(reach, arr.ind = TRUE)
    pairs[[k]] <- list(s = found[, 1L] + s_min - 1L, q = found[, 2L] - 1L)
  }
  pairs
}


test_that("GRIMMER agrees with a brute force on random value sets", {
  # The truth is whether any sample of `n` whole numbers has a mean and an SD
  # that round to the reported ones, with both bounds inclusive as under
  # `"up_or_down"`:
  pairs <- grimmer_reachable_pairs()

  # `|s / n - x| <= 0.05` and `(sd - 0.05)^2 <= var <= (sd + 0.05)^2`, with
  # `var = (n * q - s^2) / (n * (n - 1))`, both cleared of fractions:
  any_sample <- function(x, sd, n) {
    s <- pairs[[n]]$s
    q <- pairs[[n]]$q
    x10 <- round(10 * x)
    sd10 <- round(10 * sd)
    mean_ok <- 2 * abs(10 * s - x10 * n) <= n
    var400 <- 400 * (n * q - s^2)
    var_min <- if (sd10 == 0) 0 else (2 * sd10 - 1)^2 * n * (n - 1)
    var_max <- (2 * sd10 + 1)^2 * n * (n - 1)
    any(mean_ok & var400 >= var_min & var400 <= var_max)
  }

  set.seed(42)
  n <- rep(2:8, each = 1500L)
  x <- round(runif(length(n), 1, 5), 1)
  sd <- round(runif(length(n), 0, 2.5), 1)

  truth <- mapply(any_sample, x, sd, n)
  verdict <- grimmer(x, sd, n, digits_x = 1, digits_sd = 1, items_averaged = 1)
  verdict |> expect_equal(truth)
  # Both regimes are well represented: the exceptions at `n < 5` and the exact
  # verdicts from the minimum and the parity at `n >= 5`:
  truth |> sum() |> expect_equal(3125L)
})


test_that("GRIMMER agrees with a brute force under every rounding method and with two items", {
  # The same samples, rounded forward with `reround()`, which `grimmer()` does
  # not use: it inverts the rounding with `unround()`, whose bounds
  # test-unround.R checks against `reround()`. A reported pair is reachable if
  # some sample rounds to it. Under `"even"`, a tie counts either way, as under
  # `"up_or_down"` (see `?grimmer`). With two items, the whole numbers are the
  # respondents' totals, so the means and SDs are theirs divided by 2; the
  # smaller ranges of `x` and `sd` keep every total within `-6:12`.
  pairs <- grimmer_reachable_pairs()
  reachable <- function(n, items, rounding) {
    s <- pairs[[n]]$s
    q <- pairs[[n]]$q
    rounding_forward <- if (rounding == "even") "up_or_down" else rounding
    means <- reround(s / (n * items), 1, rounding_forward)
    sds <- reround(sqrt((n * q - s^2) / (n * (n - 1))) / items, 1, rounding_forward)
    # A compound `rounding` returns both variants per sample, interleaved:
    means <- matrix(round(10 * means), ncol = length(s))
    sds <- matrix(round(10 * sds), ncol = length(s))
    keys <- character(0L)
    for (i in seq_len(nrow(means))) {
      for (j in seq_len(nrow(sds))) {
        keys <- c(keys, paste(means[i, ], sds[j, ]))
      }
    }
    unique(keys)
  }

  roundings <- c("up_or_down", "up", "down", "even", "ceiling", "floor", "trunc", "anti_trunc")
  set.seed(42)
  n_checked <- 0L
  for (items in 1:2) {
    for (rounding in roundings) {
      for (n in 2:8) {
        x <- round(runif(60L, 1, if (items == 1L) 5 else 3), 1)
        sd <- round(runif(60L, 0, if (items == 1L) 2.5 else 1), 1)
        truth <- paste(round(10 * x), round(10 * sd)) %in% reachable(n, items, rounding)
        verdict <- grimmer(x, sd, n, digits_x = 1, digits_sd = 1, items_averaged = items, rounding = rounding)
        info <- paste0("items = ", items, ", rounding = ", rounding, ", n = ", n)
        verdict |> expect_equal(truth, info = info)
        n_checked <- n_checked + sum(truth)
      }
    }
  }
  # Consistent cases are well represented:
  n_checked |> expect_gt(1000L)
})


test_that("GRIMMER agrees with an exact oracle at two decimal places for `n` up to 12", {
  # The sums and sums of squares that `n` whole numbers from `values` can have,
  # found by adding one number at a time: cheap where enumerating the samples
  # themselves is not. With two decimal places, the reported pairs are far
  # sparser than above, so this is a sharper check of the exact verdict for
  # `n >= 5`, from the minimum and the parity of the sum of squares alone.
  reachable_sums <- function(n, values = -8:12) {
    s_max <- n * max(abs(values))
    q_max <- n * max(values^2)
    reach <- matrix(FALSE, 2 * s_max + 1, q_max + 1)
    reach[s_max + 1, 1] <- TRUE
    for (i in seq_len(n)) {
      cells <- which(reach, arr.ind = TRUE)
      reach[] <- FALSE
      for (v in values) {
        reach[cbind(cells[, 1] + v, cells[, 2] + v^2)] <- TRUE
      }
    }
    cells <- which(reach, arr.ind = TRUE)
    list(s = cells[, 1] - s_max - 1, q = cells[, 2] - 1)
  }

  # As in the test above, but with the forward rounding exact: a mean or SD on
  # a tie is reported either way under `"up_or_down"`.
  reachable <- function(n) {
    s <- reachable_sums(n)$s
    q <- reachable_sums(n)$q
    means <- reround(s / n, 2, "up_or_down")
    sds <- reround(sqrt((n * q - s^2) / (n * (n - 1))), 2, "up_or_down")
    means <- matrix(round(100 * means), ncol = length(s))
    sds <- matrix(round(100 * sds), ncol = length(s))
    unique(c(paste(means[1, ], sds[1, ]), paste(means[1, ], sds[2, ]), paste(means[2, ], sds[1, ]), paste(means[2, ], sds[2, ])))
  }

  grid <- expand.grid(x = seq(0.5, 2.5, by = 0.01), sd = seq(0, 1.5, by = 0.01))
  for (n in c(9L, 12L)) {
    truth <- paste(round(100 * grid$x), round(100 * grid$sd)) %in% reachable(n)
    verdict <- grimmer(round(grid$x, 2), round(grid$sd, 2), n, digits_x = 2, digits_sd = 2, items_averaged = 1)
    verdict |> expect_equal(truth, info = paste("n =", n))
    truth |> sum() |> expect_gt(100L)
  }
})


# SD type (#95) -----------------------------------------------------------

# 2, 4, 4, 5, 7 have a mean of 4.4, a sample SD of 1.82, and a population SD of
# 1.62.

test_that("`sd_type` decides which SD a value set is consistent with", {
  4.4 |> grimmer(1.62, 5, 1, 2, 1)                         |> expect_false()
  4.4 |> grimmer(1.62, 5, 1, 2, 1, sd_type = "population") |> expect_true()
  4.4 |> grimmer(1.62, 5, 1, 2, 1, sd_type = "either")     |> expect_true()
  4.4 |> grimmer(1.82, 5, 1, 2, 1, sd_type = "population") |> expect_false()
  4.4 |> grimmer(1.82, 5, 1, 2, 1, sd_type = "either")     |> expect_true()
})

test_that("`sd_type` is checked with `rlang::arg_match()`", {
  4.4 |> grimmer(1.62, 5, 1, 2, 1, sd_type = "pop") |> expect_error("must be one of")
  4.4 |> grimmer(1.62, 5, 1, 2, 1, sd_type = NA)    |> expect_error("must be a")
  # The error names the function the user called:
  error <- tibble::tibble(x = 4.4, sd = 1.62, n = 5) |>
    grimmer_map(digits_x = 1, digits_sd = 2, items_averaged = 1, sd_type = "Sample") |>
    expect_error("must be one of")
  error$call[[1L]] |> expect_equal(quote(grimmer_map))
})

test_that("GRIMMER agrees with a brute force on population SDs", {
  # As in the test with every rounding method above, but with the population
  # SD, which divides by `n` rather than `n - 1`, so `n = 1` is testable too:
  pairs <- grimmer_reachable_pairs()
  reachable <- function(n, items, rounding, sd_type) {
    s <- pairs[[n]]$s
    q <- pairs[[n]]$q
    n_dev <- if (sd_type == "population") n else n - 1
    rounding_forward <- if (rounding == "even") "up_or_down" else rounding
    means <- reround(s / (n * items), 1, rounding_forward)
    sds <- reround(sqrt((n * q - s^2) / (n * n_dev)) / items, 1, rounding_forward)
    means <- matrix(round(10 * means), ncol = length(s))
    sds <- matrix(round(10 * sds), ncol = length(s))
    keys <- character(0L)
    for (i in seq_len(nrow(means))) {
      for (j in seq_len(nrow(sds))) {
        keys <- c(keys, paste(means[i, ], sds[j, ]))
      }
    }
    unique(keys)
  }

  set.seed(95)
  n_checked <- 0L
  for (items in 1:2) {
    for (rounding in c("up_or_down", "up", "even", "floor")) {
      for (n in 1:8) {
        x <- round(runif(60L, 1, if (items == 1L) 5 else 3), 1)
        sd <- round(runif(60L, 0, if (items == 1L) 2.5 else 1), 1)
        key <- paste(round(10 * x), round(10 * sd))
        is_population <- key %in% reachable(n, items, rounding, "population")
        # A single value has no sample SD:
        is_sample <- if (n == 1L) FALSE else key %in% reachable(n, items, rounding, "sample")
        info <- paste0("items = ", items, ", rounding = ", rounding, ", n = ", n)

        x |>
          grimmer(sd, n, 1, 1, items, rounding = rounding, sd_type = "population") |>
          expect_equal(is_population, info = info)
        x |>
          grimmer(sd, n, 1, 1, items, rounding = rounding, sd_type = "either") |>
          expect_equal(is_sample | is_population, info = info)
        n_checked <- n_checked + sum(is_population)
      }
    }
  }
  n_checked |> expect_gt(500L)
})

test_that("`grimmer_map()` names the SD type that a value set passed under", {
  out <- tibble::tibble(x = c(4.4, 4.4, 3.6, 4), sd = c(1.82, 1.62, 0.2, 0.5), n = c(5, 5, 11, 1)) |>
    grimmer_map(digits_x = 1, digits_sd = 2, items_averaged = 1, sd_type = "either")

  # A single value has no sample SD, so the population SD decides:
  out$consistency |> expect_equal(c(TRUE, TRUE, FALSE, FALSE))
  out$reason |> expect_equal(c("Passed all (sample SD)", "Passed all (population SD)", "GRIMMER inconsistent (test 1)", "GRIMMER inconsistent (test 1)"))
  out |> scrutiny_meta() |> _$args$sd_type |> expect_equal("either")
})

test_that("`sd_type` reaches the sequence and total-n mappers", {
  df <- tibble::tibble(x = 4.4, sd = 1.62, n = 5)
  df |> grimmer_map_seq(digits_x = 1, digits_sd = 2, items_averaged = 1)                         |> nrow() |> expect_gt(0L)
  df |> grimmer_map_seq(digits_x = 1, digits_sd = 2, items_averaged = 1, sd_type = "population") |> expect_warning("No inconsistent cases")
  df |> grimmer_map_seq(digits_x = 1, digits_sd = 2, items_averaged = 1, sd_type = "population") |> suppressWarnings() |> nrow() |> expect_equal(0L)

  seq_either <- df |> grimmer_map_seq(digits_x = 1, digits_sd = 2, items_averaged = 1, sd_type = "either", include_consistent = TRUE)
  seq_either |> scrutiny_meta() |> _$fun_args$sd_type |> expect_equal("either")
  seq_either |> audit_seq() |> nrow() |> expect_equal(1L)

  # Only groups of 5 and 5 fit, and only as population SDs:
  total <- tibble::tibble(x1 = 4.4, x2 = 4.4, sd1 = 1.62, sd2 = 1.62, n = 10)
  total |> grimmer_map_total_n(digits_x = 1, digits_sd = 2, items_averaged = 1)                         |> _$both_consistent |> any() |> expect_false()
  total |> grimmer_map_total_n(digits_x = 1, digits_sd = 2, items_averaged = 1, sd_type = "population") |> _$both_consistent |> which() |> expect_equal(1:2)
})

test_that("`audit()` counts each \"either\" case once, by the population SD's reason", {
  out <- tibble::tibble(x = 4.59, sd = 1.96, n = 17) |> grimmer_map(digits_x = 2, digits_sd = 2, items_averaged = 1, sd_type = "either")
  out$reason |> expect_equal("Sample SD: GRIMMER inconsistent (test 1); population SD: GRIMMER inconsistent (test 3)")
  audit <- out |> audit()
  audit$fail_test1 |> expect_equal(0L)
  audit$fail_test3 |> expect_equal(1L)
})

test_that("the sequence mapper disperses `n` down to 1 only for a population SD", {
  df <- tibble::tibble(x = 2.5, sd = 0.9, n = 3)
  seq_n <- function(sd_type) {
    df |> grimmer_map_seq(digits_x = 1, digits_sd = 1, items_averaged = 1, sd_type = sd_type, var = "n", include_consistent = TRUE) |> _$n |> min()
  }
  "sample"     |> seq_n() |> expect_equal(2L)
  "population" |> seq_n() |> expect_equal(1L)
  "either"     |> seq_n() |> expect_equal(1L)
})

test_that("explanations agree with the test under every SD type", {
  grid <- tidyr::expand_grid(x = c(3.6, 4.4, 5.2), sd = c(0, 0.5, 1.62, 1.82), n = c(1, 2, 5, 11))
  for (sd_type in c("sample", "population", "either")) {
    verdict <- grid$x |> grimmer(grid$sd, grid$n, 1, 2, 1, sd_type = sd_type)
    explained <- grid |>
      purrr::pmap_lgl(function(x, sd, n) {
        explanation <- x |> grimmer_explain(sd, n, 1, 2, 1, sd_type = sd_type)
        explanation$consistency
      })
    explained |> expect_equal(verdict, info = sd_type)
  }

  explanation <- 4.4 |> grimmer_explain(1.62, 5, 1, 2, 1, sd_type = "either") |> format()
  explanation |> expect_match("sd_type = \"either\"", fixed = TRUE)
  explanation |> expect_match("population SD, which divides by n rather than n - 1", fixed = TRUE)
  explanation |> expect_match("==> Consistent", fixed = TRUE)

  # At `n = 1`, only the sample SD is undecidable; at `n = 1.5`, both are, and
  # the population SD's requirement is the one stated:
  4 |> grimmer_explain(0.5, 1,   1, 1, 1, sd_type = "either") |> format() |> expect_match("A single value has no sample SD", fixed = TRUE)
  4 |> grimmer_explain(0.5, 1.5, 1, 1, 1, sd_type = "either") |> format() |> expect_no_match("sample", fixed = TRUE)

  out <- tibble::tibble(x = c(4.4, 4.4), sd = c(1.62, 1.82), n = 5) |>
    grimmer_map(digits_x = 1, digits_sd = 2, items_averaged = 1, sd_type = "population") |>
    explain_map()
  out$explanation |> expect_match("For a population SD,\\s+it is 5 \\* SD\\^2")
})

test_that("`grimmer_plot_values()` takes `sd_type`", {
  4.4 |> grimmer_plot_values(1.62, 5, 1, 2, 1, sd_type = "population") |> expect_s3_class("ggplot")
  4   |> grimmer_plot_values(0, 1, 1, 2, 1, sd_type = "either")        |> expect_s3_class("ggplot")
  4   |> grimmer_plot_values(0.5, 1, 1, 2, 1, sd_type = "either")      |> expect_s3_class("ggplot")
  4   |> grimmer_plot_values(0, 1, 1, 2, 1)                            |> expect_error("at least 2")
})
