#
#
# t_stat <- function (m1, sd1, n1, m2, sd2, n2) {
#   var.p <- (((sd1 ^ 2) * (n1 - 1)) + ((sd2 ^ 2) * (n2 - 1))) / (n1 + n2 - 2)
#   se.p <- sqrt((var.p / n1) + (var.p / n2))
#   result <- (m1 - m2) / se.p
#   return(result)
# }
#
#
#
# rivets_perfect_mean_sd <- function (x1, sd1, n1, x2, sd2, n2,
#                                     rounding, threshold = 5,
#                                     symmetric = FALSE, n_sample = 1000000,
#                                     grim_test = FALSE, grim_items = 1) {
#
#   # Checks ---
#
#   check_type(x1,  "character")
#   check_type(x2,  "character")
#   check_type(sd1, "character")
#   check_type(sd2, "character")
#
#   digits_x1 <- decimal_places_scalar(x1)
#   digits_x2 <- decimal_places_scalar(x2)
#
#   if (digits_x1 != digits_x2) {
#     cli::cli_abort(c(
#       "`x1` and `x2` have different numbers of decimal places",
#       "x" = "`x1`(`{x1}`) has {digits}, `x2` (`{x2}`) has {digits_x2}."
#     ))
#   }
#
#
#   # Main part ---
#
#   # RIVETS core (?): `p10`
#   p10 <- 10 ^ digits
#
#   # RIVETS core: `reround()` calls
#   x1_rounded  <- reround(
#     x = x1,  digits = digits, rounding = rounding,
#     threshold = threshold, symmetric = symmetric
#   )
#   sd1_rounded <- reround(
#     x = sd1, digits = digits, rounding = rounding,
#     threshold = threshold, symmetric = symmetric
#   )
#   x2_rounded  <- reround(
#     x = x2,  digits = digits, rounding = rounding,
#     threshold = threshold, symmetric = symmetric
#   )
#   sd2_rounded <- reround(
#     x = sd2, digits = digits, rounding = rounding,
#     threshold = threshold, symmetric = symmetric
#   )
#
#   # [NICK] t_published ["t.pub"] is the statistic we will get from the rounded means and sds.
#   # [NICK] For simplicity of the code with min/max and sign changes, we use 0 as one of the means.
#   diff_means <- (x1_rounded - x2_rounded)
#   t_published <- t_stat(
#     m1 = diff_means, sd1 = sd1_rounded, n1 = n1,
#     m2 = 0,  sd2 = sd2_rounded, n2 = n2
#   )
#
#   # RIVETS core: `reround()` call to reconstruct the rounded test statistic
#   # (replacing Nick's `round(t_published, digits)` call)
#   t_published <- reround(
#     x = t_published, digits = digits, rounding = rounding,
#     threshold = threshold, symmetric = symmetric
#   )
#
#   # [NICK] Calculate the minimum and maximum t statistics.
#   diff_half <- 0.49999999 / p10
#   diff_min_max <- 2 * diff_half * sign(diff_means)
#
#   diff_means_min <- diff_means - diff_min_max
#   diff_means_max <- diff_means + diff_min_max
#
#   t_min <- t_stat(
#     x1 = diff_means_min, sd1 = sd1_rounded + d, n1 = n1,
#     x2 = 0, sd2 = sd2_rounded + d, n2 = n2
#   )
#   t_max <- t_stat(
#     x1 = diff_means_max, sd1 = sd1_rounded - d, n1 = n1,
#     x2 = 0, sd2 = sd2_rounded - d, n2 = n2
#   )
#
#   t_min <- reround(
#     x = t_min, digits = digits, rounding = rounding,
#     threshold = threshold, symmetric = symmetric
#   )
#   t_max <- reround(
#     x = t_max, digits = digits, rounding = rounding,
#     threshold = threshold, symmetric = symmetric
#   )
#
#   if (sign(diff_means) == -1) {
#     placeholder <- t_max
#     t_max <- t_min
#     t_min <- placeholder
#   }
#
#   # [NICK] Generate plausible unrounded mean and SD values.
#   sds_sample_1 <- rep(sd1_rounded, n_sample) + ((runif(n_sample) - 0.5) / p10)
#   sds_sample_2 <- rep(sd2_rounded, n_sample) + ((runif(n_sample) - 0.5) / p10)
#
#   if (grim_test) {
#     means_possible_1 <- rivets_possible_values(x1, n1, grim_items, digits)  # CHECK ARGS HERE
#     means_possible_2 <- rivets_possible_values(x2, n2, grim_items, digits)  # CHECK ARGS HERE
#
#     means_sample_1 <- sample(means_possible_1, n_sample, replace = TRUE)
#     # [NICK]    sds_sample_1 <- rep(sd1_rounded, n_sample) + round(((runif(n_sample) - 0.5) / p10), digits + 1) * 0.9999
#     gsds_sample_1 <<- sds_sample_1
#     gsd1_rounded <<- sd1_rounded
#
#     means_sample_2 <- sample(means_possible_2, n_sample, replace = TRUE)
#     # [NICK]    sds_sample_2 <- rep(sd2_rounded, n_sample) + round(((runif(n_sample) - 0.5) / p10), digits + 1) * 0.9999
#     cat("#M=", length(means_possible_1), "/", length(means_possible_2), " #SD=", length(table(sds_sample_1)), "/", length(table(sds_sample_2)), "\n", sep="")
#   }
#   else {
#     means_sample_1 <- rep(x1_rounded, n_sample)  + ((runif(n_sample) - 0.5) / p10)
#     means_sample_2 <- rep(x2_rounded, n_sample)  + ((runif(n_sample) - 0.5) / p10)
#     # Are the next two lines necessary? The same calls were made above
#     sds_sample_1   <- rep(sd1_rounded, n_sample) + ((runif(n_sample) - 0.5) / p10)
#     sds_sample_2   <- rep(sd2_rounded, n_sample) + ((runif(n_sample) - 0.5) / p10)
#   }
#
#   # [NICK] Sanity check that generated numbers round to published ones.
#   x1.sc <- sum(round(means_sample_1, digits) - x1_rounded)
#   sd1.sc <- sum(round(sds_sample_1, digits) - sd1_rounded)
#   x2.sc <- sum(round(means_sample_2, digits) - x2_rounded)
#   sd2.sc <- sum(round(sds_sample_2, digits) - sd2_rounded)
#   if ((x1.sc != 0) || (sd1.sc != 0) || (x2.sc != 0) || (sd2.sc != 0)) {
#     print(paste(x1.sc, sd1.sc, x2.sc, sd2.sc))
#     stop("Bad rounding of mean or SD")
#   }
#
#   # [NICK] Generate the t values from our "ragged" means and SDs.
#   t.ns <- t_stat(means_sample_1, sds_sample_1, n1, means_sample_2, sds_sample_2, n2)
#
#   # [NICK] Round the t values and see how many match the published one.
#   t.round <- round(t.ns, digits)
#   t_minobs <- min(t.round)
#   t_maxobs <- max(t.round)
#   t.exact <- (t.round == t_published)
#   frac.exact <- sum(t.exact) / n_sample
#   return(list(t_published, t_min, t_max, frac.exact, t_minobs, t_maxobs))
# }
