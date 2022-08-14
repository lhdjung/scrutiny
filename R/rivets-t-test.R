# #
# #
# # # Source: Nick Brown; https://osf.io/7fdxh/
# #
# #
# # # Simulate "perfect t statistics" from M/SD/N or b/SE.
# #
# # # Helper function to calculate the t statistic for two independent samples.
# # t.stat <- function (m1, sd1, n1, m2, sd2, n2) {
# #   var.p <- (((sd1 ^ 2) * (n1 - 1)) + ((sd2 ^ 2) * (n2 - 1))) / (n1 + n2 - 2)
# #   se.p <- sqrt((var.p / n1) + (var.p / n2))
# #   result <- (m1 - m2) / se.p
# #
# #   return(result)
# # }
# #
# #
# # # Check that a mean is GRIM-compatible.
# check.GRIM <- function (tMean, N, dp) {
#   dust <- 1e-12
#   gMean <- tMean
#   int <- round(tMean * N)         # nearest integer; doesn't matter if this rounds up or down
#   frac <- int / N
#   dif <- abs(tMean - frac)
#   gran <- ((0.1 ^ dp) / 2) + dust   # allow for rounding errors
#   if (dif > gran) {
#     gMean <- round(int / N, dp)
#     dpformat <- paste("%.", dp, "f", sep="")
#     s <- paste("Mean ", sprintf(dpformat, tMean), " fails GRIM test - using ", sprintf(dpformat, gMean), sep="")
#     warning(s)
#   }
#
#   return(gMean)
# }
#
#
#
# # # Make a vector of possible GRIM-compatible means.
# make.poss <- function (m.p, n, items, dp=2) {
#   N <- n * items      # effective sample size for GRIM is total number of items completed by participants
#   m <- check.GRIM(m.p, N, dp)
#
#   p10 <- 10 ^ dp
#   d <- 0.49999999 / p10
#   m.min <- m - d
#   m.max <- m + d
#   gran <- 1 / N
#
#   m.poss <- c(m)
#
#   m.cur <- m
#   repeat {
#     m.cur <- m.cur - gran
#     if (m.cur < m.min) {
#       break
#     }
#     m.poss <- c(m.poss, m.cur)
#   }
#
#   m.cur <- m
#   repeat {
#     m.cur <- m.cur + gran
#     if (m.cur > m.max) {
#       break
#     }
#     m.poss <- c(m.poss, m.cur)
#   }
#
#   if (length(m.poss) == 1) {      # need at least 2 items so sample() sees a vector
#     m.poss <- c(m.poss, m.poss)
#   }
#
#   return(m.poss)
# }
#
#
# # # dp: number of decimal places
# # # nsamp: number of samples to try
# perfectMSD <- function (m1, sd1, n1, m2, sd2, n2, dp=2, nsamp=1000000, GRIMitems=0) {
#   p10 <- 10 ^ dp
#   m1.r <- round(m1, dp)
#   sd1.r <- round(sd1, dp)
#   m2.r <- round(m2, dp)
#   sd2.r <- round(sd2, dp)
#
#   # t.pub is the statistic we will get from the rounded means and sds.
#   # For simplicity of the code with min/max and sign changes, we use 0 as one of the means.
#   md <- (m1.r - m2.r)
#   t.pub <- round(t.stat(md, sd1.r, n1, 0, sd2.r, n2), dp)
#
#   # Calculate the minimum and maximum t statistics.
#   d <- 0.49999999 / p10
#   dm <- 2 * d * sign(md)
#   md.min <- md - dm
#   t.min <- round(t.stat(md.min, sd1.r + d, n1, 0, sd2.r + d, n2), dp)
#   md.max <- md + dm
#   t.max <- round(t.stat(md.max, sd1.r - d, n1, 0, sd2.r - d, n2), dp)
#   if (sign(md) == -1) {
#     x <- t.max
#     t.max <- t.min
#     t.min <- x
#   }
#
#   # Generate plausible unrounded mean and SD values.
#   sd1.ns <- rep(sd1.r, nsamp) + ((runif(nsamp) - 0.5) / p10)
#   sd2.ns <- rep(sd2.r, nsamp) + ((runif(nsamp) - 0.5) / p10)
#
#   if (GRIMitems > 0) {
#     poss.1 <- make.poss(m1, n1, GRIMitems, dp)
#     m1.ns <- sample(poss.1, nsamp, replace=TRUE)
#     #    sd1.ns <- rep(sd1.r, nsamp) + round(((runif(nsamp) - 0.5) / p10), dp + 1) * 0.9999
#     gsd1.ns <<- sd1.ns
#     gsd1.r <<- sd1.r
#
#     poss.2 <- make.poss(m2, n2, GRIMitems, dp)
#     m2.ns <- sample(poss.2, nsamp, replace=TRUE)
#     #    sd2.ns <- rep(sd2.r, nsamp) + round(((runif(nsamp) - 0.5) / p10), dp + 1) * 0.9999
#     cat("#M=", length(poss.1), "/", length(poss.2), " #SD=", length(table(sd1.ns)), "/", length(table(sd2.ns)), "\n", sep="")
#   }
#   else {
#     m1.ns <- rep(m1.r, nsamp) + ((runif(nsamp) - 0.5) / p10)
#     m2.ns <- rep(m2.r, nsamp) + ((runif(nsamp) - 0.5) / p10)
#     sd1.ns <- rep(sd1.r, nsamp) + ((runif(nsamp) - 0.5) / p10)
#     sd2.ns <- rep(sd2.r, nsamp) + ((runif(nsamp) - 0.5) / p10)
#   }
#
#   # Sanity check that generated numbers round to published ones.
#   m1.sc <- sum(round(m1.ns, dp) - m1.r)
#   sd1.sc <- sum(round(sd1.ns, dp) - sd1.r)
#   m2.sc <- sum(round(m2.ns, dp) - m2.r)
#   sd2.sc <- sum(round(sd2.ns, dp) - sd2.r)
#   if ((m1.sc != 0) || (sd1.sc != 0) || (m2.sc != 0) || (sd2.sc != 0)) {
#     print(paste(m1.sc, sd1.sc, m2.sc, sd2.sc))
#     stop("Bad rounding of mean or SD")
#   }
#
#   # Generate the t values from our "ragged" means and SDs.
#   t.ns <- t.stat(m1.ns, sd1.ns, n1, m2.ns, sd2.ns, n2)
#
#   # Round the t values and see how many match the published one.
#   t.round <- round(t.ns, dp)
#   t.minobs <- min(t.round)
#   t.maxobs <- max(t.round)
#   t.exact <- (t.round == t.pub)
#   frac.exact <- sum(t.exact) / nsamp
#   return(list(t.pub, t.min, t.max, frac.exact, t.minobs, t.maxobs))
# }
#
#
# #
# # perfectBSE <- function (b, se, dp=2, nsamp=1000000) {
# #   p10 <- 10 ^ dp
# #   b.r <- round(b, dp)
# #   se.r <- round(se, dp)
# #
# #   # t.pub is the statistic we will get from the rounded b and SE.
# #   t.pub <- round(b.r / se.r, dp)
# #
# #   # Calculate the minimum and maximum t statistics.
# #   d <- 0.49999999 / (10 ^ dp)
# #   b.r.min <- b.r - (d * sign(b.r))
# #   se.r.min <- se.r + (d * sign(se.r))
# #   b.r.max <- b.r + (d * sign(b.r))
# #   se.r.max <- se.r - (d * sign(se.r))
# #   t.min <- round(b.r.min / se.r.min, dp)
# #   t.max <- round(b.r.max / se.r.max, dp)
# #   if (sign(t.pub) == -1) {
# #     x <- t.max
# #     t.max <- t.min
# #     t.min <- x
# #   }
# #
# #   # Generate plausible unrounded b and SE values.
# #   b.ns <- rep(b.r, nsamp) + ((runif(nsamp) - 0.5) / p10)
# #   se.ns <- rep(se.r, nsamp) + ((runif(nsamp) - 0.5) / p10)
# #
# #   # Sanity check that generated numbers round to published ones.
# #   b.sc <- sum(round(b.ns, dp) - b.r)
# #   se.sc <- sum(round(se.ns, dp) - se.r)
# #   if ((b.sc != 0) || (se.sc != 0)) {
# #     stop("Bad rounding of b or SE")
# #   }
# #
# #   # Generate the t values from our "ragged" bs and SEs.
# #   t.ns <- b.ns / se.ns
# #
# #   # Round the t values and see what fraction many match the published one.
# #   t.round <- round(t.ns, dp)
# #   t.minobs <- min(t.round)
# #   t.maxobs <- max(t.round)
# #   t.exact <- (t.round == t.pub)
# #   frac.exact <- sum(t.exact) / nsamp
# #
# #   return(list(t.pub, t.min, t.max, frac.exact, t.minobs, t.maxobs))
# # }
# #
# # # Output a summary of the central/minimum/maximum t statistics (mean/SD version).
# # catFracMSD <- function (m1, sd1, n1, m2, sd2, n2, dp=2, nsamp=1000000, GRIMitems=0) {
# #   perf <- perfectMSD(m1, sd1, n1, m2, sd2, n2, dp, nsamp, GRIMitems)
# #   format <- paste("%.", dp, "f", sep="")
# #   cat("DP=", dp, " n1=", n1, " n2=", n2,
# #       " Matched=", sprintf(format, perf[[4]] * 100), "%",
# #       " tPerf=", sprintf(format, perf[[1]]),
# #       " tMin=", sprintf(format, perf[[2]]),
# #       " tMinObs=", sprintf(format, perf[[5]]),
# #       " tMax=", sprintf(format, perf[[3]]),
# #       " tMaxObs=", sprintf(format, perf[[6]]),
# #       "\n", sep="")
# # }
# #
# # # Output a summary of the central/minimum/maximum t statistics (b/SE version).
# # catFracBSE <- function (b, se, dp=2, nsamp=1000000) {
# #   perf <- perfectBSE(b, se, dp, nsamp)
# #   format <- paste("%.", dp, "f", sep="")
# #   cat("DP=", dp,
# #       " Matched=", sprintf("%.4f", perf[[4]]),    # as fraction to 4dp
# #       " tPerf=", sprintf(format, perf[[1]]),
# #       " tMin=", sprintf(format, perf[[2]]),
# #       " tMinObs=", sprintf(format, perf[[5]]),
# #       " tMax=", sprintf(format, perf[[3]]),
# #       " tMaxObs=", sprintf(format, perf[[6]]),
# #       "\n", sep="")
# # }
# #
# # # Brown and Heathers, RIVETS manuscript, Tables 1 and 2.
# # #set.seed(1)
# # #nsamp <- 1000000
# # #catFracMSD(4.53, 1.07, 55, 3.73, 0.92, 53, 2, nsamp, 0)
# # #catFracMSD(8.18, 2.12, 55, 9.57, 1.89, 53, 2, nsamp, 0)
# # #catFracMSD(1.77, 0.29, 55, 1.38, 0.34, 53, 2, nsamp, 0)
# # #catFracMSD(1.47, 0.25, 55, 1.32, 0.33, 53, 2, nsamp, 0)
# # ## stop()
# # #
# # ## Stewart (2003), Tables 4 and 5.
# # #set.seed(1)
# # #catFracBSE(0.941, 0.019, dp=3)
# # #catFracBSE(-0.212, 0.031, dp=3)
# # #catFracBSE(-0.145, 0.042, dp=3)
# # #catFracBSE(-0.397, 0.039, dp=3)
# # #catFracBSE(-0.011, 0.014, dp=3)
# # #catFracBSE(-0.431, 0.039, dp=3)
# # #catFracBSE(-0.138, 0.043, dp=3)
# # #catFracBSE(-0.108, 0.047, dp=3)
# # #catFracBSE(0.032, 0.029, dp=3)
# # #catFracBSE(-0.102, 0.049, dp=3)
# # #catFracBSE(0.345, 0.048, dp=3)
# # #catFracBSE(0.099, 0.049, dp=3)
# # #catFracBSE(-0.103, 0.049, dp=3)
# # #catFracBSE(0.034, 0.029, dp=3)
# # #catFracBSE(0.013, 0.016, dp=3)
# # #catFracBSE(0.928, 0.021, dp=3)
# # #catFracBSE(0.039, 0.041, dp=3)
# # #catFracBSE(0.197, 0.042, dp=3)
# # #catFracBSE(0.017, 0.026, dp=3)
# # #catFracBSE(0.173, 0.044, dp=3)
# # #catFracBSE(0.032, 0.029, dp=3)
# # #catFracBSE(-0.035, 0.029, dp=3)
# # #catFracBSE(-0.212, 0.031, dp=3)
# # #catFracBSE(-0.145, 0.042, dp=3)
# # #catFracBSE(-0.397, 0.039, dp=3)
# # #catFracBSE(-0.011, 0.014, dp=3)
# # #catFracBSE(-0.431, 0.039, dp=3)
# # #catFracBSE(-0.138, 0.043, dp=3)
# # #catFracBSE(-0.108, 0.047, dp=3)
# # #catFracBSE(0.032, 0.029, dp=3)
# # #catFracBSE(-0.102, 0.049, dp=3)
# # #catFracBSE(0.345, 0.048, dp=3)
# # #catFracBSE(0.099, 0.049, dp=3)
# # #catFracBSE(-0.103, 0.049, dp=3)
# # #catFracBSE(0.034, 0.029, dp=3)
# # #catFracBSE(0.013, 0.016, dp=3)
