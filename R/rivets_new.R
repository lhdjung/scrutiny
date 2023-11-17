# #
# #
# # # Notes on Nick's functions -----------------------------------------------
# #
# # # `check.GRIM()` --> replace by scrutiny's GRIM implementation
# #
# # # `make.poss()` --> replace by `rivets_possible_values()`, a function that
# # calls `seq_endpoint()` (or `seq_distance()`), `grim()`, and `unround()`; or
# # simply by calls to these # functions, with those calls being internal to a
# # high-level, general RIVETS # function. In particular, replace the
# # determination of `m.min` and `m.max` by `unround()` calls, but keep the
# # `repeat` statements (essentially as they are) to subsequently determine the
# # granularity-adjusted minima and maxima.
# #
# #
# # # `perfectMSD()` --> rename to `rivets_perfect_mean_sd()`
# #
# # # `perfectBSE()` --> rename to `rivets_perfect_b_se()`
# #
# # # `catFracMSD()` --> rename to `rivets_summarize_mean_sd()` -- or `audit()`
# #
# # # `catFracBSE()` --> rename to `rivets_summarize_b_se()` -- or `audit()`
# #
# # # any other functions like `t.stat()`, which was rewritten as `rivets_t_test()`
# # # --> rename by the same pattern
#
# # # Replace the entire reliance on numbers just marginally different from 0.5 by
# # `unround()`, such that the `transgresses_at_*()` functions inside of
# # `rivets_possible_values()` automatically know when to stop -- before the bound
# # or right after it. Question: Is this implemented already? If so, the
# # granularity seems to be so high that `x_current` always remains at the
# # original value unless `n` is very high; on the order of five figures.
#
# #
# #
# # # RIVETS helper: t-test for two independent samples -----------------------
# #
# # rivets_t_test <- function (x1, sd1, n1, x2, sd2, n2) {
# #   p_var <- (((sd1 ^ 2) * (n1 - 1)) + ((sd2 ^ 2) * (n2 - 1))) / (n1 + n2 - 2)
# #   p_se <- sqrt((p_var / n1) + (p_var / n2))
# #
# #   (x1 - x2) / p_se
# # }
# #
#
#
#
# # This largely rewrites Nick's `make.poss()` function, but integrates it with
# # scrutiny's broader design (e.g., naming conventions) and infrastructure
# # (mainly unrounding):
#
#
# # Title
# #
# # @param x
# # @param n
# # @param items
# # @param length_test
# # @param rounding
# # @param threshold
# # @param digits
# #
# # @return
# # @export
# #
# # @examples
#
#
# rivets_possible_values <- function(x,
#                                    n,
#                                    items,
#                                    dispersion = 5,
#                                    rounding = "up_or_down",
#                                    threshold = 5,
#                                    symmetric = FALSE,
#                                    tolerance = .Machine$double.eps^0.5,
#                                    digits = NULL) {
#
#   # Checks ---
#
#   check_type(x, "character")
#
#   check_length(x, 1)
#   check_length(n, 1)
#   check_length(items, 1)
#
#   # Check if the arguments are GRIM-consistent...
#   grim_consistent <- grim_scalar(
#     x = x, n = n, items = items, percent = FALSE, show_rec = FALSE,
#     rounding = rounding, threshold = threshold, symmetric = symmetric,
#     tolerance = tolerance
#   )
#
#   # ...and, if they aren't, simply return a "handout" tibble with GRIM-tested
#   # mean values around `x`:
#   if (!grim_consistent) {
#     neighbor_sequence <- seq_distance_df(x, n = {{ n }}, .offset_from = -4)
#     cli::cli_warn(c(
#       "!" = "`x` ({x}) and `n` ({n}) are GRIM-inconsistent given the other \\
#       arguments in the `rivets_possible_values()` call.",
#       "i" = "The above tibble shows which values around {x} are \\
#       GRIM-consistent. It is returned here instead of any \\
#       RIVETS-informative output."
#     ))
#     return(grim_map(
#       neighbor_sequence, items = items, percent = FALSE, show_rec = FALSE,
#       rounding = rounding, threshold = threshold, symmetric = symmetric,
#       tolerance = tolerance
#     ))
#   }
#
#
#   # Main part ---
#
#   # The `+ 1` makes sure that, e.g., 2.5 gets unrounded to 2.495 and 2.505
#   # instead of 2.45 and 2.55.
#   digits <- decimal_places_scalar(x) + 1
#   # p10 <- 10 ^ digits
#
#   granularity <- 1 / (n * items)
#
#   x_unrounded <- unround(
#     x = x, rounding = rounding, threshold = threshold, digits = digits
#   )
#
#   x_lower      <- x_unrounded$lower
#   x_upper      <- x_unrounded$upper
#   x_incl_lower <- x_unrounded$incl_lower
#   x_incl_upper <- x_unrounded$incl_upper
#
#   # Trailing zeros are no longer needed, so from now on, `x` is only relevant in
#   # terms of its numeric value:
#   x <- as.numeric(x)
#
#   # What exactly it means to "transgress at" the limits reconstructed by
#   # unrounding depends on whether or not these limits are meant to be inclusive.
#   # The functions that will be used in the `repeat` loops below, therefore, are
#   # defined in dependence on the logical `x_incl_*` values. First, the lower
#   # bound...
#   if (x_incl_lower) {
#     transgresses_at_lower <- `<`
#   } else {
#     transgresses_at_lower <- `<=`
#   }
#
#   # ...and second, the upper bound:
#   if (x_incl_upper) {
#     transgresses_at_upper <- `>`
#   } else {
#     transgresses_at_upper <- `>=`
#   }
#
#
#   x_possible <- x
#
#   # Lower bound...
#   x_current <- x
#   repeat {
#     x_current <- x_current - granularity
#     if (transgresses_at_lower(x_current, x_lower)) {
#       break
#     }
#     x_possible <- c(x_possible, x_current)
#   }
#
#   # ... and upper bound:
#   x_current <- x
#   repeat {
#     x_current <- x_current + granularity
#     if (transgresses_at_upper(x_current, x_upper)) {
#       break
#     }
#     x_possible <- c(x_possible, x_current)
#   }
#
#   if (length(x_possible) == 1) {
#     x_possible <- c(x_possible, x_possible)
#   }
#
#
#   return(x_possible)
#
# }
#
#
# #
# #
# # # For the `make.poss()` replacement:
# #
# #
# # x <- unround("5.30")$x
# # lower <- unround("5.30")$lower
# # upper <- unround("5.30")$upper
# #
#
#
# # # For a later function that will convert formula inputs
# # # to argument vectors [BETTER NOT]:
# # string <- "(((sd1 ^ 2) * (n1 - 1)) + ((sd2 ^ 2) * (n2 - 1))) / (n1 + n2 - 2)"
# #
# # arguments <- string %>%
# #   stringr::str_replace_all("[:punct:]|[:symbol:]", " ") %>%
# #   stringr::str_remove_all(" [:digit:]") %>%
# #   stringr::str_remove_all("^[:digit:]|$[:digit:]") %>%
# #   stringr::str_squish() %>%
# #   stringr::str_split(" ") %>%
# #   unlist()
#
#
#
