#
#
# # Notes on Nick's functions -----------------------------------------------
#
# # `check.GRIM()` --> replace by scrutiny's GRIM implementation
#
# # `make.poss()` --> replace by a function that calls `seq_endpoint()` (or
# # `seq_distance()`), `grim()`, and `unround()`; or simply by calls to these
# # functions, with those calls being internal to a high-level, general RIVETS
# # function
#
# # `perfectMSD()` --> rename to `rivets_perfect_mean_sd()`
#
# # `perfectBSE()` --> rename to `rivets_perfect_b_se()`
#
# # `catFracMSD()` --> rename to `rivets_summarize_mean_sd()`
#
# # `catFracBSE()` --> rename to `rivets_summarize_b_se()`
#
# # any other functions like `t.stat()`, which was rewritten as `rivets_t_test()`
# # --> rename by the same pattern
#
#
#
# # RIVETS helper: t-test for two independent samples -----------------------
#
# rivets_t_test <- function (x1, sd1, n1, x2, sd2, n2) {
#   p_var <- (((sd1 ^ 2) * (n1 - 1)) + ((sd2 ^ 2) * (n2 - 1))) / (n1 + n2 - 2)
#   p_se <- sqrt((p_var / n1) + (p_var / n2))
#
#   (x1 - x2) / p_se
# }
#
#
# rivets_possible_values <- function(x, n, items, length_test = 10,
#                                    rounding = "up_or_down", threshold = 5,
#                                    digits = NULL) {
#
#   if (!is.character(x)) {
#     cli::cli_abort(c(
#       "`x` is {an_a_type(x)}",
#       "x" = "It needs to be a string."
#     ))
#   }
#
#   n_items <- n * items
#   granularity <- 1 / (n_items)
#
#   x_min <- x - granularity
#   x_max <- x + granularity
#
#   x_unrounded <- unround(x = x, rounding = rounding, threshold = threshold,
#                          digits = digits)
#
#   x_lower <- x_unrounded$lower
#   x_upper <- x_unrounded$upper
#
#   vec <- seq_endpoint(x_min, x_max)
#
#   # vec <- seq_distance(x, length_out = length_test)
#   vec <- grim(vec, n = n)
#   vec <- vec[vec]  # only retains `TRUE` values
#
# }
#
#
# # For the `make.poss()` replacement:
#
# # # All of this is just `m - gran`:
# # m.cur <- m
# # repeat {
# #   m.cur <- m.cur - gran
# #   if (m.cur < m.min) {
# #     break
# #   }
# #   m.poss <- c(m.poss, m.cur)
# # }
#
#
# x <- unround("5.30")$x
# lower <- unround("5.30")$lower
# upper <- unround("5.30")$upper
#


# # For a later function that will convert formula inputs
# # to argument vectors [BETTER NOT]:
# string <- "(((sd1 ^ 2) * (n1 - 1)) + ((sd2 ^ 2) * (n2 - 1))) / (n1 + n2 - 2)"
#
# arguments <- string %>%
#   stringr::str_replace_all("[:punct:]|[:symbol:]", " ") %>%
#   stringr::str_remove_all(" [:digit:]") %>%
#   stringr::str_remove_all("^[:digit:]|$[:digit:]") %>%
#   stringr::str_squish() %>%
#   stringr::str_split(" ") %>%
#   unlist()



