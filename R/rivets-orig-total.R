#
#
# rivets_orig_total <- function(x, n, items = 1, percent = FALSE) {
#
#   decimals <- decimal_places(x)
#   if (percent) decimals <- decimals + 2
#
#   res <- (n * items) / (10 ^ decimals)
#
#   res %>%
#     trunc() %>%
#     as.integer()
# }
#
