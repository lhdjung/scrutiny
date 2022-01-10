
# # [DOESN'T WORK] Anaya's variance function, translated into R:
# variance <- function(data) {
#   u <- sum(data) / length(data)
#
#   ((data - u) ^ 2) / length(data)
# }
#
# # Irrelevant:
# func_for_combination_rows <- function(combinations, digits) {}
#
# if (any(combinations == n - 1)) {}
#
#
# purrr::pmap(combinations, )


# # My own NSE variance function:
# variance_dots <- function(...) {
#   numbers <- rlang::enexprs(...)
#   numbers <- purrr::map_dbl(numbers, rlang::eval_bare)
#
#   var(numbers)
# }
#
# # My own example:
# data <- combine_elements(0:4, 5)
#
#
# # Are these the GRIMMER variances?
# variances <- data %>%
#   purrr::pmap_dbl(variance_dots)
#
#
# # An *attempt* at a GRIMMER function to generate variances:
# grimmer_variances <- function(x, n) {
#   combine_elements(x, n) %>%
#     purrr::pmap_dbl(variance_dots) %>%
#     sort() %>%
#     unique()
# }


