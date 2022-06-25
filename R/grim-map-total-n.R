
#' GRIM-testing with hypothetical group sizes
#'
#' @description When reporting group means, some published studies only report
#'   the total sample size but no group sizes corresponding to each mean.
#'   However, group sizes are crucial for
#'   \href{https://lhdjung.github.io/scrutiny/articles/grim.html#handling-unknown-group-sizes-with-grim_map_total_n}{GRIM-testing}.
#'
#'   In the two-groups case, `grim_map_total_n()` helps in these ways:

#' - It creates hypothetical group sizes. With an even total sample size, it
#' incrementally moves up and down from half the total sample size. For example,
#' with a total sample size of 40, it starts at 20, goes on to 19 and 21, then
#' to 18 and 22, etc. With odd sample sizes, it starts from the two integers
#' around half.
#' - It GRIM-tests all of these values together with the group means.
#' - It reports all the scenarios in which both "dispersed" hypothetical group
#' sizes are GRIM-consistent with the group means.
#'
#' All of this works with one or more total sample sizes at a time. Call
#' `audit_total_n()` for summary statistics.

#' @param data Data frame with string columns `x1` and `x2`, and numeric column
#'   `n`. The first two are group mean or percentage values with unknown group
#'   sizes, and `n` is the total sample size. It is not very important whether a
#'   value is in `x1` or in `x2` because, after the first round of tests, the
#'   function switches roles between `x1` and `x2`, and reports the outcomes
#'   both ways.
#' @param dispersion Numeric. Steps up and down from half the `n` values.
#'   Default is `0:5`, i.e., half `n` itself followed by five steps up and down.
#' @param n_min Numeric. Minimal group size. Default is 1.
#' @param n_max Numeric. Maximal group size. Default is `NULL`, i.e., no
#'   maximum.
#' @param
#' items,percent,show_rec,show_prob,rounding,threshold,symmetric,tolerance,extra
#' Arguments passed down to `grim_map()` via `...` (the dots).

#' @include function-map-total-n.R
#'
#' @references Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It
#'   Light or Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://doi.org/10.1177/09567976211058727
#'
#'   Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple
#'   Technique Detects Numerous Anomalies in the Reporting of Results in
#'   Psychology. *Social Psychological and Personality Science*, 8(4), 363–369.
#'   https://doi.org/10.1177/1948550616673876

#' @seealso `function_map_total_n()`, which created the present function using
#'   `grim_map()`.

#' @return A tibble with these columns:
#' - `x`, the group-wise reported input statistic, is repeated in row pairs.
#' - `n` is dispersed from half the input `n`, with `n_change` tracking the
#' differences.
#' - `both_consistent` flags scenarios where both reported `x` values are
#' consistent with the hypothetical `n` values.
#' - `case` corresponds to the row numbers of the input data frame.
#' - `dir` is `"forth"` in the first half of rows and `"back"` in the second
#' half. `"forth"` means that `x2` from the input is paired with the larger
#' dispersed `n`, whereas `"back"` means that `x1` is paired with the larger
#' dispersed `n`.
#' - Other columns from `grim_map()` are preserved.

#' @section Summaries with `audit_total_n()`: You can call
#'   `audit_total_n()` following up on `grim_map_total_n()`
#'   to get a tibble with summary statistics. It will have these columns:
#'  - `x1`, `x2`, and `n` are the original inputs.
#'  - `hits_forth` is the number of scenarios in which both
#'  `x1` and `x2` are GRIM-consistent with the dispersed
#'  `n` values when `x2` is paired with the
#'  larger dispersed `n`.
#'  - `hits_back` is the same, except `x1` is
#'  paired with the larger dispersed `n` value.
#'  - `hits_total` is the sum of `hits_forth` and `hits_back`, i.e.,
#'  the total number of both-consistent scenarios.
#'  - `scenarios_total` is the total number of test scenarios,
#'  whether or not both `x1` and `x2` are consistent.
#'  - `hit_rate` is the ratio of `hits_total` to `scenarios_total`.

#' @export

#' @examples
#' # Run `grim_map_total_n()` on data like these:
#' df <- tibble::tribble(
#'   ~x1,    ~x2,   ~n,
#'   "3.43", "5.28", 90,
#'   "2.97", "4.42", 103
#' )
#' df
#'
#' grim_map_total_n(df)
#'
#' # `audit_total_n()` summaries can be more important than
#' # the detailed results themselves.
#' # The `hits_total` column shows all scenarios in
#' # which both divergent `n` values are GRIM-consistent
#' # with the `x*` values when paired with them both ways:
#' df %>%
#'   grim_map_total_n() %>%
#'   audit_total_n()
#'
#' # By default (`dispersion = 0:5`), the function goes
#' # five steps up and down from `n`. If this sequence
#' # gets longer, the number of hits tends to increase:
#' df %>%
#'   grim_map_total_n(dispersion = 0:10) %>%
#'   audit_total_n()



grim_map_total_n <- function_map_total_n(
  .fun = grim_map,
  .reported = "x",
  .name_test = "GRIM"
)





# grim_map_total_n <- function(data, dispersion = 0:5, n_min = 1, n_max = NULL,
#                               x1 = NULL, x2 = NULL, show_all = FALSE,
#                               items = 1, percent = FALSE, show_rec = FALSE,
#                               show_prob = FALSE, rounding = "up_or_down",
#                               threshold = 5, symmetric = FALSE,
#                               tolerance = .Machine$double.eps^0.5,
#                               extra = Inf) {
#
#   x1 <- rlang::enexpr(x1)
#   x2 <- rlang::enexpr(x2)
#
#   # Checks ---
#
#   if (!is.null(x1)) {
#     data <- data %>%
#       dplyr::rename(x1 = {{ x1 }})
#   } else if (!("x1" %in% colnames(data))) {
#     cli::cli_abort(c(
#       "`x1` column missing",
#       ">" = "The column in `data` with the `x` value that hypothetically \\
#       corresponds to the lower `n` value needs to be named `x1`, \\
#       or else specify the `x1` argument as the name of that column."
#     ))
#   }
#
#   if (!is.null(x2)) {
#     data <- data %>%
#       dplyr::rename(x2 = {{ x2 }})
#   } else if (!("x2" %in% colnames(data))) {
#     cli::cli_abort(c(
#       "`x2` column missing",
#       ">" = "The column in `data` with the `x` value that hypothetically \\
#       corresponds to the higher `n` value needs to be named `x2`, \\
#       or else specify the `x2` argument as the name of that column."
#     ))
#   }
#
#   if (!all(is_whole_number(data$n))) {
#     offenders <- data$n[!is_whole_number(data$n)]
#     if (length(offenders) > 3) {
#       offenders <- offenders[1:3]
#       msg_offenders <- ", starting with"
#     } else {
#       msg_offenders <- ":"
#     }
#     cli::cli_abort(c(
#       "`n` values must be whole numbers",
#       "x" = "The `n` column includes decimal \\
#       numbers{msg_offenders} {offenders}.",
#       "i" = "They are supposed to be (total) sample sizes."
#     ))
#   }
#
#
#   # Main part ---
#
#   data_forth <- data
#
#   data_back <- data %>%
#     dplyr::mutate(
#       temp = x2,
#       x2 = x1,
#       x1 = temp,
#       temp = NULL
#     )
#
#   out_forth <- proto_grim_map_total_n(
#     data = data_forth, dispersion = dispersion, n_min = n_min, n_max = n_max,
#     x1 = x1, x2 = x2, items = items, percent = percent,
#     show_rec = show_rec, show_prob = show_prob, rounding = rounding,
#     threshold = threshold, symmetric = symmetric, tolerance = tolerance,
#     extra = extra
#   )
#
#   out_back <- proto_grim_map_total_n(
#     data = data_back, dispersion = dispersion, n_min = n_min, n_max = n_max,
#     x1 = x1, x2 = x2, items = items, percent = percent,
#     show_rec = show_rec, show_prob = show_prob, rounding = rounding,
#     threshold = threshold, symmetric = symmetric, tolerance = tolerance,
#     extra = extra
#   )
#
#   # Isolate the number of both-consistent scenarios for each pairing...
#   hits_forth <- out_forth[[1]]$hits
#   hits_back <- out_back[[1]]$hits
#
#   # ...and sum them up:
#   hits_total <- hits_forth + hits_back
#
#   # Create an overall summary tibble with the input values and the key results:
#   out_summary <- data %>%
#     dplyr::mutate(hits_total, hits_forth, hits_back)
#
#
#   if (show_all) {
#
#     # Detailed results are prefixed with an explanation of the various tibbles:
#     cli::cli_inform(c(
#       "i" = "Explanation:",
#       "*" = "Tibble [[1]] summarizes test results.",
#       "*" = "Tibbles under [[2]] and [[3]] contain detailed analyses, \\
#       with one tibble for each row in the input data frame.",
#       "*" = "Those under [[2]] correspond to `hits_forth`, \\
#       where `x2` is paired with the larger group.",
#       "*" = "Those under [[3]] correspond to `hits_back`, \\
#       where `x1` is paired with the larger group.",
#       ">" = "(A \"hit\" is a scenario in which both dispersed `n` values \\
#       are GRIM-consistent with their corresponding `x*` values.)"
#     ))
#
#     # Remove the summary tibbles on top of each `proto_grim_map_total_n()`
#     # output list; they are no longer needed:
#     out_forth <- out_forth[[-1]]
#     out_back <- out_back[[-1]]
#
#     # Return results in a list with the overall summary tibble on top, followed
#     # by the `out_forth` and `out_back` lists of tibbles with detailed results:
#     return(list(out_summary, out_forth, out_back))
#
#   } else {
#
#     # With the default `show_all = FALSE`, no lists with detailed results are
#     # returned, only the overall summary tibble:
#     return(out_summary)
#   }
#
# }


