
#' Reverse and summarize the `*_map_total_n()` process
#'
#' @description Two functions that deal with the output of a function created by
#'   `function_map_total_n()`:
#'
#'   - `reverse_map_total_n()` reconstructs the data frame of reported
#'   statistics which the manufactured function took as an input.
#'
#'   - `summarize_map_total_n()` computes summaries of the output of the
#'   manufactured function and attaches them to the reconstructed input. This
#'   can be useful as a helper within `audit()` methods for classes created by
#'   `function_map_total_n()`. Indeed, recommended use for such methods is to
#'   only consist of a call to `summarize_map_total_n()`.

#' @param data Data frame.
#'
#' @return A tibble (data frame). For an explanation of
#'   `summarize_map_total_n()`'s output using the example of GRIM, see
#'   `grim_map_total_n()`, section *Summaries with `audit()`*.
#'
#' @export
#'
#' @examples
#' # Original data frame:
#' df <- tibble::tribble(
#'   ~x1,    ~x2,   ~n,
#'   "3.43", "5.28", 90,
#'   "2.97", "4.42", 103
#' )
#'
#' # Test it using a product of the
#' # `function_map_total_n()` factory:
#' df_tested <- grim_map_total_n(df)
#' df_tested
#'
#' # Reconstruct the original data frame:
#' reverse_map_total_n(df_tested)
#'
#' # Compute summary statistics:
#' summarize_map_total_n(df_tested)


reverse_map_total_n <- function(data) {

  # Take the first row of each original-`n` block:
  data_reduced <- data %>%
    dplyr::group_by(case) %>%
    dplyr::slice(1:2) %>%
    dplyr::ungroup()

  # `n_was_odd` is `1` if the original, total `n` was odd, and `0` otherwise. In
  # this way, when `n_was_odd` is later added to `2 * n`, it completes the
  # restoration of the original total:
  n_was_odd <- data_reduced$n_change %>%
    stringr::str_detect("n2") %>%
    as.numeric()

  data_reduced <- data_reduced %>%
    dplyr::mutate(
      n_was_odd, .after = n,
      n = (2 * n) - n_was_odd
    )

  nrow_data_reduced <- nrow(data_reduced)

  locations1 <- seq(from = 1, to = nrow_data_reduced - 1, by = 2)
  locations2 <- seq(from = 2, to = nrow_data_reduced,     by = 2)

  data1 <- data_reduced %>% dplyr::slice(locations1)
  data2 <- data_reduced %>% dplyr::slice(locations2)

  # Number of columns before `n` (i.e., the columns with hypothetical values
  # dispersed from the reported statistics):
  ncol_before_n <- match("n", colnames(data)) - 1

  colnames_reported <- colnames(data_reduced)[1:ncol_before_n]

  data_reported_1 <- data1[, colnames_reported]
  data_reported_2 <- data2[, colnames_reported]

  colnames(data_reported_1) <- paste0(colnames_reported, "1")
  colnames(data_reported_2) <- paste0(colnames_reported, "2")

  colnames_in_order <- colnames_reported %>%
    rep(each = 2) %>%
    paste0(c("1", "2"))

  out <- dplyr::bind_cols(data_reported_1, data_reported_2)

  out <- out %>%
    dplyr::relocate(all_of(colnames_in_order)) %>%
    dplyr::mutate(n = data2$n)

  return(out)
}



#' @rdname reverse_map_total_n
#' @export

summarize_map_total_n <- function(data) {

  df_list <- split(data, data$case)

  df_list_hits <- df_list %>%
    purrr::map(dplyr::filter, both_consistent)

  hits_forth <- df_list_hits %>%
    purrr::map(dplyr::filter, dir == "forth") %>%
    purrr::map_int(nrow) %>%
    `/`(2)

  hits_back <- df_list_hits %>%
    purrr::map(dplyr::filter, dir == "back") %>%
    purrr::map_int(nrow) %>%
    `/`(2)

  hits_total <- hits_forth + hits_back

  scenarios_total <- df_list %>%
    purrr::map_int(nrow) %>%
    `/`(2)

  hit_rate <- hits_total / scenarios_total

  data_rec <- reverse_map_total_n(data)

  out <- data_rec %>%
    dplyr::mutate(hits_forth, hits_back, hits_total, scenarios_total, hit_rate)

  return(out)
}

