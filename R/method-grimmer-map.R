#' @export

audit.scrutiny_grimmer_map <- function(data) {
  out <- data |>
    audit_cols_minimal("GRIMMER")
  # Output with no rows has no `reason` column either way (see
  # `write_result_cols()`), but there is nothing to count, so all counts are 0:
  if (any(colnames(data) == "reason") || nrow(data) == 0L) {
    reason <- data[["reason"]]
    reason <- reason[!is.na(reason)]
    fail_grim <- length(reason[stringr::str_detect(
      reason,
      "GRIM inconsistent"
    )])
    fail_test1 <- length(reason[stringr::str_detect(reason, "test 1")])
    fail_test2 <- length(reason[stringr::str_detect(reason, "test 2")])
    fail_test3 <- length(reason[stringr::str_detect(reason, "test 3")])
    # Zero unless `min_val` and `max_val` were specified:
    fail_scale <- length(reason[stringr::str_detect(reason, "scale range")])
    out <- out |>
      dplyr::mutate(
        fail_grim,
        fail_test1,
        fail_test2,
        fail_test3,
        fail_scale
      )
  } else {
    cli::cli_alert(
      "In `grimmer_map()`, set `show_reason` to `TRUE` so that \\
      `audit()` will count the reasons for inconsistencies."
    )
  }
  out
}
