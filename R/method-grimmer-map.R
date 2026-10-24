#' @export

audit.scrutiny_grimmer_map <- function(data) {
  out <- data |>
    audit_cols_minimal("GRIMMER")
  # Output with no rows has no `reason` column either way (see
  # `write_result_cols()`), but there is nothing to count, so all counts are 0:
  if (any(colnames(data) == "reason") || nrow(data) == 0L) {
    reason <- data[["reason"]]
    reason <- reason[!is.na(reason)]
    # Under `sd_type = "either"`, a reason may name the sample SD's failure and
    # the population SD's. Both failed, so either could count; the sample SD's
    # does, as under the default `sd_type`, so that each case counts once:
    reason <- sub("^Sample SD: (.*); population SD: .*$", "\\1", reason)
    fail_grim <- length(reason[stringr::str_detect(
      reason,
      "GRIM inconsistent"
    )])
    fail_test1 <- length(reason[stringr::str_detect(reason, "test 1")])
    fail_test3 <- length(reason[stringr::str_detect(reason, "test 3")])
    out <- out |>
      dplyr::mutate(
        fail_grim,
        fail_test1,
        fail_test3
      )
  }
  out
}
