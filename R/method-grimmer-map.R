

#' @export

audit.scr_grimmer_map <- function(data) {
  out <- audit_cols_minimal(data, "GRIMMER")
  if ("reason" %in% colnames(data)) {
    reason <- data$reason[!is.na(data$reason)]
    fail_grim  <- length(reason[stringr::str_detect(reason, "GRIM inconsistent")])
    fail_test1 <- length(reason[stringr::str_detect(reason, "test 1")])
    fail_test2 <- length(reason[stringr::str_detect(reason, "test 2")])
    fail_test3 <- length(reason[stringr::str_detect(reason, "test 3")])
    out <- dplyr::mutate(out, fail_grim, fail_test1, fail_test2, fail_test3)
  } else {
    cli::cli_alert(
      "In `grimmer_map()`, set `show_reason` to `TRUE` so that \\
      `audit()` will count the reasons for inconsistencies."
    )
  }
  return(out)
}

