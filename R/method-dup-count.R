
#' @include duplicate-count.R
#' @export

audit.scr_dup_count <- function(data) {
  if (any(colnames(data) == "locations_n")) {
    audit_summary_stats(data, c("frequency", "locations_n"))
  } else {
    audit_summary_stats(data, "frequency")
  }
}
