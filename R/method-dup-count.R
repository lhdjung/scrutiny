
#' @include duplicate-count.R
#' @export

audit.scr_dup_count <- function(data) {
  if ("locations_n" %in% colnames(data)) {
    audit_summary_stats(data, c("count", "locations_n"))
  } else {
    audit_summary_stats(data, "count")
  }
}
