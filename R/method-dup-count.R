
#' @include duplicate-count.R
#' @export

audit.scr_dup_count <- function(data) {
  audit_summary_stats(data, c(count, locations_n))
}

