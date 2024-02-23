
#' @include duplicate-count-colpair.R
#' @export

audit.scr_dup_count_colpair <- function(data) {
  audit_summary_stats(data, -c("x", "y"))
}
