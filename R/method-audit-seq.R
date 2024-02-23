
#' @include mapper-function-helpers.R
#' @export

audit.scr_audit_seq <- function(data) {
  audit_summary_stats(
    data, selection = starts_with("hits_") | starts_with("diff_")
  )
}
